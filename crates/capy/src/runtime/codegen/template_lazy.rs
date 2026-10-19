//! Lazily materialized template-interpreter handlers.

use std::io;
use std::sync::LazyLock;

use bytecodegen::OperandWidth;
use parking_lot::Mutex;

use super::image::LoadedImage;
use super::masm::{Masm, RuntimeExtern, SymbolKind};
use super::stubs;
use super::template::emit;
use super::template::{opcode_name, parse_bail_filter};
use super::template_image::{declare_slow_paths, resolve_slow_path};
use crate::bytecode::def::{self, OpcodeId};
use crate::runtime::interp::markers;
use crate::runtime::interp::template_interp::{CodeAddr, InterpreterTemplateContext};
use crate::runtime::sabi;

const WIDTH_NAMES: [&str; 3] = ["narrow", "wide16", "wide32"];

struct State {
    done: Vec<bool>,
    forced: Option<[bool; 256]>,
    images: Vec<LoadedImage>,
    ranges: Vec<(u64, u64)>,
}

unsafe impl Send for State {}
unsafe impl Sync for State {}

static STATE: LazyLock<Mutex<State>> = LazyLock::new(|| {
    Mutex::new(State {
        done: vec![false; 3 * 256],
        forced: None,
        images: Vec::new(),
        ranges: Vec::new(),
    })
});

/// Build the real handler for the `(width, byte)` map slot and patch the
/// slot to it.
pub(crate) fn materialize(width: u8, byte: u8) {
    if width > 2 {
        return;
    }
    let slot = width as usize * 256 + byte as usize;
    let mut state = STATE.lock();
    if state.done[slot] {
        return;
    }
    state.done[slot] = true;
    if byte == u8::MAX || byte == def::WIDE16_PREFIX || byte == def::WIDE32_PREFIX {
        return;
    }
    let forced = *state.forced.get_or_insert_with(parse_bail_filter);
    if forced[byte as usize] {
        return;
    }
    if stubs::bail_oneway() {
        return;
    }
    let Some(op) = OpcodeId::from_u8(byte) else {
        return;
    };
    let (Some(map_base), Some(image)) = (
        super::template_image::map_base(),
        super::template_image::loaded(),
    ) else {
        return;
    };
    let interp_return = markers::interpreter_return() as usize;
    match build_satellite(width as usize, byte, op, map_base, interp_return) {
        Ok((satellite, start, end)) => {
            let base = image.base().as_usize();
            let at = map_base + slot * 8 - base;
            if let Err(err) = image.patch_live_u64(at, start as u64) {
                log::warn!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "template: cannot publish lazy {} handler: {err}",
                    WIDTH_NAMES[width as usize],
                );
                return;
            }
            log::debug!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "template: materialized {}.{} at {start:p}",
                opcode_name(byte),
                WIDTH_NAMES[width as usize],
            );
            state.ranges.push((start as u64, end as u64));
            state.images.push(satellite);
        }
        Err(err) => {
            log::warn!(
                target: crate::heap::logging::JIT_LOG_TARGET,
                "template: cannot materialize {}.{} handler: {err}",
                opcode_name(byte),
                WIDTH_NAMES[width as usize],
            );
        }
    }
}

/// Whether `addr` lies in a materialized satellite handler, for profiler
/// attribution alongside the main template image range.
pub(crate) fn contains(addr: u64) -> bool {
    STATE
        .lock()
        .ranges
        .iter()
        .any(|&(start, end)| addr >= start && addr < end)
}

/// Emit one handler with absolute map/return addresses and load it as its
/// own image. Returns the image and the handler's address range.
fn build_satellite(
    width: usize,
    byte: u8,
    op: OpcodeId,
    map_base: usize,
    interp_return: usize,
) -> io::Result<(LoadedImage, *const u8, *const u8)> {
    let mut masm = Masm::new();
    let bail = masm.declare_rust_fn(RuntimeExtern::Bail);
    let paths = declare_slow_paths(&mut masm);
    let start = masm.label();
    masm.bind(start);
    stubs::emit_op_count(&mut masm, width, byte, stubs::op_stats_enabled());
    let mut context = InterpreterTemplateContext {
        width: OperandWidth::from_index(width as u8),
        op,
        map: CodeAddr::Absolute(map_base),
        interpreter_return: CodeAddr::Absolute(interp_return),
        bail,
        paths,
    };
    emit::emit_template(&mut masm, &mut context, op);
    let end = masm.label();
    masm.bind(end);
    masm.cfi_region(start, end, sabi::SCHEME_ENTRY_CFA_OFFSET as u32);
    let name = format!("{}.{}", opcode_name(byte), WIDTH_NAMES[width],);
    masm.define_symbol(&name, start, end, SymbolKind::Handler);
    let image = masm.finish().map_err(io::Error::other)?;
    let start_off = image
        .finished
        .offset(start)
        .expect("the satellite handler is bound") as usize;
    let end_off = image
        .finished
        .offset(end)
        .expect("the satellite end is bound") as usize;
    let loaded = super::image::load(image, |name| resolve_slow_path(name))?;
    let base = loaded.base().as_usize();
    Ok((
        loaded,
        (base + start_off) as *const u8,
        (base + end_off) as *const u8,
    ))
}
