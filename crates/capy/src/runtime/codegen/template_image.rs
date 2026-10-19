//! The template interpreter generator
use std::io;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use onlineasm::{Label, onlineasm};

use super::image::{self, LoadedImage, TablePatch};
use super::masm::{Masm, RuntimeExtern, SymbolKind};
use super::stubs;
use super::template::emit;
use super::template::{SlowPaths, parse_bail_filter};
use crate::bytecode::def::{self, OpcodeId, WIDE16_PREFIX, WIDE32_PREFIX};
use crate::runtime::interp::markers::{self, MarkerValues};
use crate::runtime::interp::stack::Slot;
use crate::runtime::interp::template_interp::{
    CodeAddr, InterpreterTemplateContext, MAP_ENTER_SLOT, MAP_TABLE_SLOTS,
};
use crate::runtime::sabi;
use crate::runtime::value::NativeReturn;
use crate::runtime::{Engine, MutationContext};

/// Proves the template consts are reachable from the generator. Decoding
/// goes through them. Bail handlers validate widths through `LEN` below.
const _: () = {
    assert!(def::OpCall::OPERAND_ARGC == 0);
    assert!(def::OpCall::OPERAND_BASE == 1);
    assert!(def::OpReturn::OPERAND_SRC == 0);
    assert!(def::OpAdd::OPERAND_DST == 0);
    assert!(def::OpAdd::OPERAND_LHS == 1);
    assert!(def::OpAdd::OPERAND_RHS == 2);
    assert!(def::OpReturnPoint::OPERAND_DST == 3);
};

static TEMPLATE_INTERPRETER: OnceLock<TemplateInterpreter> = OnceLock::new();

pub struct TemplateInterpreter {
    image: LoadedImage,
    scheme_entry: *const u8,
    gen_micros: u128,
    handler_kinds: [[HandlerKind; 256]; 3],
    /// Absolute address of the opcode map
    map_base: usize,
}

/// How one opcode-map slot's handler was emitted.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum HandlerKind {
    /// No handler: the map slot holds `op_invalid`.
    None,
    /// A real template (`emit::emit_template`).
    Template,
    /// The generic one-instruction bail (only `op_invalid` and opcodes forced
    /// by `CAPY_TEMPLATE_BAIL`).
    Bail,
}

// The image is written once before publication and only read afterwards.
unsafe impl Send for TemplateInterpreter {}
unsafe impl Sync for TemplateInterpreter {}

/// Generate and load the interpreter image once, install its markers, and log
/// the generation time and size.
pub fn ensure_loaded() -> &'static TemplateInterpreter {
    TEMPLATE_INTERPRETER.get_or_init(|| {
        let start = std::time::Instant::now();
        let (image, handler_kinds, map_base) =
            generate().expect("template: failed to generate interpreter image");
        let gen_micros = start.elapsed().as_micros();
        let scheme_entry = image
            .symbol_addr("scheme_entry")
            .expect("template: scheme_entry missing");

        install_markers(&image);
        stubs::maybe_report_bail_stats();
        stubs::maybe_report_op_stats();
        super::gc::maybe_report_alloc_stats();
        super::jit::maybe_report_jit_stats();

        TemplateInterpreter {
            image,
            scheme_entry,
            gen_micros,
            handler_kinds,
            map_base,
        }
    })
}

/// The loaded image's opcode-map address
pub(crate) fn map_base() -> Option<usize> {
    TEMPLATE_INTERPRETER.get().map(|t| t.map_base)
}

/// How each opcode-map handler was emitted: `[width][byte]`.
pub fn handler_kinds() -> Option<&'static [[HandlerKind; 256]; 3]> {
    TEMPLATE_INTERPRETER.get().map(|t| &t.handler_kinds)
}

/// The loaded image, if TemplateInterpreter has been initialized.
pub fn loaded() -> Option<&'static LoadedImage> {
    TEMPLATE_INTERPRETER.get().map(|t| &t.image)
}

/// The loaded image's machine range, for classifying sampled `RIP`s.
/// `None` until the image loads.
pub(crate) fn code_range() -> Option<(u64, u64)> {
    TEMPLATE_INTERPRETER.get().map(|t| {
        let base = t.image.base().as_usize() as u64;
        (base, base.wrapping_add(t.image.code_size() as u64))
    })
}

/// The generated `interpreter_enter_cp` glue, installed with the markers: the
/// machine entry of every interpreted closure. Every caller that jumps
/// through `closure.code` presets `CP`, so closures skip the `CP` reload at
/// the top of `interpreter_enter` (which `scheme_entry` still needs).
static INTERPRETER_ENTER: AtomicUsize = AtomicUsize::new(0);

fn set_interpreter_enter(addr: *const u8) {
    INTERPRETER_ENTER.store(addr as usize, Ordering::Relaxed);
}

/// The machine entry for new closures, once the image is loaded.
pub(crate) fn interpreter_enter_if_loaded() -> Option<*const u8> {
    let addr = INTERPRETER_ENTER.load(Ordering::Relaxed);
    (addr != 0).then_some(addr as *const u8)
}

/// The generated `subr_call` glue, installed with the markers: the machine
/// entry of every native closure. Native calls link like bytecode calls and
/// land here through `jump [CP + CLOSURE_CODE]`.
static SUBR_CALL: AtomicUsize = AtomicUsize::new(0);

fn set_subr_call(addr: *const u8) {
    SUBR_CALL.store(addr as usize, Ordering::Relaxed);
}

/// The machine entry for new native closures, once the image is loaded.
pub(crate) fn subr_call_if_loaded() -> Option<*const u8> {
    let addr = SUBR_CALL.load(Ordering::Relaxed);
    (addr != 0).then_some(addr as *const u8)
}

/// The generated `generic_call` entry: the machine entry of generic
/// function closures, in front of `subr_call`.
static GENERIC_CALL: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn generic_call_if_loaded() -> Option<*const u8> {
    let addr = GENERIC_CALL.load(Ordering::Relaxed);
    (addr != 0).then_some(addr as *const u8)
}

/// Run the activation at `fp` under TemplateInterpreter: call the generated `scheme_entry`.
pub unsafe fn run_template<'gc>(
    mutator: MutationContext<'gc>,
    fp: Slot<'gc>,
    argc: usize,
) -> NativeReturn<'gc> {
    // SAFETY: the image's `scheme_entry` is generated with this signature,
    // and the returned values live in the mutator's heap, so shortening
    // them to 'gc is sound. The caller upholds `fp`/`argc` (unsafe fn).
    unsafe {
        let template_interpreter = ensure_loaded();
        let entry: extern "C" fn(*const (), Slot, usize) -> NativeReturn<'static> =
            std::mem::transmute(template_interpreter.scheme_entry);
        std::mem::transmute::<NativeReturn<'static>, NativeReturn<'gc>>(entry(
            mutator.as_ptr(),
            fp,
            argc,
        ))
    }
}

/// Whether this thread runs Scheme through the generated image.
pub fn runs_template(mutator: MutationContext<'_>) -> bool {
    mutator.state().engine == Engine::Template
}

fn install_markers(image: &LoadedImage) {
    let addr = |name: &str| {
        image
            .symbol_addr(name)
            .unwrap_or_else(|| panic!("template: marker {name} missing")) as usize
    };
    stubs::set_interpreter_resume(addr("interp_resume") as *const u8);
    stubs::set_exit_glue(addr("exit_glue") as *const u8);
    set_interpreter_enter(addr("interp_enter_cp") as *const u8);
    set_subr_call(addr("subr_call") as *const u8);
    GENERIC_CALL.store(addr("generic_call"), Ordering::Relaxed);
    markers::set_markers(&MarkerValues {
        interpreter_return: addr("interp_return"),
        scheme_exit: addr("scheme_exit"),
        underflow: addr("underflow"),
        subr_ret: addr("subr_ret"),
        raise_ret: addr("raise_ret"),
    });
}

/// Declare the shared slow paths in `masm` (resolved by
/// [`resolve_slow_path`] at load).
pub(crate) fn declare_slow_paths(masm: &mut Masm) -> SlowPaths {
    SlowPaths {
        hot_enter: masm.declare_rust_fn(RuntimeExtern::HotEnter),
        yield_loop: masm.declare_rust_fn(RuntimeExtern::YieldLoop),
        hot_loop: masm.declare_rust_fn(RuntimeExtern::HotLoop),
        overflow: masm.declare_rust_fn(RuntimeExtern::Overflow),
        arity: masm.declare_rust_fn(RuntimeExtern::Arity),
        call_slow: masm.declare_rust_fn(RuntimeExtern::CallSlow),
        return_mv: masm.declare_rust_fn(RuntimeExtern::ReturnMv),
        raise: masm.declare_rust_fn(RuntimeExtern::Raise),
        raise_kind: masm.declare_rust_fn(RuntimeExtern::RaiseKind),
        number_arith: masm.declare_rust_fn(RuntimeExtern::NumberArith),
        compare_jump: masm.declare_rust_fn(RuntimeExtern::CompareJump),
        vector_op: masm.declare_rust_fn(RuntimeExtern::VectorOp),
        vector_length: masm.declare_rust_fn(RuntimeExtern::VectorLength),
        prim_n: masm.declare_rust_fn(RuntimeExtern::PrimN),
        prim_error: masm.declare_rust_fn(RuntimeExtern::PrimError),
        get_slot: masm.declare_rust_fn(RuntimeExtern::GetSlot),
        put_slot: masm.declare_rust_fn(RuntimeExtern::PutSlot),
        runtime_proc: masm.declare_rust_fn(RuntimeExtern::RuntimeProc),
        nop: masm.declare_rust_fn(RuntimeExtern::Nop),
    }
}

fn generate() -> io::Result<(LoadedImage, [[HandlerKind; 256]; 3], usize)> {
    let mut masm = Masm::new();
    let image_start = masm.label();
    masm.bind(image_start);

    let bail = masm.declare_rust_fn(RuntimeExtern::Bail);
    let overflow_path = masm.declare_rust_fn(RuntimeExtern::Overflow);
    let arity_path = masm.declare_rust_fn(RuntimeExtern::Arity);
    let subr_call_path = masm.declare_rust_fn(RuntimeExtern::SubrCall);
    let subr_ret_path = masm.declare_rust_fn(RuntimeExtern::SubrRet);
    let underflow_path = masm.declare_rust_fn(RuntimeExtern::Underflow);
    let raise_ret_path = masm.declare_rust_fn(RuntimeExtern::RaiseRet);
    let exit_path = masm.declare_rust_fn(RuntimeExtern::Exit);
    let values_error_path = masm.declare_rust_fn(RuntimeExtern::ValuesError);
    let paths = declare_slow_paths(&mut masm);

    // The opcode map lives at the end of the image, dispatch sites reference
    // it forward.
    let map = masm.label();

    // One return glue with a self-referential header: a multiple-values
    // return to an interpreted caller resumes at `interpreter_return` itself.
    let interpreter_return = masm.emit_rp_header_self(0, 0, sabi::INTERPRETER_FRAME_SIZE);
    stubs::emit_interpreter_return(&mut masm, map);
    let interpreter_return_end = masm.label();
    masm.bind(interpreter_return_end);
    masm.define_symbol(
        "interp_return",
        interpreter_return,
        interpreter_return_end,
        SymbolKind::Glue,
    );

    let exit_glue = masm.label();
    masm.bind(exit_glue);
    stubs::emit_exit_glue(&mut masm);
    let exit_glue_end = masm.label();
    masm.bind(exit_glue_end);
    masm.define_symbol("exit_glue", exit_glue, exit_glue_end, SymbolKind::Glue);

    let scheme_exit = masm.emit_rp_header_self(0, 0, 0);
    stubs::emit_stub(&mut masm, exit_path);
    let scheme_exit_end = masm.label();
    masm.bind(scheme_exit_end);
    masm.define_symbol(
        "scheme_exit",
        scheme_exit,
        scheme_exit_end,
        SymbolKind::Stub,
    );

    macro_rules! stub_with_header {
        ($name:literal, $path:expr, $livemask:expr, $frame_size:expr) => {{
            let ra = masm.emit_rp_header_self($livemask, 0, $frame_size);
            stubs::emit_stub(&mut masm, $path);
            let end = masm.label();
            masm.bind(end);
            masm.define_symbol($name, ra, end, SymbolKind::Stub);
        }};
    }
    macro_rules! stub_plain {
        ($name:literal, $path:expr) => {{
            let start = masm.label();
            masm.bind(start);
            stubs::emit_stub(&mut masm, $path);
            let end = masm.label();
            masm.bind(end);
            masm.define_symbol($name, start, end, SymbolKind::Stub);
        }};
    }

    stub_with_header!("underflow", underflow_path, 0, 0);
    // A native continuation's frame: return address and closure.
    stub_with_header!(
        "subr_ret",
        subr_ret_path,
        markers::SUBR_RET_LIVEMASK,
        markers::SUBR_RET_FRAME_SIZE
    );
    stub_with_header!("raise_ret", raise_ret_path, 0, 0);
    {
        let start = masm.label();
        masm.bind(start);
        stubs::emit_subr_call(&mut masm, subr_call_path);
        let end = masm.label();
        masm.bind(end);
        masm.define_symbol("subr_call", start, end, SymbolKind::Stub);
        let generic_start = masm.label();
        masm.bind(generic_start);
        stubs::emit_generic_call(&mut masm, start);
        let generic_end = masm.label();
        masm.bind(generic_end);
        masm.define_symbol("generic_call", generic_start, generic_end, SymbolKind::Stub);
    }
    stub_plain!("overflow", overflow_path);
    stub_plain!("arity", arity_path);
    stub_plain!("values_error", values_error_path);

    let interpreter_enter = masm.label();
    masm.bind(interpreter_enter);
    let interpreter_enter_cp = stubs::emit_interpreter_enter(&mut masm, map);
    let interpreter_enter_end = masm.label();
    masm.bind(interpreter_enter_end);
    masm.define_symbol(
        "interp_enter",
        interpreter_enter,
        interpreter_enter_end,
        SymbolKind::Glue,
    );
    // The same glue past the `CP` reload: the machine entry of every
    // interpreted closure, whose callers all preset `CP`.
    masm.define_symbol(
        "interp_enter_cp",
        interpreter_enter_cp,
        interpreter_enter_end,
        SymbolKind::Glue,
    );

    let interpreter_resume = masm.label();
    masm.bind(interpreter_resume);
    stubs::emit_interpreter_resume(&mut masm, map);
    let interpreter_resume_end = masm.label();
    masm.bind(interpreter_resume_end);
    masm.define_symbol(
        "interp_resume",
        interpreter_resume,
        interpreter_resume_end,
        SymbolKind::Glue,
    );

    let scheme_entry = masm.label();
    masm.bind(scheme_entry);
    masm.scheme_entry_prologue();
    stubs::emit_scheme_entry(&mut masm, interpreter_enter, map);
    let scheme_entry_end = masm.label();
    masm.bind(scheme_entry_end);
    masm.define_symbol(
        "scheme_entry",
        scheme_entry,
        scheme_entry_end,
        SymbolKind::Glue,
    );

    let op_stats = stubs::op_stats_enabled();

    let op_wide16 = masm.label();
    masm.bind(op_wide16);
    stubs::emit_op_count(&mut masm, 0, WIDE16_PREFIX, op_stats);
    emit_wide(&mut masm, map, 1);
    let op_wide16_end = masm.label();
    masm.bind(op_wide16_end);
    masm.define_symbol("op_wide16", op_wide16, op_wide16_end, SymbolKind::Handler);

    let op_wide32 = masm.label();
    masm.bind(op_wide32);
    stubs::emit_op_count(&mut masm, 0, WIDE32_PREFIX, op_stats);
    emit_wide(&mut masm, map, 2);
    let op_wide32_end = masm.label();
    masm.bind(op_wide32_end);
    masm.define_symbol("op_wide32", op_wide32, op_wide32_end, SymbolKind::Handler);

    const _: () = assert!(OpcodeId::COUNT <= u8::MAX as usize);
    let op_invalid = masm.label();
    masm.bind(op_invalid);
    stubs::emit_bail_handler(&mut masm, bail, u8::MAX, 0);
    let op_invalid_end = masm.label();
    masm.bind(op_invalid_end);
    masm.define_symbol(
        "op_invalid",
        op_invalid,
        op_invalid_end,
        SymbolKind::Handler,
    );

    // One handler per opcode per width: the specialized template eagerly,
    // or a bail handler when forced by `CAPY_TEMPLATE_BAIL`.
    let forced_bail = parse_bail_filter();
    let eager = crate::utils::flags::template_eager();
    let mut handlers: [[Option<Label>; 256]; 3] = [[None; 256]; 3];
    let mut kinds = [[HandlerKind::None; 256]; 3];
    macro_rules! emit_handlers {
        ($(($Op:ident, $snake:ident),)*) => {
            $(
                for width in 0..3usize {
                    let len = def::$Op::LEN[width];
                    let header = [1, 3, 5][width];
                    debug_assert!(len >= header);
                    let name = format!(
                        "{}.{}",
                        stringify!($snake),
                        ["narrow", "wide16", "wide32"][width]
                    );
                    let start = masm.label();
                    masm.bind(start);
                    let byte = OpcodeId::$Op as usize;
                    stubs::emit_op_count(&mut masm, width, byte as u8, op_stats);
                    let mut context = InterpreterTemplateContext {
                        width: bytecodegen::OperandWidth::from_index(width as u8),
                        op: OpcodeId::$Op,
                        map: CodeAddr::Label(map),
                        interpreter_return: CodeAddr::Label(interpreter_return),
                        bail,
                        paths,
                    };
                    if forced_bail[byte] || !eager {
                        stubs::emit_bail_handler(&mut masm, bail, byte as u8, width as u8);
                        kinds[width][byte] = HandlerKind::Bail;
                    } else {
                        emit::emit_template(&mut masm, &mut context, OpcodeId::$Op);
                        kinds[width][byte] = HandlerKind::Template;
                    }
                    let end = masm.label();
                    masm.bind(end);
                    masm.define_symbol(&name, start, end, SymbolKind::Handler);
                    handlers[width][byte] = Some(start);
                    let _ = len;
                }
            )*
        };
    }
    def::for_each_opcode!(emit_handlers);

    let image_end = masm.label();
    masm.bind(image_end);
    masm.cfi_region(image_start, image_end, sabi::SCHEME_ENTRY_CFA_OFFSET as u32);

    // Opcode map: 3 × 256 × 8 bytes of handler addresses plus the
    // `interpreter_return` and `interpreter_enter` slots templates read through
    // `MAP_RETURN_OFF` and `MAP_ENTER_OFF`, all filled after load.
    masm.align(16);
    masm.bind(map);
    let map_data = vec![0u8; (MAP_ENTER_SLOT + 1) * 8];
    let _ = masm.assembler().embed_data(&map_data);
    let map_end = masm.label();
    masm.bind(map_end);
    masm.define_symbol("opcode_map", map, map_end, SymbolKind::Handler);

    let mut entries = Vec::with_capacity(MAP_ENTER_SLOT + 1);
    for width in 0..3 {
        for slot in 0..256 {
            let label = if width == 0 && slot == WIDE16_PREFIX as usize {
                op_wide16
            } else if width == 0 && slot == WIDE32_PREFIX as usize {
                op_wide32
            } else if slot == WIDE16_PREFIX as usize || slot == WIDE32_PREFIX as usize {
                op_invalid
            } else {
                handlers[width][slot].unwrap_or(op_invalid)
            };
            entries.push(label);
        }
    }
    debug_assert_eq!(entries.len(), MAP_TABLE_SLOTS);
    entries.push(interpreter_return);
    entries.push(interpreter_enter_cp);

    let mut image = masm.finish().map_err(io::Error::other)?;
    image.tables.push(TablePatch { base: map, entries });
    let map_off = image.finished.offset(map).expect("the opcode map is bound") as usize;
    let loaded = image::load(image, |name| resolve_slow_path(name))?;
    let map_base = loaded.base().as_usize() + map_off;
    Ok((loaded, kinds, map_base))
}

fn emit_wide(masm: &mut Masm, map: Label, width: u8) {
    let pc = sabi::regs::PC;
    let (t5, t7) = (onlineasm::regs::t5, onlineasm::regs::t7);
    let disp = if width == 1 { 256 * 8 } else { 512 * 8 } as i32;
    let slot = masm.assembler().mem_indexed(t7, t5, 8, disp);
    if width == 1 {
        onlineasm!(*masm.assembler();
            loadh {t5}, [{pc} + 1];
            leap {t7}, {map};
            jmp {slot};
        );
    } else {
        onlineasm!(*masm.assembler();
            loadi {t5}, [{pc} + 1];
            leap {t7}, {map};
            jmp {slot};
        );
    }
}

pub(crate) fn resolve_slow_path(name: &onlineasm::asmkit::ExternalName) -> *const u8 {
    use onlineasm::asmkit::ExternalName;
    if let ExternalName::User(key) = name
        && key.namespace == super::masm::EXTERN_NAMESPACE_PRIM
    {
        return crate::runtime::prims::calls::resolve_prim_direct(key.index);
    }
    match RuntimeExtern::from_external(name) {
        Some(helper) => helper.address(),
        None => panic!("template: unknown extern {name:?}"),
    }
}
