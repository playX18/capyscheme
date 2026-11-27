//! Stable hashes of the compiled-library metadata: the source hash, the interface hash
//! and the compiler id of a library decl (see `boot/environments.scm`).
//!
//! The hash is SHA-256 cut to 128 bits and written as 32 lowercase hex digits. It does not
//! depend on the process, the platform or the Rust version, so it can be stored in a FASL
//! file and compared by a later run.

use sha2::{Digest, Sha256};

use crate::runtime::objectmodel::{Str, Symbol, Value, Vector};

/// Number of hex digits of a hash.
const HASH_DIGITS: usize = 32;

fn finish(hasher: Sha256) -> String {
    let digest = hasher.finalize();
    let mut out = String::with_capacity(HASH_DIGITS);
    for byte in digest.iter().take(HASH_DIGITS / 2) {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

/// The hash of a byte string.
pub fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    finish(hasher)
}

/// Feeds the datum `value` into `hasher`: pairs, vectors, symbols, strings and the immediate
/// values by structure, tagged so that `(a)` and `a` differ. Other objects by their printed
/// form.
fn feed<'gc>(hasher: &mut Sha256, value: Value<'gc>) {
    let mut current = value;
    loop {
        if current.is_pair() {
            hasher.update(*b"(");
            feed(hasher, current.car());
            current = current.cdr();
            continue;
        }
        break;
    }
    if current.is_null() {
        hasher.update(*b"N");
    } else if let Some(n) = current.int32() {
        hasher.update(*b"i");
        hasher.update(n.to_le_bytes());
    } else if let Some(x) = current.flonum() {
        hasher.update(*b"f");
        hasher.update(x.to_bits().to_le_bytes());
    } else if current.is_bool() {
        hasher.update([b'b', current.as_bool() as u8]);
    } else if current.is_char() {
        hasher.update(*b"c");
        hasher.update((current.char() as u32).to_le_bytes());
    } else if current.is::<Symbol>() {
        let text = current.downcast::<Symbol>().to_string();
        hasher.update(*b"s");
        hasher.update((text.len() as u64).to_le_bytes());
        hasher.update(text.as_bytes());
    } else if current.is::<Str>() {
        let text = current.downcast::<Str>().to_string();
        hasher.update(*b"S");
        hasher.update((text.len() as u64).to_le_bytes());
        hasher.update(text.as_bytes());
    } else if current.is::<Vector>() {
        let vector = current.downcast::<Vector>();
        hasher.update(*b"#");
        hasher.update((vector.len() as u64).to_le_bytes());
        for item in vector.iter() {
            feed(hasher, item.get());
        }
    } else {
        let text = format!("{current}");
        hasher.update(*b"o");
        hasher.update((text.len() as u64).to_le_bytes());
        hasher.update(text.as_bytes());
    }
    hasher.update(*b")");
}

/// The hash of the datum `value`.
pub fn hash_datum<'gc>(value: Value<'gc>) -> String {
    let mut hasher = Sha256::new();
    feed(&mut hasher, value);
    finish(hasher)
}

/// The layout of a library decl, see `boot/environments.scm`: the number of its fields. A
/// decl of another layout is not read, so it is part of the compiler id.
const DECL_LAYOUT: u32 = 14;

/// The id of this compiler: the format hash of the bytecode and primitive table, the layout of
/// the library decls and the version of capy. A compiled library made by another compiler is
/// recompiled.
pub fn compiler_id() -> String {
    format!(
        "{:016x}-d{}-{}",
        crate::bytecode::def::format_hash(),
        DECL_LAYOUT,
        env!("CARGO_PKG_VERSION")
    )
}
