#![allow(
    incomplete_features,
    internal_features,
    dead_code,
    path_statements,
    unused_must_use,
    unused_mut
)]
#![feature(
    arbitrary_self_types,
    try_trait_v2,
    link_llvm_intrinsics,
    min_specialization,
    const_type_name,
    try_trait_v2_residual
)]

//! CapyScheme: R6RS/R7RS Scheme compiler and runtime.

pub mod api;
pub mod bin_support;
pub mod compiler;
pub mod cps;
pub mod expander;
pub mod frontend;
pub mod rsgc;
pub mod runtime;
pub mod utils;

pub use capy_derive::__unelide_lifetimes;
pub use capy_derive::Trace;
pub use rsgc::*;

pub mod prelude {
    pub use crate::global;
    pub use crate::list;
    pub use crate::rsgc::{Rootable, Trace, barrier, *};
    pub use crate::runtime::global::Global;
    pub use crate::runtime::modules::Module;
    pub use crate::runtime::modules::ModuleRef;
    pub use crate::runtime::modules::current_module;
    pub use crate::runtime::prelude::*;
    pub use crate::runtime::vm::{NativeCallContext, NativeCallReturn};
    pub use crate::vector;
    pub use capy_derive::scheme;
    pub use capy_derive::scm_match;
}

pub use capy_derive::scheme;
pub use capy_derive::scm_match;

#[cfg(test)]
mod tests {
    use crate::{prelude::*, runtime::Scheme};
    use std::path::{Path, PathBuf};

    #[test]
    fn matching() {
        let scm = Scheme::new_uninit();

        scm.enter(|ctx| {
            let sym = ctx.intern("foo");
            let val = list!(ctx, sym, 2, list!(ctx, 3, 4), 5, 6);

            scm_match!(ctx, val, {
                    ('foo 2 x . rest) => {
                        println!("{x} {rest}");
                    }
                }
            );
        });
    }

    #[test]
    fn legacy_header_terms_are_absent_from_source() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("capy crate should live in the workspace");
        let roots = [
            workspace.join("capy/src"),
            workspace.join("capy-derive/src"),
            workspace.join("lib"),
            workspace.join("tests"),
            workspace.join("docs"),
        ];
        let legacy_terms = [
            concat!("type", "code"),
            concat!("Type", "Code"),
            concat!("Heap", "Type", "Info"),
            concat!("Info", "Id", "Bits"),
            concat!("Type", "Bits"),
            concat!("V", "Table", "Of"),
            concat!("new", "_with", "_info"),
            concat!("allocate", "_with", "_info"),
            concat!("raw", "_allocate", "_with", "_info"),
            concat!("alloc", "_with", "_info"),
            concat!("type", "_info"),
            concat!("info", "_id"),
            concat!("has_", "ty", "p8"),
            concat!("has_", "ty", "p16"),
            concat!("ty", "p8"),
            concat!("ty", "p16"),
            concat!("is_heap_object_tc", "8"),
            concat!("is_heap_object_tc", "16"),
        ];

        let mut files = Vec::new();
        for root in roots {
            collect_source_files(&root, &mut files);
        }

        let mut hits = Vec::new();
        for file in files {
            let text = std::fs::read_to_string(&file).unwrap_or_else(|err| {
                panic!("failed to read {}: {err}", file.display());
            });
            let lower = text.to_ascii_lowercase();
            for term in legacy_terms {
                let matched = if term.chars().all(|ch| ch.is_ascii_lowercase()) {
                    lower.contains(term)
                } else {
                    text.contains(term)
                };
                if matched {
                    hits.push(format!("{} contains {term}", file.display()));
                }
            }
        }

        assert!(
            hits.is_empty(),
            "legacy type/vtable header terms must stay out of source:\n{}",
            hits.join("\n")
        );
    }

    fn collect_source_files(root: &Path, out: &mut Vec<PathBuf>) {
        if !root.exists() {
            return;
        }

        let entries = std::fs::read_dir(root).unwrap_or_else(|err| {
            panic!("failed to read {}: {err}", root.display());
        });
        for entry in entries {
            let path = entry
                .unwrap_or_else(|err| panic!("failed to read directory entry: {err}"))
                .path();
            if path.is_dir() {
                collect_source_files(&path, out);
            } else if matches!(
                path.extension().and_then(|ext| ext.to_str()),
                Some("rs" | "scm" | "sls" | "sld" | "md")
            ) {
                out.push(path);
            }
        }
    }
}
