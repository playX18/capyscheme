use std::{mem, ptr};

const fn cmp_max(a: usize, b: usize) -> usize {
    if a > b { a } else { b }
}

/// A namespace for generic statics.
///
/// # Safety
///
/// Implementing this trait is not unsafe per-se but you should use the [`define_namespace`]
/// instead.
pub unsafe trait Namespace: 'static + Send + Sync + Copy + Clone {
    /// The returned reference points to the static namespaced global variable for each
    /// generic `T` (with lifetimes erased). The static's value is zero-initialized.
    ///
    /// For caveats and limitations, refer to [top-module](crate#caveats-and-limitations).
    #[inline(never)]
    #[must_use]
    fn generic_static<'a, T>() -> &'a T {
        #[allow(unused_assignments)]
        let mut addr: *const () = ptr::null();

        // HACK: We have to "use" the generic `T` in some way to force the compiler to emit every
        // instantiation of this function, otherwise rustc might be smart and merge instantiations.
        //
        // `TypeId` would also work for `'static` types, but `type_name` lets the static be keyed by
        // types carrying GC lifetimes.
        let type_name = std::any::type_name::<(Self, T)>().as_ptr();

        #[cfg(all(
            target_arch = "aarch64",
            any(target_os = "macos", target_os = "ios", target_os = "tvos")
        ))]
        unsafe {
            std::arch::asm!(
                "/* {type_name} */",
                "adrp {x}, 2f@PAGE",
                "add {x}, {x}, 2f@PAGEOFF",
                ".pushsection __DATA,__data",
                ".p2align {align}, 0",
                "2: .zero {size}",
                ".popsection",
                size = const { cmp_max(mem::size_of::<T>(), 1) },
                align = const { mem::align_of::<T>().ilog2() },
                type_name = in(reg) type_name,
                x = out(reg) addr,
                options(nostack)
            );
        }

        #[cfg(all(
            target_arch = "aarch64",
            any(target_os = "none", target_os = "linux", target_os = "freebsd")
        ))]
        unsafe {
            std::arch::asm!(
                "/* {type_name} */",
                "adrp {x}, 2f",
                "add {x}, {x}, :lo12:2f",
                ".pushsection .bss.generic_statics,\"aw\",@nobits",
                ".p2align {align}, 0",
                "2: .zero {size}",
                ".popsection",
                size = const { cmp_max(mem::size_of::<T>(), 1) },
                align = const { mem::align_of::<T>().ilog2() },
                type_name = in(reg) type_name,
                x = out(reg) addr,
                options(nostack)
            );
        }

        #[cfg(all(
            target_arch = "x86_64",
            any(target_os = "macos", target_os = "ios", target_os = "tvos")
        ))]
        unsafe {
            std::arch::asm!(
                "/* {type_name} */",
                "lea {x}, [rip + 2f]",
                ".pushsection __DATA,__data",
                ".p2align {align}, 0",
                "2: .zero {size}",
                ".popsection",
                size = const { cmp_max(mem::size_of::<T>(), 1) },
                align = const { mem::align_of::<T>().ilog2() },
                type_name = in(reg) type_name,
                x = out(reg) addr,
                options(nostack)
            );
        }

        #[cfg(all(
            target_arch = "x86_64",
            any(target_os = "none", target_os = "linux", target_os = "freebsd")
        ))]
        unsafe {
            std::arch::asm!(
                "/* {type_name} */",
                "lea {x}, [rip + 2f]",
                ".pushsection .bss.generic_statics,\"aw\",@nobits",
                ".p2align {align}, 0",
                "2: .zero {size}",
                ".popsection",
                size = const { cmp_max(mem::size_of::<T>(), 1) },
                align = const { mem::align_of::<T>().ilog2() },
                type_name = in(reg) type_name,
                x = out(reg) addr,
                options(nostack)
            );
        }

        #[cfg(all(target_arch = "x86_64", target_os = "windows"))]
        unsafe {
            std::arch::asm!(
                "/* {type_name} */",
                "lea {x}, [rip + 2f]",
                ".pushsection .bss.generic_statics,\"bw\"",
                ".p2align {align}, 0",
                "2: .zero {size}",
                ".popsection",
                size = const { cmp_max(mem::size_of::<T>(), 1) },
                align = const { mem::align_of::<T>().ilog2() },
                type_name = in(reg) type_name,
                x = out(reg) addr,
                options(nostack)
            );
        }

        #[cfg(not(any(
            target_os = "none",
            target_os = "linux",
            target_os = "freebsd",
            target_os = "macos",
            target_os = "ios",
            target_os = "tvos",
            target_os = "windows",
        )))]
        std::compile_error!("static-generics is not supported on this platform");

        assert!(!addr.is_null(), "unsupported platform");

        unsafe { &*addr.cast::<T>() }
    }
}

#[macro_export]
macro_rules! define_namespace {
    ($vis:vis $name:ident) => {
        #[derive(Debug, Copy, Clone)]
        $vis struct $name;

        unsafe impl $crate::utils::generic_static::Namespace for $name {}
    };
}
