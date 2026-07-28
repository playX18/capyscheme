//! Typed conversions between Rust values and [`Ref`].

use crate::{Env, Ref};

#[derive(Debug, Clone)]
pub struct ConversionError {
    pub arg_index: usize,
    pub expected: &'static str,
}

impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected {} at argument {}",
            self.expected, self.arg_index
        )
    }
}

impl ConversionError {
    pub fn type_mismatch(arg_index: usize, expected: &'static str) -> Self {
        Self {
            arg_index,
            expected,
        }
    }
}

pub trait TryFromScm<'env>: Sized {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError>;
}

pub trait IntoScm<'env> {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env>;
}

#[derive(Clone, Copy, Debug)]
pub struct Arity {
    pub min: usize,
    pub max: Option<usize>,
}

impl Arity {
    pub fn is_valid(self, n: usize) -> bool {
        n >= self.min && self.max.is_none_or(|m| n <= m)
    }
}

pub trait FromScmArgs<'env>: Sized {
    const ARITY: Arity;
    fn from_scm_args(env: &mut Env<'env>, args: &[Ref<'env>]) -> Result<Self, ConversionError>;
}

impl<'env> TryFromScm<'env> for Ref<'env> {
    fn try_from_scm(_env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        Ok(value)
    }
}

impl<'env> IntoScm<'env> for Ref<'env> {
    fn into_scm(self, _env: &mut Env<'env>) -> Ref<'env> {
        self
    }
}

impl<'env> TryFromScm<'env> for bool {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        // SAFETY: standing invariants (see `Env`); value is a rooted ref.
        if unsafe { capy_sni_sys::sni_is_bool(env.as_ptr(), value.as_raw()) } {
            Ok(unsafe { capy_sni_sys::sni_to_bool(env.as_ptr(), value.as_raw()) })
        } else {
            // Scheme truthiness: only #f is false
            Ok(unsafe { capy_sni_sys::sni_is_true(env.as_ptr(), value.as_raw()) })
        }
    }
}

impl<'env> IntoScm<'env> for bool {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.bool(self)
    }
}

impl<'env> TryFromScm<'env> for i32 {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        let mut out = 0i32;
        // SAFETY: standing invariants; out is a valid out-pointer.
        if unsafe { capy_sni_sys::sni_to_i32(env.as_ptr(), value.as_raw(), &mut out) } {
            Ok(out)
        } else {
            Err(ConversionError::type_mismatch(0, "i32"))
        }
    }
}

impl<'env> IntoScm<'env> for i32 {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.fixnum(self)
    }
}

impl<'env> TryFromScm<'env> for i64 {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        let mut out = 0i64;
        // SAFETY: standing invariants; out is a valid out-pointer.
        if unsafe { capy_sni_sys::sni_to_i64(env.as_ptr(), value.as_raw(), &mut out) } {
            Ok(out)
        } else {
            Err(ConversionError::type_mismatch(0, "i64"))
        }
    }
}

impl<'env> IntoScm<'env> for i64 {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.int64(self)
    }
}

impl<'env> TryFromScm<'env> for u32 {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        let mut out = 0u32;
        // SAFETY: standing invariants; out is a valid out-pointer.
        if unsafe { capy_sni_sys::sni_to_u32(env.as_ptr(), value.as_raw(), &mut out) } {
            Ok(out)
        } else {
            Err(ConversionError::type_mismatch(0, "u32"))
        }
    }
}

impl<'env> IntoScm<'env> for u32 {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.uint32(self)
    }
}

impl<'env> TryFromScm<'env> for u64 {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        let mut out = 0u64;
        // SAFETY: standing invariants; out is a valid out-pointer.
        if unsafe { capy_sni_sys::sni_to_u64(env.as_ptr(), value.as_raw(), &mut out) } {
            Ok(out)
        } else {
            Err(ConversionError::type_mismatch(0, "u64"))
        }
    }
}

impl<'env> IntoScm<'env> for u64 {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.uint64(self)
    }
}

impl<'env> TryFromScm<'env> for f64 {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        let mut out = 0f64;
        // SAFETY: standing invariants; out is a valid out-pointer.
        if unsafe { capy_sni_sys::sni_to_f64(env.as_ptr(), value.as_raw(), &mut out) } {
            Ok(out)
        } else {
            Err(ConversionError::type_mismatch(0, "f64"))
        }
    }
}

impl<'env> IntoScm<'env> for f64 {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.flonum(self)
    }
}

impl<'env> TryFromScm<'env> for () {
    fn try_from_scm(_env: &mut Env<'env>, _value: Ref<'env>) -> Result<Self, ConversionError> {
        Ok(())
    }
}

impl<'env> IntoScm<'env> for () {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.unspecified()
    }
}

impl<'env> TryFromScm<'env> for String {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        env.string_to_utf8(value)
            .map_err(|_| ConversionError::type_mismatch(0, "string"))
    }
}

impl<'env> IntoScm<'env> for String {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        // On interior NUL, string() raises an assertion violation into
        // Scheme; the null fallback is reached only if that raise returns.
        env.string(&self).unwrap_or_else(|_| Ref::null())
    }
}

impl<'env> IntoScm<'env> for &str {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        env.string(self).unwrap_or_else(|_| Ref::null())
    }
}

impl<'env, T: TryFromScm<'env>> TryFromScm<'env> for Option<T> {
    fn try_from_scm(env: &mut Env<'env>, value: Ref<'env>) -> Result<Self, ConversionError> {
        if value.is_null() {
            return Ok(None);
        }
        // SAFETY: standing invariants; value is a rooted ref.
        if unsafe { capy_sni_sys::sni_is_bool(env.as_ptr(), value.as_raw()) }
            && !unsafe { capy_sni_sys::sni_is_true(env.as_ptr(), value.as_raw()) }
        {
            return Ok(None);
        }
        T::try_from_scm(env, value).map(Some)
    }
}

impl<'env, T: IntoScm<'env>> IntoScm<'env> for Option<T> {
    fn into_scm(self, env: &mut Env<'env>) -> Ref<'env> {
        match self {
            Some(v) => v.into_scm(env),
            None => env.bool(false),
        }
    }
}

impl<'env> FromScmArgs<'env> for () {
    const ARITY: Arity = Arity {
        min: 0,
        max: Some(0),
    };
    fn from_scm_args(_env: &mut Env<'env>, _args: &[Ref<'env>]) -> Result<Self, ConversionError> {
        Ok(())
    }
}

macro_rules! impl_tuple_from_scm {
    ($(($n:tt, $T:ident)),+) => {
        impl<'env, $($T: TryFromScm<'env>),+> FromScmArgs<'env> for ($($T,)+) {
            const ARITY: Arity = Arity { min: impl_tuple_from_scm!(@count $($T)+), max: Some(impl_tuple_from_scm!(@count $($T)+)) };
            fn from_scm_args(env: &mut Env<'env>, args: &[Ref<'env>]) -> Result<Self, ConversionError> {
                let expected = Self::ARITY.min;
                if args.len() != expected {
                    return Err(ConversionError::type_mismatch(0, "arity"));
                }
                Ok(($(
                    {
                        match $T::try_from_scm(env, args[$n]) {
                            Ok(v) => v,
                            Err(mut err) => {
                                err.arg_index = $n;
                                return Err(err);
                            }
                        }
                    },
                )+))
            }
        }
    };
    (@count $T:ident) => { 1 };
    (@count $T:ident $($rest:ident)+) => { 1 + impl_tuple_from_scm!(@count $($rest)+) };
}

impl_tuple_from_scm!((0, T0));
impl_tuple_from_scm!((0, T0), (1, T1));
impl_tuple_from_scm!((0, T0), (1, T1), (2, T2));
impl_tuple_from_scm!((0, T0), (1, T1), (2, T2), (3, T3));
impl_tuple_from_scm!((0, T0), (1, T1), (2, T2), (3, T3), (4, T4));
impl_tuple_from_scm!((0, T0), (1, T1), (2, T2), (3, T3), (4, T4), (5, T5));
