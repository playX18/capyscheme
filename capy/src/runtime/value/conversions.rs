//! Conversion traits between runtime values and Rust native values.
//!
//! This module is the shared adapter layer used by native procedures. It
//! converts Scheme [`Value`] arguments into Rust types, converts Rust return
//! values back into runtime values, and records enough arity/type information
//! for native-call error reporting.

use super::*;
use crate::rsgc::Gc;
use crate::runtime::Context;

/// An error produced while converting native-call arguments.
pub enum ConversionError<'gc> {
    /// An argument had the wrong runtime type.
    TypeMismatch {
        /// The zero-based argument position where conversion failed.
        pos: usize,
        /// The human-readable type expected by the converter.
        expected: &'static str,
        /// The value that could not be converted.
        found: Value<'gc>,
    },
    /// The argument list did not have the required number of values.
    ArityMismatch {
        /// The zero-based argument position where arity failed.
        pos: usize,
        /// The accepted arity at that position.
        expected: Arity,
        /// The number of values available at that position.
        found: usize,
    },
}

impl<'gc> std::fmt::Display for ConversionError<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch {
                pos,
                expected,
                found,
            } => write!(
                f,
                "ConversionError at argument {}: expected type {}, found value {}",
                pos, expected, found
            ),
            Self::ArityMismatch {
                pos,
                expected,
                found,
            } => {
                if let Some(max) = expected.max {
                    write!(
                        f,
                        "ConversionError at argument {}: expected arity between {} and {}, found {}",
                        pos, expected.min, max, found
                    )
                } else {
                    write!(
                        f,
                        "ConversionError at argument {}: expected arity at least {}, found {}",
                        pos, expected.min, found
                    )
                }
            }
        }
    }
}

impl<'gc> std::fmt::Debug for ConversionError<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl<'gc> ConversionError<'gc> {
    /// Return this error with its argument position shifted by `offset`.
    pub fn with_appended_pos(self, offset: usize) -> Self {
        match self {
            Self::TypeMismatch {
                pos,
                expected,
                found,
            } => Self::TypeMismatch {
                pos: pos + offset,
                expected,
                found,
            },
            Self::ArityMismatch {
                pos,
                expected,
                found,
            } => Self::ArityMismatch {
                pos: pos + offset,
                expected,
                found,
            },
        }
    }

    /// Construct a type-mismatch error at `pos`.
    pub fn type_mismatch(pos: usize, expected: &'static str, found: Value<'gc>) -> Self {
        Self::TypeMismatch {
            pos,
            expected,
            found,
        }
    }

    /// Construct an arity-mismatch error at `pos`.
    pub fn arity_mismatch(pos: usize, expected: Arity, found: usize) -> Self {
        Self::ArityMismatch {
            pos,
            expected,
            found,
        }
    }

    /// Return the zero-based argument position where conversion failed.
    pub fn pos(&self) -> usize {
        match self {
            Self::TypeMismatch { pos, .. } => *pos,
            Self::ArityMismatch { pos, .. } => *pos,
        }
    }
}

/// A value type that can be identified by the runtime tag bits.
///
/// # Safety
///
/// Implementors must keep the type codes consistent with the allocation and
/// downcast layout for the Rust type. An incorrect tag declaration can make
/// [`Value::downcast`] produce a `Gc<T>` for an object with a different layout.
pub unsafe trait Tagged {
    /// Human-readable name used in conversion errors.
    const TYPE_NAME: &'static str;

    /// The compact 8-bit type code for this value type.
    const TC8: TypeCode8;

    /// Extended 16-bit type codes accepted for this value type.
    const TC16: &[TypeCode16] = &[];

    /// Set to true when the type can only be encodded as a TC16. In that case
    /// `Value::is` and `Value::downcast` will always check `TC16` and not `TC8`.
    const ONLY_TC16: bool = false;
}

/// Converts a Rust value into a runtime [`Value`].
pub trait IntoValue<'gc> {
    /// Convert `self` into a value allocated in `mc` when allocation is needed.
    fn into_value(self, mc: Context<'gc>) -> Value<'gc>;
}

impl<'gc> FromValue<'gc> for Value<'gc> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        Ok(value)
    }
}

/// Converts a Rust value into either a runtime value or a runtime error value.
pub trait TryIntoValue<'gc> {
    /// Convert `self`, returning `Err` when the conversion represents a throw.
    fn try_into_value(self, mc: Context<'gc>) -> Result<Value<'gc>, Value<'gc>>;
}
/*
impl<'gc, T, E> TryIntoValue<'gc> for Result<T, E>
where
    T: TryIntoValue<'gc>,
    E: TryIntoValue<'gc>,
{
    fn try_into_value(self, mc: Context<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        match self {
            Ok(v) => v.try_into_value(mc),
            Err(e) => e.try_into_value(mc),
        }
    }
}*/

impl<'gc, T: IntoValue<'gc>> TryIntoValue<'gc> for T {
    default fn try_into_value(self, _mc: Context<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        Ok(self.into_value(_mc))
    }
}

/// Converts one runtime [`Value`] into a Rust type.
pub trait FromValue<'gc>: Sized {
    /// Try to convert `value` using `ctx` for conversions that need allocation
    /// or access to runtime state.
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>>;
}

/// Provides a human-readable name for values accepted by a converter.
pub trait ExpectedValueType {
    /// Return the type name to use in diagnostics.
    fn expected_value_type() -> &'static str;
}

impl<'gc> ExpectedValueType for Value<'gc> {
    fn expected_value_type() -> &'static str {
        "value"
    }
}

impl ExpectedValueType for char {
    fn expected_value_type() -> &'static str {
        "char"
    }
}

impl ExpectedValueType for bool {
    fn expected_value_type() -> &'static str {
        "value"
    }
}

impl ExpectedValueType for () {
    fn expected_value_type() -> &'static str {
        "no values"
    }
}

impl ExpectedValueType for &str {
    fn expected_value_type() -> &'static str {
        "string"
    }
}

macro_rules! impl_expected_value_type {
    ($($ty:ty),* $(,)?) => { $(
        impl ExpectedValueType for $ty {
            fn expected_value_type() -> &'static str {
                stringify!($ty)
            }
        }
    )* };
}

impl_expected_value_type!(
    u8, i8, i16, u16, u32, usize, i32, i64, isize, u64, u128, f32, f64,
);

impl<'gc, T: Tagged> ExpectedValueType for Gc<'gc, T> {
    fn expected_value_type() -> &'static str {
        T::TYPE_NAME
    }
}

impl<'gc> FromValue<'gc> for char {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is_char() {
            Ok(value.char())
        } else {
            Err(ConversionError::type_mismatch(0, "char", value))
        }
    }
}

impl<'gc> FromValue<'gc> for bool {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        Ok(value != Value::new(false))
    }
}

macro_rules! impl_from_value_number {
    ($($ty:ty => $method:ident),* $(,)?) => { $(
        impl<'gc> FromValue<'gc> for $ty {
            fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
                let Some(n) = value.number() else {
                    return Err(ConversionError::type_mismatch(0, stringify!($ty), value));
                };
                n.$method()
                    .ok_or_else(|| ConversionError::type_mismatch(0, stringify!($ty), value))
            }
        }
    )* };
}

impl_from_value_number!(
    u8 => exact_integer_to_u8,
    i8 => exact_integer_to_i8,
    i16 => exact_integer_to_i16,
    u16 => exact_integer_to_u16,
    u32 => exact_integer_to_u32,
    usize => exact_integer_to_usize,
    i64 => exact_integer_to_i64,
    isize => exact_integer_to_isize,
    u64 => exact_integer_to_u64,
    u128 => exact_integer_to_u128,
);

impl<'gc> FromValue<'gc> for i32 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            Ok(value.as_int32())
        } else {
            // Fall back to generic number path for bignums that fit in i32
            let Some(n) = value.number() else {
                return Err(ConversionError::type_mismatch(0, "i32", value));
            };
            n.exact_integer_to_i32()
                .ok_or_else(|| ConversionError::type_mismatch(0, "i32", value))
        }
    }
}

impl<'gc> FromValue<'gc> for f32 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if let Some(n) = value.flonum() {
            Ok(n as f32)
        } else {
            Err(ConversionError::type_mismatch(0, "f32", value))
        }
    }
}

impl<'gc> FromValue<'gc> for f64 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if let Some(n) = value.flonum() {
            Ok(n)
        } else {
            Err(ConversionError::type_mismatch(0, "f64", value))
        }
    }
}

impl<'gc, T: Tagged> FromValue<'gc> for Gc<'gc, T> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is::<T>() {
            Ok(value.downcast::<T>())
        } else {
            Err(ConversionError::type_mismatch(0, T::TYPE_NAME, value))
        }
    }
}

impl<'gc> IntoValue<'gc> for Value<'gc> {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        self
    }
}

macro_rules! impl_into_value_number {
    ($($ty:ty => $method:ident),* $(,)?) => { $(
        impl<'gc> IntoValue<'gc> for $ty {
            fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
                Number::$method(mc, self).into_value(mc)
            }
        }
    )* };
}

macro_rules! impl_into_value_number_nomc {
    ($($ty:ty => $method:ident),* $(,)?) => { $(
        impl<'gc> IntoValue<'gc> for $ty {
            fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
                Number::$method(self).into_value(mc)
            }
        }
    )* };
}

impl_into_value_number!(
    usize => from_usize,
    isize => from_isize,
    i64 => from_i64,
    u64 => from_u64,
    u128 => from_u128,
    u32 => from_u32,
);

impl_into_value_number_nomc!(
    u16 => from_u16,
    u8 => from_u8,
    i16 => from_i16,
    i8 => from_i8,
);

impl<'gc> IntoValue<'gc> for () {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::undefined()
    }
}

impl<'gc> IntoValue<'gc> for f32 {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::new(self as f64)
    }
}

impl<'gc> From<i32> for Value<'gc> {
    fn from(v: i32) -> Self {
        Value::from_i32(v)
    }
}

impl<'gc> From<f64> for Value<'gc> {
    fn from(v: f64) -> Self {
        Value::from_f64(v)
    }
}

impl<'gc> From<bool> for Value<'gc> {
    fn from(v: bool) -> Self {
        Value::from_bool(v)
    }
}

impl<'gc> From<()> for Value<'gc> {
    fn from(_: ()) -> Self {
        Value::undefined()
    }
}

impl<'gc> From<char> for Value<'gc> {
    fn from(v: char) -> Self {
        Value::from_char(v)
    }
}

impl<'gc, T: Tagged> IntoValue<'gc> for Gc<'gc, T> {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_gc(self)
    }
}

impl<'gc> IntoValue<'gc> for i32 {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_i32(self)
    }
}

impl<'gc> IntoValue<'gc> for f64 {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_f64(self)
    }
}

impl<'gc> IntoValue<'gc> for bool {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_bool(self)
    }
}

impl<'gc> IntoValue<'gc> for &str {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        let string = Str::new(*mc, self, false);

        Value::from_gc(string)
    }
}

impl<'gc> IntoValue<'gc> for char {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_char(self)
    }
}

/// The number of values accepted by an argument converter.
pub struct Arity {
    /// The minimum number of required values.
    pub min: usize,
    /// The maximum accepted values, or `None` for a variadic converter.
    pub max: Option<usize>,
}

impl Arity {
    /// Return whether `n` is accepted by this arity.
    pub fn is_valid(&self, n: usize) -> bool {
        n >= self.min && self.max.is_none_or(|max| n <= max)
    }
}

/// Converts a slice of runtime values into a typed native argument list.
pub trait FromNativeArgs<'gc>: Sized {
    /// Build `Self` from all values in `values`.
    fn from_native_args(
        ctx: Context<'gc>,
        values: &[Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>>;
}

impl<'gc, T> FromNativeArgs<'gc> for T
where
    T: for<'a> FromValues<'gc, 'a>,
{
    fn from_native_args(
        ctx: Context<'gc>,
        values: &[Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>> {
        if !T::ARITY.is_valid(values.len()) {
            return Err(ConversionError::arity_mismatch(0, T::ARITY, values.len()));
        }

        let mut pos = 0;
        let args = T::from_values(ctx, &mut pos, values)?;
        if pos == values.len() {
            Ok(args)
        } else {
            Err(ConversionError::arity_mismatch(
                pos,
                Arity {
                    min: 0,
                    max: Some(0),
                },
                values.len() - pos,
            ))
        }
    }
}

/// Converts a Rust native procedure result into a runtime return record.
pub trait IntoNativeReturn<'gc> {
    /// Convert `self` into the return code and value expected by the VM.
    fn into_native_return(self, ctx: Context<'gc>) -> NativeReturn<'gc>;
}

impl<'gc, T> IntoNativeReturn<'gc> for T
where
    T: TryIntoValue<'gc>,
{
    fn into_native_return(self, ctx: Context<'gc>) -> NativeReturn<'gc> {
        match self.try_into_value(ctx) {
            Ok(value) => NativeReturn {
                code: ReturnCode::ReturnOk,
                value,
            },
            Err(value) => NativeReturn {
                code: ReturnCode::ReturnErr,
                value,
            },
        }
    }
}

impl<'gc, T, E> IntoNativeReturn<'gc> for Result<T, E>
where
    T: TryIntoValue<'gc>,
    E: IntoValue<'gc>,
{
    fn into_native_return(self, ctx: Context<'gc>) -> NativeReturn<'gc> {
        match self {
            Ok(value) => value.into_native_return(ctx),
            Err(err) => NativeReturn {
                code: ReturnCode::ReturnErr,
                value: err.into_value(ctx),
            },
        }
    }
}

/// Converts zero or more runtime values into a Rust type.
pub trait FromValues<'gc, 'a>: Sized {
    /// The number of input values consumed by this converter.
    const ARITY: Arity = Arity {
        min: 1,
        max: Some(1),
    };

    /// Convert values starting at `pos`, advancing `pos` past consumed values.
    fn from_values(
        ctx: Context<'gc>,
        pos: &mut usize,
        values: &'a [Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>>;
}

impl<'gc, 'a, T: FromValue<'gc>> FromValues<'gc, 'a> for T {
    fn from_values(
        ctx: Context<'gc>,
        pos: &mut usize,
        values: &'a [Value<'gc>],
    ) -> Result<T, ConversionError<'gc>> {
        if let Some(value) = values.get(*pos) {
            *pos += 1;
            T::try_from_value(ctx, *value)
        } else {
            Err(ConversionError::ArityMismatch {
                pos: *pos,
                expected: T::ARITY,
                found: values.len() - *pos,
            })
        }
    }
}

impl<'gc, 'a, T: FromValue<'gc>> FromValues<'gc, 'a> for Option<T> {
    const ARITY: Arity = Arity {
        min: 0,
        max: Some(1),
    };
    fn from_values(
        ctx: Context<'gc>,
        pos: &mut usize,
        values: &'a [Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>> {
        if let Some(value) = values.get(*pos) {
            *pos += 1;
            T::try_from_value(ctx, *value)
                .map(Some)
                .map_err(|e| e.with_appended_pos(*pos - 1))
        } else {
            Ok(None)
        }
    }
}

impl<T: ExpectedValueType> ExpectedValueType for Option<T> {
    fn expected_value_type() -> &'static str {
        T::expected_value_type()
    }
}

impl<'gc, 'a> FromValues<'gc, 'a> for () {
    const ARITY: Arity = Arity {
        min: 0,
        max: Some(0),
    };
    fn from_values(
        _ctx: Context<'gc>,
        _pos: &mut usize,
        _values: &'a [Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>> {
        Ok(())
    }
}

macro_rules! impl_tuple {
    () => {

    };

    ($first:ident $($rest:ident)*) => {

        impl<'gc, $first $(,$rest)*> IntoValues<'gc> for ($first, $($rest,)*)
        where
            $first: IntoValue<'gc>,
            $($rest: IntoValue<'gc>,)*
        {
            #[allow(non_snake_case)]
            fn into_values(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
                let ($first, $($rest,)*) = self;
                std::iter::once($first.into_value(ctx))
                    $(.chain(std::iter::once($rest.into_value(ctx))))*
            }
        }

        impl<'gc, $first $(,$rest)*> TryIntoValues<'gc> for ($first, $($rest,)*)
        where
            $first: TryIntoValue<'gc>,
            $($rest: TryIntoValue<'gc>,)*
        {
            #[allow(non_snake_case)]
            fn try_into_values(
                self,
                ctx: Context<'gc>,
            ) -> Result<impl Iterator<Item = Value<'gc>>, Value<'gc>> {
                let ($first, $($rest,)*) = self;
                Ok(std::iter::once($first.try_into_value(ctx)?)
                    $(.chain(std::iter::once($rest.try_into_value(ctx)?)))*)
            }
        }

        impl<'gc, 'a, $first $(,$rest)*> FromValues<'gc, 'a> for ($first, $($rest,)*)
        where
            $first: FromValues<'gc, 'a>,
            $($rest: FromValues<'gc, 'a>,)*
        {
            #[allow(unused_mut, unused_assignments, unused_variables)]
            // Do our best at statically checking that tuple arity is correct
            // and tuple can be constructed from the values.
            const ARITY: Arity = {
                let first = <$first as FromValues<'gc, 'a>>::ARITY;
                let mut min = first.min;
                let mut max = first.max;
                let mut prev_was_optional = first.min == 0; // True if the first argument is Option-like (min arity 0)
                $(
                    let rest = <$rest as FromValues<'gc, 'a>>::ARITY; // Arity of the current $rest element
                    let current_is_optional = rest.min == 0; // True if current argument is Option-like

                    // Check if an optional argument is followed by a non-optional (required) one
                    if prev_was_optional && !current_is_optional {
                        panic!("Invalid argument order: An optional argument (e.g., Option<T>) cannot be followed by a required argument in a tuple definition. Example: (Option<i32>, i32) is invalid.");
                    }

                    min = min + rest.min;

                    max = match max {
                        Some(max_val) => match rest.max { // max_val is accumulated_max_before_current, rest.max is current_element_max
                            Some(rest_max) => Some(max_val + rest_max),
                            None => None,
                        },
                        None => {
                            panic!("Invalid argument order: After variadic arguments, there can be no more arguments.");
                        },
                    };

                    prev_was_optional = current_is_optional; // Update for the next iteration
                )*

                Arity { min, max }
            };


            fn from_values(ctx: Context<'gc>, pos: &mut usize, values: &'a [Value<'gc>]) -> Result<Self, ConversionError<'gc>> {

                paste::paste! { let x = *pos;
                    let [<_ $first: lower>] = $first::from_values(ctx, pos, values)
                        .map_err(|e| e.with_appended_pos(x))?;
                    $(
                        let x = *pos;
                        let [<_ $rest: lower>] = $rest::from_values(ctx, pos, values)
                            .map_err(|e| e.with_appended_pos(x))?;
                    )*
                    Ok((
                        [<_ $first: lower>],
                        $(
                            [<_ $rest: lower>],
                        )*
                    ))
                }
            }
        }

        impl_tuple!($($rest)*);
    }
}

impl<'gc, 'a> FromValues<'gc, 'a> for &'a [Value<'gc>] {
    const ARITY: Arity = Arity { min: 0, max: None };
    fn from_values(
        _ctx: Context<'gc>,
        pos: &mut usize,
        values: &'a [Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>> {
        Ok(&values[*pos..])
    }
}

impl_tuple!(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ);

/// Converts a Rust return value into one or more runtime values.
pub trait IntoValues<'gc> {
    /// Convert `self` into an iterator of returned values.
    fn into_values(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>>;
}

impl<'gc, T: IntoValue<'gc>> IntoValues<'gc> for T {
    fn into_values(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        std::iter::once(self.into_value(ctx))
    }
}

/// Converts a Rust return value into runtime values or a runtime error value.
pub trait TryIntoValues<'gc> {
    /// Convert `self`, returning `Err` when the conversion represents a throw.
    fn try_into_values(
        self,
        ctx: Context<'gc>,
    ) -> Result<impl Iterator<Item = Value<'gc>>, Value<'gc>>;
}

impl<'gc, T: TryIntoValue<'gc>> TryIntoValues<'gc> for T {
    fn try_into_values(
        self,
        ctx: Context<'gc>,
    ) -> Result<impl Iterator<Item = Value<'gc>>, Value<'gc>> {
        Ok(std::iter::once(self.try_into_value(ctx)?))
    }
}

impl<'gc, T: TryIntoValues<'gc>, E: IntoValue<'gc>> TryIntoValues<'gc> for Result<T, E> {
    fn try_into_values(
        self,
        ctx: Context<'gc>,
    ) -> Result<impl Iterator<Item = Value<'gc>>, Value<'gc>> {
        match self {
            Ok(v) => v.try_into_values(ctx),
            Err(e) => Err(e.into_value(ctx)),
        }
    }
}

impl<'gc> TryIntoValues<'gc> for &[Value<'gc>] {
    fn try_into_values(
        self,
        _ctx: Context<'gc>,
    ) -> Result<impl Iterator<Item = Value<'gc>>, Value<'gc>> {
        Ok(self.iter().copied())
    }
}

/// A typed view over the remaining native-call arguments.
pub struct RestOf<'a, 'gc, T: FromValue<'gc>> {
    /// The unconverted remaining values.
    pub rest: &'a [Value<'gc>],
    /// Marker for the type produced when elements are requested.
    pub _marker: std::marker::PhantomData<T>,
}

impl<'a, 'gc, T: FromValue<'gc>> FromValues<'gc, 'a> for RestOf<'a, 'gc, T> {
    const ARITY: Arity = Arity { min: 0, max: None };
    fn from_values(
        _ctx: Context<'gc>,
        pos: &mut usize,
        values: &'a [Value<'gc>],
    ) -> Result<Self, ConversionError<'gc>> {
        let rest = &values[*pos..];
        *pos = values.len();
        Ok(Self {
            rest,
            _marker: std::marker::PhantomData,
        })
    }
}

impl<'a, 'gc, T: FromValue<'gc>> RestOf<'a, 'gc, T> {
    /// Convert the remaining value at `index`.
    pub fn at(&self, ctx: Context<'gc>, index: usize) -> Result<T, ConversionError<'gc>> {
        if let Some(value) = self.rest.get(index) {
            T::try_from_value(ctx, *value).map_err(|e| e.with_appended_pos(index))
        } else {
            Err(ConversionError::ArityMismatch {
                pos: index,
                expected: T::ARITY,
                found: self.rest.len(),
            })
        }
    }

    /// Iterate over the remaining values, converting each item on demand.
    pub fn iter(
        &self,
        ctx: Context<'gc>,
    ) -> impl Iterator<Item = Result<T, ConversionError<'gc>>> + '_ {
        self.rest
            .iter()
            .enumerate()
            .map(move |(i, v)| T::try_from_value(ctx, *v).map_err(|e| e.with_appended_pos(i)))
    }

    /// Return the number of remaining values.
    pub fn len(&self) -> usize {
        self.rest.len()
    }

    /// Return the unconverted remaining values.
    pub fn as_slice(&self) -> &'a [Value<'gc>] {
        self.rest
    }

    /// Return whether there are no remaining values.
    pub fn is_empty(&self) -> bool {
        self.rest.is_empty()
    }
}

impl<'a, 'gc, T: FromValue<'gc> + ExpectedValueType> ExpectedValueType for RestOf<'a, 'gc, T> {
    fn expected_value_type() -> &'static str {
        T::expected_value_type()
    }
}

/// A value that can be converted as one of two Rust types.
pub enum Either<L, R> {
    /// The value converted as the left-hand type.
    Left(L),
    /// The value converted as the right-hand type.
    Right(R),
}

impl<L, R> ExpectedValueType for Either<L, R> {
    fn expected_value_type() -> &'static str {
        "either"
    }
}

impl<'gc, L, R> FromValue<'gc> for Either<L, R>
where
    L: FromValue<'gc>,
    R: FromValue<'gc>,
{
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        match L::try_from_value(ctx, value) {
            Ok(l) => Ok(Either::Left(l)),
            Err(_) => {
                let r = R::try_from_value(ctx, value)?;
                Ok(Either::Right(r))
            }
        }
    }
}

impl<'gc, L, R> IntoValue<'gc> for Either<L, R>
where
    L: IntoValue<'gc>,
    R: IntoValue<'gc>,
{
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        match self {
            Either::Left(l) => l.into_value(mc),
            Either::Right(r) => r.into_value(mc),
        }
    }
}
