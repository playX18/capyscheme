use super::*;
use crate::runtime::Context;
use rsgc::Gc;

pub enum ConversionError<'gc> {
    TypeMismatch {
        pos: usize,
        expected: &'static str,
        found: Value<'gc>,
    },
    ArityMismatch {
        pos: usize,
        expected: Arity,
        found: usize,
    },
}

impl<'gc> ConversionError<'gc> {
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
    pub fn type_mismatch(pos: usize, expected: &'static str, found: Value<'gc>) -> Self {
        Self::TypeMismatch {
            pos,
            expected,
            found,
        }
    }

    pub fn arity_mismatch(pos: usize, expected: Arity, found: usize) -> Self {
        Self::ArityMismatch {
            pos,
            expected,
            found,
        }
    }

    pub fn pos(&self) -> usize {
        match self {
            Self::TypeMismatch { pos, .. } => *pos,
            Self::ArityMismatch { pos, .. } => *pos,
        }
    }
}

pub unsafe trait Tagged {
    const TYPE_NAME: &'static str;
    const TC8: TypeCode8;
    const TC16: &[TypeCode16] = &[];

    /// Set to true when the type can only be encodded as a TC16. In that case
    /// `Value::is` and `Value::downcast` will always check `TC16` and not `TC8`.
    const ONLY_TC16: bool = false;
}

pub trait IntoValue<'gc> {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc>;
}

impl<'gc> FromValue<'gc> for Value<'gc> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        Ok(value)
    }
}

pub trait TryIntoValue<'gc> {
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

pub trait FromValue<'gc>: Sized {
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>>;
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

impl<'gc> FromValue<'gc> for i32 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        if value.is_int32() {
            return Ok(value.as_int32());
        } else {
            return Err(ConversionError::type_mismatch(0, "i32", value));
        }
    }
}

impl<'gc> FromValue<'gc> for usize {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        let Some(n) = value.number() else { todo!() };

        n.exact_integer_to_usize()
            .ok_or_else(|| ConversionError::type_mismatch(0, "usize", value))
    }
}

impl<'gc> FromValue<'gc> for i64 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        let Some(n) = value.number() else { todo!() };

        n.exact_integer_to_i64()
            .ok_or_else(|| ConversionError::type_mismatch(0, "i64", value))
    }
}

impl<'gc> FromValue<'gc> for isize {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        let Some(n) = value.number() else { todo!() };

        n.exact_integer_to_isize()
            .ok_or_else(|| ConversionError::type_mismatch(0, "isize", value))
    }
}

impl<'gc> FromValue<'gc> for u64 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        let Some(n) = value.number() else { todo!() };

        n.exact_integer_to_u64()
            .ok_or_else(|| ConversionError::type_mismatch(0, "u64", value))
    }
}

impl<'gc> FromValue<'gc> for u8 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, ConversionError<'gc>> {
        let Some(n) = value.number() else {
            return Err(ConversionError::TypeMismatch {
                pos: 0,
                expected: "number",
                found: value,
            });
        };

        n.exact_integer_to_u8()
            .ok_or_else(|| ConversionError::TypeMismatch {
                pos: 0,
                expected: "u8",
                found: value,
            })
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

impl<'gc> IntoValue<'gc> for usize {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_usize(mc, self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for isize {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_isize(mc, self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for i64 {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_i64(mc, self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for u64 {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_u64(mc, self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for u32 {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_u32(mc, self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for u16 {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_u16(self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for u8 {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        Number::from_u8(self).into_value(mc)
    }
}

impl<'gc> IntoValue<'gc> for () {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::undefined()
    }
}

impl<'gc> Into<Value<'gc>> for i32 {
    fn into(self) -> Value<'gc> {
        Value::from_i32(self)
    }
}

impl<'gc> Into<Value<'gc>> for f64 {
    fn into(self) -> Value<'gc> {
        Value::from_f64(self)
    }
}

impl<'gc> Into<Value<'gc>> for bool {
    fn into(self) -> Value<'gc> {
        Value::from_bool(self)
    }
}

impl<'gc> Into<Value<'gc>> for () {
    fn into(self) -> Value<'gc> {
        Value::undefined()
    }
}

impl<'gc> Into<Value<'gc>> for char {
    fn into(self) -> Value<'gc> {
        Value::from_char(self)
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
        let string = Str::new(&mc, self, false);

        Value::from_gc(string)
    }
}

impl<'gc> IntoValue<'gc> for char {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_char(self)
    }
}

pub struct Arity {
    pub min: usize,
    pub max: Option<usize>,
}

impl Arity {
    pub fn is_valid(&self, n: usize) -> bool {
        n >= self.min && self.max.map_or(true, |max| n <= max)
    }
}

pub trait FromValues<'gc, 'a>: Sized {
    const ARITY: Arity = Arity {
        min: 1,
        max: Some(1),
    };

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

pub trait IntoValues<'gc> {
    fn into_values(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>>;
}

impl<'gc, T: IntoValue<'gc>> IntoValues<'gc> for T {
    fn into_values(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        std::iter::once(self.into_value(ctx))
    }
}

pub trait TryIntoValues<'gc> {
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
