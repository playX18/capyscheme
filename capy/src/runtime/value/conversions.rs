use super::*;
use crate::runtime::Context;
use rsgc::Gc;

pub unsafe trait Tagged {
    const TC8: TypeCode8;
    const TC16: &[TypeCode16] = &[];

    /// Set to true when the type can only be encodded as a TC16. In that case
    /// `Value::is` and `Value::downcast` will always check `TC16` and not `TC8`.
    const ONLY_TC16: bool = false;
}

pub trait IntoValue<'gc> {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc>;
}

pub trait FromValue<'gc>: Sized {
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>>;
}

impl<'gc> FromValue<'gc> for i32 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is_int32() {
            return Ok(value.as_int32());
        } else {
            todo!()
        }
    }
}

impl<'gc> IntoValue<'gc> for Value<'gc> {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        self
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
        Value::null()
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

impl<'gc> IntoValue<'gc> for () {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::null()
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

impl<'gc> IntoValue<'gc> for usize {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Number::from_usize(_mc, self).into_value(_mc)
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

pub trait FromValues<'gc>: Sized {
    const ARITY: Arity = Arity {
        min: 1,
        max: Some(1),
    };

    fn from_values(
        ctx: Context<'gc>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, Value<'gc>>;
}

impl<'gc, T: FromValue<'gc>> FromValues<'gc> for T {
    fn from_values(
        ctx: Context<'gc>,
        mut values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<T, Value<'gc>> {
        if let Some(value) = values.next() {
            T::try_from_value(ctx, value)
        } else {
            todo!()
        }
    }
}

impl<'gc, T: FromValue<'gc>> FromValues<'gc> for Option<T> {
    fn from_values(
        ctx: Context<'gc>,
        mut values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, Value<'gc>> {
        if let Some(value) = values.next() {
            T::try_from_value(ctx, value).map(Some)
        } else {
            Ok(None)
        }
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

        impl<'gc, $first $(,$rest)*> FromValues<'gc> for ($first, $($rest,)*)
        where
            $first: FromValues<'gc>,
            $($rest: FromValues<'gc>,)*
        {
            #[allow(unused_mut, unused_assignments, unused_variables)]
            // Do our best at statically checking that tuple arity is correct
            // and tuple can be constructed from the values.
            const ARITY: Arity = {
                let first = <$first as FromValues<'gc>>::ARITY;
                let mut min = first.min;
                let mut max = first.max;
                let mut prev_was_optional = first.min == 0; // True if the first argument is Option-like (min arity 0)
                $(
                    let rest = <$rest as FromValues<'gc>>::ARITY; // Arity of the current $rest element
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


            fn from_values(ctx: Context<'gc>, mut values: impl Iterator<Item = Value<'gc>>) -> Result<Self, Value<'gc>> {
                Ok((
                    $first::from_values(ctx, &mut values)?,
                    $(
                        $rest::from_values(ctx, &mut values)?,
                    )*
                ))
            }
        }

        impl_tuple!($($rest)*);
    }
}

pub struct Rest<'gc>(pub Value<'gc>);

impl<'gc> Rest<'gc> {
    pub fn next<T: FromValue<'gc>>(&mut self, ctx: Context<'gc>) -> Result<T, Value<'gc>> {
        let value = self.0.car();
        self.0 = self.0.cdr();
        T::try_from_value(ctx, value)
    }
}

impl<'gc> FromValues<'gc> for Rest<'gc> {
    const ARITY: Arity = Arity { min: 0, max: None };

    fn from_values(
        _ctx: Context<'gc>,
        values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, Value<'gc>> {
        Ok(Rest(Value::null().list_append(_ctx, values)))
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
