use std::any::type_name;

use crate::runtime::{errors::Violation, thread::Context};

use super::*;
pub unsafe trait Tagged {
    const TC8: TypeCode8;
    const TC16: &[TypeCode16] = &[];

    /// Set to true when the type can only be encodded as a TC16. In that case
    /// `Value::is` and `Value::downcast` will always check `TC16` and not `TC8`.
    const ONLY_TC16: bool = false;
}

pub trait FromValue<'gc>: Sized {
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>>;
}

impl<'gc, T: Tagged> FromValue<'gc> for Gc<'gc, T> {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is::<T>() {
            Ok(value.downcast())
        } else {
            Err(Violation::WrongTypeArgument {
                who: None,
                position: 0,
                expected: type_name::<T>().into(),
                got: value,
                args: list!(value),
            }
            .into_value(_ctx))
        }
    }
}

impl<'gc, T> FromValue<'gc> for &T
where
    T: Tagged,
{
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is::<T>() {
            Ok(unsafe { value.desc.ptr.to_raw_address().as_ref() })
        } else {
            Err(Violation::WrongTypeArgument {
                who: None,
                position: 0,
                expected: type_name::<T>().into(),
                got: value,
                args: list!(value),
            }
            .into_value(_ctx))
        }
    }
}

impl<'gc> FromValue<'gc> for i32 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is_int32() {
            Ok(value.as_int32())
        } else {
            Err(Violation::WrongTypeArgument {
                who: None,
                position: 0,
                expected: "i32".into(),
                got: value,
                args: list!(value),
            }
            .into_value(_ctx))
        }
    }
}

impl<'gc> FromValue<'gc> for f64 {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is_flonum() {
            Ok(value.as_flonum())
        } else {
            Err(Violation::WrongTypeArgument {
                who: None,
                position: 0,
                expected: "f64".into(),
                got: value,
                args: list!(value),
            }
            .into_value(_ctx))
        }
    }
}

impl<'gc> FromValue<'gc> for bool {
    fn try_from_value(_: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        Ok(value.raw_i64() != Value::VALUE_FALSE)
    }
}

impl<'gc> FromValue<'gc> for char {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is_char() {
            Ok(value.char())
        } else {
            Err(Violation::WrongTypeArgument {
                who: None,
                position: 0,
                expected: "char".into(),
                got: value,
                args: list!(value),
            }
            .into_value(_ctx))
        }
    }
}

impl<'gc> FromValue<'gc> for () {
    fn try_from_value(_ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>> {
        if value.is_void() {
            Ok(())
        } else {
            Err(Violation::WrongTypeArgument {
                who: None,
                position: 0,
                expected: "()".into(),
                got: value,
                args: list!(value),
            }
            .into_value(_ctx))
        }
    }
}

impl<'gc> IntoValue<'gc> for Infallible {
    fn into_value(self, _ctx: Context<'gc>) -> Value<'gc> {
        panic!("Infallible conversion failed");
    }
}

/// An attempted conversion from a type to a `Value`.
///
/// Accepts mutation context to allow allocating heap objects.
pub trait TryIntoValue<'gc>: Sized {
    type Error;
    /// Performs the conversion.
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error>;
}

impl<'gc, T: Tagged> TryIntoValue<'gc> for Gc<'gc, T> {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Infallible> {
        Ok(Value::from(self))
    }
}
impl<'gc> TryIntoValue<'gc> for i32 {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_i32(self))
    }
}

impl<'gc> TryIntoValue<'gc> for f64 {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_f64(self))
    }
}

impl<'gc> TryIntoValue<'gc> for bool {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_bool(self))
    }
}

impl<'gc> TryIntoValue<'gc> for char {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_char(self))
    }
}

impl<'gc> TryIntoValue<'gc> for () {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::void())
    }
}

impl<'gc> TryIntoValue<'gc> for Value<'gc> {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(self)
    }
}

impl<'gc> TryIntoValue<'gc> for u8 {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for u16 {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for u32 {
    type Error = Self;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        if self > i32::MAX as u32 {
            return Err(self);
        }
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for u64 {
    type Error = Self;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        if self > i32::MAX as u64 {
            return Err(self);
        }
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for i64 {
    type Error = Self;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        if self > i32::MAX as i64 || self < i32::MIN as i64 {
            return Err(self);
        }
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for usize {
    type Error = Self;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        if self > i32::MAX as usize {
            return Err(self);
        }
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for isize {
    type Error = Self;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        if self > i32::MAX as isize || self < i32::MIN as isize {
            return Err(self);
        }
        Ok(Value::from_i32(self as i32))
    }
}

impl<'gc> TryIntoValue<'gc> for f32 {
    type Error = Infallible;
    fn try_into_value(self, _: Context<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(Value::from_f64(self as f64))
    }
}

impl<'gc> IntoValue<'gc> for &str {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::new(String::new(&ctx, self, false))
    }
}
impl<'gc> IntoValue<'gc> for std::string::String {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::new(String::new(&ctx, self.as_str(), false))
    }
}

/// A trait for types that can be converted into a `Value`.
pub trait IntoValue<'gc> {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc>;
}

impl<'gc, T> IntoValue<'gc> for T
where
    T: TryIntoValue<'gc, Error = Infallible>,
{
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        self.try_into_value(ctx)
            .unwrap_or_else(|_| panic!("infallible conversion failed"))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Arity {
    /// The minimum number of arguments.
    pub min: usize,
    /// The maximum number of arguments.
    pub max: Option<usize>,
}
#[derive(Clone, Copy)]
pub struct ValuesRef<'a, 'gc> {
    values: &'a [Value<'gc>],
    // current index into values.
    cursor: usize,
}

impl<'a, 'gc> ValuesRef<'a, 'gc> {
    pub fn args(self, ctx: Context<'gc>) -> Value<'gc> {
        Value::null().list_append(ctx, self.values.iter().copied())
    }

    pub fn remaining(self) -> &'a [Value<'gc>] {
        &self.values[self.cursor..]
    }

    pub fn from_slice(values: &'a [Value<'gc>]) -> Self {
        Self { values, cursor: 0 }
    }
}

impl<'a, 'gc> Iterator for ValuesRef<'a, 'gc> {
    type Item = Value<'gc>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor < self.values.len() {
            let value = self.values[self.cursor];
            self.cursor += 1;
            Some(value)
        } else {
            None
        }
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
    const ARITY: Arity = Arity {
        min: 1,
        max: Some(1),
    };

    fn from_values(
        ctx: Context<'gc>,
        mut values: impl Iterator<Item = Value<'gc>>,
    ) -> Result<Self, Value<'gc>> {
        if let Some(value) = values.next() {
            T::try_from_value(ctx, value)
        } else {
            Err(Violation::WrongNumberOfArguments {
                who: None,
                required_min: 1,
                required_max: Some(1),
                args: Value::null(),
            }
            .into_value(ctx))
        }
    }
}

impl<'gc, T: FromValue<'gc>> FromValues<'gc> for Option<T> {
    const ARITY: Arity = Arity {
        min: 0,
        max: Some(1),
    };

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

impl<'gc, T: IntoValue<'gc>, E: IntoValue<'gc>> IntoValues<'gc> for Result<T, E> {
    fn into_values(self, ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        enum ResultIter<'gc, I> {
            Ok(I),
            Err(std::iter::Once<Value<'gc>>),
        }

        impl<'gc, I> Iterator for ResultIter<'gc, I>
        where
            I: Iterator<Item = Value<'gc>>,
        {
            type Item = Value<'gc>;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    ResultIter::Ok(i) => i.next(),
                    ResultIter::Err(i) => i.next(),
                }
            }
        }

        match self {
            Ok(v) => std::iter::once(true.into()).chain(ResultIter::Ok(v.into_values(ctx))),
            Err(e) => std::iter::once(false.into())
                .chain(ResultIter::Err(std::iter::once(e.into_value(ctx)))),
        }
    }
}

impl<'gc> IntoValues<'gc> for Rest<'gc> {
    fn into_values(self, _ctx: Context<'gc>) -> impl Iterator<Item = Value<'gc>> {
        ListIterator::new(self.0)
    }
}

pub trait IntoSchemeFn<'gc, Args, R> {
    fn into_primitive(
        self,
    ) -> impl Fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Value<'gc>>;
}

macro_rules! impl_fn {
    () => {

        #[allow(non_snake_case)]
        impl<'gc, F, Return> IntoSchemeFn<'gc, (), Return> for F
        where F: Fn(Context<'gc>) -> Return,
            Return: IntoValues<'gc>,
        {
            fn into_primitive(self) -> impl Fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {
                move |ctx, args| {
                    if args.len() != 0 {
                        return Err(Violation::WrongNumberOfArguments {
                            who: None,
                            required_min: 0,
                            required_max: Some(0),
                            args: Value::null().list_append(ctx, args.iter().copied()),
                        }.into_value(ctx));
                    }
                    let mut values = self(ctx).into_values(ctx);
                    let first = values.next();
                    match first {
                        Some(first) => {
                            match values.next() {
                                Some(second) => {
                                    Ok(Value::new($crate::runtime::value::values::Values::from_list(ctx, $crate::list!(ctx, first, second).list_append(ctx, values))))
                                }
                                _ => Ok(first),
                            }
                        }

                        None => Ok(Value::void())
                    }
                }
            }
        }

    };

    ($first: ident $($rest:ident)*) => {
        #[allow(non_snake_case)]
        impl<'gc, F, $first, $($rest,)* Return: IntoValues<'gc>> IntoSchemeFn<'gc, ($first, $($rest,)*), Return> for F
        where
            F: Fn(Context<'gc>, $first, $($rest,)*) -> Return,
            ($first, $($rest,)*) : FromValues<'gc>,
        {
            fn into_primitive(self) -> impl Fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> {

                move |ctx, args| {

                    let mut values = ValuesRef::from_slice(args);
                    let ($first, $($rest,)*) = <($first, $($rest),*)>::from_values(ctx, &mut values)?;
                    if values.cursor != args.len() {
                        return Err(Violation::WrongNumberOfArguments {
                            who: None,
                            required_min: <($first, $($rest),*)>::ARITY.min,
                            required_max: <($first, $($rest),*)>::ARITY.max,
                            args: Value::null().list_append(ctx, args.iter().copied()),
                        }.into_value(ctx));
                    }
                    let mut values = self(ctx, $first, $($rest),*).into_values(ctx);

                    let first = values.next();
                    match first {
                        Some(first) => {
                            match values.next() {
                                Some(second) => {
                                    Ok(Value::new($crate::runtime::value::values::Values::from_list(ctx, $crate::list!(ctx, first, second).list_append(ctx, values))))
                                }
                                _ => Ok(first),
                            }
                        }

                        None => Ok(Value::void())
                    }
                }
            }
        }


        impl_fn!($($rest)*);
    }
}

impl_fn!(A B C D E F1 G H I J K L M N O P Q R S T U V W X Y Z AA AB AC AD AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY AZ);

impl<'gc> IntoValue<'gc> for isize {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        if self > i32::MAX as isize || self < i32::MIN as isize {
            return Value::new(BigInt::from_i128(ctx, self as i128));
        }
        Value::new(self as i32)
    }
}

impl<'gc> IntoValue<'gc> for usize {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        if self > i32::MAX as usize {
            return Value::new(BigInt::from_u128(ctx, self as u128));
        }
        Value::new(self as i32)
    }
}

impl<'gc> IntoValue<'gc> for u64 {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        if self > i32::MAX as u64 {
            return Value::new(BigInt::from_u128(ctx, self as u128));
        }
        Value::new(self as i32)
    }
}

impl<'gc> IntoValue<'gc> for u32 {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        if self > i32::MAX as u32 {
            return Value::new(BigInt::from_u128(ctx, self as u128));
        }
        Value::new(self as i32)
    }
}

impl<'gc> IntoValue<'gc> for i64 {
    fn into_value(self, ctx: Context<'gc>) -> Value<'gc> {
        if self > i32::MAX as i64 || self < i32::MIN as i64 {
            return Value::new(BigInt::from_i128(ctx, self as i128));
        }
        Value::new(self as i32)
    }
}