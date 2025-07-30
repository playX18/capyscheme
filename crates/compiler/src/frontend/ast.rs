use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt,
    hash::Hash,
    rc::Rc,
    sync::LazyLock,
};

use crate::runtime::{
    Context,
    value::{Str, Symbol},
};

use super::{Error, number::Number, source::Span};
use ariadne::Report;
use rsgc::{Gc, Trace};

impl<'gc> fmt::Debug for Datum<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone, Trace)]
#[collect(no_drop)]
pub struct Datum<'gc> {
    span: Cell<Span>,
    pub value: DatumValue<'gc>,
}

impl<'gc> From<Gc<'gc, Symbol<'gc>>> for Datum<'gc> {
    fn from(value: Gc<'gc, Symbol<'gc>>) -> Self {
        Datum {
            value: DatumValue::Symbol(value),
            span: Cell::new(Span::default()),
        }
    }
}

pub type DatumRef<'gc> = Gc<'gc, Datum<'gc>>;

#[derive(Clone, Debug, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum DatumValue<'gc> {
    Undefined,
    Void,
    Eof,
    Null,
    True,
    False,
    Symbol(Gc<'gc, Symbol<'gc>>),
    Number(Number),
    Char(char),
    Str(Gc<'gc, Str<'gc>>),
    Bytes(Vec<u8>),
    Pair(DatumRef<'gc>, DatumRef<'gc>),
    Vector(Vec<DatumRef<'gc>>),
    /// ERROR node.
    Error {
        /// Error itself
        error: Box<Error<'gc>>,
        /// Expressions that are stored in this node, they might also be `ERROR` nodes themselves.
        exprs: Vec<DatumRef<'gc>>,
    },
}

macro_rules! cxr {
    ($(
        ($cxr: ident => $first: ident $($cxr_op: ident)*)
    )*) => {
        impl<'gc> Datum<'gc> {
            $(
                pub fn $cxr(&self) -> Option<DatumRef<'gc>> {
                    self.$first()$(.and_then(|$cxr| $cxr.$cxr_op()))*
                }
            )*
        }
    };
}

cxr! {
    (caar => car car)
    (cadr => cdr car)
    (cdar => car cdr)
    (cddr => cdr cdr)
    (caaar => car car car)
    (caadr => cdr car car)
    (cadar => car cdr car)
    (caddr => cdr cdr car)
    (cdaar => car car cdr)
    (cdadr => cdr car cdr)
    (cddar => car cdr cdr)
    (cdddr => cdr cdr cdr)
    (caaaar => car car car car)
    (caaadr => cdr car car car)
    (caadar => car cdr car car)
    (caaddr => cdr cdr car car)
    (cadaar => car car cdr car)
    (cadadr => cdr car cdr car)
    (caddar => car cdr cdr car)
    (cadddr => cdr cdr cdr car)
    (cdaaar => car car car cdr)
    (cdaadr => cdr car car cdr)
    (cdadar => car cdr car cdr)
    (cdaddr => cdr cdr car cdr)
    (cddaar => car car cdr cdr)
    (cddadr => cdr car cdr cdr)
    (cdddar => car cdr cdr cdr)
    (cddddr => cdr cdr cdr cdr)
}

impl<'gc> Datum<'gc> {
    pub fn is_symbol(&self) -> bool {
        matches!(self.value, DatumValue::Symbol(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self.value, DatumValue::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self.value, DatumValue::Str(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self.value, DatumValue::Char(_))
    }

    pub fn is_bytes(&self) -> bool {
        matches!(self.value, DatumValue::Bytes(_))
    }

    pub fn is_pair(&self) -> bool {
        matches!(self.value, DatumValue::Pair(_, _))
    }

    pub fn is_vector(&self) -> bool {
        matches!(self.value, DatumValue::Vector(_))
    }

    pub fn new(ctx: Context<'gc>, value: DatumValue<'gc>) -> DatumRef<'gc> {
        Gc::new(
            &ctx,
            Self {
                span: Cell::new(Span::default()),
                value,
            },
        )
    }

    pub fn new_maybe_at(
        ctx: Context<'gc>,
        span: Option<Span>,
        value: DatumValue<'gc>,
    ) -> DatumRef<'gc> {
        let datum = Gc::new(
            &ctx,
            Self {
                span: Cell::new(span.unwrap_or(Span::default())),
                value,
            },
        );

        datum
    }

    pub fn cons(
        ctx: Context<'gc>,
        car: DatumRef<'gc>,
        cdr: DatumRef<'gc>,
        at: Option<Span>,
    ) -> DatumRef<'gc> {
        Gc::new(
            &ctx,
            Self {
                span: Cell::new(at.unwrap_or(Span::default())),
                value: DatumValue::Pair(car, cdr),
            },
        )
    }

    pub fn make_list(
        ctx: Context<'gc>,
        exprs: impl AsRef<[DatumRef<'gc>]>,
        at: Option<Span>,
    ) -> DatumRef<'gc> {
        let exprs = exprs.as_ref();

        exprs.iter().rev().fold(
            Gc::new(
                &ctx,
                Self {
                    span: Cell::new(at.unwrap_or(Span::default())),
                    value: DatumValue::Null,
                },
            ),
            |cdr, car| {
                Gc::new(
                    &ctx,
                    Self {
                        span: Cell::new(at.unwrap_or(Span::default())),
                        value: DatumValue::Pair(car.clone(), cdr),
                    },
                )
            },
        )
    }

    pub fn make_list_with(
        ctx: Context<'gc>,
        exprs: impl AsRef<[DatumRef<'gc>]>,
        append: DatumRef<'gc>,
        at: Option<Span>,
    ) -> DatumRef<'gc> {
        let exprs = exprs.as_ref();

        exprs.iter().rev().fold(append, |cdr, car| {
            Gc::new(
                &ctx,
                Self {
                    span: Cell::new(at.unwrap_or(Span::default())),
                    value: DatumValue::Pair(car.clone(), cdr),
                },
            )
        })
    }

    pub fn make_list_append(
        ctx: Context<'gc>,
        exprs: impl AsRef<[DatumRef<'gc>]>,
        append: impl AsRef<[DatumRef<'gc>]>,
        at: Option<Span>,
    ) -> DatumRef<'gc> {
        let exprs = exprs.as_ref();
        let append = append.as_ref();

        let mut result = Self::make_list(ctx, exprs, at);

        for item in append.iter().rev() {
            result = Self::cons(ctx, item.clone(), result, at);
        }

        result
    }

    pub fn make_vector(
        ctx: Context<'gc>,
        exprs: impl AsRef<[DatumRef<'gc>]>,
        at: Option<Span>,
    ) -> DatumRef<'gc> {
        let exprs = exprs.as_ref();
        Gc::new(
            &ctx,
            Self {
                span: Cell::new(at.unwrap_or(Span::default())),
                value: DatumValue::Vector(exprs.to_vec()),
            },
        )
    }

    pub fn with_span(self: DatumRef<'gc>, span: Span) -> DatumRef<'gc> {
        self.span.set(span);
        self
    }

    pub fn new_at(ctx: Context<'gc>, span: Span, value: DatumValue<'gc>) -> DatumRef<'gc> {
        Gc::new(
            &ctx,
            Self {
                span: Cell::new(span),
                value,
            },
        )
    }

    pub fn from_symbol(ctx: Context<'gc>, sym: Gc<'gc, Symbol<'gc>>) -> DatumRef<'gc> {
        Self::new(ctx, DatumValue::Symbol(sym))
    }

    pub fn make_symbol(ctx: Context<'gc>, name: &str, at: Option<Span>) -> DatumRef<'gc> {
        Gc::new(
            &ctx,
            Self {
                span: Cell::new(at.unwrap_or(Span::default())),
                value: DatumValue::Symbol(Symbol::from_str(ctx, name)),
            },
        )
    }

    pub fn make_str(ctx: Context<'gc>, str: &str) -> DatumRef<'gc> {
        Self::new(ctx, DatumValue::Str(Str::new(&ctx, str, true)))
    }

    pub fn span(&self) -> Span {
        self.span.get()
    }

    pub fn value(&self) -> &DatumValue<'gc> {
        &self.value
    }

    pub fn is_list(self: &DatumRef<'gc>) -> bool {
        if matches!(self.value, DatumValue::Null) {
            return true;
        }

        let mut ls = self;
        while let DatumValue::Pair(_, cdr) = &ls.value {
            ls = cdr;
        }

        matches!(ls.value, DatumValue::Null)
    }

    pub fn is_null(&self) -> bool {
        matches!(self.value, DatumValue::Null)
    }

    pub fn car(&self) -> Option<DatumRef<'gc>> {
        if let DatumValue::Pair(car, _) = &self.value {
            Some(*car)
        } else {
            None
        }
    }

    pub fn cdr(&self) -> Option<DatumRef<'gc>> {
        if let DatumValue::Pair(_, cdr) = &self.value {
            Some(*cdr)
        } else {
            None
        }
    }

    pub fn length(self: &DatumRef<'gc>) -> usize {
        let mut length = 0;
        let mut current = self;

        while let DatumValue::Pair(_, cdr) = &current.value {
            length += 1;
            current = cdr;
        }

        length
    }

    pub fn list_length(self: &DatumRef<'gc>) -> Option<usize> {
        let mut length = 0;
        let mut current = self;
        while let DatumValue::Pair(_, cdr) = &current.value {
            length += 1;
            current = cdr;
        }

        if matches!(current.value, DatumValue::Null) {
            Some(length)
        } else {
            None // Not a proper list
        }
    }

    pub fn is_tagged_list(self: &DatumRef<'gc>, value: DatumRef) -> bool {
        self.list_length()
            .filter(|&len| len > 0)
            .and_then(|_| self.car())
            .map(|tag| tag == value)
            .unwrap_or(false)
    }

    pub fn is_tagged_with_symbol(self: &DatumRef<'gc>, sym: &str) -> bool {
        self.list_length()
            .filter(|&len| len > 0)
            .and_then(|_| self.car())
            .map(|tag| match &tag.value {
                DatumValue::Symbol(symbol) => &**symbol == sym,
                _ => false,
            })
            .unwrap_or(false)
    }

    pub fn try_symbol(&self) -> Option<Gc<'gc, Symbol<'gc>>> {
        if let DatumValue::Symbol(symbol) = &self.value {
            Some(*symbol)
        } else {
            None
        }
    }

    pub fn try_string(&self) -> Option<Gc<'gc, Str<'gc>>> {
        if let DatumValue::Str(spur) = &self.value {
            Some(*spur)
        } else {
            None
        }
    }

    pub fn try_pair(&self) -> Option<(DatumRef<'gc>, DatumRef<'gc>)> {
        if let DatumValue::Pair(car, cdr) = &self.value {
            Some((*car, *cdr))
        } else {
            None
        }
    }

    pub fn collect_errors_into<'a>(&'a self, out: &mut Vec<&'a Box<Error<'gc>>>) {
        match &self.value {
            DatumValue::Error { error, exprs } => {
                out.push(error);
                for expr in exprs {
                    expr.collect_errors_into(out);
                }
            }

            DatumValue::Pair(car, cdr) => {
                car.collect_errors_into(out);
                cdr.collect_errors_into(out);
            }

            DatumValue::Vector(vec) => {
                for datum in vec {
                    datum.collect_errors_into(out);
                }
            }
            _ => {}
        }
    }

    pub fn collect_errors(self: DatumRef<'gc>) -> Vec<&'gc Box<Error<'gc>>> {
        let mut errors = Vec::new();
        self.collect_errors_into(&mut errors);

        errors
    }

    /// Fold left over a list with a function and initial accumulator
    /// foldl(f, acc, list) = foldl(f, f(acc, car(list)), cdr(list))
    pub fn foldl<T, F>(self: &DatumRef<'gc>, f: F, mut acc: T) -> T
    where
        F: Fn(T, &DatumRef<'gc>) -> T,
    {
        let mut current = self;
        while let DatumValue::Pair(car, cdr) = &current.value {
            acc = f(acc, car);
            current = cdr;
        }
        acc
    }

    /// Fold right over a list with a function and initial accumulator
    /// foldr(f, acc, list) = f(car(list), foldr(f, acc, cdr(list)))
    pub fn foldr<T, F>(self: DatumRef<'gc>, f: F, acc: T) -> T
    where
        F: Fn(DatumRef<'gc>, T) -> T,
    {
        fn foldr<'gc, T>(this: DatumRef<'gc>, f: &dyn Fn(DatumRef<'gc>, T) -> T, acc: T) -> T {
            match &this.value {
                DatumValue::Null => acc,
                DatumValue::Pair(car, cdr) => {
                    let rest_result = cdr.foldr(f, acc);
                    f(*car, rest_result)
                }
                _ => acc, // Not a proper list
            }
        }

        foldr(self, &f, acc)
    }

    /// Map a function over a list, returning a new list
    pub fn map<F>(self: DatumRef<'gc>, ctx: Context<'gc>, f: F) -> DatumRef<'gc>
    where
        F: Fn(DatumRef<'gc>) -> DatumRef<'gc>,
    {
        match &self.value {
            DatumValue::Null => self.clone(),
            DatumValue::Pair(car, cdr) => {
                let new_car = f(*car);
                let new_cdr = cdr.map(ctx, f);
                Self::cons(ctx, new_car, new_cdr, Some(self.span()))
            }
            _ => self.clone(), // Not a proper list, return as-is
        }
    }

    pub fn try_map<F, E>(self: DatumRef<'gc>, ctx: Context<'gc>, f: F) -> Result<DatumRef<'gc>, E>
    where
        F: Fn(DatumRef<'gc>) -> Result<DatumRef<'gc>, E>,
    {
        match &self.value {
            DatumValue::Null => Ok(self.clone()),
            DatumValue::Pair(car, cdr) => {
                let new_car = f(*car)?;
                let new_cdr = cdr.try_map(ctx, f)?;
                Ok(Self::cons(ctx, new_car, new_cdr, Some(self.span())))
            }
            _ => Ok(self.clone()), // Not a proper list, return as-is
        }
    }

    /// Filter a list with a predicate function
    pub fn filter<F>(self: DatumRef<'gc>, ctx: Context<'gc>, predicate: F) -> DatumRef<'gc>
    where
        F: Fn(DatumRef<'gc>) -> bool,
    {
        let mut ls = self.clone();
        let mut new = Self::new_at(ctx, self.span.get().clone(), DatumValue::Null);
        while let DatumValue::Pair(car, cdr) = &ls.value {
            if predicate(*car) {
                new = Self::cons(ctx, car.clone(), new, Some(self.span()));
            }
            ls = cdr.clone();
        }

        new
    }

    /// Reverse a list
    pub fn reverse(self: DatumRef<'gc>, ctx: Context<'gc>) -> DatumRef<'gc> {
        self.foldl(
            |acc, item| Self::cons(ctx, item.clone(), acc, Some(self.span())),
            Self::new(ctx, DatumValue::Null),
        )
    }

    /// Append two lists together
    pub fn append(self: DatumRef<'gc>, ctx: Context<'gc>, other: &DatumRef<'gc>) -> DatumRef<'gc> {
        match &self.value {
            DatumValue::Null => other.clone(),
            DatumValue::Pair(car, cdr) => {
                let appended_cdr = cdr.append(ctx, other);
                Self::cons(ctx, car.clone(), appended_cdr, Some(self.span()))
            }
            _ => other.clone(), // Not a proper list
        }
    }

    /// Find the first element in a list that satisfies a predicate
    pub fn find<F>(self: &DatumRef<'gc>, predicate: F) -> Option<DatumRef<'gc>>
    where
        F: Fn(&DatumRef<'gc>) -> bool,
    {
        let mut current = self;
        while let DatumValue::Pair(car, cdr) = &current.value {
            if predicate(car) {
                return Some(car.clone());
            }
            current = cdr;
        }
        None
    }

    /// Check if any element in a list satisfies a predicate
    pub fn any<F>(self: &DatumRef<'gc>, predicate: F) -> bool
    where
        F: Fn(&DatumRef<'gc>) -> bool,
    {
        let mut current = self;
        while let DatumValue::Pair(car, cdr) = &current.value {
            if predicate(car) {
                return true;
            }
            current = cdr;
        }
        false
    }

    /// Check if all elements in a list satisfy a predicate
    pub fn all<F>(self: &DatumRef<'gc>, predicate: F) -> bool
    where
        F: Fn(&DatumRef<'gc>) -> bool,
    {
        let mut current = self;
        while let DatumValue::Pair(car, cdr) = &current.value {
            if !predicate(car) {
                return false;
            }
            current = cdr;
        }
        true
    }

    /// Take the first n elements from a list
    pub fn take(self: DatumRef<'gc>, ctx: Context<'gc>, n: usize) -> DatumRef<'gc> {
        if n == 0 {
            return Self::new(ctx, DatumValue::Null);
        }

        match &self.value {
            DatumValue::Null => self.clone(),
            DatumValue::Pair(car, cdr) => {
                let taken_cdr = cdr.take(ctx, n - 1);
                Self::cons(ctx, car.clone(), taken_cdr, Some(self.span()))
            }
            _ => self.clone(), // Not a proper list
        }
    }

    /// Drop the first n elements from a list
    pub fn drop(self: &DatumRef<'gc>, n: usize) -> DatumRef<'gc> {
        let mut current = self;
        let mut remaining = n;

        while remaining > 0 {
            match &current.value {
                DatumValue::Pair(_, cdr) => {
                    current = cdr;
                    remaining -= 1;
                }
                _ => return current.clone(), // Reached end or not a proper list
            }
        }

        current.clone()
    }

    /// Get the nth element from a list (0-indexed)
    pub fn nth(self: &DatumRef<'gc>, n: usize) -> Option<DatumRef<'gc>> {
        let mut current = self;
        let mut index = 0;

        while let DatumValue::Pair(car, cdr) = &current.value {
            if index == n {
                return Some(car.clone());
            }
            current = cdr;
            index += 1;
        }
        None
    }

    /// Convert a list to a Vec<P<Datum>>
    pub fn to_vec(self: &DatumRef<'gc>) -> Vec<DatumRef<'gc>> {
        self.foldl(
            |mut acc, item| {
                acc.push(item.clone());
                acc
            },
            Vec::new(),
        )
    }

    /// Create a list from a Vec<P<Datum>>
    pub fn from_vec(ctx: Context<'gc>, vec: Vec<DatumRef<'gc>>) -> DatumRef<'gc> {
        Self::make_list(ctx, vec, None)
    }

    /// Zip two lists together, stopping at the shorter list
    pub fn zip(self: &DatumRef<'gc>, ctx: Context<'gc>, other: DatumRef<'gc>) -> DatumRef<'gc> {
        match (&self.value, &other.value) {
            (DatumValue::Pair(car1, cdr1), DatumValue::Pair(car2, cdr2)) => {
                let pair = Self::make_list(ctx, [car1.clone(), car2.clone()], Some(self.span()));
                let zipped_rest = cdr1.zip(ctx, *cdr2);
                Self::cons(ctx, pair, zipped_rest, Some(self.span()))
            }
            _ => Self::new(ctx, DatumValue::Null), // One or both lists ended
        }
    }

    /// Flatten a list of lists into a single list
    pub fn flatten(self: &DatumRef<'gc>, ctx: Context<'gc>) -> DatumRef<'gc> {
        match &self.value {
            DatumValue::Null => self.clone(),
            DatumValue::Pair(car, cdr) => {
                let flattened_cdr = cdr.flatten(ctx);
                match &car.value {
                    DatumValue::Null => flattened_cdr,
                    DatumValue::Pair(_, _) => car.append(ctx, &flattened_cdr),
                    _ => Self::cons(ctx, car.clone(), flattened_cdr, Some(self.span())),
                }
            }
            _ => self.clone(), // Not a proper list
        }
    }

    /// Partition a list into two lists based on a predicate
    pub fn partition<F>(
        self: &DatumRef<'gc>,
        ctx: Context<'gc>,
        predicate: F,
    ) -> (DatumRef<'gc>, DatumRef<'gc>)
    where
        F: Fn(&DatumRef<'gc>) -> bool,
    {
        let (trues, falses) = self.foldl(
            |mut acc, item| {
                if predicate(item) {
                    acc.0.push(item.clone());
                } else {
                    acc.1.push(item.clone());
                }
                acc
            },
            (Vec::new(), Vec::new()),
        );

        (Self::from_vec(ctx, trues), Self::from_vec(ctx, falses))
    }

    pub fn is_lambda(self: &DatumRef<'gc>) -> bool {
        self.is_tagged_with_symbol("lambda")
    }

    pub fn is_quote(self: &DatumRef<'gc>) -> bool {
        self.is_tagged_with_symbol("quote")
    }

    pub fn is_begin(self: &DatumRef<'gc>) -> bool {
        self.is_tagged_with_symbol("begin")
    }

    pub fn is_define(self: &DatumRef<'gc>) -> bool {
        self.is_tagged_with_symbol("define")
    }

    pub fn is_if(self: &DatumRef<'gc>) -> bool {
        self.is_tagged_with_symbol("if")
    }

    pub fn is_if_syntax(self: &DatumRef<'gc>) -> bool {
        self.is_if() && (self.length() == 3 || self.length() == 4)
    }

    pub fn is_set(self: &DatumRef<'gc>) -> bool {
        self.is_tagged_with_symbol("set!")
    }

    pub fn if_condition(self: DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_if() { self.cadr() } else { None }
    }

    pub fn if_then(self: DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_if() { self.caddr() } else { None }
    }

    pub fn if_else(self: DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_if() { self.cadddr() } else { None }
    }

    pub fn if_has_else(self: &DatumRef<'gc>) -> bool {
        self.is_if() && self.length() > 3
    }

    pub fn is_app(self: &DatumRef<'gc>) -> bool {
        self.is_pair()
    }

    pub fn is_constant(&self) -> bool {
        matches!(
            self.value,
            DatumValue::Undefined
                | DatumValue::Void
                | DatumValue::Eof
                | DatumValue::Null
                | DatumValue::True
                | DatumValue::False
                | DatumValue::Number(_)
                | DatumValue::Char(_)
                | DatumValue::Str(_)
                | DatumValue::Bytes(_)
        )
    }

    pub fn set_var(self: DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_set() { self.cadr() } else { None }
    }

    pub fn set_value(self: &DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_set() { self.caddr() } else { None }
    }

    pub fn is_define_lambda(self: DatumRef<'gc>) -> bool {
        let Some(var) = self.cadr() else {
            return false;
        };

        (var.is_list() && var.length() > 0 && var.car().map(|car| car.is_symbol()).unwrap_or(false))
            || (var.is_pair() && var.car().map(|car| car.is_symbol()).unwrap_or(false))
    }

    pub fn define_to_lambda(self: &DatumRef<'gc>, ctx: Context<'gc>) -> DatumRef<'gc> {
        if self.is_define_lambda() {
            let Some(var) = self.caadr() else {
                return self.clone();
            };
            let Some(args) = self.cdadr() else {
                return self.clone();
            };
            let Some(body) = self.cddr() else {
                return self.clone();
            };

            let lambda = Datum::make_list_with(
                ctx,
                &[
                    Datum::new_at(
                        ctx,
                        var.span(),
                        DatumValue::Symbol(Symbol::from_str(ctx, "lambda")),
                    ),
                    args.clone(),
                ],
                body.clone(),
                Some(self.span()),
            );

            Datum::make_list(
                ctx,
                &[
                    Datum::new_at(
                        ctx,
                        self.span(),
                        DatumValue::Symbol(Symbol::from_str(ctx, "define")),
                    ),
                    var.clone(),
                    lambda,
                ],
                Some(self.span()),
            )
        } else {
            self.clone()
        }
    }

    pub fn define_var(self: &DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_define_lambda() {
            self.caadr()
        } else {
            self.cadr()
        }
    }

    pub fn define_exp(&self) -> Option<DatumRef<'gc>> {
        self.cddr()
    }

    pub fn lambda_exp(self: &DatumRef<'gc>) -> Option<DatumRef<'gc>> {
        if self.is_lambda() { self.cddr() } else { None }
    }
}

impl<'gc> PartialEq for Datum<'gc> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.value, &other.value) {
            (DatumValue::Symbol(s1), DatumValue::Symbol(s2)) => s1 == s2,
            (DatumValue::Number(n1), DatumValue::Number(n2)) => n1 == n2,
            (DatumValue::Str(s1), DatumValue::Str(s2)) => s1 == s2,
            (DatumValue::Char(c1), DatumValue::Char(c2)) => c1 == c2,
            (DatumValue::Bytes(b1), DatumValue::Bytes(b2)) => b1 == b2,
            (DatumValue::Pair(p1, p2), DatumValue::Pair(q1, q2)) => p1 == q1 && p2 == q2,
            (DatumValue::Vector(v1), DatumValue::Vector(v2)) => v1 == v2,
            (DatumValue::Null, DatumValue::Null) => true,
            (DatumValue::True, DatumValue::True) => true,
            (DatumValue::False, DatumValue::False) => true,
            _ => false,
        }
    }
}

impl<'gc> Eq for Datum<'gc> {}

impl<'gc> Hash for Datum<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.value {
            DatumValue::Symbol(s) => s.hash(state),
            DatumValue::Number(n) => n.hash(state),
            DatumValue::Str(s) => s.hash(state),
            DatumValue::Char(c) => c.hash(state),
            DatumValue::Bytes(b) => b.hash(state),
            DatumValue::Pair(p1, p2) => {
                p1.hash(state);
                p2.hash(state);
            }
            DatumValue::Vector(v) => v.hash(state),
            _ => (),
        }
    }
}

impl<'gc> fmt::Display for Datum<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            DatumValue::Undefined => write!(f, "#<undefined>"),
            DatumValue::Void => write!(f, "#<void>"),
            DatumValue::Eof => write!(f, "#<eof>"),
            DatumValue::Null => write!(f, "()"),
            DatumValue::True => write!(f, "#t"),
            DatumValue::False => write!(f, "#f"),
            DatumValue::Symbol(symbol) => write!(f, "{}", symbol),
            DatumValue::Number(number) => write!(f, "{}", number),
            DatumValue::Char(ch) => match ch {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                '\r' => write!(f, "#\\return"),
                c if c.is_ascii_graphic() => write!(f, "#\\{}", c),
                c => write!(f, "#\\x{:X}", *c as u32),
            },
            DatumValue::Str(spur) => {
                write!(
                    f,
                    "\"{}\"",
                    spur.to_string().replace('\\', "\\\\").replace('"', "\\\"")
                )
            }
            DatumValue::Bytes(bytes) => {
                write!(f, "#u8(")?;
                for (i, byte) in bytes.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", byte)?;
                }
                write!(f, ")")
            }
            DatumValue::Pair(car, cdr) => {
                write!(f, "(")?;
                self.fmt_pair_contents(f, car, cdr)?;
                write!(f, ")")
            }
            DatumValue::Vector(vec) => {
                write!(f, "#(")?;
                for (i, datum) in vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", datum)?;
                }
                write!(f, ")")
            }
            DatumValue::Error { .. } => write!(f, "#<error>"),
            _ => write!(f, "#<unknown>"),
        }
    }
}

impl<'gc> Datum<'gc> {
    fn fmt_pair_contents(
        &self,
        f: &mut fmt::Formatter<'_>,
        car: &DatumRef<'gc>,
        cdr: &DatumRef<'gc>,
    ) -> fmt::Result {
        write!(f, "{}", car)?;

        match &cdr.value {
            DatumValue::Null => Ok(()),
            DatumValue::Pair(next_car, next_cdr) => {
                write!(f, " ")?;
                cdr.fmt_pair_contents(f, next_car, next_cdr)
            }
            _ => write!(f, " . {}", cdr),
        }
    }
}

pub fn collect_errors_and_report<'a, 'gc: 'a>(
    program: impl Iterator<Item = DatumRef<'gc>>,
    output: &'gc mut Vec<Report<'gc, Span>>,
) {
    program
        .flat_map(|datum| datum.collect_errors())
        .for_each(|error| error.build_reports(output));
}

pub struct PairIterator<'a> {
    current: Option<DatumRef<'a>>,
}

impl<'a> PairIterator<'a> {
    pub fn new(start: DatumRef<'a>) -> Self {
        Self {
            current: Some(start),
        }
    }
}

impl<'a> Iterator for PairIterator<'a> {
    type Item = DatumRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            match &current.value {
                DatumValue::Pair(car, cdr) => {
                    self.current = match &cdr.value {
                        DatumValue::Pair(_, _) => Some(*cdr),
                        DatumValue::Null => None,
                        _ => Some(*cdr), // If it's not a proper pair, we still return the cdr
                    };
                    Some(*car)
                }
                _ => {
                    if matches!(current.value, DatumValue::Null) {
                        self.current = None;
                        return None;
                    }
                    self.current = None;
                    Some(current)
                }
            }
        } else {
            None
        }
    }
}

impl<'gc> Datum<'gc> {
    pub fn iter_pairs(self: DatumRef<'gc>) -> PairIterator<'gc> {
        PairIterator::new(self)
    }
}

impl<'gc> From<i32> for Datum<'gc> {
    fn from(value: i32) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Number(Number::ExactFixnum(value as _)),
        }
    }
}

impl<'gc> From<f64> for Datum<'gc> {
    fn from(value: f64) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Number(Number::InexactReal(value)),
        }
    }
}

impl<'gc> From<i64> for Datum<'gc> {
    fn from(value: i64) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Number(Number::ExactFixnum(value as _)),
        }
    }
}

impl<'gc> From<bool> for Datum<'gc> {
    fn from(value: bool) -> Self {
        let value = if value {
            DatumValue::True
        } else {
            DatumValue::False
        };

        Datum {
            span: Cell::new(Span::default()),
            value,
        }
    }
}
