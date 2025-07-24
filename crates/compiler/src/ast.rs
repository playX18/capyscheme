use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt,
    hash::Hash,
    rc::Rc,
    sync::LazyLock,
};

use super::expand::*;
use crate::{il::term::IForm, number::Number, source::Span, Error};
use ariadne::Report;
use lasso::{Spur, ThreadedRodeo};
use once_cell::unsync::OnceCell;

pub type P<T> = Rc<T>;

#[allow(non_snake_case)]
pub fn P<T>(value: T) -> P<T> {
    P::new(value)
}

thread_local! {
    static SYMBOLS: RefCell<HashMap<Spur, P<Symbol>>> = RefCell::new(HashMap::new());
}

#[derive(Clone)]
pub enum Symbol {
    Interned(Spur),
    Uninterned(Spur),
    Generated(P<Self>, ()),
}

impl Symbol {
    pub fn new(name: &str) -> P<Self> {
        let spur = INTERNER.get_or_intern(name);
        SYMBOLS.with(|symbols| {
            let mut symbols = symbols.borrow_mut();
            if let Some(symbol) = symbols.get(&spur) {
                return symbol.clone();
            }
            let symbol = P(Symbol::Interned(spur));

            symbols.insert(spur, symbol.clone());
            symbol
        })
    }

    pub fn uninterned(name: &str) -> P<Self> {
        let spur = INTERNER.get_or_intern(name);
        P(Symbol::Uninterned(spur))
    }

    pub fn str(&self) -> &str {
        match self {
            Symbol::Interned(spur) => INTERNER.resolve(spur),
            Symbol::Uninterned(spur) => INTERNER.resolve(spur),
            Symbol::Generated(rc, _) => INTERNER.resolve(rc.as_ref().spur()),
        }
    }
    pub fn root(self: &P<Self>) -> P<Self> {
        match &**self {
            Symbol::Interned(_) | Symbol::Uninterned(_) => self.clone(),
            Symbol::Generated(rc, _) => rc.root(),
        }
    }

    pub fn spur(&self) -> &Spur {
        match self {
            Symbol::Interned(spur) => spur,
            Symbol::Uninterned(spur) => spur,
            Symbol::Generated(rc, _) => rc.as_ref().spur(),
        }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Interned(spur) => spur.hash(state),
            Symbol::Uninterned(spur) => spur.hash(state),
            Symbol::Generated(rc, _) => rc.as_ref().hash(state),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", INTERNER.resolve(self.spur()))
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<symbol '{}'>", INTERNER.resolve(self.spur()))
    }
}

pub static INTERNER: LazyLock<ThreadedRodeo> = LazyLock::new(|| ThreadedRodeo::new());

impl fmt::Debug for Datum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone)]
pub struct Datum {
    span: Cell<Span>,
    pub comments: Vec<String>,
    pub value: DatumValue,
}

impl From<P<Symbol>> for Datum {
    fn from(value: P<Symbol>) -> Self {
        Datum {
            value: DatumValue::Symbol(value),
            span: Cell::new(Span::default()),
            comments: Vec::new(),
        }
    }
}

unsafe impl Send for Datum {}
unsafe impl Sync for Datum {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DatumValue {
    Undefined,
    Void,
    Eof,
    Null,
    True,
    False,
    Symbol(P<Symbol>),
    Number(Number),
    Char(char),
    Str(Spur),
    Bytes(Vec<u8>),
    Pair(P<Datum>, P<Datum>),
    Vector(Vec<P<Datum>>),
    /// ERROR node.
    Error {
        /// Error itself
        error: Box<Error>,
        /// Expressions that are stored in this node, they might also be `ERROR` nodes themselves.
        exprs: Vec<P<Datum>>,
    },

    Identifier(P<Identifier>),
    PVRef(PVRef),
    SyntaxRules(P<SyntaxRules>),
    Special(fn(&P<Datum>, &mut Cenv) -> Result<P<IForm>, Box<Error>>),
}

macro_rules! cxr {
    ($(
        ($cxr: ident => $first: ident $($cxr_op: ident)*)
    )*) => {
        impl Datum {
            $(
                pub fn $cxr(&self) -> Option<&P<Self>> {
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

impl Datum {
    pub fn has_id(&self, id: Spur) -> bool {
        *self.identifier_to_symbol().spur() == id
    }

    pub fn is_id(&self) -> bool {
        matches!(
            self.value,
            DatumValue::Symbol(_) | DatumValue::Identifier(_)
        )
    }
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

    pub fn new(value: DatumValue) -> P<Self> {
        P(Self {
            span: Cell::new(Span::default()),
            value,
            comments: Vec::new(),
        })
    }

    pub fn new_maybe_at(span: Option<Span>, value: DatumValue) -> P<Self> {
        let datum = P(Self {
            span: Cell::new(span.unwrap_or(Span::default())),
            value,
            comments: Vec::new(),
        });

        datum
    }

    pub fn cons(car: P<Self>, cdr: P<Self>, at: Option<Span>) -> P<Self> {
        P(Self {
            span: Cell::new(at.unwrap_or(Span::default())),
            value: DatumValue::Pair(car, cdr),
            comments: Vec::new(),
        })
    }

    pub fn make_list(exprs: impl AsRef<[P<Self>]>, at: Option<Span>) -> P<Self> {
        let exprs = exprs.as_ref();

        exprs.iter().rev().fold(
            P(Self {
                span: Cell::new(at.unwrap_or(Span::default())),
                value: DatumValue::Null,
                comments: Vec::new(),
            }),
            |cdr, car| {
                P(Self {
                    span: Cell::new(at.unwrap_or(Span::default())),
                    value: DatumValue::Pair(car.clone(), cdr),
                    comments: Vec::new(),
                })
            },
        )
    }

    pub fn make_list_with(
        exprs: impl AsRef<[P<Self>]>,
        append: P<Self>,
        at: Option<Span>,
    ) -> P<Self> {
        let exprs = exprs.as_ref();

        exprs.iter().rev().fold(append, |cdr, car| {
            P(Self {
                span: Cell::new(at.unwrap_or(Span::default())),
                value: DatumValue::Pair(car.clone(), cdr),
                comments: Vec::new(),
            })
        })
    }

    pub fn make_list_append(
        exprs: impl AsRef<[P<Self>]>,
        append: impl AsRef<[P<Self>]>,
        at: Option<Span>,
    ) -> P<Self> {
        let exprs = exprs.as_ref();
        let append = append.as_ref();

        let mut result = Self::make_list(exprs, at);

        for item in append.iter().rev() {
            result = Self::cons(item.clone(), result, at);
        }

        result
    }

    pub fn make_vector(exprs: impl AsRef<[P<Self>]>, at: Option<Span>) -> P<Self> {
        let exprs = exprs.as_ref();
        P(Self {
            span: Cell::new(at.unwrap_or(Span::default())),
            value: DatumValue::Vector(exprs.to_vec()),
            comments: Vec::new(),
        })
    }

    pub fn with_span(self: P<Self>, span: Span) -> P<Self> {
        self.span.set(span);
        self
    }

    pub fn new_at(span: Span, value: DatumValue) -> P<Self> {
        P(Self {
            span: Cell::new(span),
            value,
            comments: Vec::new(),
        })
    }

    pub fn from_symbol(sym: P<Symbol>) -> P<Self> {
        Self::new(DatumValue::Symbol(sym))
    }

    pub fn make_symbol(name: &str, at: Option<Span>) -> P<Self> {
        P(Self {
            span: Cell::new(at.unwrap_or(Span::default())),
            value: DatumValue::Symbol(Symbol::new(name)),
            comments: Vec::new(),
        })
    }

    pub fn make_str(str: &str) -> P<Self> {
        Self::new(DatumValue::Str(INTERNER.get_or_intern(str)))
    }

    pub fn span(&self) -> Span {
        self.span.get()
    }

    pub fn value(&self) -> &DatumValue {
        &self.value
    }

    pub fn is_list(self: &P<Self>) -> bool {
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

    pub fn car(&self) -> Option<&P<Self>> {
        if let DatumValue::Pair(car, _) = &self.value {
            Some(car)
        } else {
            None
        }
    }

    pub fn cdr(&self) -> Option<&P<Self>> {
        if let DatumValue::Pair(_, cdr) = &self.value {
            Some(cdr)
        } else {
            None
        }
    }

    pub fn length(self: &P<Self>) -> usize {
        let mut length = 0;
        let mut current = self;

        while let DatumValue::Pair(_, cdr) = &current.value {
            length += 1;
            current = cdr;
        }

        length
    }

    pub fn list_length(self: &P<Self>) -> Option<usize> {
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

    pub fn is_tagged_list(self: &P<Self>, value: &Datum) -> bool {
        self.list_length()
            .filter(|&len| len > 0)
            .and_then(|_| self.car())
            .map(|tag| &**tag == value)
            .unwrap_or(false)
    }

    pub fn is_tagged_with_symbol(self: &P<Self>, sym: &str) -> bool {
        self.list_length()
            .filter(|&len| len > 0)
            .and_then(|_| self.car())
            .map(|tag| match &tag.value {
                DatumValue::Symbol(symbol) => symbol.str() == sym,
                _ => false,
            })
            .unwrap_or(false)
    }

    pub fn try_symbol(&self) -> Option<&P<Symbol>> {
        if let DatumValue::Symbol(symbol) = &self.value {
            Some(symbol)
        } else {
            None
        }
    }

    pub fn try_string(&self) -> Option<&str> {
        if let DatumValue::Str(spur) = &self.value {
            Some(INTERNER.resolve(spur))
        } else {
            None
        }
    }

    pub fn try_pair(&self) -> Option<(&P<Self>, &P<Self>)> {
        if let DatumValue::Pair(car, cdr) = &self.value {
            Some((car, cdr))
        } else {
            None
        }
    }

    pub fn collect_errors_into<'a>(&'a self, out: &mut Vec<&'a Box<Error>>) {
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

    pub fn collect_errors<'a>(&'a self) -> Vec<&'a Box<Error>> {
        let mut errors = Vec::new();
        self.collect_errors_into(&mut errors);

        errors
    }

    /// Fold left over a list with a function and initial accumulator
    /// foldl(f, acc, list) = foldl(f, f(acc, car(list)), cdr(list))
    pub fn foldl<T, F>(self: &P<Self>, f: F, mut acc: T) -> T
    where
        F: Fn(T, &P<Self>) -> T,
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
    pub fn foldr<T, F>(self: &P<Self>, f: F, acc: T) -> T
    where
        F: Fn(&P<Self>, T) -> T,
    {
        fn foldr<T>(this: &P<Datum>, f: &dyn Fn(&P<Datum>, T) -> T, acc: T) -> T {
            match &this.value {
                DatumValue::Null => acc,
                DatumValue::Pair(car, cdr) => {
                    let rest_result = cdr.foldr(&f, acc);
                    f(car, rest_result)
                }
                _ => acc, // Not a proper list
            }
        }

        foldr(self, &f, acc)
    }

    /// Map a function over a list, returning a new list
    pub fn map<F>(self: &P<Self>, f: F) -> P<Self>
    where
        F: Fn(&P<Self>) -> P<Self>,
    {
        match &self.value {
            DatumValue::Null => self.clone(),
            DatumValue::Pair(car, cdr) => {
                let new_car = f(car);
                let new_cdr = cdr.map(f);
                Self::cons(new_car, new_cdr, Some(self.span()))
            }
            _ => self.clone(), // Not a proper list, return as-is
        }
    }

    pub fn try_map<F, E>(self: &P<Self>, f: F) -> Result<P<Self>, E>
    where
        F: Fn(&P<Self>) -> Result<P<Self>, E>,
    {
        match &self.value {
            DatumValue::Null => Ok(self.clone()),
            DatumValue::Pair(car, cdr) => {
                let new_car = f(car)?;
                let new_cdr = cdr.try_map(f)?;
                Ok(Self::cons(new_car, new_cdr, Some(self.span())))
            }
            _ => Ok(self.clone()), // Not a proper list, return as-is
        }
    }

    /// Filter a list with a predicate function
    pub fn filter<F>(self: &P<Self>, predicate: F) -> P<Self>
    where
        F: Fn(&P<Self>) -> bool,
    {
        let mut ls = self.clone();
        let mut new = Self::new_at(self.span.get().clone(), DatumValue::Null);
        while let DatumValue::Pair(car, cdr) = &ls.value {
            if predicate(car) {
                new = Self::cons(car.clone(), new, Some(self.span()));
            }
            ls = cdr.clone();
        }

        new
    }

    /// Reverse a list
    pub fn reverse(self: &P<Self>) -> P<Self> {
        self.foldl(
            |acc, item| Self::cons(item.clone(), acc, Some(self.span())),
            Self::new(DatumValue::Null),
        )
    }

    /// Append two lists together
    pub fn append(self: &P<Self>, other: &P<Self>) -> P<Self> {
        match &self.value {
            DatumValue::Null => other.clone(),
            DatumValue::Pair(car, cdr) => {
                let appended_cdr = cdr.append(other);
                Self::cons(car.clone(), appended_cdr, Some(self.span()))
            }
            _ => other.clone(), // Not a proper list
        }
    }

    /// Find the first element in a list that satisfies a predicate
    pub fn find<F>(self: &P<Self>, predicate: F) -> Option<P<Self>>
    where
        F: Fn(&P<Self>) -> bool,
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
    pub fn any<F>(self: &P<Self>, predicate: F) -> bool
    where
        F: Fn(&P<Self>) -> bool,
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
    pub fn all<F>(self: &P<Self>, predicate: F) -> bool
    where
        F: Fn(&P<Self>) -> bool,
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
    pub fn take(self: &P<Self>, n: usize) -> P<Self> {
        if n == 0 {
            return Self::new(DatumValue::Null);
        }

        match &self.value {
            DatumValue::Null => self.clone(),
            DatumValue::Pair(car, cdr) => {
                let taken_cdr = cdr.take(n - 1);
                Self::cons(car.clone(), taken_cdr, Some(self.span()))
            }
            _ => self.clone(), // Not a proper list
        }
    }

    /// Drop the first n elements from a list
    pub fn drop(self: &P<Self>, n: usize) -> P<Self> {
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
    pub fn nth(self: &P<Self>, n: usize) -> Option<P<Self>> {
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
    pub fn to_vec(self: &P<Self>) -> Vec<P<Self>> {
        self.foldl(
            |mut acc, item| {
                acc.push(item.clone());
                acc
            },
            Vec::new(),
        )
    }

    /// Create a list from a Vec<P<Datum>>
    pub fn from_vec(vec: Vec<P<Self>>) -> P<Self> {
        Self::make_list(vec, None)
    }

    /// Zip two lists together, stopping at the shorter list
    pub fn zip(self: &P<Self>, other: &P<Self>) -> P<Self> {
        match (&self.value, &other.value) {
            (DatumValue::Pair(car1, cdr1), DatumValue::Pair(car2, cdr2)) => {
                let pair = Self::make_list([car1.clone(), car2.clone()], Some(self.span()));
                let zipped_rest = cdr1.zip(cdr2);
                Self::cons(pair, zipped_rest, Some(self.span()))
            }
            _ => Self::new(DatumValue::Null), // One or both lists ended
        }
    }

    /// Flatten a list of lists into a single list
    pub fn flatten(self: &P<Self>) -> P<Self> {
        match &self.value {
            DatumValue::Null => self.clone(),
            DatumValue::Pair(car, cdr) => {
                let flattened_cdr = cdr.flatten();
                match &car.value {
                    DatumValue::Null => flattened_cdr,
                    DatumValue::Pair(_, _) => car.append(&flattened_cdr),
                    _ => Self::cons(car.clone(), flattened_cdr, Some(self.span())),
                }
            }
            _ => self.clone(), // Not a proper list
        }
    }

    /// Partition a list into two lists based on a predicate
    pub fn partition<F>(self: &P<Self>, predicate: F) -> (P<Self>, P<Self>)
    where
        F: Fn(&P<Self>) -> bool,
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

        (Self::from_vec(trues), Self::from_vec(falses))
    }

    pub fn is_lambda(self: &P<Self>) -> bool {
        self.is_tagged_with_symbol("lambda")
    }

    pub fn is_quote(self: &P<Self>) -> bool {
        self.is_tagged_with_symbol("quote")
    }

    pub fn is_begin(self: &P<Self>) -> bool {
        self.is_tagged_with_symbol("begin")
    }

    pub fn is_define(self: &P<Self>) -> bool {
        self.is_tagged_with_symbol("define")
    }

    pub fn is_if(self: &P<Self>) -> bool {
        self.is_tagged_with_symbol("if")
    }

    pub fn is_if_syntax(self: &P<Self>) -> bool {
        self.is_if() && (self.length() == 3 || self.length() == 4)
    }

    pub fn is_set(self: &P<Self>) -> bool {
        self.is_tagged_with_symbol("set!")
    }

    pub fn if_condition(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_if() { self.cadr() } else { None }
    }

    pub fn if_then(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_if() { self.caddr() } else { None }
    }

    pub fn if_else(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_if() { self.cadddr() } else { None }
    }

    pub fn if_has_else(self: &P<Self>) -> bool {
        self.is_if() && self.length() > 3
    }

    pub fn is_app(self: &P<Self>) -> bool {
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

    pub fn set_var(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_set() { self.cadr() } else { None }
    }

    pub fn set_value(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_set() { self.caddr() } else { None }
    }

    pub fn is_define_lambda(self: &P<Datum>) -> bool {
        let Some(var) = self.cadr() else {
            return false;
        };

        (var.is_list() && var.length() > 0 && var.car().map(|car| car.is_symbol()).unwrap_or(false))
            || (var.is_pair() && var.car().map(|car| car.is_symbol()).unwrap_or(false))
    }

    pub fn define_to_lambda(self: &P<Self>) -> P<Datum> {
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
                &[
                    Datum::new_at(var.span(), DatumValue::Symbol(Symbol::new("lambda"))),
                    args.clone(),
                ],
                body.clone(),
                Some(self.span()),
            );

            Datum::make_list(
                &[
                    Datum::new_at(self.span(), DatumValue::Symbol(Symbol::new("define"))),
                    var.clone(),
                    lambda,
                ],
                Some(self.span()),
            )
        } else {
            self.clone()
        }
    }

    pub fn define_var(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_define_lambda() {
            self.caadr()
        } else {
            self.cadr()
        }
    }

    pub fn define_exp(&self) -> Option<&P<Self>> {
        self.cddr()
    }

    pub fn lambda_exp(self: &P<Self>) -> Option<&P<Self>> {
        if self.is_lambda() { self.cddr() } else { None }
    }
}

impl PartialEq for Datum {
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

impl Eq for Datum {}

impl Hash for Datum {
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

impl fmt::Display for Datum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            DatumValue::Identifier(id) => write!(f, "{id}"),
            DatumValue::PVRef(_) => write!(f, "#<pvref>"),
            DatumValue::SyntaxRules(_) => write!(f, "#<synrules>"),
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
                let s = INTERNER.resolve(spur);
                write!(f, "\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
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

impl Datum {
    fn fmt_pair_contents(
        &self,
        f: &mut fmt::Formatter<'_>,
        car: &P<Datum>,
        cdr: &P<Datum>,
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

pub fn collect_errors_and_report<'a>(
    program: impl Iterator<Item = &'a P<Datum>>,
    output: &mut Vec<Report<'a, Span>>,
) {
    program
        .flat_map(|datum| datum.collect_errors())
        .for_each(|error| error.build_reports(output));
}

pub struct PairIterator<'a> {
    current: Option<&'a P<Datum>>,
}

impl<'a> PairIterator<'a> {
    pub fn new(start: &'a P<Datum>) -> Self {
        Self {
            current: Some(start),
        }
    }
}

impl<'a> Iterator for PairIterator<'a> {
    type Item = &'a P<Datum>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current {
            match &current.value {
                DatumValue::Pair(car, cdr) => {
                    self.current = match &cdr.value {
                        DatumValue::Pair(_, _) => Some(cdr),
                        DatumValue::Null => None,
                        _ => Some(cdr), // If it's not a proper pair, we still return the cdr
                    };
                    Some(car)
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

impl Datum {
    pub fn iter_pairs<'a>(self: &'a P<Self>) -> PairIterator<'a> {
        PairIterator::new(self)
    }
}

pub struct Identifier {
    pub name: P<Datum>,
    pub global_name: OnceCell<P<Symbol>>,
    pub frames: Option<P<Frame>>,
    pub frame: OnceCell<Option<P<Frame>>>,
    pub env: P<SyntaxEnv>,
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Identifier {}

impl Identifier {
    pub fn new(name: P<Datum>, env: P<SyntaxEnv>) -> P<Self> {
        P(Self {
            name,
            global_name: OnceCell::new(),
            frames: None,
            frame: OnceCell::new(),
            env,
        })
    }
    pub fn global_name(self: &P<Self>) -> P<Symbol> {
        self.global_name
            .get()
            .cloned()
            .unwrap_or_else(|| self.unwrap())
    }

    pub fn gensym(self: &P<Self>) {
        self.global_name.get_or_init(|| {
            let sym = self.unwrap();

            let name = Symbol::Uninterned(*sym.spur());

            P(name)
        });
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<identifier {}>", self.name)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxRuleBranch {
    pub pattern: P<Datum>,
    pub template: P<Datum>,
    pub num_pvars: u32,
    pub max_level: u32,
}

#[derive(Debug, Clone)]
pub struct SyntaxPattern {
    pub pattern: P<Datum>,
    pub vars: P<Datum>,
    pub level: i16,
    pub num_following_items: i16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PVRef {
    pub level: i16,
    pub count: i16,
}

#[derive(Debug, Clone)]
pub struct SyntaxRules {
    pub name: P<Datum>,
    pub max_num_pvars: u32,
    pub env: P<Datum>,
    pub syntax_env: P<SyntaxEnv>,
    pub rules: Vec<SyntaxRuleBranch>,
}

impl PartialEq for SyntaxRules {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for SyntaxRules {}




#[macro_export]
macro_rules! quote {


    ($id: ident) => {
        $crate::ast::Datum::make_symbol(stringify!($id), None)
    };

    ($val: literal) => {
        $crate::ast::P($crate::ast::Datum::from($val))
    };

    (($x: tt $($rest:tt)*)) => {
        {
            let car = quote!($x);
            let cdr = quote!(($($rest)*));

            $crate::ast::Datum::cons(car, cdr, None)
        }
    };

    (@list ( . $last: tt)) => {
        quote!($last)
    };

    (@list ($head: tt $($tail: tt)*)) => {
        {
            let car = quote!($head);
            let cdr = quote!(@list $($tail)*);

            if cdr.is_null() {
                car
            } else {
                $crate::ast::Datum::cons(car, cdr, None)
            }
        }
    };

    (($x: tt $($rest:tt)*)) => {
        {
            let car = quote!($x);
            let cdr = quote!(@list $($rest)*);

            if cdr.is_null() {
                car
            } else {
                $crate::ast::Datum::cons(car, cdr, None)
            }
        }
    };


    (()) => {
        $crate::ast::Datum::new($crate::ast::DatumValue::Null)
    };
}

impl From<i32> for Datum {
    fn from(value: i32) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Number(Number::ExactFixnum(value as _)),
            comments: Vec::new(),
        }
    }
}

impl From<f64> for Datum {
    fn from(value: f64) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Number(Number::InexactReal(value)),
            comments: Vec::new(),
        }
    }
}

impl From<i64> for Datum {
    fn from(value: i64) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Number(Number::ExactFixnum(value as _)),
            comments: Vec::new(),
        }
    }
}

impl From<bool> for Datum {
    fn from(value: bool) -> Self {
        let value = if value {
            DatumValue::True
        } else {
            DatumValue::False
        };

        Datum {
            span: Cell::new(Span::default()),
            value,
            comments: Vec::new(),
        }
    }
}

impl From<&str> for Datum {
    fn from(value: &str) -> Self {
        Datum {
            span: Cell::new(Span::default()),
            value: DatumValue::Str(INTERNER.get_or_intern(value)),
            comments: Vec::new(),
        }
    }
}