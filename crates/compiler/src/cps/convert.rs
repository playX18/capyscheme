use std::rc::Rc;

use crate::{
    ast::{Datum, INTERNER, P},
    cps::term::{Atom, Cont, Func, Term},
    il::term::{IForm, ITerm, LVar, Proc, ProcCase},
    source::Span,
};

pub struct CPSBuilder {
    varcount: u32,
}

impl CPSBuilder {
    pub fn convert_toplevel(mut self, seq: &[P<IForm>]) -> Rc<Func> {
        let proc = Proc {
            name: Some(Datum::make_symbol("%entrypoint", None)),
            loc: Some(Span::default()),
            cases: vec![ProcCase {
                args: vec![],
                loc: Some(Span::default()),
                variadic: None,
                body: P(IForm {
                    span: Span::default(),
                    term: ITerm::Seq(seq.iter().cloned().collect()),
                }),
            }],
        };

        self.convert_proc(&proc, None)
    }

    pub fn fresh_variable(&mut self, prefix: &str) -> P<LVar> {
        let ix = self.varcount;
        let var = LVar::new(Datum::make_symbol(&format!("{prefix}{ix}"), None), None);
        self.varcount += 1;
        var
    }

    pub fn letk<'a>(&mut self, conts: Vec<Rc<Cont>>) -> ((), FCont<'a>) {
        let cont = Box::new(move |_this: &mut CPSBuilder, body: Rc<Term>| {
            Rc::new(Term::Letk(conts, body))
        });
        ((), cont)
    }

    pub fn new() -> Self {
        CPSBuilder { varcount: 0 }
    }
}

pub type FCont<'a> = Box<dyn FnOnce(&mut CPSBuilder, Rc<Term>) -> Rc<Term> + 'a>;

#[macro_export]
macro_rules! with_cps {
    ($cps: ident; $binder: ident <- $expr: expr; $($rest:tt)+) => {{
        let ($binder, cont): (_, $crate::cps::convert::FCont) = $expr;

        let inner = { with_cps!($cps; $($rest)+) };

        cont($cps, inner)
    }};

    ($cps: ident; letk $($k: ident ($($arg:ident),*) $(@ $span: ident)? = $e: expr),*; $($rest:tt)+) => {{
        let mut conts = Vec::new();
        $(
            let $k = $cps.fresh_variable("k");
        )*
        $(
            {$(
                let $arg = $cps.fresh_variable(stringify!($arg));
            )*
            #[allow(unused_mut)]
            let mut span = Span::default();
            $(
                span = $span;
            )?
            let cont = $crate::cps::term::Cont {
                name: None,
                binding: $k.clone(),
                args: vec![$($arg.clone()),*],
                variadic: None,
                body: $e,
                span
            };

            conts.push(::std::rc::Rc::new(cont));
        }
        )*


        with_cps!($cps;
            _letk <- $cps.letk(conts);
            $($rest)+
        )
    }};

    ($cps: ident; letk $k: ident ($args: ident ...) @ $span: ident = $e: expr; $($rest:tt)+) => {{
        let $k = $cps.fresh_variable(stringify!($k));
        let cont = $crate::cps::term::Cont {
            name: None,
            binding: $k.clone(),
            args: $args.to_vec(),
            variadic: None,
            body: $e,
            span: $span,
        };
        let cont = ::std::rc::Rc::new(cont);

        with_cps!($cps;
            _letk <- $cps.letk(vec![cont]);
            $($rest)+
        )
    }};

    ($cps: ident; continue $k: ident ($($arg:expr),*)) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::Continue($k.clone(), vec![$($arg.clone()),*], Span::default()))
    }};

    ($cps: ident; continue $k: ident $args: ident ...) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::Continue($k.clone(), $args, Span::default()))
    }};

    ($cps: ident; continue $k: ident ($($arg:expr),*) @ $span: expr) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::Continue($k.clone(), vec![$($arg.clone()),*], $span))
    }};

    ($cps: ident; continue $k: ident $args: ident ... @ $span: expr) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::Continue($k.clone(), $args, $span))
    }};

    ($cps: ident; $callee: ident ($k: ident $(,)? $($arg: ident),*)) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::App($callee.clone(), $k.clone(), vec![$($arg.clone()),*], Span::default()))
    }};

    ($cps: ident; $callee: ident ($k: ident $(,)? $args: ident ...)) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::App($callee.clone(), $k.clone(), $args, Span::default()))
    }};

    ($cps: ident; $callee: ident ($k: ident $(,)? $($arg: ident),*) @ $span: expr) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::App($callee.clone(), $k.clone(), vec![$($arg.clone()),*], $span))
    }};

    ($cps: ident; $callee: ident ($k: ident $(,)? $args: ident ...) @ $span: expr) => {{
        let _ = $cps;
        ::std::rc::Rc::new($crate::cps::term::Term::App($callee.clone(), $k.clone(), $args, $span))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident ($($arg: expr),*); $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::ast::INTERNER.get_or_intern(stringify!($binder));
        let e = $crate::cps::term::Expression::PrimCall($prim, vec![$($arg.clone()),*], Span::default());
        let inner = with_cps!($cps; ;$($rest)+);
        Box::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident ($($arg: expr),*) @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();

        let e = $crate::cps::term::Expression::PrimCall($prim, vec![$($arg.clone()),*], $span);
        let inner = with_cps!($cps; $($rest)+);
        Box::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($($arg: expr),*); $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let name = $crate::ast::INTERNER.get_or_intern($prim);
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall($prim, vec![$($arg.clone()),*], Span::default());
        let inner = with_cps!($cps; $($rest)+);
        Box::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($($arg: expr),*) @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let name = $crate::ast::INTERNER.get_or_intern($prim);
        let args = vec![$($arg.clone()),*];
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall(name, args, $span);
        let inner = with_cps!($cps; $($rest)+);
        ::std::rc::Rc::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident $args: ident ... @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let e = $crate::cps::term::Expression::PrimCall($prim, $args, $span);
        let rv = $binder.clone();
        let inner = with_cps!($cps; $($rest)+);
        ::std::rc::Rc::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident $args: ident ...; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall($prim, $args, Span::default());
        let inner = with_cps!($cps; $($rest)+);
        ::std::rc::Rc::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal $args: ident ... @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::ast::INTERNER.get_or_intern(stringify!($prim));
        let e = $crate::cps::term::Expression::PrimCall(name, $args, $span);
        let inner = with_cps!($cps; $($rest)+);
        ::std::rc::Rc::new($crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal $args: ident ...; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::ast::INTERNER.get_or_intern(stringify!($prim));
        let e = $crate::cps::term::Expression::PrimCall(name, $args, Span::default());
        let inner = with_cps!($cps; $($rest)+);
        ::std::rc::Rc::new($crate::cps::term::Term::Let($binder, e, inner))
    }};


    ($cps: ident; let $binder: ident = $e: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        with_cps!($cps; @tk $binder = $e; $($rest)+)
    }};

    ($cps: ident; @letk* $($rest:tt)+) => {{
        with_cps!($cps; letk $($rest)+)
    }};

    ($cps: ident; @letk $($rest:tt)+) => {{
        let conts = smallvec::SmallVec::new();
        with_cps!($cps; letk $($rest)+)
    }};

    ($cps: ident; @letk $($rest:tt)+) => {{
        let conts = smallvec::SmallVec::new();
        with_cps!($cps; letk $($rest)+)
    }};

    ($cps: ident; @letk $($rest:tt)+) => {{
        let conts = smallvec::SmallVec::new();
        with_cps!($cps; letk $($rest)+)
    }};

    ($cps: ident; @tk $binder: ident =  $e: expr; $($rest:tt)+) => {{
        let e = $e;
        $cps.t_k(e, Box::new(move |$cps, $binder| {
            with_cps!($cps; $($rest)+)
        }))
    }};

    ($cps: ident; @tk* $binder: ident = $e: expr; $($rest:tt)+) => {{
        let e = $e;
        $cps.t_k_many(e, Box::new(move |$cps, $binder| {
            with_cps!($cps; $($rest)+)
        }))
    }};

    ($cps: ident; @tc ($k: ident) $e: ident) => {{
        $cps.t_c(&$e, $k.clone())
    }};

    ($cps: ident; @m $e: ident; $($rest:tt)+) => {{
        $cps.m(&$e)
    }};

    ($cps: ident; @proc $proc: ident; $($rest:tt)+) => {{
        $cps.convert_proc(&$proc, None)
    }};

    ($cps: ident; @t_k_many $forms: expr; $fk: expr; $($rest:tt)+) => {{
        $cps.t_k_many(&$forms, Box::new(move |$cps, vars| {
            with_cps!($cps; $fk(vars); $($rest)+)
        }))
    }};

    ($cps: ident; # $s: stmt; $($rest:tt)+) => {{
        $s;
        with_cps!($cps; $($rest)+)
    }};

    ($cps: ident; # $expr: expr) => {{
        $expr
    }};
}

impl CPSBuilder {
    pub fn t_k<'a>(
        &mut self,
        form: &'a IForm,
        fk: Box<dyn FnOnce(&mut CPSBuilder, Atom) -> Rc<Term> + 'a>,
    ) -> Rc<Term> {
        let span = form.span;
        let cps = self;

        match &form.term {
            ITerm::Const(_)
            | ITerm::GRef(_)
            | ITerm::LRef(_)
            | ITerm::PrimRef(_)
            => {
                let atom = cps.m(form);
                fk(cps, atom)
            }

            ITerm::Proc(proc) => {
                let tmp = cps.fresh_variable("ftmp");
                let f = cps.convert_proc(proc, Some(tmp.clone()));
                let rest = fk(cps, Atom::Local(tmp));

                Rc::new(Term::Fix(vec![f], rest))
            }

            ITerm::App(callee, args) => with_cps!(cps;
                letk after_call (rv) = fk(cps, Atom::Local(rv));
                @tk* args = args;
                @tk callee = callee;
                callee(after_call, args...) @ span
            ),

            ITerm::PrimApp(prim, args) => {
                let prim = *prim;

                with_cps!(cps;
                    @tk* args = args;
                    let rv = #% prim args... @ span;
                    # fk(cps, Atom::Local(rv))
                )
            }

            ITerm::Define(var, val) => with_cps!(cps;
                @tk atom = val;
                let rv = #% "define" (Atom::Global(var.clone(), span), atom) @ span;
                # fk(cps, Atom::Local(rv))
            ),

            ITerm::GSet(var, val) => with_cps!(cps;
                @tk atom = val;
                let rv = #% "gset" (Atom::Global(var.clone(), span), atom) @ span;
                # fk(cps, Atom::Local(rv))
            ),

            ITerm::Fix(fix) => {
                let funcs = fix
                    .variables
                    .iter()
                    .zip(fix.procedures.iter())
                    .map(|(var, proc)| cps.convert_proc(proc, Some(var.clone())))
                    .collect::<Vec<_>>();

                let body = cps.t_k(&fix.body, fk);

                Rc::new(Term::Fix(funcs, body))
            }

            ITerm::Let(let_) => {
                let args = let_.variables.as_slice();

                with_cps!(cps;
                    @tk* aexps = &let_.initializers;
                    letk r#let (args...) @ span = cps.t_k(&let_.body, fk);
                    continue r#let aexps...
                )
            }

            ITerm::Seq(seq) => with_cps!(cps;
                @tk* aexps = seq;
                # fk(cps, aexps.last().expect("Expected at least one expression in sequence").clone())
            ),

            ITerm::LSet(_, _) => {
                panic!("LSet is not supported in CPS conversion");
            }

            ITerm::If(test, cons, alt) => with_cps!(cps;
                @tk atest = test;
                letk cont (rv) = fk(cps, Atom::Local(rv));
                letk kcons () = cps.t_c(cons, cont.clone());
                letk kalt () = cps.t_c(alt, cont);
                # ::std::rc::Rc::new(Term::If(atest,  kcons, kalt))
            ),
        }
    }

    pub fn t_c(&mut self, form: &IForm, cont: P<LVar>) -> Rc<Term> {
        let span = form.span;
        let cps = self;

        match &form.term {
            ITerm::Const(_)
            | ITerm::GRef(_)
            | ITerm::LRef(_)
            | ITerm::PrimRef(_)
            | ITerm::Proc(_) => {
                let atom = cps.m(form);
                with_cps!(cps;
                    continue cont(atom)
                )
            }

            ITerm::App(callee, args) => with_cps!(cps;
                @tk* args = args;
                @tk callee = callee;
                callee(cont, args...) @ span
            ),

            ITerm::PrimApp(prim, args) => {
                let prim = *prim;

                with_cps!(cps;
                    @tk* args = args;
                    let rv = #% prim args... @ span;
                    continue cont(Atom::Local(rv))
                )
            }

            ITerm::Define(var, val) => with_cps!(cps;
                @tk atom = val;
                let rv = #% "define" (Atom::Global(var.clone(), span), atom) @ span;
                continue cont(Atom::Local(rv))
            ),

            ITerm::GSet(var, val) => with_cps!(cps;
                @tk atom = val;
                let rv = #% "gset" (Atom::Global(var.clone(), span), atom) @ span;
                continue cont(Atom::Local(rv))
            ),

            ITerm::Fix(fix) => {
                let funcs = fix
                    .variables
                    .iter()
                    .zip(fix.procedures.iter())
                    .map(|(var, proc)| cps.convert_proc(proc, Some(var.clone())))
                    .collect::<Vec<_>>();

                let body = cps.t_c(&fix.body, cont);

                Rc::new(Term::Fix(funcs, body))
            }

            ITerm::Let(let_) => {
                let args = let_.variables.as_slice();

                with_cps!(cps;
                    @tk* aexps = &let_.initializers;
                    letk r#let (args...) @ span = cps.t_c(&let_.body, cont);
                    continue r#let aexps...
                )
            }

            ITerm::Seq(seq) => with_cps!(cps;
                @tk* aexps = seq;
                continue cont(aexps.last().expect("Expected at least one expression in sequence").clone())
            ),

            ITerm::LSet(_, _) => {
                panic!("LSet is not supported in CPS conversion");
            }

            ITerm::If(test, cons, alt) => with_cps!(cps;
                @tk atest = test;
                letk kcons () = cps.t_c(cons, cont.clone());
                letk kalt () = cps.t_c(alt, cont);
                # ::std::rc::Rc::new(Term::If(atest,  kcons, kalt))
            ),
        }
    }

    pub fn m(&mut self, form: &IForm) -> Atom {
        let span = form.span;
        match &form.term {
            ITerm::GRef(var) => Atom::Global(var.clone(), span),
            ITerm::LRef(var) => Atom::Local(var.clone()),
            ITerm::Proc(proc) => Atom::Func(self.convert_proc(proc, None), span),
            ITerm::Const(const_) => Atom::Constant(const_.clone()),
            ITerm::PrimRef(prim) => Atom::Prim(prim.clone(), span),
            _ => unreachable!(),
        }
    }

    pub fn convert_proc(&mut self, proc: &Proc, binding: Option<P<LVar>>) -> Rc<Func> {
        let return_cont = self.fresh_variable("ret");
        let case = proc
            .cases
            .first()
            .expect("Expected at least one case in proc");
        let body = self.t_c(&case.body, return_cont.clone());
        let func = Func {
            name: proc
                .name
                .as_ref()
                .map(|s| INTERNER.get_or_intern(&s.to_string())),
            return_cont: return_cont,
            binding: binding.unwrap_or_else(|| self.fresh_variable("func")),
            args: case.args.clone(),
            variadic: case.variadic.clone(),
            body,
            span: proc.loc.unwrap_or(Span::default()),
            reified: false,
        };
        Rc::new(func)
    }

    pub fn t_k_many<'a>(
        &mut self,
        forms: &'a [P<IForm>],
        fk: Box<dyn FnOnce(&mut CPSBuilder, Vec<Atom>) -> Rc<Term> + 'a>,
    ) -> Rc<Term> {
        if forms.is_empty() {
            return fk(self, vec![]);
        }

        let (first, rest) = forms.split_first().unwrap();

        self.t_k(
            first,
            Box::new(move |cps, first| {
                if rest.is_empty() {
                    fk(cps, vec![first])
                } else {
                    cps.t_k_many(
                        rest,
                        Box::new(move |cps, rest| {
                            fk(cps, std::iter::once(first).chain(rest).collect())
                        }),
                    )
                }
            }),
        )
    }
}
