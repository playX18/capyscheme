//! Compile to CPS

use rsgc::Gc;

use crate::cps::term::*;
use crate::expander::core::{LVarRef, fresh_lvar};
use crate::runtime::Context;
use crate::runtime::value::Symbol;

pub struct CPSBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub varcount: u32,
}

pub type FCont<'a, 'gc> =
    Box<dyn FnOnce(&'a mut CPSBuilder<'gc>, TermRef<'gc>) -> TermRef<'gc> + 'a>;

impl<'gc> CPSBuilder<'gc> {
    pub fn fresh_variable(&mut self, prefix: &str) -> LVarRef<'gc> {
        let ix = self.varcount;
        let var = fresh_lvar(
            self.ctx,
            Symbol::from_str_uninterned(&self.ctx, &format!("{prefix}{ix}"), None).into(),
        );

        self.varcount += 1;

        var
    }

    pub fn letk<'a>(&mut self, conts: Conts<'gc>) -> ((), FCont<'a, 'gc>) {
        let cont = Box::new(
            move |this: &mut CPSBuilder<'gc>, body: TermRef<'gc>| -> TermRef<'gc> {
                Gc::new(&this.ctx, Term::Letk(conts, body))
            },
        );

        ((), cont)
    }

    pub fn new(ctx: Context<'gc>) -> Self {
        CPSBuilder { ctx, varcount: 0 }
    }
}

#[macro_export]
macro_rules! with_cps {
    ($builder: ident; $binder: ident <- $expr: expr; $($rest:tt)+) => {
        {
            let ($binder, cont): (_, $crate::cps::builder::FCont) = $expr;

            let inner = {with_cps!($builder; $($rest)+)};

            cont($builder, inner)
        }
    };

    ($builder: ident; letk $($k: ident ($($arg: ident),*) $(@ $src: ident)? = $e: expr),*;$($rest:tt)+) => {{
        let mut conts = Vec::new();
        $(
            let $k = $builder.fresh_variable(stringify!($k));
        )*

        $(
            {
                $(
                    let $arg = $builder.fresh_variable(stringify!($arg));
                )*

                #[allow(unused_mut)]
                let mut src = $crate::runtime::value::Value::new(false);
                $(
                    src = $src;
                )?
                let args = $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, &[$($arg),*]);
                let body = $e;

                let cont = $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Cont::Local {
                    name: $crate::runtime::value::Value::new(false),
                    binding: $k,
                    args: args,
                    variadic: None,
                    body,
                    source: src,
                    reified: false,
                });

                conts.push(cont);
            }
        )*
        let conts = $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, &conts);
        with_cps!($builder;
            _letk <- $builder.letk(conts);
            $($rest)+
        )
    }
    };

    ($builder: ident; letk $k: ident ($args: ident ...) @ $src: ident = $e: expr; $($rest:tt)+) => {
        {let $k = $builder.fresh_variable(stringify!($k));

            let args = $args;
            let body = $e;

        let cont = $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Cont::Local {
            reified: false,
            name: $crate::runtime::value::Value::new(false),
            binding: $k,
            args,
            variadic: None,
            body,
            source: $src,
        });
        let conts = $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, &[cont]);
        with_cps!($builder;
            _letk <- $builder.letk(conts);
            $($rest)+
        )}
    };

    ($builder: ident; continue $k: ident ($($arg: expr),*)) => {
        {
          $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::Continue(
            $k,
            $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, &[$($arg.into()),*]),
            $crate::runtime::value::Value::new(false),
          ))
        }
    };

    ($builder: ident; raise $k: ident ($($arg: expr),*)) => {
        {
          $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::Raise(
            $k,
            $crate::rsgc::alloc::array::Array::from_array(&[$($arg.into()),*]),
            $crate::runtime::value::Value::new(false),
          ))
        }
    };

    ($builder: ident; continue $k: ident $args: ident ...) => {
        {
            let args = $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, $args);
            $crate::rsgc::Gc::new(&$builder.ctx,$crate::cps::term::Term::Continue(
                $k,
                args,
                $crate::runtime::value::Value::new(false),
            ))
        }
    };

    ($builder: ident; raise $k: ident ($args: ident ...)) => {
        {
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::Raise(
                $k,
                $args,
                $crate::runtime::value::Value::new(false),
            ))
        }
    };

    ($builder: ident; $callee: ident ($k: ident $(,)? $($arg:ident),*) @ $src: expr) => {
        {
            let args = $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, &[$($arg),*]);
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::App(
                $callee,
                $k,
                args,
                $src,
            ))
        }
    };

    ($builder: ident; $callee: ident ($k: ident, $args: ident ...) @ $src: expr) => {
        {
            let args = $crate::rsgc::alloc::array::Array::from_array(&$builder.ctx, $args);
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::App(
                $callee,
                $k,

                args,
                $src,
            ))
        }
    };

    ($builder: ident; if $test: expr => $kcons: ident | $kalt: ident) => {
        {
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::If(
                $test.into(),
                $kcons,
                $kalt,
                [BranchHint::Normal, BranchHint::Normal],
            ))
        }
    };

    ($cps: ident; let $binder: ident = #% $prim: ident ($($arg: expr),*); $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall($prim, $crate::rsgc::alloc::array::Array::from_array(&$cps.ctx, [$($arg.clone().into()),*]), $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; ;$($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident ($($arg: expr),*) @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();

        let e = $crate::cps::term::Expression::PrimCall($prim, $crate::rsgc::alloc::array::Array::from_array(&$cps.ctx, [$($arg.clone().into()),*]), $span);
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($($arg: expr),*); $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $crate::rsgc::alloc::array::Array::from_array(&$cps.tx, [$($arg.clone().into()),*]), $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($($arg: expr),*) @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $crate::rsgc::alloc::array::Array::from_array(&$cps.ctx, [$($arg.clone().into()),*]), $span);
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident $args: ident ... @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let args = $crate::rsgc::alloc::array::Array::from_array(&$cps.ctx, $args);
        let e = $crate::cps::term::Expression::PrimCall($prim, args, $span);
        let rv = $binder.clone();
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident $args: ident ...; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall($prim, $args, $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal $args: ident ... @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $args, $span);
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal $args: ident ...; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $args, $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};


    ($cps: ident; let $binder: ident = $e: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        with_cps!($cps; @tk $binder = $e; $($rest)+)
    }};

    ($cps: ident; @tk $binder: ident = $e: expr; $($rest:tt)+) => {{
        let e = $e;
        $crate::expander::compile_cps::t_k($cps, e, Box::new(move |$cps, $binder| {
            with_cps!($cps; $($rest)+)
        }))
    }};

    ($cps: ident; @tk* $binder: ident = $e: expr; $($rest:tt)+) => {{
        let e = $e;
        $crate::expander::compile_cps::t_k_many($cps, e, Box::new(move |$cps, $binder| {
            with_cps!($cps; $($rest)+)
        }))
    }};

    ($cps: ident; @tc ($k: ident) $e: expr) => {{
        let e = $e;
        $crate::expander::compile_cps::t_c($cps, e, $k)
    }};

    ($cps: ident; @m $binder: ident = $e: expr; $($rest:tt)+) => {{
        let $binder = $crate::expander::compile_cps::m($cps, $e);
        with_cps!($cps; $($rest)+)
    }};

    ($cps: ident; # $s: stmt; $($rest:tt)+) => {{
        $s;
        with_cps!($cps; $($rest)+)
    }};

    ($cps: ident; # $expr: expr) => {{
        $expr
    }};

    ($cps: ident; throw $key: expr, $val: expr; $src: expr) => {
        {
            $crate::rsgc::Gc::new(&$cps.ctx,$crate::cps::term::Term::Throw($crate::cps::term::Throw::Throw(
                $key.into(),
                $val.into(),
            ), $src))
        }
    };

    ($cps: ident; throw_val_data $key: expr, $val: expr; $src: expr) => {
        {
            $crate::rsgc::Gc::new(&$cps.ctx,$crate::cps::term::Term::Throw($crate::cps::term::Throw::ValueAndData(
                $key,
                $val,
                $src
            )))
        }
    };

    ($cps: ident; throw_value $key: expr, $val: expr; $src: expr) => {
        {
            $crate::rsgc::Gc::new(&$cps.ctx,$crate::cps::term::Term::Throw($crate::cps::term::Throw::Value(
                $key,
                $val,
                $src
            )))
        }
    };

}
