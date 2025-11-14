//! Compile to CPS

use crate::rsgc::Gc;

use crate::cps::term::*;
use crate::expander::core::{LVarRef, fresh_lvar};
use crate::runtime::Context;
use crate::runtime::value::{Symbol, Value};

pub struct CPSBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub varcount: u32,
    pub scope_id: u32,
    pub current_topbox_scope: Option<u32>,
    pub current_meta: Value<'gc>,
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
        CPSBuilder {
            ctx,
            varcount: 0,
            current_topbox_scope: None,
            scope_id: 0,
            current_meta: Value::null(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.current_topbox_scope = Some(self.scope_id);
        self.scope_id += 1;
    }

    pub fn set_scope(&mut self, scope: Option<u32>) {
        self.current_topbox_scope = scope;
    }
}

#[macro_export]
macro_rules! with_cps {
    ($builder: ident; $binder: ident <- $expr: expr; $($rest:tt)+) => {
        {
            let ($binder, cont): (_, $crate::cps::builder::FCont) = $expr;

            let inner = with_cps!($builder; $($rest)+);

            cont($builder, inner)
        }
    };

    ($builder: ident; letk ($h: ident) $($k: ident ($($arg: ident),* $(& $variadic: ident)? ) $(@ $src: ident $($meta: ident)?)? = $e: expr),*;$($rest:tt)+) => {{
        let mut conts = Vec::new();
        $(
            let $k = $builder.fresh_variable(stringify!($k));
        )*

        $(


            {
                $(
                    let $arg = $builder.fresh_variable(stringify!($arg));
                )*
                #[allow(unused_mut, unused_assignments)]
                let mut variadic = None;
                $(
                    let $variadic = $builder.fresh_variable(stringify!($variadic));
                    variadic = Some($variadic);
                )?
                #[allow(unused_mut, unused_assignments)]
                let mut src = $crate::runtime::value::Value::new(false);
                #[allow(unused_mut, unused_assignments)]
                let mut meta = $crate::runtime::value::Value::new(false);
                $(
                    src = $src;
                    $(
                        meta = $meta;
                    )?
                )?


                let args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($arg),*]);
                let body = $e;

                let cont = $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Cont {
                    name: $crate::runtime::value::Value::new(false),
                    binding: $k,
                    args: args,
                    meta,
                    noinline: false,

                    variadic,
                    body: body.into(),
                    source: src,
                    reified: std::cell::Cell::new(false),
                    free_vars: $crate::rsgc::cell::Lock::new(None),
                    handler: $crate::rsgc::cell::Lock::new($h),
                    cold: false

                });

                conts.push(cont);
            }
        )*
        let conts = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &conts);
        with_cps!($builder;
            _letk <- $builder.letk(conts);
            $($rest)+
        )
    }
    };

        ($builder: ident; letk noinline ($h: ident) $($k: ident ($($arg: ident),*) $(@ $src: ident $($meta: ident)?)? = $e: expr),*;$($rest:tt)+) => {{
        let mut conts = Vec::new();
        $(
            let $k = $builder.fresh_variable(stringify!($k));
        )*

        $(


            {
                $(
                    let $arg = $builder.fresh_variable(stringify!($arg));
                )*
                #[allow(unused_mut, unused_assignments)]
                let mut src = $crate::runtime::value::Value::new(false);
                #[allow(unused_mut, unused_assignments)]
                let mut meta = $crate::runtime::value::Value::new(false);
                $(
                    src = $src;
                    $(
                        meta = $meta;
                    )?
                )?


                let args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($arg),*]);
                let body = $e;

                let cont = $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Cont {
                    name: $crate::runtime::value::Value::new(false),
                    binding: $k,
                    args: args,
                    meta,
                    noinline: true,

                    variadic: None,
                    body: body.into(),
                    source: src,
                    reified: std::cell::Cell::new(false),
                    free_vars: $crate::rsgc::cell::Lock::new(None),
                    handler: $crate::rsgc::cell::Lock::new($h),
                    cold: false

                });

                conts.push(cont);
            }
        )*
        let conts = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &conts);
        with_cps!($builder;
            _letk <- $builder.letk(conts);
            $($rest)+
        )
    }
    };

    ($builder: ident; letk cold ($h: ident) $($k: ident ($($arg: ident),*) $(@ $src: ident $($meta: ident)? )? = $e: expr),*;$($rest:tt)+) => {{
        let mut conts = Vec::new();
        $(
            let $k = $builder.fresh_variable(stringify!($k));
        )*

        $(

            {

                $(
                    let $arg = $builder.fresh_variable(stringify!($arg));
                )*
                #[allow(unused_mut, unused_assignments)]
                let mut src = $crate::runtime::value::Value::new(false);
                #[allow(unused_mut, unused_assignments)]
                let mut meta = $crate::runtime::value::Value::new(false);
                $(
                    src = $src;
                    $(
                        meta = $meta;
                    )?
                )?
                let args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($arg),*]);
                let body = $e;

                let cont = $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Cont {
                    name: $crate::runtime::value::Value::new(false),
                    binding: $k,
                    meta,
                    noinline: false,
                    args: args,
                    variadic: None,
                    body: body.into(),

                    source: src,
                    reified: std::cell::Cell::new(false),
                    free_vars: $crate::rsgc::cell::Lock::new(None),
                    handler: $crate::rsgc::cell::Lock::new($h),
                    cold: true

                });

                conts.push(cont);
            }
        )*
        let conts = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &conts);
        with_cps!($builder;
            _letk <- $builder.letk(conts);
            $($rest)+
        )
    }
    };

    ($builder: ident; letk ($h: ident) $k: ident ($args: ident ... $(&$variadic: ident)?) @ $src: ident $($meta: ident)? = $e: expr; $($rest:tt)+) => {
        {let $k = $builder.fresh_variable(stringify!($k));

            let args = $args;
            let body = $e;
            #[allow(unused_mut, unused_assignments)]
            let mut meta = $crate::runtime::value::Value::new(false);
            $(
                meta = $meta;
            )?
            #[allow(unused_mut, unused_assignments)]
            let mut variadic = None;

            $(
                let $variadic = $builder.fresh_variable(stringify!($variadic));
                variadic = Some($variadic);
            )?

        let cont = $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Cont {
            name: $crate::runtime::value::Value::new(false),
            binding: $k,
            args,
            variadic,

            body: body.into(),
            source: $src,
            meta,
            noinline: false,
            free_vars: $crate::rsgc::cell::Lock::new(None),
            reified: std::cell::Cell::new(false),
            handler: $crate::rsgc::cell::Lock::new($h),
            cold: false
        });
        let conts = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[cont]);
        with_cps!($builder;
            _letk <- $builder.letk(conts);
            $($rest)+
        )}
    };

    ($builder: ident; continue $k: ident ($($arg: expr),*) $(@ $src: expr)?) => {
        {
            #[allow(unused_mut, unused_assignments)]
            let mut src = $crate::runtime::value::Value::new(false);
            $(
                src = $src;
            )*
          $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::Continue(
            $k,
            $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($arg.into()),*]),
            src,
          ))
        }
    };

    ($builder: ident; raise $k: ident ($($arg: expr),*)) => {
        {
          $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::Raise(
            $k,
            $crate::rsgc::alloc::array::Array::from_slice(&[$($arg.into()),*]),
            $crate::runtime::value::Value::new(false),
          ))
        }
    };

    ($builder: ident; continue $k: ident $args: ident ... $(@ $src : expr)?) => {
        {
            #[allow(unused_mut, unused_assignments)]
            let mut src = $crate::runtime::value::Value::new(false);
            $(
                src = $src;
            )*
            let args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, $args);
            $crate::rsgc::Gc::new(&$builder.ctx,$crate::cps::term::Term::Continue(
                $k,
                args,
                src,
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

    ($builder: ident; $callee: ident ($k: ident, $h: ident $(,)? $($arg:ident),*) @ $src: expr) => {
        {
            let args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($arg),*]);
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::App(
                $callee,
                $k,
                $h,
                args,
                $src,
            ))
        }
    };

    ($builder: ident; $callee: ident ($k: ident, $h: ident, $args: ident ...) @ $src: expr) => {
        {
            let args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, $args);
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::App(
                $callee.into(),
                $k,
                $h,
                args,
                $src,
            ))
        }
    };

    ($builder: ident; if $test: expr => $kcons: ident | $kalt: ident) => {
        {
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::If {
                test: $test.into(),
                consequent: $kcons,
                consequent_args: None,
                alternative: $kalt,
                alternative_args: None,
                hints: [BranchHint::Normal, BranchHint::Normal],
            })
        }
    };

    ($builder: ident; if $test: expr => $kcons: ident ($($cons_arg: expr),*) | $kalt: ident ($($alt_arg: expr),*)) => {
        {
            let cons_args = [$($cons_arg.into()),*];
            let alt_args = [$($alt_arg.into()),*];

            let cons_args = if cons_args.is_empty() { None } else { Some($crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &cons_args)) };
            let alt_args = if alt_args.is_empty() { None } else { Some($crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &alt_args)) };
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::If {
                test: $test.into(),
                consequent: $kcons,
                consequent_args: cons_args,
                alternative: $kalt,
                alternative_args: alt_args,
                hints: [BranchHint::Normal, BranchHint::Normal],
            })
        }
    };

    ($builder: ident; if cold $test: expr => $kcons: ident | $kalt: ident) => {
        {
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::If {
                test: $test.into(),
                consequent: $kcons,
                consequent_args: None,
                alternative: $kalt,
                alternative_args: None,
                hints: [BranchHint::Cold, BranchHint::Cold],
            })
        }
    };

    ($builder: ident; if cold $test: expr => $kcons: ident ($($cons_arg: expr),*) | $kalt: ident ($($alt_arg: expr),*)) => {
        {
            let cons_args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($cons_arg),*]);
            let alt_args = $crate::rsgc::alloc::array::Array::from_slice(&$builder.ctx, &[$($alt_arg),*]);
            $crate::rsgc::Gc::new(&$builder.ctx, $crate::cps::term::Term::If {
                test: $test.into(),
                consequent: $kcons,
                consequent_args: Some(cons_args),
                alternative: $kalt,
                alternative_args: Some(alt_args),
                hints: [BranchHint::Cold, BranchHint::Cold],
            })
        }
    };

    ($cps: ident; let $binder: ident = #% $prim: ident ($h: expr, $($arg: expr),*); $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall($prim, $crate::rsgc::alloc::array::Array::from_slice(&$cps.ctx, [$($arg.clone().into()),*]), $h, $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; ;$($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident ($h: expr, $($arg: expr),*) @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();

        let e = $crate::cps::term::Expression::PrimCall($prim, $crate::rsgc::alloc::array::Array::from_slice(&$cps.ctx, [$($arg.clone().into()),*]), $h, $span);
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($h: ident $(,)? $($arg: expr),*); $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $crate::rsgc::alloc::array::Array::from_slice(&$cps.ctx, [$($arg.clone().into()),*]), $h, $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($h: expr, $($arg: expr),*) @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $crate::rsgc::alloc::array::Array::from_slice(&$cps.ctx, [$($arg.clone().into()),*]), $h, $span);
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident ($h: expr) $args: ident ... @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let args = $crate::rsgc::alloc::array::Array::from_slice(&$cps.ctx, $args);
        let e = $crate::cps::term::Expression::PrimCall($prim, args, $h, $span);
        let rv = $binder.clone();
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: ident ($h: expr) $args: ident ...; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let e = $crate::cps::term::Expression::PrimCall($prim, $args, $h, $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($h: expr) $args: ident ... @ $span: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $args, $h, $span);
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};

    ($cps: ident; let $binder: ident = #% $prim: literal ($h: expr) $args: ident ...; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        let rv = $binder.clone();
        let name = $crate::runtime::value::Symbol::from_str($cps.ctx, $prim);
        let e = $crate::cps::term::Expression::PrimCall(name.into(), $args, $h, $crate::runtime::value::Value::new(false));
        let inner = with_cps!($cps; $($rest)+);
        $crate::rsgc::Gc::new(&$cps.ctx, $crate::cps::term::Term::Let(rv, e, inner))
    }};


    ($cps: ident; let $binder: ident = $e: expr; $($rest:tt)+) => {{
        let $binder = $cps.fresh_variable(stringify!($binder));
        with_cps!($cps; @tk $binder = $e; $($rest)+)
    }};

    ($cps: ident; @m $binder: ident = $e: expr; $($rest:tt)+) => {{
        let $binder = $crate::expander::compile_cps::m($cps, $e);
        with_cps!($cps; $($rest)+)
    }};

    ($cps: ident; # $s: stmt; $($rest:tt)+) => {{
        $s
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
