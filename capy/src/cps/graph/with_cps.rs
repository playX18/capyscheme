//! A macro to build CPS graphs using a more natural syntax.
//!

use std::collections::HashSet;

use super::term::*;
use crate::prelude::Value;

pub type FCont<'a, 'gc> = Box<dyn FnOnce(&'a mut CPSBuilder<'gc>, TermId) -> TermId>;
#[macro_export]
macro_rules! cps {
    ($builder: ident; $binder: ident <- $expr: expr; $($rest:tt)*) => {{
        {
            let ($binder, cont): (_, $crate::cps::graph::cps::FCont) =
                $expr;
            let inner = cps!($builder; $($rest)*);
            cont($builder, inner)
        }
    }};

    ($builder: ident; let $binder : ident = const $val: expr; $($rest:tt)*) => {{
        let $binder = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
        let inner = cps!($builder; $($rest)*);
        let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Let(
            $binder,
            $crate::cps::graph::term::Expression::Constant($val.into()),
            inner
        ), None);
        $builder.graph.set_term_parent(inner, Some($crate::cps::graph::term::ParentLink::Term(term)));
        $builder.graph[$binder].def = VarDef::Let(term);
        term
    }};

    ($builder: ident; let $binder : ident = #% $prim : ident ($($arg: expr),*); $($rest:tt)*) => {{
        let $binder = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
        let inner = cps!($builder; $($rest)*);

        let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Let(
            $binder,
            $crate::cps::graph::term::Expression::Constant($crate::prelude::Value::new(false)),
            inner
        ), None);

        let occurrences = vec![
            $(
                {
                    let is_rec = $builder.is_recursive($arg);
                    $builder.graph.add_occurrence($arg, term, is_rec)
                }
            ),*
        ];

        $builder.graph.set_term_parent(inner, Some($crate::cps::graph::term::ParentLink::Term(term)));

        let list = $builder.graph.occurrence_list(occurrences);

        $builder.graph[term].kind = $crate::cps::graph::term::TermKind::Let(
            $binder,
            $crate::cps::graph::term::Expression::PrimCall(
                $crate::cps::graph::intrinsic::Intrinsic::$prim,
                list
            ),
            inner
        );

        $builder.graph[$binder].def = VarDef::Let(term);

        term
    }};

    ($builder: ident; let $binder : ident = #% $prim : ident (($($arg: expr),*)) $(@ $src: expr)?; $($rest:tt)*)
    => {{
        let $binder = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
        let inner = cps!($builder; $($rest)*);

        let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Let(
            $binder,
            $crate::cps::graph::term::Expression::Constant($crate::prelude::Value::new(false)),
            inner
        ), None);

        let occurrences = vec![
            $(
                {
                    let is_rec = $builder.is_recursive($arg);
                    $builder.graph.add_occurrence($arg, term, is_rec)
                }
            ),*
        ];

        let list = $builder.graph.occurrence_list(occurrences);

        $builder.graph[term].kind = $crate::cps::graph::term::TermKind::Let(
            $binder,
            $crate::cps::graph::term::Expression::PrimCall(
                $crate::cps::graph::intrinsic::Intrinsic::$prim,
                list
            ),
            inner
        );

        $builder.graph.set_term_parent(inner, Some($crate::cps::graph::term::ParentLink::Term(term)));

        $(
            $builder.graph[term].src = $src.into();
        )?

        $builder.graph[$binder].def = VarDef::Let(term);

        term
    }};

    ($builder: ident; letk $($k : ident ($args: ident ...) $(@ ($meta: expr))? = $kbody: expr),*; $($rest:tt)*) => {{
        let mut conts = Vec::new();



        $(
            let $k = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
        )*
        $(
            {
                $builder.push_recursive_scope($args.iter().copied());
                let args = $builder.graph.occurrence_list($args);
                let mut meta = $crate::prelude::Value::new(false);
                $(
                    meta = $meta.into();
                )?

                let kbody = $kbody;
                $builder.pop_recursive_scope();

                let k = $builder.graph.add_cont($k, meta, kbody, args, None, None);
                $builder.graph[$k].def = VarDef::Cont(k);
                conts.push(k);
            }
        )*

        let conts_list = $builder.graph.conts_list(conts);
        let inner = cps!($builder; $($rest)*);

        let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Letk(
            conts_list,
            inner
        ), None);

        $builder.graph.set_term_parent(inner, Some($crate::cps::graph::term::ParentLink::Term(term)));

        term
    }};

    ($builder: ident; letk $($k : ident ($($arg: ident),*  $(& $variadic: ident)?) $(@ ($meta: expr))? = $kbody: expr),*; $($rest:tt)*) => {{
        let mut conts = Vec::new();

        $(
            let $k = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
        )*
        $(
            #[allow(unused_mut)]
            {
                $(
                    let $arg = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
                )*
                let args_vec = $builder.graph.var_list(vec![$($arg),*]);
                let mut meta = $crate::prelude::Value::new(false);
                $(
                    meta = $meta.into();
                )?
                $builder.push_recursive_scope([

                        $k

                ]);
                let kbody = $kbody;
                $builder.pop_recursive_scope();

                let k = $builder.graph.add_cont($k, meta, Some(kbody), args_vec, None, None);
                $builder.graph[$k].def = VarDef::Cont(k);
                conts.push(k);
            }
        )*

        let conts_list = $builder.graph.conts_list(conts);
        let inner = cps!($builder; $($rest)*);

        let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Letk(
            conts_list,
            inner
        ), None);

        let conts_len = conts_list.len(&$builder.graph.cont_pool);
        for i in 0..conts_len {
            let cont = conts_list.as_slice(&$builder.graph.cont_pool)[i];
            $builder.graph[cont].parent = Some(term);
        }

        $builder.graph.set_term_parent(inner, Some($crate::cps::graph::term::ParentLink::Term(term)));

        term
    }};

    ($builder: ident; letrec $($f : ident ($retk: ident$(,)? $($arg: ident),*  $(& $variadic: ident)?) $(@ ($meta: expr))? = $fbody: expr),*; $($rest:tt)*) => {{
        let mut funcs = Vec::new();

        $(
            let $f = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
        )*
        $(
            #[allow(unused_mut)]
            {
                $(
                    let $arg = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
                )*
                let args_vec = $builder.graph.var_list(vec![$($arg),*]);
                let mut meta = $crate::prelude::Value::new(false);
                $(
                    meta = $meta.into();
                )?
                let $retk = $builder.graph.add_var($crate::cps::graph::term::VarDef::NotDefined);
                $builder.push_recursive_scope([$f]);
                let fbody = $fbody;
                $builder.pop_recursive_scope();

                let f = $builder.graph.add_func($f, meta, Some(fbody), $retk, args_vec, None, None);
                $builder.graph[$f].def = VarDef::Func(f);
                funcs.push(f);
            }
        )*

        let funcs_list = $builder.graph.funcs_list(funcs);
        let inner = cps!($builder; $($rest)*);

        let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Fix(
            funcs_list,
            inner
        ), None);

        let funcs_len = funcs_list.len(&$builder.graph.func_pool);
        for i in 0..funcs_len {
            let func = funcs_list.as_slice(&$builder.graph.func_pool)[i];
            $builder.graph[func].parent = Some(term);
        }

        $builder.graph.set_term_parent(inner, Some($crate::cps::graph::term::ParentLink::Term(term)));

        term
    }};

    ($builder: ident; # $s: stmt; $($rest:tt)*) => {{
        $s
        cps!($builder; $($rest)*)
    }};

    ($cps: ident; # $expr: expr) => {
        $expr
    };

    ($builder : ident; $callee: ident ($k: expr, $($arg: expr),*) $(@ $src: expr)?) => {
        {
            let callee_rec = $builder.is_recursive($callee);
            let callee = $builder.graph.add_future_occurrence($callee, callee_rec);
            let k_rec = $builder.is_recursive($k);
            let k = $builder.graph.add_future_occurrence($k, k_rec);
            let occurrences = vec![

                $(
                    {
                        let is_rec = $builder.is_recursive($arg);
                        $builder.graph.add_future_occurrence($arg, is_rec)
                    }
                ),*
            ];

            let list = $builder.graph.occurrence_list(occurrences);

            let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::App(
                callee,
                k,
                list
            ), None);

            $builder.graph.set_site(callee, term);
            $builder.graph.set_site(k, term);
            $builder.graph.set_site_many(list, term);


            $(
                $builder.graph[term].src = $src.into();
            )?

            term
        }
    };

    ($builder: ident; continue $k : ident ($($arg: expr),*) $(@ $src: expr)?) => {
        {
            let k_rec = $builder.is_recursive($k);
            let k = $builder.graph.add_future_occurrence($k, k_rec);
            let occurrences = vec![
                $(
                    {
                        let is_rec = $builder.is_recursive($arg);
                        $builder.graph.add_future_occurrence($arg, is_rec)
                    }
                ),*
            ];



            let list = $builder.graph.occurrence_list(occurrences);

            let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::Continue(
                k,
                list
            ), None);

            $builder.graph.set_site(k, term);
            $builder.graph.set_site_many(list, term);

            $(
                $builder.graph[term].src = $src.into();
            )?

            term
        }
    };

    ($builder: ident; if $cond : ident then $then_branch : ident ($($arg: expr),*) else $else_branch : ident ($($else_arg: expr),*)) => {
        {
            let cond_rec = $builder.is_recursive($cond);
            let cond = $builder.graph.add_future_occurrence($cond, cond_rec);

            let then_occurrences = vec![
                $(
                    {
                        let is_rec = $builder.is_recursive($arg);
                        $builder.graph.add_future_occurrence($arg, is_rec)
                    }
                ),*
            ];

            let else_occurrences = vec![
                $(
                    {
                        let is_rec = $builder.is_recursive($else_arg);
                        $builder.graph.add_future_occurrence($else_arg, is_rec)
                    }
                ),*
            ];

            let then_is_rec = $builder.is_recursive($then_branch);
            let else_is_rec = $builder.is_recursive($else_branch);
            let then = $builder.graph.add_future_occurrence($then_branch, then_is_rec);
            let else_ = $builder.graph.add_future_occurrence($else_branch, else_is_rec);
            let then_list = $builder.graph.occurrence_list(then_occurrences);
            let else_list = $builder.graph.occurrence_list(else_occurrences);

            let term = $builder.graph.add_term($crate::cps::graph::term::TermKind::If(
                cond,
                then,
                then_list,
                else_,
                else_list
            ), None);

            $builder.graph.set_site(cond, term);
            $builder.graph.set_site_many(then_list, term);
            $builder.graph.set_site_many(else_list, term);
            $builder.graph.set_site(then, term);
            $builder.graph.set_site(else_, term);



            term
        }
    };

    ($builder: ident; $term: expr) => {
        $term
    };
}

pub struct CPSBuilder<'gc> {
    pub graph: Graph<'gc>,
    pub varcount: u32,
    pub scope_id: u32,
    pub current_topbox_scope: Option<u32>,
    pub current_meta: Value<'gc>,
    pub recursive_scope: Vec<HashSet<VarId>>,
}

impl<'gc> CPSBuilder<'gc> {
    pub fn new(graph: Graph<'gc>) -> Self {
        Self {
            graph,
            varcount: 0,
            scope_id: 0,
            current_topbox_scope: None,
            current_meta: Value::new(false),
            recursive_scope: Vec::new(),
        }
    }

    pub fn is_recursive(&self, var: VarId) -> bool {
        self.recursive_scope
            .iter()
            .any(|scope| scope.contains(&var))
    }

    pub fn enter_scope(&mut self) {
        self.current_topbox_scope = Some(self.scope_id);
        self.scope_id += 1;
    }

    pub fn set_scope(&mut self, scope: Option<u32>) {
        self.current_topbox_scope = scope;
    }

    pub fn push_recursive_scope(&mut self, vars: impl IntoIterator<Item = VarId>) {
        self.recursive_scope.push(vars.into_iter().collect());
    }

    pub fn pop_recursive_scope(&mut self) {
        self.recursive_scope.pop();
    }
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityList;
    use log::LevelFilter;
    use pretty::BoxAllocator;

    use crate::{cps::graph::contify::Contification, runtime::Scheme};

    use super::*;

    #[test]
    fn test_cps_macro() {
        env_logger::builder()
            .filter_level(LevelFilter::Debug)
            .init();
        let scm = Scheme::new_uninit();

        scm.enter(|ctx| {
            let graph = Graph::new(ctx);
            let mut builder = CPSBuilder::new(graph);

            let retk = builder.graph.add_var(VarDef::NotDefined);

            let root_body = cps!(builder;
                letrec fac(retk, n, acc) = cps!(builder;
                    let one = const 1;
                    letk calc() = cps!(builder;
                        let min1 = #% Minus(n, one);
                        let new_acc = #% Times(acc, n);
                        fac(retk, min1, new_acc)
                    );
                    let is_zero = #% NumericLte(n, one);
                    if is_zero then
                        retk(acc)
                    else
                        calc()
                );

                let five = const 5;
                let acc0 = const 1;
                fac(retk, five, acc0)
            );

            let fvar = builder.graph.add_var(VarDef::NotDefined);
            let rootf = builder.graph.add_func(
                fvar,
                Value::new(false),
                Some(root_body),
                retk,
                EntityList::new(),
                None,
                None,
            );

            builder.graph.root = Some(rootf);

            let alloc = BoxAllocator;

            builder
                .graph
                .pretty::<_, &pretty::BoxAllocator>(&alloc)
                .1
                .render(80, &mut std::io::stdout())
                .unwrap();
            println!();
            let mut contifier = Contification::new(&mut builder.graph);
            contifier.run();

            //builder.graph.reduce();

            builder
                .graph
                .pretty::<_, &pretty::BoxAllocator>(&alloc)
                .1
                .render(80, &mut std::io::stdout())
                .unwrap();
            println!();
        });
    }
}
