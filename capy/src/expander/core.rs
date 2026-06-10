//! Core macro expansion algorithm.  Contains the top-level `expand` function,
//! per-form expanders (`expand_define`, `expand_if`, …), helper routines, and
//! the `CompileError` / `Define` types.  Imports term types from `super::term`
//! and the expansion environment from `super::cenv`.

use std::{cell::Cell, collections::HashMap, sync::OnceLock};

use crate::{
    expander::{datum_sourcev, get_source_property},
    list,
    rsgc::{Gc, Global, Trace, alloc::array::Array, barrier, cell::Lock},
    runtime::{
        Context,
        modules::{Module, get_current_module},
        value::{Str, Symbol, Value},
        vm::syntax::props_to_sourcev,
    },
    static_symbols,
};

// Re-export types from focused submodules for backward compatibility.
pub use crate::expander::cenv::{Cenv, Denotations, Frame};
pub use crate::expander::term::{
    Fix, LVar, LVarRef, Let, LetStyle, Proc, ProcRef, Term, TermKind, TermRef, call_term, constant,
    define, fix_term, if_term, let_term, lref, lset, module_ref, module_set, prim_call_term,
    proc_term, seq, seq_from_slice, toplevel_ref, toplevel_set,
};

type RootedDenotations = crate::Rootable!(Denotations<'_>);

static DENOTATIONS: OnceLock<Global<RootedDenotations>> = OnceLock::new();

static_symbols!(
    DENOTATION_OF_DEFINE = "define"
    DENOTATION_OF_LET = "let"
    DENOTATION_OF_LET_STAR = "let*"
    DENOTATION_OF_LET_REC = "letrec"
    DENOTATION_OF_LET_REC_STAR = "letrec*"
    DENOTATION_OF_LAMBDA = "lambda"
    DENOTATION_OF_CASE_LAMBDA = "case-lambda"
    DENOTATION_OF_BEGIN = "begin"
    DENOTATION_OF_IF = "if"
    DENOTATION_OF_QUOTE = "quote"
    DENOTATION_OF_SET = "set!"
    DENOTATION_OF_VALUES = "values"
    DENOTATION_OF_RECEIVE = "receive"
    DENOTATION_OF_COND = "cond"
    DENOTATION_OF_CASE = "case"
    DENOTATION_OF_AND = "and"
    DENOTATION_OF_OR = "or"
    DENOTATION_OF_DO = "do"
    DENOTATION_OF_WHEN = "when"
    DENOTATION_OF_UNLESS = "unless"
    DENOTATION_OF_WCM = "with-continuation-mark"
    DENOTATION_OF_DEFINE_STRUCT = "define-struct"
    DENOTATION_OF_PUBLIC_REF = "@"
    DENOTATION_OF_PRIVATE_REF = "@@"
);

pub fn denotations<'gc>(ctx: Context<'gc>) -> &'gc Denotations<'gc> {
    DENOTATIONS
        .get_or_init(|| {
            Global::new(Denotations {
                denotation_of_do: denotation_of_do(ctx).into(),
                denotation_of_and: denotation_of_and(ctx).into(),
                denotation_of_cond: denotation_of_cond(ctx).into(),
                denotation_of_case: denotation_of_case(ctx).into(),
                denotation_of_or: denotation_of_or(ctx).into(),
                denotation_of_define: denotation_of_define(ctx).into(),
                denotation_of_let: denotation_of_let(ctx).into(),
                denotation_of_let_star: denotation_of_let_star(ctx).into(),
                denotation_of_let_rec: denotation_of_let_rec(ctx).into(),
                denotation_of_let_rec_star: denotation_of_let_rec_star(ctx).into(),
                denotation_of_lambda: denotation_of_lambda(ctx).into(),
                denotation_of_case_lambda: denotation_of_case_lambda(ctx).into(),
                denotation_of_begin: denotation_of_begin(ctx).into(),
                denotation_of_if: denotation_of_if(ctx).into(),
                denotation_of_quote: denotation_of_quote(ctx).into(),
                denotation_of_set: denotation_of_set(ctx).into(),
                denotation_of_values: denotation_of_values(ctx).into(),
                denotation_of_receive: denotation_of_receive(ctx).into(),
                denotation_of_when: denotation_of_when(ctx).into(),
                denotation_of_unless: denotation_of_unless(ctx).into(),
                denotation_of_wcm: denotation_of_wcm(ctx).into(),
                denotation_of_define_struct: denotation_of_define_struct(ctx).into(),
                denotation_of_public_ref: denotation_of_public_ref(ctx).into(),
                denotation_of_private_ref: denotation_of_private_ref(ctx).into(),
            })
        })
        .fetch(*ctx)
}

#[derive(Trace, Debug)]
#[collect(no_drop)]
pub struct CompileError<'gc> {
    pub message: String,
    pub irritants: Vec<Value<'gc>>,
    pub sourcev: Value<'gc>,
}

impl<'gc> std::fmt::Display for CompileError<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        if !self.irritants.is_empty() {
            write!(f, "\nIrritants:")?;
            for irritant in &self.irritants {
                write!(f, "\n  {}", irritant)?;
            }
        }
        write!(f, "\nAt: {}", self.sourcev)
    }
}

pub type Error<'gc> = Box<CompileError<'gc>>;

// ---------------------------------------------------------------------------
// Main expansion entry point
// ---------------------------------------------------------------------------

pub fn expand<'gc>(cenv: &mut Cenv<'gc>, program: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if program.try_as::<Symbol>().is_some() {
        let module = get_current_module(cenv.ctx).downcast::<Module>();
        let module_name = module.name();
        match cenv.get(program) {
            Some(lvar) => Ok(lref(cenv.ctx, lvar)),
            None => Ok(toplevel_ref(
                cenv.ctx,
                module_name,
                program,
                Value::new(false),
            )),
        }
    } else if program.is_pair() {
        let _source =
            get_source_property(cenv.ctx, program).map(|source| props_to_sourcev(cenv.ctx, source));
        let proc = program.car();
        let args = program.cdr();

        let term = if proc == cenv.denotations.denotation_of_quote {
            expand_quote(cenv, program)
        } else if proc == cenv.denotations.denotation_of_begin {
            expand_begin(cenv, program)
        } else if proc == cenv.denotations.denotation_of_cond {
            expand_cond(cenv, program)
        } else if proc == cenv.denotations.denotation_of_case {
            expand_case(cenv, program)
        } else if proc == cenv.denotations.denotation_of_do {
            expand_do(cenv, program)
        } else if proc == cenv.denotations.denotation_of_if {
            expand_if(cenv, program)
        } else if proc == cenv.denotations.denotation_of_when {
            expand_when(cenv, program)
        } else if proc == cenv.denotations.denotation_of_unless {
            expand_unless(cenv, program)
        } else if proc == cenv.denotations.denotation_of_wcm {
            expand_wcm(cenv, program)
        } else if proc == cenv.denotations.denotation_of_and {
            expand_and(cenv, program)
        } else if proc == cenv.denotations.denotation_of_or {
            expand_or(cenv, program)
        } else if proc == cenv.denotations.denotation_of_define {
            if cenv.frames.is_some() {
                return Err(Box::new(CompileError {
                    message: "invalid define usage".to_string(),
                    irritants: vec![program],
                    sourcev: datum_sourcev(cenv.ctx, program),
                }));
            }

            expand_define(cenv, program)
        } else if proc == cenv.denotations.denotation_of_lambda {
            expand_lambda(cenv, program)
        } else if proc == cenv.denotations.denotation_of_let {
            expand_let(cenv, program)
        } else if proc == cenv.denotations.denotation_of_let_rec {
            expand_letrec(cenv, program)
        } else if proc == cenv.denotations.denotation_of_let_rec_star {
            expand_letrec_star(cenv, program)
        } else if proc == cenv.denotations.denotation_of_let_star {
            expand_let_star(cenv, program)
        } else if proc == cenv.denotations.denotation_of_set {
            expand_set(cenv, program)
        } else if proc == cenv.denotations.denotation_of_receive {
            expand_receive(cenv, program)
        } else if proc == cenv.denotations.denotation_of_values {
            expand_values(cenv, program)
        } else if proc == cenv.denotations.denotation_of_public_ref {
            expand_public_ref(cenv, program)
        } else if proc == cenv.denotations.denotation_of_private_ref {
            expand_private_ref(cenv, program)
        } else {
            let proc = expand(cenv, proc)?;
            let mut xs = args;
            let mut terms = Vec::new();

            while xs.is_pair() {
                let term = expand(cenv, xs.car())?;
                terms.push(term);
                xs = xs.cdr();
            }

            if !xs.is_null() {
                return Err(Box::new(CompileError {
                    message: "invalid syntax in function call".to_string(),
                    irritants: vec![program],
                    sourcev: datum_sourcev(cenv.ctx, program),
                }));
            }

            Ok(call_term(
                cenv.ctx,
                proc,
                terms,
                datum_sourcev(cenv.ctx, program),
            ))
        };

        term.inspect(|&term| {
            let termw = Gc::write(*cenv.ctx, term);

            barrier::field!(termw, Term, source)
                .unlock()
                .set(_source.unwrap_or(Value::new(false)));
        })
    } else {
        Ok(Gc::new(
            *cenv.ctx,
            Term {
                source: Lock::new(datum_sourcev(cenv.ctx, program)),
                kind: TermKind::Const(program),
            },
        ))
    }
}

pub fn fresh_lvar<'gc>(ctx: Context<'gc>, name: Value<'gc>) -> LVarRef<'gc> {
    Gc::new(
        *ctx,
        LVar {
            name,
            id: Value::new(false),
            set_count: Cell::new(0),
            ref_count: Cell::new(0),
        },
    )
}

pub fn fresh_lvar_derived<'gc>(
    ctx: Context<'gc>,
    base: LVarRef<'gc>,
    suffix: &str,
) -> LVarRef<'gc> {
    let name =
        Symbol::from_str_uninterned(*ctx, &format!("{}{}:{}", base.name, base.id, suffix), None);

    Gc::new(
        *ctx,
        LVar {
            name: name.into(),
            id: base.id,
            set_count: Cell::new(0),
            ref_count: Cell::new(0),
        },
    )
}

// ---------------------------------------------------------------------------
// Define parsing
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum Define<'gc> {
    Lambda(Value<'gc>, Vec<Value<'gc>>, Option<Value<'gc>>, Value<'gc>),
    Simple(Value<'gc>, Value<'gc>),
}

impl<'gc> Define<'gc> {
    pub fn parse(ctx: Context<'gc>, form: Value<'gc>) -> Result<Self, Error<'gc>> {
        if form.list_length() < 2 {
            return Err(Box::new(CompileError {
                message: "define requires at least one argument".to_string(),
                irritants: vec![form],
                sourcev: datum_sourcev(ctx, form),
            }));
        }

        let pattern = form.cadr();

        if pattern.is::<Symbol>() {
            if form.list_length() == 2 {
                Ok(Define::Simple(pattern, Value::undefined()))
            } else if form.list_length() == 3 {
                let exp = form.caddr();
                Ok(Define::Simple(pattern, exp))
            } else {
                Err(Box::new(CompileError {
                    message: "define with variable takes 0 or 1 expression".to_string(),
                    irritants: vec![form],
                    sourcev: datum_sourcev(ctx, form),
                }))
            }
        } else if pattern.is_pair() {
            if form.list_length() < 3 {
                return Err(Box::new(CompileError {
                    message: "define with function form requires exactly one body expression"
                        .to_string(),
                    irritants: vec![form],
                    sourcev: datum_sourcev(ctx, form),
                }));
            }

            let var = pattern.car();
            if !var.is::<Symbol>() {
                return Err(Box::new(CompileError {
                    message: "function name must be a symbol".to_string(),
                    irritants: vec![var],
                    sourcev: datum_sourcev(ctx, form),
                }));
            }

            let mut args = Vec::new();
            let mut arg_list = pattern.cdr();

            while arg_list.is_pair() {
                let arg = arg_list.car();
                if !arg.is::<Symbol>() {
                    return Err(Box::new(CompileError {
                        message: "function argument must be a symbol".to_string(),
                        irritants: vec![arg],
                        sourcev: datum_sourcev(ctx, form),
                    }));
                }
                args.push(arg);
                arg_list = arg_list.cdr();
            }

            let variadic = if arg_list.is::<Symbol>() {
                Some(arg_list)
            } else if !arg_list.is_null() {
                return Err(Box::new(CompileError {
                    message: "improper argument list in function definition".to_string(),
                    irritants: vec![pattern],
                    sourcev: datum_sourcev(ctx, form),
                }));
            } else {
                None
            };

            let body = form.cddr();
            Ok(Define::Lambda(var, args, variadic, body))
        } else {
            Err(Box::new(CompileError {
                message: "define pattern must be a symbol or list".to_string(),
                irritants: vec![pattern],
                sourcev: datum_sourcev(ctx, form),
            }))
        }
    }
}

// ---------------------------------------------------------------------------
// Form-specific expanders
// ---------------------------------------------------------------------------

fn expand_set<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 3 {
        return Err(Box::new(CompileError {
            message: "set! requires exactly two arguments".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }
    let old = cenv.expression_name;
    cenv.expression_name = form.cadr();

    if let Some(lvar) = cenv.get(form.cadr()) {
        let value = expand(cenv, form.caddr())?;
        cenv.expression_name = old;
        return Ok(lset(cenv.ctx, lvar, value));
    }

    let value = expand(cenv, form.caddr())?;
    let name = form.cadr();
    let source = datum_sourcev(cenv.ctx, form);
    cenv.expression_name = old;
    let module = get_current_module(cenv.ctx).downcast::<Module>();
    let module_name = module.name();
    Ok(toplevel_set(cenv.ctx, module_name, name, value, source))
}

fn expand_define<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    let def = Define::parse(cenv.ctx, form)?;

    match def {
        Define::Lambda(name, params, variadic, mut body) => {
            let params = params
                .into_iter()
                .map(|sym| fresh_lvar(cenv.ctx, sym))
                .collect::<Vec<_>>();

            let variadic = variadic.map(|sym| fresh_lvar(cenv.ctx, sym));

            cenv.new_frame();

            for lvar in &params {
                cenv.extend(lvar.name, *lvar);
            }

            if let Some(variadic) = &variadic {
                cenv.extend(variadic.name, *variadic);
            }

            let mut meta = list!(
                cenv.ctx,
                Value::cons(
                    cenv.ctx,
                    Symbol::from_str(cenv.ctx, "source").into(),
                    datum_sourcev(cenv.ctx, form)
                ),
                Value::cons(cenv.ctx, Symbol::from_str(cenv.ctx, "name").into(), name)
            );

            if body.is_pair() && body.car().is::<Str>() && body.list_length() >= 2 {
                meta = Value::cons(
                    cenv.ctx,
                    Value::cons(
                        cenv.ctx,
                        Symbol::from_str(cenv.ctx, "documentation").into(),
                        body.car(),
                    ),
                    meta,
                );
                body = body.cdr();
            }

            let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, body))?;
            cenv.pop_frame();

            let proc = Gc::new(
                *cenv.ctx,
                Proc {
                    name,
                    args: Array::from_slice(*cenv.ctx, params),
                    variadic,
                    body: body_term,
                    source: datum_sourcev(cenv.ctx, form),
                    meta,
                },
            );

            let proc_term = Gc::new(
                *cenv.ctx,
                Term {
                    source: Lock::new(datum_sourcev(cenv.ctx, form)),
                    kind: TermKind::Proc(proc),
                },
            );

            let module = get_current_module(cenv.ctx).downcast::<Module>();
            let module_name = module.name();
            Ok(define(
                cenv.ctx,
                module_name,
                name,
                proc_term,
                datum_sourcev(cenv.ctx, form),
            ))
        }

        Define::Simple(name, value) => {
            let old = cenv.expression_name;
            cenv.expression_name = name;
            let term = expand(cenv, value)?;
            cenv.expression_name = old;
            let module = get_current_module(cenv.ctx).downcast::<Module>();
            let module_name = module.name();
            Ok(define(
                cenv.ctx,
                module_name,
                name,
                term,
                datum_sourcev(cenv.ctx, form),
            ))
        }
    }
}

fn expand_quote<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 2 {
        return Err(Box::new(CompileError {
            message: "quote requires exactly one argument".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let datum = form.cadr();

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::Const(datum),
        },
    ))
}

fn expand_when<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "when requires at least test and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let test = form.cadr();
    let body = form.cddr();

    let test_term = expand(cenv, test)?;
    let body_term = if body.cdr().is_null() {
        expand(cenv, body.car())?
    } else {
        let begin_form = Value::cons(cenv.ctx, cenv.denotations.denotation_of_begin, body);
        expand(cenv, begin_form)?
    };

    let undefined_term = constant(cenv.ctx, Value::undefined());

    Ok(if_term(cenv.ctx, test_term, body_term, undefined_term))
}

fn expand_unless<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "unless requires at least test and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let test = form.cadr();
    let body = form.cddr();

    let test_term = expand(cenv, test)?;
    let body_term = if body.cdr().is_null() {
        expand(cenv, body.car())?
    } else {
        let begin_form = Value::cons(cenv.ctx, cenv.denotations.denotation_of_begin, body);
        expand(cenv, begin_form)?
    };

    let undefined_term = constant(cenv.ctx, Value::undefined());

    Ok(if_term(cenv.ctx, test_term, undefined_term, body_term))
}

fn expand_if<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() == 4 {
        let cond = expand(cenv, form.cadr())?;
        let consequent = expand(cenv, form.caddr())?;
        let alternative = expand(cenv, form.cadddr())?;

        Ok(if_term(cenv.ctx, cond, consequent, alternative))
    } else if form.list_length() == 3 {
        let cond = expand(cenv, form.cadr())?;
        let consequent = expand(cenv, form.caddr())?;
        let alternative = Gc::new(
            *cenv.ctx,
            Term {
                source: Lock::new(false.into()),
                kind: TermKind::Const(Value::undefined()),
            },
        );

        Ok(if_term(cenv.ctx, cond, consequent, alternative))
    } else {
        Err(Box::new(CompileError {
            message: "if requires 2 or 3 arguments".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }))
    }
}

fn expand_and<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    let mut exprs = Vec::new();
    let mut current = form.cdr();
    while current.is_pair() {
        exprs.push(current.car());
        current = current.cdr();
    }

    if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed and expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    if exprs.is_empty() {
        return Ok(constant(cenv.ctx, Value::new(true)));
    }

    if exprs.len() == 1 {
        return expand(cenv, exprs[0]);
    }

    let mut it = exprs.into_iter().rev();
    let last_expr = it.next().unwrap();
    let mut result = expand(cenv, last_expr)?;

    let false_branch = constant(cenv.ctx, Value::new(false));

    for expr in it {
        let test_term = expand(cenv, expr)?;
        let temp_lvar = fresh_lvar(cenv.ctx, Symbol::from_str(cenv.ctx, "and-tmp").into());

        let if_branch = if_term(cenv.ctx, lref(cenv.ctx, temp_lvar), result, false_branch);

        result = let_term(
            cenv.ctx,
            LetStyle::Let,
            Array::from_slice(*cenv.ctx, [temp_lvar]),
            Array::from_slice(*cenv.ctx, [test_term]),
            if_branch,
            datum_sourcev(cenv.ctx, form),
        );
    }

    Ok(result)
}

fn expand_or<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    let mut exprs = Vec::new();
    let mut current = form.cdr();
    while current.is_pair() {
        exprs.push(current.car());
        current = current.cdr();
    }

    if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed or expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    if exprs.is_empty() {
        return Ok(constant(cenv.ctx, Value::new(false)));
    }

    if exprs.len() == 1 {
        return expand(cenv, exprs[0]);
    }

    let mut it = exprs.into_iter().rev();
    let last_expr = it.next().unwrap();
    let mut result = expand(cenv, last_expr)?;

    for expr in it {
        let test_term = expand(cenv, expr)?;
        let temp_lvar = fresh_lvar(cenv.ctx, Symbol::from_str(cenv.ctx, "or-tmp").into());

        let if_branch = if_term(
            cenv.ctx,
            lref(cenv.ctx, temp_lvar),
            lref(cenv.ctx, temp_lvar),
            result,
        );

        result = let_term(
            cenv.ctx,
            LetStyle::Let,
            Array::from_slice(*cenv.ctx, [temp_lvar]),
            Array::from_slice(*cenv.ctx, [test_term]),
            if_branch,
            datum_sourcev(cenv.ctx, form),
        );
    }

    Ok(result)
}

fn expand_begin<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    let mut seq_ = Vec::new();

    let mut ls = form.cdr();

    while ls.is_pair() {
        let term = expand(cenv, ls.car())?;
        seq_.push(term);
        ls = ls.cdr();
    }

    if seq_.len() == 1 {
        Ok(seq_[0])
    } else if seq_.is_empty() {
        Err(Box::new(CompileError {
            message: "begin requires at least one expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }))
    } else {
        Ok(seq_from_slice(cenv.ctx, seq_))
    }
}

fn expand_body<'gc>(
    cenv: &mut Cenv<'gc>,
    body: Value<'gc>,
    sourcev: Value<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    fn rec<'gc>(
        cenv: &mut Cenv<'gc>,
        body: Value<'gc>,
        sourcev: Value<'gc>,
        mut defs: Vec<Value<'gc>>,
    ) -> Result<TermRef<'gc>, Error<'gc>> {
        if body.is_null() {
            return Err(Box::new(CompileError {
                message: "body cannot be empty".to_string(),
                irritants: vec![body],
                sourcev,
            }));
        }
        let exp = body.car();
        if exp.is_pair() && exp.car().is::<Symbol>() {
            let e = exp.car();

            if e == cenv.denotations.denotation_of_define {
                defs.push(exp);
                rec(cenv, body.cdr(), sourcev, defs)
            } else if e == cenv.denotations.denotation_of_begin {
                rec(cenv, exp.cdr().append(cenv.ctx, body.cdr()), sourcev, defs)
            } else {
                finalize_body(cenv, body, defs)
            }
        } else {
            finalize_body(cenv, body, defs)
        }
    }

    rec(cenv, body, sourcev, Vec::new())
}

fn expand_wcm<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 4 {
        return Err(Box::new(CompileError {
            message: "with-continuation-mark requires key, value and result expressions"
                .to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let key = form.cadr();
    let value = form.caddr();
    let body = form.cdddr();

    let key_term = expand(cenv, key)?;
    let value_term = expand(cenv, value)?;
    let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::WithContinuationMark(key_term, value_term, body_term),
        },
    ))
}

fn finalize_body<'gc>(
    cenv: &mut Cenv<'gc>,
    body: Value<'gc>,
    defs: Vec<Value<'gc>>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    let defs = defs
        .into_iter()
        .map(|def| Define::parse(cenv.ctx, def))
        .collect::<Result<Vec<_>, _>>()?;

    let mut vars = HashMap::new();

    cenv.new_frame();
    for def in defs.iter() {
        match def {
            Define::Lambda(var, _, _, _) | Define::Simple(var, _) => {
                if vars.contains_key(var) {
                    return Err(Box::new(CompileError {
                        message: format!("duplicate definition of variable '{}'", var),
                        irritants: vec![*var],
                        sourcev: datum_sourcev(cenv.ctx, body),
                    }));
                }
                vars.insert(*var, fresh_lvar(cenv.ctx, *var));

                cenv.extend(*var, *vars.get(var).unwrap());
            }
        }
    }

    let mut lhs = Vec::new();
    let mut rhs: Vec<TermRef> = Vec::new();

    for def in defs {
        match def {
            Define::Lambda(name, formals, variadic, def_body) => {
                let name = *vars.get(&name).unwrap();
                let formals = formals
                    .into_iter()
                    .map(|sym| {
                        Gc::new(
                            *cenv.ctx,
                            LVar {
                                name: sym,
                                id: Value::new(false),
                                set_count: Cell::new(0),
                                ref_count: Cell::new(0),
                            },
                        )
                    })
                    .collect::<Vec<_>>();

                let variadic = variadic.map(|sym| {
                    Gc::new(
                        *cenv.ctx,
                        LVar {
                            name: sym,
                            id: Value::new(false),
                            set_count: Cell::new(0),
                            ref_count: Cell::new(0),
                        },
                    )
                });

                cenv.new_frame();

                for lvar in &formals {
                    cenv.extend(lvar.name, *lvar);
                }

                if let Some(variadic) = &variadic {
                    cenv.extend(variadic.name, *variadic);
                }

                let body_term = expand_body(cenv, def_body, datum_sourcev(cenv.ctx, def_body))?;

                cenv.pop_frame();

                let proc = Gc::new(
                    *cenv.ctx,
                    Proc {
                        name: name.name,
                        args: Array::from_slice(*cenv.ctx, formals),
                        variadic,
                        body: body_term,
                        source: datum_sourcev(cenv.ctx, def_body),
                        meta: Value::null(),
                    },
                );

                lhs.push(name);
                rhs.push(Gc::new(
                    *cenv.ctx,
                    Term {
                        source: Lock::new(datum_sourcev(cenv.ctx, def_body)),
                        kind: TermKind::Proc(proc),
                    },
                ));
            }

            Define::Simple(var, val) => {
                let lvar = *vars.get(&var).unwrap();

                cenv.extend(var, lvar);

                let val_term = expand(cenv, val)?;
                lhs.push(lvar);

                rhs.push(val_term);
            }
        }
    }

    let mut xs = body;
    let mut terms = Vec::new();
    while xs.is_pair() {
        let term = expand(cenv, xs.car())?;
        terms.push(term);
        xs = xs.cdr();
    }

    if !xs.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed body".to_string(),
            irritants: vec![body],
            sourcev: datum_sourcev(cenv.ctx, body),
        }));
    }
    cenv.pop_frame();
    if terms.is_empty() {
        Err(Box::new(CompileError {
            message: "body cannot be empty".to_string(),
            irritants: vec![body],
            sourcev: datum_sourcev(cenv.ctx, body),
        }))
    } else if terms.len() == 1 && lhs.is_empty() {
        Ok(terms[0])
    } else {
        let seq = if terms.len() == 1 {
            terms[0]
        } else {
            seq_from_slice(cenv.ctx, terms)
        };

        if lhs.is_empty() {
            return Ok(seq);
        }

        let l = let_term(
            cenv.ctx,
            LetStyle::LetRecStar,
            Array::from_slice(*cenv.ctx, lhs),
            Array::from_slice(*cenv.ctx, rhs),
            seq,
            datum_sourcev(cenv.ctx, body),
        );
        Ok(l)
    }
}

fn collect_formals<'gc>(
    ctx: Context<'gc>,
    formals: Value<'gc>,
) -> Result<(Vec<LVarRef<'gc>>, Option<LVarRef<'gc>>), Error<'gc>> {
    let mut args = Vec::new();
    let mut current = formals;

    while current.is_pair() {
        let arg = current.car();
        if !arg.is::<Symbol>() {
            return Err(Box::new(CompileError {
                message: "formal parameter must be a symbol".to_string(),
                irritants: vec![arg],
                sourcev: datum_sourcev(ctx, formals),
            }));
        }

        let lvar = Gc::new(
            *ctx,
            LVar {
                name: arg,
                id: Value::new(false),
                set_count: Cell::new(0),
                ref_count: Cell::new(0),
            },
        );
        args.push(lvar);
        current = current.cdr();
    }

    let variadic = if current.is::<Symbol>() {
        Some(Gc::new(
            *ctx,
            LVar {
                name: current,
                id: Value::new(false),
                set_count: Cell::new(0),
                ref_count: Cell::new(0),
            },
        ))
    } else if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "improper formals list".to_string(),
            irritants: vec![formals],
            sourcev: datum_sourcev(ctx, formals),
        }));
    } else {
        None
    };

    Ok((args, variadic))
}

fn expand_lambda<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "lambda requires at least formals and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let formals = form.cadr();
    let mut body = form.cddr();

    let (args, variadic) = collect_formals(cenv.ctx, formals)?;

    cenv.new_frame();

    for lvar in &args {
        cenv.extend(lvar.name, *lvar);
    }

    if let Some(variadic) = &variadic {
        cenv.extend(variadic.name, *variadic);
    }

    let mut meta = list!(
        cenv.ctx,
        Value::cons(
            cenv.ctx,
            Symbol::from_str(cenv.ctx, "name").into(),
            cenv.expression_name
        ),
        Value::cons(
            cenv.ctx,
            Symbol::from_str(cenv.ctx, "source").into(),
            datum_sourcev(cenv.ctx, form)
        )
    );

    if body.is_pair() && body.car().is::<Str>() && body.list_length() >= 2 {
        let doc = body.car();
        body = body.cdr();
        meta = Value::cons(
            cenv.ctx,
            Value::cons(
                cenv.ctx,
                Symbol::from_str(cenv.ctx, "documentation").into(),
                doc,
            ),
            meta,
        );
    }

    let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;
    cenv.pop_frame();

    let proc = Gc::new(
        *cenv.ctx,
        Proc {
            name: Value::new(false),
            args: Array::from_slice(*cenv.ctx, args),
            variadic,
            body: body_term,
            source: datum_sourcev(cenv.ctx, form),
            meta,
        },
    );

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::Proc(proc),
        },
    ))
}

fn expand_let<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "let requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    if bindings.is::<Symbol>() {
        if form.list_length() < 4 {
            return Err(Box::new(CompileError {
                message: "named let requires name, bindings, and at least one body expression"
                    .to_string(),
                irritants: vec![form],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }

        let name = bindings;
        let actual_bindings = form.caddr();
        let body = form.cdddr();

        let (vars, vals) = parse_let_bindings(cenv.ctx, actual_bindings)?;

        let proc_args = vars
            .iter()
            .map(|var| fresh_lvar(cenv.ctx, *var))
            .collect::<Vec<_>>();

        let name_lvar = fresh_lvar(cenv.ctx, name);

        cenv.new_frame();

        cenv.extend(name, name_lvar);

        for (i, lvar) in proc_args.iter().enumerate() {
            cenv.extend(vars[i], *lvar);
        }

        let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;
        cenv.pop_frame();

        let proc = Gc::new(
            *cenv.ctx,
            Proc {
                name,
                args: Array::from_slice(*cenv.ctx, proc_args),
                variadic: None,
                body: body_term,
                source: datum_sourcev(cenv.ctx, form),
                meta: Value::null(),
            },
        );

        let proc_term = Gc::new(
            *cenv.ctx,
            Term {
                source: Lock::new(datum_sourcev(cenv.ctx, form)),
                kind: TermKind::Proc(proc),
            },
        );

        let val_terms = vals
            .into_iter()
            .map(|val| expand(cenv, val))
            .collect::<Result<Vec<_>, _>>()?;

        let call_term = call_term(
            cenv.ctx,
            lref(cenv.ctx, name_lvar),
            val_terms,
            datum_sourcev(cenv.ctx, form),
        );

        Ok(let_term(
            cenv.ctx,
            LetStyle::LetRec,
            Array::from_slice(*cenv.ctx, vec![name_lvar]),
            Array::from_slice(*cenv.ctx, vec![proc_term]),
            call_term,
            datum_sourcev(cenv.ctx, form),
        ))
    } else {
        let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

        let lvars = vars
            .iter()
            .map(|var| fresh_lvar(cenv.ctx, *var))
            .collect::<Vec<_>>();

        let val_terms = vals
            .into_iter()
            .map(|val| expand(cenv, val))
            .collect::<Result<Vec<_>, _>>()?;

        cenv.new_frame();

        for (i, lvar) in lvars.iter().enumerate() {
            cenv.extend(vars[i], *lvar);
        }

        let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;
        cenv.pop_frame();

        Ok(let_term(
            cenv.ctx,
            LetStyle::Let,
            Array::from_slice(*cenv.ctx, lvars),
            Array::from_slice(*cenv.ctx, val_terms),
            body_term,
            datum_sourcev(cenv.ctx, form),
        ))
    }
}

fn parse_let_bindings<'gc>(
    ctx: Context<'gc>,
    bindings: Value<'gc>,
) -> Result<(Vec<Value<'gc>>, Vec<Value<'gc>>), Error<'gc>> {
    let mut vars = Vec::new();
    let mut vals = Vec::new();
    let mut current = bindings;

    while current.is_pair() {
        let binding = current.car();

        if !binding.is_pair() || binding.list_length() != 2 {
            return Err(Box::new(CompileError {
                message: "let binding must be a list of exactly two elements".to_string(),
                irritants: vec![binding],
                sourcev: datum_sourcev(ctx, bindings),
            }));
        }

        let var = binding.car();
        let val = binding.cadr();

        if !var.is::<Symbol>() {
            return Err(Box::new(CompileError {
                message: "let binding variable must be a symbol".to_string(),
                irritants: vec![var],
                sourcev: datum_sourcev(ctx, bindings),
            }));
        }

        vars.push(var);
        vals.push(val);
        current = current.cdr();
    }

    if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed let bindings".to_string(),
            irritants: vec![bindings],
            sourcev: datum_sourcev(ctx, bindings),
        }));
    }

    Ok((vars, vals))
}

fn expand_do<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "do requires at least bindings and test clause".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let test_clause = form.caddr();
    let commands = form.cdddr();

    let mut vars = Vec::new();
    let mut inits = Vec::new();
    let mut steps = Vec::new();
    let mut current = bindings;

    while current.is_pair() {
        let binding = current.car();

        if !binding.is_pair() {
            return Err(Box::new(CompileError {
                message: "do binding must be a list".to_string(),
                irritants: vec![binding],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }

        let var = binding.car();
        if !var.is::<Symbol>() {
            return Err(Box::new(CompileError {
                message: "do binding variable must be a symbol".to_string(),
                irritants: vec![var],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }

        let binding_rest = binding.cdr();
        if !binding_rest.is_pair() {
            return Err(Box::new(CompileError {
                message: "do binding must have at least variable and init".to_string(),
                irritants: vec![binding],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }

        let init = binding_rest.car();
        let step = if binding_rest.cdr().is_pair() {
            binding_rest.cadr()
        } else if binding_rest.cdr().is_null() {
            var
        } else {
            return Err(Box::new(CompileError {
                message: "malformed do binding".to_string(),
                irritants: vec![binding],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        };

        vars.push(var);
        inits.push(init);
        steps.push(step);
        current = current.cdr();
    }

    if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed do bindings".to_string(),
            irritants: vec![bindings],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    if !test_clause.is_pair() {
        return Err(Box::new(CompileError {
            message: "do test clause must be a list".to_string(),
            irritants: vec![test_clause],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let test = test_clause.car();
    let result_exprs = test_clause.cdr();

    let loop_name = Symbol::from_str(cenv.ctx, "do-loop").into();
    let loop_lvar = fresh_lvar(cenv.ctx, loop_name);

    let loop_vars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    let init_terms = inits
        .into_iter()
        .map(|init| expand(cenv, init))
        .collect::<Result<Vec<_>, _>>()?;

    cenv.new_frame();

    cenv.extend(loop_name, loop_lvar);

    for (i, lvar) in loop_vars.iter().enumerate() {
        cenv.extend(vars[i], *lvar);
    }

    let test_term = expand(cenv, test)?;

    let result_term = if result_exprs.is_null() {
        constant(cenv.ctx, Value::undefined())
    } else if result_exprs.cdr().is_null() {
        expand(cenv, result_exprs.car())?
    } else {
        let begin_form = Value::cons(cenv.ctx, cenv.denotations.denotation_of_begin, result_exprs);
        expand(cenv, begin_form)?
    };

    let mut command_terms = Vec::new();
    let mut cmd_list = commands;
    while cmd_list.is_pair() {
        let cmd_term = expand(cenv, cmd_list.car())?;
        command_terms.push(cmd_term);
        cmd_list = cmd_list.cdr();
    }

    let step_terms = steps
        .into_iter()
        .map(|step| expand(cenv, step))
        .collect::<Result<Vec<_>, _>>()?;

    cenv.pop_frame();

    let loop_call = call_term(
        cenv.ctx,
        lref(cenv.ctx, loop_lvar),
        step_terms,
        datum_sourcev(cenv.ctx, form),
    );

    let false_branch = if command_terms.is_empty() {
        loop_call
    } else {
        command_terms.push(loop_call);
        seq_from_slice(cenv.ctx, command_terms)
    };

    let loop_body = if_term(cenv.ctx, test_term, result_term, false_branch);

    let loop_proc = Gc::new(
        *cenv.ctx,
        Proc {
            name: loop_name,
            args: Array::from_slice(*cenv.ctx, loop_vars),
            variadic: None,
            body: loop_body,
            source: datum_sourcev(cenv.ctx, form),
            meta: Value::null(),
        },
    );

    let loop_proc_term = Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::Proc(loop_proc),
        },
    );

    let initial_call = call_term(
        cenv.ctx,
        lref(cenv.ctx, loop_lvar),
        init_terms,
        datum_sourcev(cenv.ctx, form),
    );

    Ok(let_term(
        cenv.ctx,
        LetStyle::LetRec,
        Array::from_slice(*cenv.ctx, vec![loop_lvar]),
        Array::from_slice(*cenv.ctx, vec![loop_proc_term]),
        initial_call,
        datum_sourcev(cenv.ctx, form),
    ))
}

fn expand_let_star<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "let* requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

    let lvars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    cenv.new_frame();

    let mut val_terms = Vec::new();
    for (i, val) in vals.into_iter().enumerate() {
        let val_term = expand(cenv, val)?;
        val_terms.push(val_term);
        cenv.extend(vars[i], lvars[i]);
    }

    let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;
    cenv.pop_frame();

    let mut result = body_term;

    for i in (0..lvars.len()).rev() {
        result = let_term(
            cenv.ctx,
            LetStyle::Let,
            Array::from_slice(*cenv.ctx, vec![lvars[i]]),
            Array::from_slice(*cenv.ctx, vec![val_terms[i]]),
            result,
            datum_sourcev(cenv.ctx, form),
        );
    }

    Ok(result)
}

fn expand_letrec<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "letrec requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

    let lvars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    cenv.new_frame();

    for (i, lvar) in lvars.iter().enumerate() {
        cenv.extend(vars[i], *lvar);
    }

    let val_terms = vals
        .into_iter()
        .map(|val| expand(cenv, val))
        .collect::<Result<Vec<_>, _>>()?;

    let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;
    cenv.pop_frame();

    Ok(let_term(
        cenv.ctx,
        LetStyle::LetRec,
        Array::from_slice(*cenv.ctx, lvars),
        Array::from_slice(*cenv.ctx, val_terms),
        body_term,
        datum_sourcev(cenv.ctx, form),
    ))
}

fn expand_receive<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 4 {
        return Err(Box::new(CompileError {
            message: "receive requires at least formals, producer, and consumer".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let formals = form.cadr();
    let producer = form.caddr();
    let consumer = form.cadddr();

    let (formals, opt_formal) = collect_formals(cenv.ctx, formals)?;

    let producer_term = expand(cenv, producer)?;

    cenv.new_frame();
    for lvar in formals.iter().chain(opt_formal.iter()) {
        cenv.extend(lvar.name, *lvar);
    }

    let consumer_term = expand(cenv, consumer)?;
    cenv.pop_frame();

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::Receive(
                Array::from_slice(*cenv.ctx, formals),
                opt_formal,
                producer_term,
                consumer_term,
            ),
        },
    ))
}

fn expand_values<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 1 {
        return Err(Box::new(CompileError {
            message: "values requires at least one value".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let mut values = Vec::new();
    let mut xs = form.cdr();

    while xs.is_pair() {
        let value = expand(cenv, xs.car())?;
        values.push(value);
        xs = xs.cdr();
    }

    if !xs.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed values form".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::Values(Array::from_slice(*cenv.ctx, values)),
        },
    ))
}

fn expand_letrec_star<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "letrec* requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

    let lvars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    cenv.new_frame();

    for (i, lvar) in lvars.iter().enumerate() {
        cenv.extend(vars[i], *lvar);
    }

    let mut val_terms = Vec::new();
    for val in vals.into_iter() {
        let val_term = expand(cenv, val)?;
        val_terms.push(val_term);
    }

    let body_term = expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?;
    cenv.pop_frame();

    Ok(let_term(
        cenv.ctx,
        LetStyle::LetRecStar,
        Array::from_slice(*cenv.ctx, lvars),
        Array::from_slice(*cenv.ctx, val_terms),
        body_term,
        datum_sourcev(cenv.ctx, form),
    ))
}

fn expand_private_ref<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 3 {
        return Err(Box::new(CompileError {
            message: "@@ requires exactly two arguments: module name and variable name".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let modname = form.cadr();
    let varname = form.caddr();

    if !varname.is::<Symbol>() {
        return Err(Box::new(CompileError {
            message: "variable name must be a symbol".to_string(),
            irritants: vec![varname],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::ModuleRef(modname, varname, false),
        },
    ))
}

fn expand_public_ref<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 3 {
        return Err(Box::new(CompileError {
            message: "@ requires exactly two arguments: module name and variable name".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let modname = form.cadr();
    let varname = form.caddr();

    if !varname.is::<Symbol>() {
        return Err(Box::new(CompileError {
            message: "variable name must be a symbol".to_string(),
            irritants: vec![varname],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    Ok(Gc::new(
        *cenv.ctx,
        Term {
            source: Lock::new(datum_sourcev(cenv.ctx, form)),
            kind: TermKind::ModuleRef(modname, varname, true),
        },
    ))
}

fn expand_cond<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 2 {
        return Err(Box::new(CompileError {
            message: "cond requires at least one clause".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let mut clauses = Vec::new();
    let mut current = form.cdr();

    while current.is_pair() {
        let clause = current.car();
        if !clause.is_pair() {
            return Err(Box::new(CompileError {
                message: "cond clause must be a list".to_string(),
                irritants: vec![clause],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }
        clauses.push(clause);
        current = current.cdr();
    }

    if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed cond expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    expand_clause(cenv, form, &clauses)
}

fn expand_clause<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
    clauses: &[Value<'gc>],
) -> Result<TermRef<'gc>, Error<'gc>> {
    if clauses.is_empty() {
        return Ok(constant(cenv.ctx, Value::undefined()));
    }

    let clause = clauses[0];
    let rest = &clauses[1..];

    let else_branch = expand_clause(cenv, form, rest)?;

    let test = clause.car();
    let body = clause.cdr();

    if test.is::<Symbol>() && test == Symbol::from_str(cenv.ctx, "else").into() {
        if body.is_null() {
            return Err(Box::new(CompileError {
                message: "else clause cannot be empty".to_string(),
                irritants: vec![clause],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }

        if body.cdr().is_null() {
            expand(cenv, body.car())
        } else {
            expand_body(cenv, body, datum_sourcev(cenv.ctx, form))
        }
    } else {
        let test_form = expand(cenv, test)?;
        let binding = fresh_lvar(cenv.ctx, Symbol::from_str(cenv.ctx, "cond-test-tmp").into());

        let lbody = if body.is_pair()
            && body.car().is::<Symbol>()
            && body.car() == Symbol::from_str(cenv.ctx, "=>").into()
        {
            if body.list_length() != 2 {
                return Err(Box::new(CompileError {
                    message: "=> clause must have exactly one procedure expression".to_string(),
                    irritants: vec![clause],
                    sourcev: datum_sourcev(cenv.ctx, form),
                }));
            }

            let proc_expr = body.cadr();
            let proc_term = expand(cenv, proc_expr)?;

            call_term(
                cenv.ctx,
                proc_term,
                vec![lref(cenv.ctx, binding)],
                datum_sourcev(cenv.ctx, clause),
            )
        } else if body.is_null() {
            lref(cenv.ctx, binding)
        } else {
            expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?
        };

        let if_ = if_term(cenv.ctx, lref(cenv.ctx, binding), lbody, else_branch);
        Ok(let_term(
            cenv.ctx,
            LetStyle::Let,
            Array::from_slice(*cenv.ctx, [binding]),
            Array::from_slice(*cenv.ctx, [test_form]),
            if_,
            datum_sourcev(cenv.ctx, form),
        ))
    }
}

fn expand_case<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "case requires at least key expression and one clause".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let key_expr = form.cadr();
    let mut clauses = Vec::new();
    let mut current = form.cddr();

    while current.is_pair() {
        let clause = current.car();
        if !clause.is_pair() {
            return Err(Box::new(CompileError {
                message: "case clause must be a list".to_string(),
                irritants: vec![clause],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }
        clauses.push(clause);
        current = current.cdr();
    }

    if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "malformed case expression".to_string(),
            irritants: vec![form],
            sourcev: datum_sourcev(cenv.ctx, form),
        }));
    }

    let key_term = expand(cenv, key_expr)?;
    let key_lvar = fresh_lvar(cenv.ctx, Symbol::from_str(cenv.ctx, "case-key").into());

    let case_body = expand_case_clauses(cenv, form, &clauses, key_lvar)?;

    Ok(let_term(
        cenv.ctx,
        LetStyle::Let,
        Array::from_slice(*cenv.ctx, vec![key_lvar]),
        Array::from_slice(*cenv.ctx, vec![key_term]),
        case_body,
        datum_sourcev(cenv.ctx, form),
    ))
}

fn expand_case_clauses<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
    clauses: &[Value<'gc>],
    key_lvar: LVarRef<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    if clauses.is_empty() {
        return Ok(constant(cenv.ctx, Value::undefined()));
    }

    let clause = clauses[0];
    let rest = &clauses[1..];

    let else_branch = expand_case_clauses(cenv, form, rest, key_lvar)?;

    let test = clause.car();
    let body = clause.cdr();

    if test.is::<Symbol>() && test == Symbol::from_str(cenv.ctx, "else").into() {
        if body.is_null() {
            return Err(Box::new(CompileError {
                message: "else clause cannot be empty".to_string(),
                irritants: vec![clause],
                sourcev: datum_sourcev(cenv.ctx, form),
            }));
        }

        if body.is_pair()
            && body.car().is::<Symbol>()
            && body.car() == Symbol::from_str(cenv.ctx, "=>").into()
        {
            if body.list_length() != 2 {
                return Err(Box::new(CompileError {
                    message: "=> clause must have exactly one procedure expression".to_string(),
                    irritants: vec![clause],
                    sourcev: datum_sourcev(cenv.ctx, form),
                }));
            }

            let proc_expr = body.cadr();
            let proc_term = expand(cenv, proc_expr)?;

            return Ok(call_term(
                cenv.ctx,
                proc_term,
                vec![lref(cenv.ctx, key_lvar)],
                datum_sourcev(cenv.ctx, clause),
            ));
        }

        if body.cdr().is_null() {
            expand(cenv, body.car())
        } else {
            expand_body(cenv, body, datum_sourcev(cenv.ctx, form))
        }
    } else {
        let memv_test = if test.is_pair() {
            let quoted_test = Value::cons(
                cenv.ctx,
                cenv.denotations.denotation_of_quote,
                Value::cons(cenv.ctx, test, Value::null()),
            );
            prim_call_term(
                cenv.ctx,
                Symbol::from_str(cenv.ctx, "memv").into(),
                vec![lref(cenv.ctx, key_lvar), expand(cenv, quoted_test)?],
                datum_sourcev(cenv.ctx, form),
            )
        } else {
            let quoted_test = Value::cons(
                cenv.ctx,
                cenv.denotations.denotation_of_quote,
                Value::cons(cenv.ctx, test, Value::null()),
            );
            prim_call_term(
                cenv.ctx,
                Symbol::from_str(cenv.ctx, "eqv?").into(),
                vec![lref(cenv.ctx, key_lvar), expand(cenv, quoted_test)?],
                datum_sourcev(cenv.ctx, form),
            )
        };

        let then_branch = if body.is_pair()
            && body.car().is::<Symbol>()
            && body.car() == Symbol::from_str(cenv.ctx, "=>").into()
        {
            if body.list_length() != 2 {
                return Err(Box::new(CompileError {
                    message: "=> clause must have exactly one procedure expression".to_string(),
                    irritants: vec![clause],
                    sourcev: datum_sourcev(cenv.ctx, form),
                }));
            }

            let proc_expr = body.cadr();
            let proc_term = expand(cenv, proc_expr)?;

            if test.is_pair() {
                let test_result_lvar = fresh_lvar(
                    cenv.ctx,
                    Symbol::from_str(cenv.ctx, "case-test-result").into(),
                );

                let car_call = prim_call_term(
                    cenv.ctx,
                    Symbol::from_str(cenv.ctx, "car").into(),
                    vec![lref(cenv.ctx, test_result_lvar)],
                    datum_sourcev(cenv.ctx, clause),
                );

                let proc_call = call_term(
                    cenv.ctx,
                    proc_term,
                    vec![car_call],
                    datum_sourcev(cenv.ctx, clause),
                );

                let if_branch = if_term(
                    cenv.ctx,
                    lref(cenv.ctx, test_result_lvar),
                    proc_call,
                    else_branch,
                );

                return Ok(let_term(
                    cenv.ctx,
                    LetStyle::Let,
                    Array::from_slice(*cenv.ctx, vec![test_result_lvar]),
                    Array::from_slice(*cenv.ctx, vec![memv_test]),
                    if_branch,
                    datum_sourcev(cenv.ctx, form),
                ));
            } else {
                call_term(
                    cenv.ctx,
                    proc_term,
                    vec![lref(cenv.ctx, key_lvar)],
                    datum_sourcev(cenv.ctx, clause),
                )
            }
        } else if body.is_null() {
            constant(cenv.ctx, Value::undefined())
        } else if body.cdr().is_null() {
            expand(cenv, body.car())?
        } else {
            expand_body(cenv, body, datum_sourcev(cenv.ctx, form))?
        };

        Ok(if_term(cenv.ctx, memv_test, then_branch, else_branch))
    }
}
