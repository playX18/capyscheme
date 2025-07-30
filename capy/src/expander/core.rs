use std::{cell::Cell, collections::HashMap};

use rsgc::{
    Gc, Trace,
    alloc::array::{Array, ArrayRef},
};

use crate::{
    expander::synclo::syntax_annotation,
    runtime::{
        Context,
        value::{Symbol, Value},
    },
};

pub struct Cenv<'gc> {
    pub ctx: Context<'gc>,
    pub expression_name: Value<'gc>,
    pub frames: Box<Frame<'gc>>,
    pub denotations: &'gc Denotations<'gc>,
}

pub struct Denotations<'gc> {
    denotation_of_define: Value<'gc>,
    denotation_of_let: Value<'gc>,
    denotation_of_let_star: Value<'gc>,
    denotation_of_let_rec: Value<'gc>,
    denotation_of_let_rec_star: Value<'gc>,
    denotation_of_lambda: Value<'gc>,
    denotation_of_case_lambda: Value<'gc>,
    denotation_of_begin: Value<'gc>,
    denotation_of_if: Value<'gc>,
    denotation_of_quote: Value<'gc>,
}

impl<'gc> Cenv<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        expression_name: Value<'gc>,
        denotations: &'gc Denotations<'gc>,
    ) -> Self {
        Cenv {
            ctx,
            expression_name,
            frames: Box::new(Frame::new(None)),
            denotations,
        }
    }

    pub fn get(&self, name: Value<'gc>) -> Option<LVarRef<'gc>> {
        self.frames.get(name)
    }

    pub fn extend(&mut self, name: Value<'gc>, lvar_ref: LVarRef<'gc>) {
        self.frames.extend(name, lvar_ref);
    }
}

pub struct Frame<'gc> {
    pub up: Option<Box<Self>>,
    pub env: HashMap<Value<'gc>, LVarRef<'gc>>,
}

impl<'gc> Frame<'gc> {
    pub fn new(up: Option<Box<Self>>) -> Self {
        Frame {
            up,
            env: HashMap::new(),
        }
    }

    pub fn get(&self, name: Value<'gc>) -> Option<LVarRef<'gc>> {
        match self.env.get(&name) {
            Some(lvar_ref) => Some(lvar_ref.clone()),
            None => self.up.as_ref().and_then(|up| up.get(name)),
        }
    }

    pub fn extend(&mut self, name: Value<'gc>, lvar_ref: LVarRef<'gc>) {
        self.env.insert(name, lvar_ref);
    }
}

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct LVar<'gc> {
    pub name: Value<'gc>,
    pub set_count: Cell<u32>,
    pub ref_count: Cell<u32>,
}

pub type LVarRef<'gc> = Gc<'gc, LVar<'gc>>;

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Term<'gc> {
    pub source: Value<'gc>,
    pub kind: TermKind<'gc>,
}

pub type TermRef<'gc> = Gc<'gc, Term<'gc>>;

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub enum TermKind<'gc> {
    LRef(LVarRef<'gc>),
    LSet(LVarRef<'gc>, TermRef<'gc>),
    GRef(Value<'gc>),
    GSet(Value<'gc>, TermRef<'gc>),

    Define(Value<'gc>, TermRef<'gc>),

    If(TermRef<'gc>, TermRef<'gc>, TermRef<'gc>),
    Seq(ArrayRef<'gc, TermRef<'gc>>),

    Let(Let<'gc>),
    Fix(Fix<'gc>),
    Proc(ProcRef<'gc>),

    Call(TermRef<'gc>, ArrayRef<'gc, TermRef<'gc>>),
    PrimCall(Value<'gc>, ArrayRef<'gc, TermRef<'gc>>),
    Const(Value<'gc>),
}

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Proc<'gc> {
    pub name: Value<'gc>,
    pub cases: ArrayRef<'gc, ProcCase<'gc>>,
}

pub type ProcRef<'gc> = Gc<'gc, Proc<'gc>>;

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct ProcCase<'gc> {
    pub args: ArrayRef<'gc, LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub body: TermRef<'gc>,
}

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Let<'gc> {
    pub style: LetStyle,
    pub lhs: ArrayRef<'gc, LVarRef<'gc>>,
    pub rhs: ArrayRef<'gc, TermRef<'gc>>,
    pub body: TermRef<'gc>,
}

#[derive(Trace, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[collect(no_drop)]
pub enum LetStyle {
    Let,
    LetStar,
    LetRec,
    LetRecStar,
}

#[derive(Trace, Debug, Clone)]
pub struct Fix<'gc> {
    pub lhs: ArrayRef<'gc, LVarRef<'gc>>,
    pub rhs: ArrayRef<'gc, ProcRef<'gc>>,
    pub body: TermRef<'gc>,
}

pub fn lref<'gc>(ctx: Context<'gc>, lvar: LVarRef<'gc>) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::LRef(lvar),
        },
    )
}

pub fn lset<'gc>(ctx: Context<'gc>, lvar: LVarRef<'gc>, value: TermRef<'gc>) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::LSet(lvar, value),
        },
    )
}

pub fn gref<'gc>(ctx: Context<'gc>, name: Value<'gc>, sourcev: Value<'gc>) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: sourcev,
            kind: TermKind::GRef(name),
        },
    )
}

pub fn gset<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    value: TermRef<'gc>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: sourcev,
            kind: TermKind::GSet(name, value),
        },
    )
}

pub fn define<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    value: TermRef<'gc>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: sourcev,
            kind: TermKind::Define(name, value),
        },
    )
}

pub fn if_term<'gc>(
    ctx: Context<'gc>,
    test: TermRef<'gc>,
    then_branch: TermRef<'gc>,
    else_branch: TermRef<'gc>,
) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::If(test, then_branch, else_branch),
        },
    )
}

pub fn seq<'gc>(ctx: Context<'gc>, terms: ArrayRef<'gc, TermRef<'gc>>) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::Seq(terms),
        },
    )
}

pub fn let_term<'gc>(
    ctx: Context<'gc>,
    style: LetStyle,
    lhs: ArrayRef<'gc, LVarRef<'gc>>,
    rhs: ArrayRef<'gc, TermRef<'gc>>,
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::Let(Let {
                style,
                lhs,
                rhs,
                body,
            }),
        },
    )
}

pub fn fix_term<'gc>(
    ctx: Context<'gc>,
    lhs: ArrayRef<'gc, LVarRef<'gc>>,
    rhs: ArrayRef<'gc, ProcRef<'gc>>,
    body: TermRef<'gc>,
) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::Fix(Fix { lhs, rhs, body }),
        },
    )
}

pub fn proc_term<'gc>(
    ctx: Context<'gc>,
    name: Value<'gc>,
    cases: impl AsRef<[ProcCase<'gc>]>,
) -> TermRef<'gc> {
    let cases = Array::from_array(&ctx, cases);
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::Proc(Gc::new(&ctx, Proc { name, cases })),
        },
    )
}

pub fn call_term<'gc>(
    ctx: Context<'gc>,
    proc: TermRef<'gc>,
    args: impl AsRef<[TermRef<'gc>]>,
) -> TermRef<'gc> {
    let args = Array::from_array(&ctx, args);
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::Call(proc, args),
        },
    )
}

pub fn prim_call_term<'gc>(
    ctx: Context<'gc>,
    proc: Value<'gc>,
    args: impl AsRef<[TermRef<'gc>]>,
) -> TermRef<'gc> {
    let args = Array::from_array(&ctx, args);
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::PrimCall(proc, args),
        },
    )
}

#[derive(Trace, Debug)]
#[collect(no_drop)]
pub struct CompileError<'gc> {
    pub message: String,
    pub irritants: Vec<Value<'gc>>,
    pub sourcev: Value<'gc>,
}

pub type Error<'gc> = Box<CompileError<'gc>>;

pub fn expand<'gc>(cenv: &mut Cenv<'gc>, program: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if let Some(_) = program.try_as::<Symbol>() {
        match cenv.get(program) {
            Some(lvar) => Ok(lref(cenv.ctx, lvar)),
            None => Ok(gref(cenv.ctx, program, Value::new(false))),
        }
    } else {
        todo!()
    }
}

fn expand_quote<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 2 {
        return Err(Box::new(CompileError {
            message: "quote requires exactly one argument".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let datum = form.cadr();

    Ok(Gc::new(
        &cenv.ctx,
        Term {
            source: syntax_annotation(cenv.ctx, form),
            kind: TermKind::Const(datum),
        },
    ))
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
            &cenv.ctx,
            Term {
                source: Value::new(false),
                kind: TermKind::Const(Value::undefined()),
            },
        );

        Ok(if_term(cenv.ctx, cond, consequent, alternative))
    } else {
        Err(Box::new(CompileError {
            message: "if requires 2 or 3 arguments".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }))
    }
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
        return Err(Box::new(CompileError {
            message: "begin requires at least one expression".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    } else {
        Ok(seq(cenv.ctx, Array::from_array(&cenv.ctx, seq_)))
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
                sourcev: sourcev,
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

enum Define<'gc> {
    Lambda(Value<'gc>, Vec<Value<'gc>>, Value<'gc>),
    Simple(Value<'gc>, Value<'gc>),
}

impl<'gc> Define<'gc> {
    fn parse(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<Self, Error<'gc>> {
        if form.list_length() < 2 {
            return Err(Box::new(CompileError {
                message: "define requires at least one argument".to_string(),
                irritants: vec![form],
                sourcev: syntax_annotation(cenv.ctx, form),
            }));
        }

        let pattern = form.cadr();

        if pattern.is::<Symbol>() {
            // (define <var>) or (define <var> <exp>)
            if form.list_length() == 2 {
                // (define <var>) - variable declaration without value
                Ok(Define::Simple(pattern, Value::undefined()))
            } else if form.list_length() == 3 {
                // (define <var> <exp>)
                let exp = form.caddr();
                Ok(Define::Simple(pattern, exp))
            } else {
                Err(Box::new(CompileError {
                    message: "define with variable takes 0 or 1 expression".to_string(),
                    irritants: vec![form],
                    sourcev: syntax_annotation(cenv.ctx, form),
                }))
            }
        } else if pattern.is_pair() {
            // (define (<var> <args> ...) <exp>)
            if form.list_length() != 3 {
                return Err(Box::new(CompileError {
                    message: "define with function form requires exactly one body expression"
                        .to_string(),
                    irritants: vec![form],
                    sourcev: syntax_annotation(cenv.ctx, form),
                }));
            }

            let var = pattern.car();
            if !var.is::<Symbol>() {
                return Err(Box::new(CompileError {
                    message: "function name must be a symbol".to_string(),
                    irritants: vec![var],
                    sourcev: syntax_annotation(cenv.ctx, form),
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
                        sourcev: syntax_annotation(cenv.ctx, form),
                    }));
                }
                args.push(arg);
                arg_list = arg_list.cdr();
            }

            if !arg_list.is_null() {
                return Err(Box::new(CompileError {
                    message: "improper argument list in function definition".to_string(),
                    irritants: vec![pattern],
                    sourcev: syntax_annotation(cenv.ctx, form),
                }));
            }

            let body = form.caddr();
            Ok(Define::Lambda(var, args, body))
        } else {
            Err(Box::new(CompileError {
                message: "define pattern must be a symbol or list".to_string(),
                irritants: vec![pattern],
                sourcev: syntax_annotation(cenv.ctx, form),
            }))
        }
    }
}

fn finalize_body<'gc>(
    cenv: &mut Cenv<'gc>,
    body: Value<'gc>,
    defs: Vec<Value<'gc>>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    todo!()
}
