use std::{cell::Cell, collections::HashMap, hash::Hash, sync::OnceLock};

use pretty::{DocAllocator, DocBuilder};
use rsgc::{
    Gc, Global, Rootable, Trace,
    alloc::array::{Array, ArrayRef},
    barrier,
};

use crate::{
    expander::{
        assignment_elimination, fix_letrec::fix_letrec, get_source_property,
        primitives::resolve_primitives, synclo::syntax_annotation,
    },
    runtime::{
        Context,
        value::{Symbol, Value},
    },
    static_symbols,
};

pub struct Cenv<'gc> {
    pub ctx: Context<'gc>,
    pub expression_name: Value<'gc>,
    pub frames: Option<Box<Frame<'gc>>>,
    pub denotations: &'gc Denotations<'gc>,
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct Denotations<'gc> {
    pub denotation_of_define: Value<'gc>,
    pub denotation_of_let: Value<'gc>,
    pub denotation_of_let_star: Value<'gc>,
    pub denotation_of_let_rec: Value<'gc>,
    pub denotation_of_let_rec_star: Value<'gc>,
    pub denotation_of_lambda: Value<'gc>,
    pub denotation_of_case_lambda: Value<'gc>,
    pub denotation_of_begin: Value<'gc>,
    pub denotation_of_if: Value<'gc>,
    pub denotation_of_quote: Value<'gc>,
    pub denotation_of_set: Value<'gc>,
    pub denotation_of_values: Value<'gc>,
    pub denotation_of_receive: Value<'gc>,
}

static DENOTATIONS: OnceLock<Global<Rootable!(Denotations<'_>)>> = OnceLock::new();

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
);

pub fn denotations<'gc>(ctx: Context<'gc>) -> &'gc Denotations<'gc> {
    &*DENOTATIONS
        .get_or_init(|| {
            Global::new(Denotations {
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
            })
        })
        .fetch(&ctx)
}

impl<'gc> Cenv<'gc> {
    pub fn toplevel(ctx: Context<'gc>) -> Self {
        Cenv {
            ctx,
            expression_name: Value::undefined(),
            frames: None,
            denotations: denotations(ctx),
        }
    }

    pub fn new(
        ctx: Context<'gc>,
        expression_name: Value<'gc>,
        denotations: &'gc Denotations<'gc>,
    ) -> Self {
        Cenv {
            ctx,
            expression_name,
            frames: None,
            denotations,
        }
    }

    pub fn get(&self, name: Value<'gc>) -> Option<LVarRef<'gc>> {
        self.frames.as_ref().and_then(|frame| frame.get(name))
    }

    pub fn extend(&mut self, name: Value<'gc>, lvar_ref: LVarRef<'gc>) {
        self.frames.as_mut().unwrap().extend(name, lvar_ref);
    }

    pub fn new_frame(&mut self) {
        let mut new_frame = Box::new(Frame::new(None));
        new_frame.up = self.frames.take();
        self.frames = Some(new_frame);
    }

    pub fn pop_frame(&mut self) {
        if let Some(frame) = self.frames.take() {
            self.frames = frame.up;
        } else {
            panic!("Cannot pop frame from empty stack");
        }
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

impl<'gc> LVar<'gc> {
    pub fn copy(&self, ctx: Context<'gc>) -> LVarRef<'gc> {
        let name = Symbol::from_str_uninterned(&ctx, &format!("{}'", self.name), None);

        Gc::new(
            &ctx,
            LVar {
                name: name.into(),
                set_count: Cell::new(self.set_count.get()),
                ref_count: Cell::new(self.ref_count.get()),
            },
        )
    }
}

impl<'gc> PartialEq for LVar<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl<'gc> Eq for LVar<'gc> {}

impl<'gc> Hash for LVar<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let hashcode = Gc::ptr_hash(unsafe { Gc::from_ptr(self) });
        state.write_u64(hashcode);
    }
}

pub type LVarRef<'gc> = Gc<'gc, LVar<'gc>>;

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Term<'gc> {
    pub source: Value<'gc>,
    pub kind: TermKind<'gc>,
}

pub type TermRef<'gc> = Gc<'gc, Term<'gc>>;

#[derive(Trace, Debug, Clone, Copy)]
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
    /// receive (<formals> . <opt-formal>) <producer> <consumer>:
    ///
    /// Execute `<producer>` and bind the result to <formals>, then
    /// execute `<consumer>` with the bound values.
    Receive(
        ArrayRef<'gc, LVarRef<'gc>>,
        Option<LVarRef<'gc>>,
        TermRef<'gc>,
        TermRef<'gc>,
    ),
    Values(ArrayRef<'gc, TermRef<'gc>>),
    Proc(ProcRef<'gc>),

    Call(TermRef<'gc>, ArrayRef<'gc, TermRef<'gc>>),
    PrimCall(Value<'gc>, ArrayRef<'gc, TermRef<'gc>>),

    Const(Value<'gc>),
}

#[derive(Trace, Debug, Clone)]
#[collect(no_drop)]
pub struct Proc<'gc> {
    pub name: Value<'gc>,
    pub source: Value<'gc>,
    pub args: ArrayRef<'gc, LVarRef<'gc>>,
    pub variadic: Option<LVarRef<'gc>>,
    pub body: TermRef<'gc>,
}

pub type ProcRef<'gc> = Gc<'gc, Proc<'gc>>;

#[derive(Trace, Debug, Clone, Copy)]
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

#[derive(Trace, Debug, Clone, Copy)]
pub struct Fix<'gc> {
    pub lhs: ArrayRef<'gc, LVarRef<'gc>>,
    pub rhs: ArrayRef<'gc, ProcRef<'gc>>,
    pub body: TermRef<'gc>,
}

pub fn constant<'gc>(ctx: Context<'gc>, value: Value<'gc>) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::Const(value),
        },
    )
}

pub fn lref<'gc>(ctx: Context<'gc>, lvar: LVarRef<'gc>) -> TermRef<'gc> {
    lvar.reference();
    Gc::new(
        &ctx,
        Term {
            source: Value::new(false),
            kind: TermKind::LRef(lvar),
        },
    )
}

pub fn lset<'gc>(ctx: Context<'gc>, lvar: LVarRef<'gc>, value: TermRef<'gc>) -> TermRef<'gc> {
    lvar.mutate();
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
    args: ArrayRef<'gc, LVarRef<'gc>>,
    variadic: Option<LVarRef<'gc>>,
    body: TermRef<'gc>,
    source: Value<'gc>,
) -> TermRef<'gc> {
    Gc::new(
        &ctx,
        Term {
            source,
            kind: TermKind::Proc(Gc::new(
                &ctx,
                Proc {
                    name,
                    args,
                    variadic,
                    body,
                    source,
                },
            )),
        },
    )
}

pub fn call_term<'gc>(
    ctx: Context<'gc>,
    proc: TermRef<'gc>,
    args: impl AsRef<[TermRef<'gc>]>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    let args = Array::from_array(&ctx, args);
    Gc::new(
        &ctx,
        Term {
            source: sourcev,
            kind: TermKind::Call(proc, args),
        },
    )
}

pub fn prim_call_term<'gc>(
    ctx: Context<'gc>,
    proc: Value<'gc>,
    args: impl AsRef<[TermRef<'gc>]>,
    sourcev: Value<'gc>,
) -> TermRef<'gc> {
    let args = Array::from_array(&ctx, args);
    Gc::new(
        &ctx,
        Term {
            source: sourcev,
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
    } else if program.is_pair() {
        let source = get_source_property(cenv.ctx, program);
        let proc = program.car();
        let args = program.cdr();

        let term = if proc == cenv.denotations.denotation_of_quote {
            expand_quote(cenv, program)
        } else if proc == cenv.denotations.denotation_of_begin {
            expand_begin(cenv, program)
        } else if proc == cenv.denotations.denotation_of_if {
            expand_if(cenv, program)
        } else if proc == cenv.denotations.denotation_of_define {
            if cenv.frames.is_some() {
                return Err(Box::new(CompileError {
                    message: "invalid define usage".to_string(),
                    irritants: vec![program],
                    sourcev: syntax_annotation(cenv.ctx, program),
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
                    sourcev: syntax_annotation(cenv.ctx, program),
                }));
            }

            Ok(call_term(
                cenv.ctx,
                proc,
                terms,
                syntax_annotation(cenv.ctx, program),
            ))
        };

        term.map(|term| {
            let termw = Gc::write(&cenv.ctx, term);
            barrier::field!(termw, Term, source).write(source.unwrap_or(Value::new(false)));

            term
        })
    } else {
        Ok(Gc::new(
            &cenv.ctx,
            Term {
                source: syntax_annotation(cenv.ctx, program),
                kind: TermKind::Const(program),
            },
        ))
    }
}

pub fn fresh_lvar<'gc>(ctx: Context<'gc>, name: Value<'gc>) -> LVarRef<'gc> {
    Gc::new(
        &ctx,
        LVar {
            name,
            set_count: Cell::new(0),
            ref_count: Cell::new(0),
        },
    )
}

fn expand_set<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() != 3 {
        return Err(Box::new(CompileError {
            message: "set! requires exactly two arguments".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    if let Some(lvar) = cenv.get(form.cadr()) {
        let value = expand(cenv, form.caddr())?;
        return Ok(lset(cenv.ctx, lvar, value));
    }

    let value = expand(cenv, form.caddr())?;
    let name = form.cadr();
    Ok(gset(
        cenv.ctx,
        name,
        value,
        syntax_annotation(cenv.ctx, form),
    ))
}

fn expand_define<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    let def = Define::parse(cenv.ctx, form)?;

    match def {
        Define::Lambda(name, params, variadic, body) => {
            let params = params
                .into_iter()
                .map(|sym| fresh_lvar(cenv.ctx, sym))
                .collect::<Vec<_>>();

            let variadic = variadic.map(|sym| fresh_lvar(cenv.ctx, sym));

            cenv.new_frame();

            for lvar in &params {
                cenv.extend(lvar.name, lvar.clone());
            }

            if let Some(variadic) = &variadic {
                cenv.extend(variadic.name, variadic.clone());
            }

            let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, body))?;
            cenv.pop_frame();

            let proc = Gc::new(
                &cenv.ctx,
                Proc {
                    name,
                    args: Array::from_array(&cenv.ctx, params),
                    variadic,
                    body: body_term,
                    source: syntax_annotation(cenv.ctx, form),
                },
            );

            let proc_term = Gc::new(
                &cenv.ctx,
                Term {
                    source: syntax_annotation(cenv.ctx, form),
                    kind: TermKind::Proc(proc),
                },
            );

            return Ok(define(
                cenv.ctx,
                name,
                proc_term,
                syntax_annotation(cenv.ctx, form),
            ));
        }

        Define::Simple(name, value) => {
            let term = expand(cenv, value)?;

            Ok(define(
                cenv.ctx,
                name,
                term,
                syntax_annotation(cenv.ctx, form),
            ))
        }
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
                sourcev: syntax_annotation(ctx, form),
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
                    sourcev: syntax_annotation(ctx, form),
                }))
            }
        } else if pattern.is_pair() {
            // (define (<var> <args> ...) <exp>)

            if form.list_length() < 3 {
                return Err(Box::new(CompileError {
                    message: "define with function form requires exactly one body expression"
                        .to_string(),
                    irritants: vec![form],
                    sourcev: syntax_annotation(ctx, form),
                }));
            }

            let var = pattern.car();
            if !var.is::<Symbol>() {
                return Err(Box::new(CompileError {
                    message: "function name must be a symbol".to_string(),
                    irritants: vec![var],
                    sourcev: syntax_annotation(ctx, form),
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
                        sourcev: syntax_annotation(ctx, form),
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
                    sourcev: syntax_annotation(ctx, form),
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
                sourcev: syntax_annotation(ctx, form),
            }))
        }
    }
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
                        sourcev: syntax_annotation(cenv.ctx, body),
                    }));
                }
                vars.insert(*var, fresh_lvar(cenv.ctx, *var));

                cenv.extend(*var, vars.get(var).unwrap().clone());
            }
        }
    }

    let mut lhs = Vec::new();
    let mut rhs: Vec<TermRef> = Vec::new();

    for def in defs {
        match def {
            Define::Lambda(name, formals, variadic, body) => {
                let name = vars.get(&name).unwrap().clone();
                let formals = formals
                    .into_iter()
                    .map(|sym| {
                        Gc::new(
                            &cenv.ctx,
                            LVar {
                                name: sym,
                                set_count: Cell::new(0),
                                ref_count: Cell::new(0),
                            },
                        )
                    })
                    .collect::<Vec<_>>();

                let variadic = variadic.map(|sym| {
                    Gc::new(
                        &cenv.ctx,
                        LVar {
                            name: sym,
                            set_count: Cell::new(0),
                            ref_count: Cell::new(0),
                        },
                    )
                });

                cenv.new_frame();

                for lvar in &formals {
                    cenv.extend(lvar.name, lvar.clone());
                }

                if let Some(variadic) = &variadic {
                    cenv.extend(variadic.name, variadic.clone());
                }

                let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, body))?;

                cenv.pop_frame();

                let proc = Gc::new(
                    &cenv.ctx,
                    Proc {
                        name: name.name,
                        args: Array::from_array(&cenv.ctx, formals),
                        variadic,
                        body: body_term,
                        source: syntax_annotation(cenv.ctx, body),
                    },
                );

                lhs.push(name);
                rhs.push(Gc::new(
                    &cenv.ctx,
                    Term {
                        source: syntax_annotation(cenv.ctx, body),
                        kind: TermKind::Proc(proc),
                    },
                ));
            }

            Define::Simple(var, val) => {
                let lvar = vars.get(&var).unwrap().clone();

                cenv.extend(var, lvar.clone());

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
            sourcev: syntax_annotation(cenv.ctx, body),
        }));
    }
    cenv.pop_frame();
    if terms.is_empty() {
        return Err(Box::new(CompileError {
            message: "body cannot be empty".to_string(),
            irritants: vec![body],
            sourcev: syntax_annotation(cenv.ctx, body),
        }));
    } else if terms.len() == 1 && lhs.is_empty() {
        // If there's only one term and no definitions, return it directly
        return Ok(terms[0].clone());
    } else {
        let seq = if terms.len() == 1 {
            terms[0].clone()
        } else {
            seq(cenv.ctx, Array::from_array(&cenv.ctx, terms))
        };

        if lhs.is_empty() {
            return Ok(seq);
        }

        let l = let_term(
            cenv.ctx,
            LetStyle::LetRecStar,
            Array::from_array(&cenv.ctx, lhs),
            Array::from_array(&cenv.ctx, rhs),
            seq,
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
                sourcev: syntax_annotation(ctx, formals),
            }));
        }

        let lvar = Gc::new(
            &ctx,
            LVar {
                name: arg,
                set_count: Cell::new(0),
                ref_count: Cell::new(0),
            },
        );
        args.push(lvar);
        current = current.cdr();
    }

    let variadic = if current.is::<Symbol>() {
        Some(Gc::new(
            &ctx,
            LVar {
                name: current,
                set_count: Cell::new(0),
                ref_count: Cell::new(0),
            },
        ))
    } else if !current.is_null() {
        return Err(Box::new(CompileError {
            message: "improper formals list".to_string(),
            irritants: vec![formals],
            sourcev: syntax_annotation(ctx, formals),
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
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let formals = form.cadr();
    let body = form.cddr();

    let (args, variadic) = collect_formals(cenv.ctx, formals)?;

    cenv.new_frame();

    for lvar in &args {
        cenv.extend(lvar.name, lvar.clone());
    }

    if let Some(variadic) = &variadic {
        cenv.extend(variadic.name, variadic.clone());
    }

    let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, form))?;
    cenv.pop_frame();

    let proc = Gc::new(
        &cenv.ctx,
        Proc {
            name: Value::new(false), // Anonymous lambda
            args: Array::from_array(&cenv.ctx, args),
            variadic,
            body: body_term,
            source: syntax_annotation(cenv.ctx, form),
        },
    );

    Ok(Gc::new(
        &cenv.ctx,
        Term {
            source: syntax_annotation(cenv.ctx, form),
            kind: TermKind::Proc(proc),
        },
    ))
}

fn expand_let<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "let requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    // Check for named let: (let name bindings body...)
    if bindings.is::<Symbol>() {
        // Named let form: (let name ((var val) ...) body...)
        if form.list_length() < 4 {
            return Err(Box::new(CompileError {
                message: "named let requires name, bindings, and at least one body expression"
                    .to_string(),
                irritants: vec![form],
                sourcev: syntax_annotation(cenv.ctx, form),
            }));
        }

        let name = bindings;
        let actual_bindings = form.caddr();
        let body = form.cdddr();

        // Parse bindings
        let (vars, vals) = parse_let_bindings(cenv.ctx, actual_bindings)?;

        // Create a procedure for the named let
        let proc_args = vars
            .iter()
            .map(|var| fresh_lvar(cenv.ctx, *var))
            .collect::<Vec<_>>();

        // Create letrec binding for the named function
        let name_lvar = fresh_lvar(cenv.ctx, name);

        cenv.new_frame();

        // Bind the function name
        cenv.extend(name, name_lvar.clone());

        // Bind the parameters
        for (i, lvar) in proc_args.iter().enumerate() {
            cenv.extend(vars[i], lvar.clone());
        }

        let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, form))?;
        cenv.pop_frame();

        let proc = Gc::new(
            &cenv.ctx,
            Proc {
                name,
                args: Array::from_array(&cenv.ctx, proc_args),
                variadic: None,
                body: body_term,
                source: syntax_annotation(cenv.ctx, form),
            },
        );

        let proc_term = Gc::new(
            &cenv.ctx,
            Term {
                source: syntax_annotation(cenv.ctx, form),
                kind: TermKind::Proc(proc),
            },
        );

        // Expand the initial values
        let val_terms = vals
            .into_iter()
            .map(|val| expand(cenv, val))
            .collect::<Result<Vec<_>, _>>()?;

        // Create the letrec + call
        let call_term = call_term(
            cenv.ctx,
            lref(cenv.ctx, name_lvar.clone()),
            val_terms,
            syntax_annotation(cenv.ctx, form),
        );

        Ok(let_term(
            cenv.ctx,
            LetStyle::LetRec,
            Array::from_array(&cenv.ctx, vec![name_lvar]),
            Array::from_array(&cenv.ctx, vec![proc_term]),
            call_term,
        ))
    } else {
        // Regular let form: (let ((var val) ...) body...)
        let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

        // Create lvars for the bindings
        let lvars = vars
            .iter()
            .map(|var| fresh_lvar(cenv.ctx, *var))
            .collect::<Vec<_>>();

        // Expand the values in the current environment
        let val_terms = vals
            .into_iter()
            .map(|val| expand(cenv, val))
            .collect::<Result<Vec<_>, _>>()?;

        cenv.new_frame();

        // Bind the variables
        for (i, lvar) in lvars.iter().enumerate() {
            cenv.extend(vars[i], lvar.clone());
        }

        let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, form))?;
        cenv.pop_frame();

        Ok(let_term(
            cenv.ctx,
            LetStyle::Let,
            Array::from_array(&cenv.ctx, lvars),
            Array::from_array(&cenv.ctx, val_terms),
            body_term,
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
                sourcev: syntax_annotation(ctx, bindings),
            }));
        }

        let var = binding.car();
        let val = binding.cadr();

        if !var.is::<Symbol>() {
            return Err(Box::new(CompileError {
                message: "let binding variable must be a symbol".to_string(),
                irritants: vec![var],
                sourcev: syntax_annotation(ctx, bindings),
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
            sourcev: syntax_annotation(ctx, bindings),
        }));
    }

    Ok((vars, vals))
}

fn expand_let_star<'gc>(
    cenv: &mut Cenv<'gc>,
    form: Value<'gc>,
) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "let* requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

    // Create lvars for the bindings
    let lvars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    cenv.new_frame();

    // Expand values sequentially, binding each variable as we go
    let mut val_terms = Vec::new();
    for (i, val) in vals.into_iter().enumerate() {
        let val_term = expand(cenv, val)?;
        val_terms.push(val_term);
        // Bind the variable after expanding its value
        cenv.extend(vars[i], lvars[i].clone());
    }

    let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, form))?;
    cenv.pop_frame();

    Ok(let_term(
        cenv.ctx,
        LetStyle::LetStar,
        Array::from_array(&cenv.ctx, lvars),
        Array::from_array(&cenv.ctx, val_terms),
        body_term,
    ))
}

fn expand_letrec<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    if form.list_length() < 3 {
        return Err(Box::new(CompileError {
            message: "letrec requires at least bindings and one body expression".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

    // Create lvars for the bindings
    let lvars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    cenv.new_frame();

    // Bind all variables first (before expanding any values)
    for (i, lvar) in lvars.iter().enumerate() {
        cenv.extend(vars[i], lvar.clone());
    }

    // Now expand all values in the extended environment
    let val_terms = vals
        .into_iter()
        .map(|val| expand(cenv, val))
        .collect::<Result<Vec<_>, _>>()?;

    let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, form))?;
    cenv.pop_frame();

    Ok(let_term(
        cenv.ctx,
        LetStyle::LetRec,
        Array::from_array(&cenv.ctx, lvars),
        Array::from_array(&cenv.ctx, val_terms),
        body_term,
    ))
}

fn expand_receive<'gc>(cenv: &mut Cenv<'gc>, form: Value<'gc>) -> Result<TermRef<'gc>, Error<'gc>> {
    // (receive (formal ...) producer consumer)
    if form.list_length() < 4 {
        return Err(Box::new(CompileError {
            message: "receive requires at least formals, producer, and consumer".to_string(),
            irritants: vec![form],
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let formals = form.cadr();
    let producer = form.caddr();
    let consumer = form.cadddr();

    let (formals, opt_formal) = collect_formals(cenv.ctx, formals)?;

    let producer_term = expand(cenv, producer)?;

    cenv.new_frame();
    for lvar in formals.iter().chain(opt_formal.iter()) {
        cenv.extend(lvar.name, lvar.clone());
    }

    let consumer_term = expand(cenv, consumer)?;
    cenv.pop_frame();

    Ok(Gc::new(
        &cenv.ctx,
        Term {
            source: syntax_annotation(cenv.ctx, form),
            kind: TermKind::Receive(
                Array::from_array(&cenv.ctx, formals),
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
            sourcev: syntax_annotation(cenv.ctx, form),
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
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    Ok(Gc::new(
        &cenv.ctx,
        Term {
            source: syntax_annotation(cenv.ctx, form),
            kind: TermKind::Values(Array::from_array(&cenv.ctx, values)),
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
            sourcev: syntax_annotation(cenv.ctx, form),
        }));
    }

    let bindings = form.cadr();
    let body = form.cddr();

    let (vars, vals) = parse_let_bindings(cenv.ctx, bindings)?;

    // Create lvars for the bindings
    let lvars = vars
        .iter()
        .map(|var| fresh_lvar(cenv.ctx, *var))
        .collect::<Vec<_>>();

    cenv.new_frame();

    // Bind all variables first (before expanding any values)
    for (i, lvar) in lvars.iter().enumerate() {
        cenv.extend(vars[i], lvar.clone());
    }

    // Expand values sequentially (like let* but with all vars pre-bound)
    let mut val_terms = Vec::new();
    for val in vals.into_iter() {
        let val_term = expand(cenv, val)?;
        val_terms.push(val_term);
    }

    let body_term = expand_body(cenv, body, syntax_annotation(cenv.ctx, form))?;
    cenv.pop_frame();

    Ok(let_term(
        cenv.ctx,
        LetStyle::LetRecStar,
        Array::from_array(&cenv.ctx, lvars),
        Array::from_array(&cenv.ctx, val_terms),
        body_term,
    ))
}

impl<'gc> Term<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        match &self.kind {
            TermKind::Values(vals) => {
                let vals_doc =
                    alloc.intersperse(vals.iter().map(|v| v.pretty(alloc)), alloc.space());

                alloc.text("values ").append(vals_doc).parens().group()
            }
            TermKind::Receive(formals, opt_formal, producer, consumer) => {
                let formals_doc = alloc.intersperse(
                    formals.iter().map(|f| alloc.text(f.name.to_string())),
                    alloc.space(),
                );

                let opt_formal_doc = if let Some(opt) = opt_formal {
                    alloc.text(opt.name.to_string())
                } else {
                    alloc.nil()
                };

                let formals_doc = if opt_formal.is_some() {
                    formals_doc + alloc.text(" . ") + opt_formal_doc
                } else {
                    formals_doc
                }
                .parens()
                .group();

                let producer_doc = producer.pretty(alloc);
                let consumer_doc = consumer.pretty(alloc);

                alloc
                    .text("receive ")
                    .append(formals_doc)
                    .append(alloc.line())
                    .append(producer_doc.nest(1).indent(1))
                    .append(alloc.line())
                    .append(consumer_doc.nest(1).indent(1))
                    .nest(1)
                    .parens()
                    .group()
            }

            TermKind::Const(c) => alloc
                .text("const ")
                .append(alloc.text(c.to_string()))
                .parens()
                .group(),

            TermKind::Call(proc, args) => {
                let proc_doc = proc.pretty(alloc);
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.line());

                let args = if args.is_empty() {
                    alloc.nil()
                } else {
                    alloc.space().append(args_doc.nest(1)).group()
                };

                alloc
                    .text("call ")
                    .append(proc_doc)
                    .append(args)
                    .parens()
                    .group()
            }

            TermKind::PrimCall(proc, args) => {
                let proc_doc = alloc.text(proc.to_string());
                let args_doc =
                    alloc.intersperse(args.iter().map(|arg| arg.pretty(alloc)), alloc.line());

                let args = if args.is_empty() {
                    alloc.nil()
                } else {
                    alloc.space().append(args_doc.nest(1)).group()
                };

                alloc
                    .text("#%")
                    .append(proc_doc)
                    .append(args)
                    .parens()
                    .group()
            }

            TermKind::If(test, cons, alt) => {
                let test_doc = test.pretty(alloc);
                let cons_doc = cons.pretty(alloc);
                let alt_doc = alt.pretty(alloc);

                alloc
                    .text("if")
                    .append(alloc.line())
                    .append(test_doc.nest(1).indent(1))
                    .append(alloc.line())
                    .append(cons_doc.indent(1).nest(1))
                    .append(alloc.line())
                    .append(alt_doc.nest(1).indent(1))
                    .nest(1)
                    .parens()
                    .group()
            }

            TermKind::Define(var, exp) => {
                let var_doc = alloc.text(var.to_string());
                let exp_doc = exp.pretty(alloc);

                alloc
                    .text("define ")
                    .append(var_doc)
                    .append(alloc.space())
                    .append(exp_doc)
                    .parens()
                    .group()
            }

            TermKind::LRef(lvar) => {
                let lvar_doc = alloc.text(lvar.name.to_string());
                alloc.text("lref ").append(lvar_doc).parens().group()
            }

            TermKind::LSet(lvar, value) => {
                let lvar_doc = alloc.text(lvar.name.to_string());
                let value_doc = value.pretty(alloc);

                alloc
                    .text("lset ")
                    .append(lvar_doc)
                    .append(alloc.space())
                    .append(value_doc)
                    .parens()
                    .group()
            }

            TermKind::GRef(var) => {
                let var_doc = alloc.text(var.to_string());
                alloc.text("gref ").append(var_doc).parens().group()
            }

            TermKind::GSet(var, value) => {
                let var_doc = alloc.text(var.to_string());
                let value_doc = value.pretty(alloc);

                alloc
                    .text("gset ")
                    .append(var_doc)
                    .append(alloc.space())
                    .append(value_doc)
                    .parens()
                    .group()
            }

            TermKind::Seq(seq) => {
                let seq_doc =
                    alloc.intersperse(seq.iter().map(|term| term.pretty(alloc)), alloc.hardline());

                alloc
                    .text("seq")
                    .append(alloc.line())
                    .append(seq_doc)
                    .nest(1)
                    .parens()
                    .group()
            }

            TermKind::Let(l) => l.pretty(alloc),
            TermKind::Fix(f) => f.pretty(alloc),
            TermKind::Proc(proc) => proc.pretty(alloc),
        }
    }
}

impl<'gc> Proc<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let args = if let Some(variadic) = self.variadic
            && self.args.is_empty()
        {
            alloc.text(variadic.name.to_string())
        } else if let Some(variadic) = self.variadic {
            alloc
                .intersperse(
                    self.args.iter().map(|arg| alloc.text(arg.name.to_string())),
                    alloc.space(),
                )
                .append(alloc.space())
                .append(alloc.text(variadic.name.to_string()))
                .parens()
        } else if !self.args.is_empty() {
            alloc
                .intersperse(
                    self.args.iter().map(|arg| alloc.text(arg.name.to_string())),
                    alloc.space(),
                )
                .parens()
        } else {
            alloc.text("()")
        };

        let body_doc = self.body.pretty(alloc);

        let case = args.group().append(alloc.line()).append(body_doc).nest(1);
        alloc.text("lambda ").append(case).group().parens()
    }
}

impl<'gc> Let<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let bindings_doc = alloc
            .intersperse(
                self.lhs.iter().zip(self.rhs.iter()).map(|(var, exp)| {
                    alloc
                        .text(var.name.to_string())
                        .append(alloc.hardline())
                        .append(exp.pretty(alloc))
                        .nest(2)
                        .brackets()
                        .group()
                }),
                alloc.line(),
            )
            .group()
            .parens();

        let body_doc = self.body.pretty(alloc);

        let style = match self.style {
            LetStyle::Let => "let",
            LetStyle::LetRec => "letrec",
            LetStyle::LetStar => "let*",
            LetStyle::LetRecStar => "letrec*",
        };

        alloc
            .text(style)
            .append(alloc.line())
            .append(bindings_doc)
            .nest(2)
            .append(alloc.line())
            .append(body_doc)
            .nest(1)
            .parens()
            .group()
    }
}

impl<'gc> Fix<'gc> {
    pub fn pretty<'a, D, A>(&self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: 'a + Clone,
    {
        let bindings_doc = alloc
            .intersperse(
                self.lhs.iter().zip(self.rhs.iter()).map(|(var, exp)| {
                    alloc
                        .text(var.name.to_string())
                        .append(alloc.softline())
                        .append(exp.pretty(alloc).nest(1))
                        .brackets()
                        .group()
                }),
                alloc.line(),
            )
            .group()
            .parens();

        let body_doc = self.body.pretty(alloc);

        alloc
            .text("fix")
            .append(alloc.line())
            .append(bindings_doc)
            .nest(1)
            .append(alloc.line())
            .append(body_doc)
            .nest(1)
            .parens()
            .group()
    }
}

impl<'gc> LVar<'gc> {
    pub fn is_mutated(&self) -> bool {
        self.set_count.get() > 0
    }

    pub fn mutate(&self) {
        self.set_count.set(self.set_count.get() + 1);
    }

    pub fn reference(&self) {
        self.ref_count.set(self.ref_count.get() + 1);
    }

    pub fn is_referenced(&self) -> bool {
        self.ref_count.get() > 0
    }
}

impl<'gc> Term<'gc> {
    pub fn is_transparent(&self) -> bool {
        match &self.kind {
            TermKind::Const(_) => true,
            TermKind::LRef(_) => true,
            TermKind::GRef(_) => true,
            TermKind::Proc(_) => true,
            TermKind::If(test, cons, alt) => {
                test.is_transparent() && cons.is_transparent() && alt.is_transparent()
            }

            TermKind::Seq(seq) => seq.iter().all(|term| term.is_transparent()),

            _ => false,
        }
    }
}

pub fn optimize<'gc>(ctx: Context<'gc>, mut term: TermRef<'gc>) -> TermRef<'gc> {
    term = fix_letrec(ctx, term);
    term = assignment_elimination::eliminate_assignments(ctx, term);
    term = resolve_primitives(ctx, term);

    term
}
