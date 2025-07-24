use std::{cell::Cell, hash::Hash};

use lasso::Spur;

use crate::{ast::{Datum, DatumValue, Symbol, P}, source::Span};


/// Immediate form.
///
/// We expand R5RS Scheme into IForms on which we can perform optimizations. IForms
/// are way easier to work and reason about since they don't have cons-lists, internal defines,
/// unresolved variables and other aambiguity that can exist in Scheme code.
///
/// IForms on their own are used for a short period of time only for a few passes:
/// - Fixing letrec: getting rid of letrec by replacing it with `fix`, `set!` and `let` forms.
/// - Primitive resolution: resolve `GRef` forms into `PrimRef` forms if they reference primitive
/// procedures or constants.
/// - Lambda lifting: removing captured variables if closure does not escape scope it was created in.
/// - Primitive beta-reduction: cases like ((lambda (x) x) 42) are by default handled and replaced with just `42`.
/// - Single Assignment Elimination: When variable is assigned to only once (using `set!`) we can skip boxing
/// the variable and instead just create a new variable.
#[derive(Debug, Clone)]
pub struct IForm {
    pub span: Span,
    pub term: ITerm,
}

impl IForm {
    pub fn constant(span: Span, val: DatumValue) -> P<Self> {
        P(Self {
            span,
            term: ITerm::Const(Datum::new(val)),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ITerm {
    Const(P<Datum>),
    LSet(P<LVar>, P<IForm>),
    LRef(P<LVar>),
    GRef(P<Symbol>),
    GSet(P<Symbol>, P<IForm>),
    PrimRef(Spur),
    Seq(Vec<P<IForm>>),
    Define(P<Symbol>, P<IForm>),

    Let(Let),
    Fix(Fix),
    Proc(P<Proc>),
    If(P<IForm>, P<IForm>, P<IForm>),
    App(P<IForm>, Vec<P<IForm>>),
    PrimApp(Spur, Vec<P<IForm>>),
}

impl IForm {
    pub fn is_transparent(&self) -> bool {
        match &self.term {
            ITerm::Const(_) | ITerm::PrimRef(_) | ITerm::Proc(_) => true,
            ITerm::LRef(var) => !var.is_mutated(),
            ITerm::If(test, cons, alt) => {
                test.is_transparent() && cons.is_transparent() && alt.is_transparent()
            }
            ITerm::Let(l) => {
                l.body.is_transparent() && l.initializers.iter().all(|x| x.is_transparent())
            }

            ITerm::Seq(seq) => seq.iter().all(|x| x.is_transparent()),

            ITerm::App(proc, args) => {
                if !args.iter().all(|x| x.is_transparent()) {
                    return false;
                }

                if let ITerm::Proc(ref proc) = proc.term {
                    proc.cases.iter().all(|case| case.body.is_transparent())
                } else {
                    false
                }
            }

            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LetStyle {
    Let,
    LetStar,
    LetRec,
    LetRecStar,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub style: LetStyle,
    pub variables: Vec<P<LVar>>,
    pub initializers: Vec<P<IForm>>,
    pub body: P<IForm>,
}

#[derive(Debug, Clone)]
pub struct Fix {
    pub variables: Vec<P<LVar>>,
    pub procedures: Vec<P<Proc>>,
    pub body: P<IForm>,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub loc: Option<Span>,
    pub name: Option<P<Datum>>,
    pub cases: Vec<ProcCase>,
}

#[derive(Debug, Clone)]
pub struct ProcCase {
    pub loc: Option<Span>,
    pub args: Vec<P<LVar>>,
    pub variadic: Option<P<LVar>>,
    pub body: P<IForm>,
}

#[derive(Debug)]
pub struct LVar {
    pub name: P<Datum>,
    pub loc: Option<Span>,
    ref_count: Cell<u32>,
    set_count: Cell<u32>,
}

impl LVar {
    pub fn new(name: P<Datum>, loc: Option<Span>) -> P<LVar> {
        P(LVar {
            name,
            loc,

            ref_count: Cell::new(0),
            set_count: Cell::new(0),
        })
    }

    pub fn set(&self) {
        self.set_count.set(self.set_count.get() + 1);
    }

    pub fn ref_(&self) {
        self.ref_count.set(self.ref_count.get() + 1);
    }

    pub fn is_mutated(&self) -> bool {
        self.set_count.get() != 0
    }

    pub fn is_mutated_once(&self) -> bool {
        self.set_count.get() == 1
    }

    pub fn unref(&self) {
        self.ref_count.set(0);
        self.set_count.set(0);
    }

    pub fn ref_count(&self) -> u32 {
        self.ref_count.get()
    }

    pub fn set_count(&self) -> u32 {
        self.set_count.get()
    }
}

impl Hash for LVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl PartialEq for LVar {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for LVar {}

impl IForm {
    pub fn count_refs(&self) {
        match &self.term {
            ITerm::Const(_)
            | ITerm::GRef(_)
            | ITerm::PrimRef(_) => (),
            ITerm::LRef(var) => {
                var.ref_();
            }

            ITerm::LSet(var, val) => {
                var.set();
                val.count_refs();
            } 

            ITerm::App(proc, args) => {
                for arg in args {
                    arg.count_refs();
                }
                proc.count_refs();
            }

            ITerm::Define(_, val) => {
                val.count_refs();
            }

            ITerm::Fix(fix) => {
                for var in fix.variables.iter() {
                    var.unref();
                }

                for proc in fix.procedures.iter() {
                    for case in proc.cases.iter() {
                        for arg in case.args.iter() {
                            arg.unref();
                        }
                        if let Some(variadic) = &case.variadic {
                            variadic.unref();
                        }
                        case.body.count_refs();
                    }
                }

                fix.body.count_refs();
            }

            ITerm::Let(l) => {
                for var in l.variables.iter() {
                    var.unref();
                }

                for init in l.initializers.iter() {
                    init.count_refs();
                }

                l.body.count_refs();
            }

            ITerm::GSet(_,val ) => {
                val.count_refs();
            }

            ITerm::If(cond, then, else_) => {
                cond.count_refs();
                then.count_refs();
                else_.count_refs();
            }

            ITerm::PrimApp(_, args) => {
                for arg in args {
                    arg.count_refs();
                }
            }

            ITerm::Proc(proc) => {
                for case in proc.cases.iter() {
                    for arg in case.args.iter() {
                        arg.unref();
                    }
                    if let Some(variadic) = &case.variadic {
                        variadic.unref();
                    }
                    case.body.count_refs();
                }
            }

            ITerm::Seq(seq) => {
                for item in seq.iter() {
                    item.count_refs();
                }
            }
        }
    }
}