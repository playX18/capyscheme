use crate::token::Punct;
use crate::types::{FloatKind, IntKind, Type};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOp {
    LOr,
    LAnd,
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl BinOp {
    /// Binary operator for a punctuator, with its precedence (higher binds tighter).
    pub fn from_punct(p: Punct) -> Option<(BinOp, u8)> {
        use BinOp::*;
        Some(match p {
            Punct::PipePipe => (LOr, 1),
            Punct::AmpAmp => (LAnd, 2),
            Punct::Pipe => (BitOr, 3),
            Punct::Caret => (BitXor, 4),
            Punct::Amp => (BitAnd, 5),
            Punct::EqEq => (Eq, 6),
            Punct::Ne => (Ne, 6),
            Punct::Lt => (Lt, 7),
            Punct::Gt => (Gt, 7),
            Punct::Le => (Le, 7),
            Punct::Ge => (Ge, 7),
            Punct::Shl => (Shl, 8),
            Punct::Shr => (Shr, 8),
            Punct::Plus => (Add, 9),
            Punct::Minus => (Sub, 9),
            Punct::Star => (Mul, 10),
            Punct::Slash => (Div, 10),
            Punct::Percent => (Rem, 10),
            _ => return None,
        })
    }

    /// Operator of a compound assignment punctuator; `Some(None)` is plain `=`.
    pub fn from_assign(p: Punct) -> Option<Option<BinOp>> {
        use BinOp::*;
        Some(match p {
            Punct::Assign => None,
            Punct::StarAssign => Some(Mul),
            Punct::SlashAssign => Some(Div),
            Punct::PercentAssign => Some(Rem),
            Punct::PlusAssign => Some(Add),
            Punct::MinusAssign => Some(Sub),
            Punct::ShlAssign => Some(Shl),
            Punct::ShrAssign => Some(Shr),
            Punct::AmpAssign => Some(BitAnd),
            Punct::CaretAssign => Some(BitXor),
            Punct::PipeAssign => Some(BitOr),
            _ => return None,
        })
    }

    pub fn is_comparison(self) -> bool {
        use BinOp::*;
        matches!(self, Eq | Ne | Lt | Gt | Le | Ge)
    }

    pub fn is_logical(self) -> bool {
        matches!(self, BinOp::LOr | BinOp::LAnd)
    }

    pub fn is_shift(self) -> bool {
        matches!(self, BinOp::Shl | BinOp::Shr)
    }

    /// Symbol used when the operator is handed to Scheme.
    pub fn sym(self) -> &'static str {
        use BinOp::*;
        match self {
            LOr => "lor",
            LAnd => "land",
            BitOr => "bor",
            BitXor => "bxor",
            BitAnd => "band",
            Eq => "eq",
            Ne => "ne",
            Lt => "lt",
            Gt => "gt",
            Le => "le",
            Ge => "ge",
            Shl => "shl",
            Shr => "shr",
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            Rem => "rem",
        }
    }

    pub fn name(self) -> &'static str {
        use BinOp::*;
        match self {
            LOr => "||",
            LAnd => "&&",
            BitOr => "|",
            BitXor => "^",
            BitAnd => "&",
            Eq => "==",
            Ne => "!=",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
            Shl => "<<",
            Shr => ">>",
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnOp {
    Plus,
    Neg,
    BitNot,
    Not,
    Deref,
    AddrOf,
    PreInc,
    PreDec,
}

impl UnOp {
    pub fn sym(self) -> &'static str {
        match self {
            UnOp::Plus => "plus",
            UnOp::Neg => "neg",
            UnOp::BitNot => "bnot",
            UnOp::Not => "not",
            UnOp::Deref => "deref",
            UnOp::AddrOf => "addr",
            UnOp::PreInc => "preinc",
            UnOp::PreDec => "predec",
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            UnOp::Plus => "+",
            UnOp::Neg => "-",
            UnOp::BitNot => "~",
            UnOp::Not => "!",
            UnOp::Deref => "*",
            UnOp::AddrOf => "&",
            UnOp::PreInc => "++",
            UnOp::PreDec => "--",
        }
    }
}

/// Compiler builtins that may appear in constant expressions.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Builtin {
    Expect,
    ConstantP,
    ChooseExpr,
    Inf(crate::types::FloatKind),
    Nan(crate::types::FloatKind),
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Builtin> {
        use crate::types::FloatKind::*;
        Some(match name {
            "__builtin_expect" | "__builtin_expect_with_probability" => Builtin::Expect,
            "__builtin_constant_p" => Builtin::ConstantP,
            "__builtin_choose_expr" => Builtin::ChooseExpr,
            "__builtin_inf" | "__builtin_huge_val" => Builtin::Inf(Double),
            "__builtin_inff" | "__builtin_huge_valf" => Builtin::Inf(Float),
            "__builtin_infl" | "__builtin_huge_vall" => Builtin::Inf(LongDouble),
            "__builtin_nan" | "__builtin_nans" => Builtin::Nan(Double),
            "__builtin_nanf" | "__builtin_nansf" => Builtin::Nan(Float),
            "__builtin_nanl" | "__builtin_nansl" => Builtin::Nan(LongDouble),
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IncDec {
    Inc,
    Dec,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i128, IntKind),
    Float(f64, FloatKind),
    Str(Vec<u8>),
    Char(i128, IntKind),
    Ident(Rc<str>),
    Call(Box<Expr>, Vec<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    PostInc(IncDec, Box<Expr>),
    /// `lhs = rhs`, or `lhs op= rhs` when an operator is given.
    Assign(Option<BinOp>, Box<Expr>, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Cast(Type, Box<Expr>),
    SizeofType(Type),
    SizeofExpr(Box<Expr>),
    AlignofType(Type),
    Member(Box<Expr>, Rc<str>),
    Arrow(Box<Expr>, Rc<str>),
    Index(Box<Expr>, Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
    StmtExpr(Vec<Stmt>),
    CompoundLit(Type, Vec<(Vec<Designator>, Init)>),
    Generic(Box<Expr>, Vec<(Option<Type>, Expr)>),
    OffsetOf(Type, Vec<OffsetStep>),
    Bool(bool),
    Nullptr,
    /// An identifier annotated with its type from the scope it was parsed in.
    Typed(Type, Box<Expr>),
    /// A construct that is parsed but cannot be translated.
    Unsupported(Rc<str>),
}

#[derive(Clone, Debug)]
pub enum OffsetStep {
    Field(Rc<str>),
    Index(Expr),
}

#[derive(Clone, Debug)]
pub enum Designator {
    Field(Rc<str>),
    Index(Expr),
}

#[derive(Clone, Debug)]
pub enum Init {
    Expr(Expr),
    List(Vec<(Vec<Designator>, Init)>),
}

#[derive(Clone, Debug)]
pub struct LocalDecl {
    pub name: Rc<str>,
    pub ty: Type,
    pub init: Option<Init>,
    pub is_static: bool,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Decl(Vec<LocalDecl>),
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    DoWhile(Box<Stmt>, Expr),
    For(Option<Box<Stmt>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Switch(Expr, Box<Stmt>),
    Case(Expr, Box<Stmt>),
    Default(Box<Stmt>),
    Break,
    Continue,
    Return(Option<Expr>),
    Goto(Rc<str>),
    Label(Rc<str>, Box<Stmt>),
    Asm,
    Empty,
}
