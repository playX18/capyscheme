use crate::ast::*;
use crate::consteval::ConstVal;
use crate::token::Loc;
use crate::types::*;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

macro_rules! lst {
    ($b:expr, $($x:expr),* $(,)?) => { $b.list(vec![$($x),*]) };
}

pub trait Emit {
    type V;
    fn int(&mut self, v: i128) -> Self::V;
    fn float(&mut self, v: f64) -> Self::V;
    fn str(&mut self, v: &str) -> Self::V;
    fn bytes(&mut self, v: &[u8]) -> Self::V;
    fn sym(&mut self, v: &'static str) -> Self::V;
    fn bool(&mut self, v: bool) -> Self::V;
    fn list(&mut self, items: Vec<Self::V>) -> Self::V;
    fn enter(&mut self) {}
    fn leave(&mut self, v: Self::V) -> Self::V {
        v
    }
}

pub struct Builder<E: Emit>(RefCell<E>);

impl<E: Emit> Builder<E> {
    pub fn new(e: E) -> Self {
        Builder(RefCell::new(e))
    }

    pub fn into_inner(self) -> E {
        self.0.into_inner()
    }

    pub fn int(&self, v: impl Into<i128>) -> E::V {
        self.0.borrow_mut().int(v.into())
    }

    pub fn float(&self, v: f64) -> E::V {
        self.0.borrow_mut().float(v)
    }

    pub fn str(&self, v: impl AsRef<str>) -> E::V {
        self.0.borrow_mut().str(v.as_ref())
    }

    pub fn bytes(&self, v: &[u8]) -> E::V {
        self.0.borrow_mut().bytes(v)
    }

    pub fn sym(&self, v: &'static str) -> E::V {
        self.0.borrow_mut().sym(v)
    }

    pub fn bool(&self, v: bool) -> E::V {
        self.0.borrow_mut().bool(v)
    }

    pub fn list(&self, items: Vec<E::V>) -> E::V {
        self.0.borrow_mut().list(items)
    }

    /// `(sym item ...)`
    pub fn tagged(&self, tag: &'static str, items: impl IntoIterator<Item = E::V>) -> E::V {
        let mut v = vec![self.sym(tag)];
        v.extend(items);
        self.list(v)
    }

    pub fn scoped(&self, f: impl FnOnce() -> E::V) -> E::V {
        self.0.borrow_mut().enter();
        let v = f();
        self.0.borrow_mut().leave(v)
    }

    pub fn opt_str(&self, v: &Option<impl AsRef<str>>) -> E::V {
        match v {
            Some(x) => self.str(x.as_ref()),
            None => self.bool(false),
        }
    }

    pub fn loc(&self, l: Loc) -> E::V {
        lst![self, self.sym("loc"), self.int(l.file), self.int(l.line)]
    }
}

/// Prints the tree as Scheme text, for tests and debugging.
#[derive(Default)]
pub struct TextEmit;

impl Emit for TextEmit {
    type V = String;

    fn int(&mut self, v: i128) -> String {
        v.to_string()
    }

    fn float(&mut self, x: f64) -> String {
        if x.is_nan() {
            "+nan.0".into()
        } else if x.is_infinite() {
            (if x > 0.0 { "+inf.0" } else { "-inf.0" }).into()
        } else {
            format!("{x:?}")
        }
    }

    fn str(&mut self, t: &str) -> String {
        let mut f = String::from('"');
        for c in t.chars() {
            match c {
                '"' => f.push_str("\\\""),
                '\\' => f.push_str("\\\\"),
                '\n' => f.push_str("\\n"),
                '\t' => f.push_str("\\t"),
                '\r' => f.push_str("\\r"),
                c if (c as u32) < 0x20 || c as u32 == 0x7f => {
                    let _ = write!(f, "\\x{:x};", c as u32);
                }
                c => f.push(c),
            }
        }
        f.push('"');
        f
    }

    fn bytes(&mut self, b: &[u8]) -> String {
        let items: Vec<String> = b.iter().map(|x| x.to_string()).collect();
        format!("#u8({})", items.join(" "))
    }

    fn sym(&mut self, v: &'static str) -> String {
        v.into()
    }

    fn bool(&mut self, b: bool) -> String {
        (if b { "#t" } else { "#f" }).into()
    }

    fn list(&mut self, items: Vec<String>) -> String {
        format!("({})", items.join(" "))
    }
}

pub struct Ser<'a, E: Emit> {
    pub u: &'a Unit,
    pub b: &'a Builder<E>,
}

impl<E: Emit> Deref for Ser<'_, E> {
    type Target = Builder<E>;
    fn deref(&self) -> &Builder<E> {
        self.b
    }
}

impl<E: Emit> Ser<'_, E> {
    pub fn int_ty(&self, k: IntKind) -> E::V {
        lst![
            self,
            self.sym("int"),
            self.int(self.u.abi.int_size(k) as i128),
            self.bool(self.u.abi.is_unsigned(k))
        ]
    }

    pub fn ty(&self, t: &Type) -> E::V {
        match t {
            Type::Void => lst![self, self.sym("void")],
            Type::Bool => lst![self, self.sym("bool")],
            Type::Int(k) => self.int_ty(*k),
            Type::BitInt(n, signed) => {
                lst![self, self.sym("bitint"), self.int(*n), self.bool(*signed)]
            }
            Type::Float(k) => lst![
                self,
                self.sym("float"),
                self.int(self.u.abi.float_size(*k) as i128)
            ],
            Type::Complex(k) => lst![
                self,
                self.sym("complex"),
                self.int(self.u.abi.float_size(*k) as i128)
            ],
            Type::Ptr(t) => lst![self, self.sym("ptr"), self.ty(t)],
            Type::Array(t, len) => {
                let n = match len {
                    ArrLen::Fixed(n) => self.int(*n),
                    _ => self.bool(false),
                };
                lst![self, self.sym("array"), self.ty(t), n]
            }
            Type::Func(f) => self.func_ty(f),
            Type::Record(id) => lst![self, self.sym("record"), self.int(*id as i128)],
            Type::Enum(id) => lst![self, self.sym("enum"), self.int(*id as i128)],
            Type::Typedef(n) => lst![self, self.sym("typedef"), self.str(n)],
            Type::Vector(t, bytes) => lst![self, self.sym("vector"), self.ty(t), self.int(*bytes)],
            Type::Atomic(t) => lst![self, self.sym("atomic"), self.ty(t)],
            Type::VaList => lst![self, self.sym("va-list")],
            Type::NullPtr => lst![self, self.sym("nullptr")],
            Type::Unsupported(why) => lst![self, self.sym("unsupported"), self.str(why)],
        }
    }

    fn func_ty(&self, f: &FuncType) -> E::V {
        lst![
            self,
            self.sym("func"),
            self.ty(&f.ret),
            self.list(f.params.iter().map(|p| self.ty(p)).collect()),
            self.bool(f.variadic),
            self.bool(f.prototyped)
        ]
    }

    fn record(&self, id: RecId) -> E::V {
        let r = &self.u.records[id];
        let kind = match r.kind {
            RecKind::Struct => self.sym("struct"),
            RecKind::Union => self.sym("union"),
        };
        let fields = r
            .fields
            .iter()
            .map(|f| {
                lst![
                    self,
                    self.opt_str(&f.name),
                    self.ty(&f.ty),
                    f.bits.map_or(self.bool(false), |b| self.int(b))
                ]
            })
            .collect();
        let layout = match &r.layout {
            Some(l) if r.complete && l.unsupported.is_none() => {
                let offs = l
                    .fields
                    .iter()
                    .map(|fl| match &fl.bit {
                        Some(b) => lst![
                            self,
                            self.int(fl.offset),
                            self.int(b.bit_offset),
                            self.int(b.width),
                            self.int(b.unit_size)
                        ],
                        None => self.int(fl.offset),
                    })
                    .collect();
                lst![
                    self,
                    self.sym("layout"),
                    self.int(l.size),
                    self.int(l.align),
                    self.list(offs)
                ]
            }
            _ => self.bool(false),
        };
        lst![
            self,
            self.sym("record"),
            self.int(id as i128),
            kind,
            self.opt_str(&r.tag),
            self.bool(r.complete),
            self.list(fields),
            layout,
            self.bool(r.packed),
            self.loc(r.loc)
        ]
    }

    fn enumeration(&self, id: EnumId) -> E::V {
        let e = &self.u.enums[id];
        let consts = e
            .consts
            .iter()
            .map(|(n, v)| lst![self, self.str(n), self.int(*v)])
            .collect();
        lst![
            self,
            self.sym("enum"),
            self.int(id as i128),
            self.opt_str(&e.tag),
            self.ty(&self.u.enum_underlying(id)),
            self.bool(e.complete),
            self.list(consts),
            self.loc(e.loc)
        ]
    }

    fn storage(&self, st: Storage) -> E::V {
        self.sym(match st {
            Storage::None => "none",
            Storage::Typedef => "typedef",
            Storage::Extern => "extern",
            Storage::Static => "static",
            Storage::Auto => "auto",
            Storage::Register => "register",
            Storage::ThreadLocal => "thread-local",
        })
    }

    fn attrs(&self, a: &Attrs) -> E::V {
        let mut v = Vec::new();
        for (on, n) in [
            (a.noreturn, "noreturn"),
            (a.deprecated, "deprecated"),
            (a.unavailable, "unavailable"),
            (a.weak_import, "weak-import"),
        ] {
            if on {
                v.push(self.sym(n));
            }
        }
        self.list(v)
    }

    pub fn const_val(&self, c: &ConstVal) -> E::V {
        match c {
            ConstVal::Int(v, _) => lst![self, self.sym("int"), self.int(*v)],
            ConstVal::Float(x, _) => lst![self, self.sym("float"), self.float(*x)],
            ConstVal::Str(b) => lst![self, self.sym("string"), self.bytes(b)],
            ConstVal::Ptr(v) => lst![self, self.sym("ptr"), self.int(*v)],
        }
    }

    fn decl(&self, d: &Decl) -> E::V {
        match d {
            Decl::Typedef {
                name: n,
                ty,
                loc: l,
            } => lst![
                self,
                self.sym("typedef"),
                self.str(n),
                self.ty(ty),
                self.loc(*l)
            ],
            Decl::Record(id) => lst![self, self.sym("record-ref"), self.int(*id as i128)],
            Decl::Enum(id) => lst![self, self.sym("enum-ref"), self.int(*id as i128)],
            Decl::Func(f) => {
                let mut v = vec![
                    self.sym("function"),
                    self.str(&f.name),
                    self.ty(&f.ret),
                    self.list(f.params.iter().map(|p| self.ty(&p.ty)).collect()),
                    self.list(f.params.iter().map(|p| self.opt_str(&p.name)).collect()),
                    self.bool(f.variadic),
                    self.storage(f.storage),
                    self.bool(f.inline),
                    self.opt_str(&f.asm_label),
                    self.attrs(&f.attrs),
                    self.loc(f.loc),
                ];
                if let Some(b) = &f.body {
                    v.push(lst![
                        self,
                        self.sym("body"),
                        self.list(b.iter().map(|s| self.stmt(s)).collect())
                    ]);
                }
                self.list(v)
            }
            Decl::Var(x) => lst![
                self,
                self.sym("var"),
                self.str(&x.name),
                self.ty(&x.ty),
                self.storage(x.storage),
                self.opt_str(&x.asm_label),
                self.bool(x.is_const),
                x.init
                    .as_ref()
                    .map_or(self.bool(false), |c| self.const_val(c)),
                self.attrs(&x.attrs),
                self.loc(x.loc)
            ],
        }
    }

    pub fn expr(&self, e: &Expr) -> E::V {
        let b = |e: &Expr| self.expr(e);
        match e {
            Expr::Int(v, _) => lst![self, self.sym("int"), self.int(*v)],
            Expr::Float(x, _) => lst![self, self.sym("float"), self.float(*x)],
            Expr::Str(bytes) => lst![self, self.sym("str"), self.bytes(bytes)],
            Expr::Char(v, _) => lst![self, self.sym("char"), self.int(*v)],
            Expr::Ident(n) => lst![self, self.sym("id"), self.str(n)],
            Expr::Call(f, args) => {
                let mut v = vec![self.sym("call"), b(f)];
                v.extend(args.iter().map(b));
                self.list(v)
            }
            Expr::Binary(op, l, r) => lst![self, self.sym("bin"), self.sym(op.sym()), b(l), b(r)],
            Expr::Unary(op, x) => lst![self, self.sym("un"), self.sym(op.sym()), b(x)],
            Expr::PostInc(op, x) => lst![
                self,
                self.sym("postinc"),
                self.sym(if *op == IncDec::Inc { "inc" } else { "dec" }),
                b(x)
            ],
            Expr::Assign(op, l, r) => lst![
                self,
                self.sym("assign"),
                op.map_or(self.bool(false), |o| self.sym(o.sym())),
                b(l),
                b(r)
            ],
            Expr::Cond(c, t, f) => lst![self, self.sym("cond"), b(c), b(t), b(f)],
            Expr::Cast(t, x) => lst![self, self.sym("cast"), self.ty(t), b(x)],
            Expr::SizeofType(t) => lst![self, self.sym("sizeof-type"), self.ty(t)],
            Expr::SizeofExpr(x) => lst![self, self.sym("sizeof"), b(x)],
            Expr::AlignofType(t) => lst![self, self.sym("alignof"), self.ty(t)],
            Expr::Member(x, n) => lst![self, self.sym("member"), b(x), self.str(n)],
            Expr::Arrow(x, n) => lst![self, self.sym("arrow"), b(x), self.str(n)],
            Expr::Index(x, i) => lst![self, self.sym("index"), b(x), b(i)],
            Expr::Comma(l, r) => lst![self, self.sym("comma"), b(l), b(r)],
            Expr::StmtExpr(stmts) => {
                let mut v = vec![self.sym("stmt-expr")];
                v.extend(stmts.iter().map(|s| self.stmt(s)));
                self.list(v)
            }
            Expr::CompoundLit(t, items) => {
                lst![self, self.sym("compound"), self.ty(t), self.inits(items)]
            }
            Expr::Generic(c, arms) => {
                let mut v = vec![self.sym("generic"), b(c)];
                for (t, x) in arms {
                    v.push(lst![
                        self,
                        t.as_ref().map_or(self.bool(false), |t| self.ty(t)),
                        b(x)
                    ]);
                }
                self.list(v)
            }
            Expr::OffsetOf(t, steps) => {
                let steps = steps
                    .iter()
                    .map(|st| match st {
                        OffsetStep::Field(n) => lst![self, self.sym("field"), self.str(n)],
                        OffsetStep::Index(i) => lst![self, self.sym("index"), b(i)],
                    })
                    .collect();
                lst![self, self.sym("offsetof"), self.ty(t), self.list(steps)]
            }
            Expr::Bool(v) => lst![self, self.sym("bool"), self.bool(*v)],
            Expr::Nullptr => lst![self, self.sym("nullptr")],
            Expr::Typed(t, x) => lst![self, self.sym("typed"), self.ty(t), b(x)],
            Expr::Unsupported(why) => lst![self, self.sym("unsupported"), self.str(why)],
        }
    }

    fn desig(&self, d: &Designator) -> E::V {
        match d {
            Designator::Field(n) => lst![self, self.sym("field"), self.str(n)],
            Designator::Index(i) => lst![self, self.sym("index"), self.expr(i)],
        }
    }

    fn init(&self, i: &Init) -> E::V {
        match i {
            Init::Expr(e) => self.expr(e),
            Init::List(items) => lst![self, self.sym("list"), self.inits(items)],
        }
    }

    fn inits(&self, items: &[(Vec<Designator>, Init)]) -> E::V {
        self.list(
            items
                .iter()
                .map(|(d, i)| {
                    lst![
                        self,
                        self.list(d.iter().map(|d| self.desig(d)).collect()),
                        self.init(i)
                    ]
                })
                .collect(),
        )
    }

    pub fn stmt(&self, st: &Stmt) -> E::V {
        let b = |s: &Stmt| self.stmt(s);
        let e = |x: &Expr| self.expr(x);
        let oe = |x: &Option<Expr>| x.as_ref().map_or(self.bool(false), |x| self.expr(x));
        match st {
            Stmt::Block(v) => {
                let mut l = vec![self.sym("block")];
                l.extend(v.iter().map(b));
                self.list(l)
            }
            Stmt::Decl(ds) => {
                let mut l = vec![self.sym("decl")];
                for d in ds {
                    l.push(lst![
                        self,
                        self.str(&d.name),
                        self.ty(&d.ty),
                        d.init.as_ref().map_or(self.bool(false), |i| self.init(i)),
                        self.bool(d.is_static)
                    ]);
                }
                self.list(l)
            }
            Stmt::Expr(x) => lst![self, self.sym("expr"), e(x)],
            Stmt::If(c, t, f) => lst![
                self,
                self.sym("if"),
                e(c),
                b(t),
                f.as_ref().map_or(self.bool(false), |f| b(f))
            ],
            Stmt::While(c, body) => lst![self, self.sym("while"), e(c), b(body)],
            Stmt::DoWhile(body, c) => lst![self, self.sym("do-while"), b(body), e(c)],
            Stmt::For(i, c, step, body) => lst![
                self,
                self.sym("for"),
                i.as_ref().map_or(self.bool(false), |i| b(i)),
                oe(c),
                oe(step),
                b(body)
            ],
            Stmt::Switch(c, body) => lst![self, self.sym("switch"), e(c), b(body)],
            Stmt::Case(v, body) => lst![self, self.sym("case"), e(v), b(body)],
            Stmt::Default(body) => lst![self, self.sym("default"), b(body)],
            Stmt::Break => lst![self, self.sym("break")],
            Stmt::Continue => lst![self, self.sym("continue")],
            Stmt::Return(x) => lst![self, self.sym("return"), oe(x)],
            Stmt::Goto(l) => lst![self, self.sym("goto"), self.str(l)],
            Stmt::Label(l, body) => lst![self, self.sym("label"), self.str(l), b(body)],
            Stmt::Asm => lst![self, self.sym("asm")],
            Stmt::Empty => lst![self, self.sym("empty")],
        }
    }

    /// The `records`, `enums` and `decls` sections.
    pub fn declarations(&self) -> Vec<E::V> {
        let u = self.u;
        vec![
            self.list(
                std::iter::once(self.sym("records"))
                    .chain((0..u.records.len()).map(|i| self.scoped(|| self.record(i))))
                    .collect(),
            ),
            self.list(
                std::iter::once(self.sym("enums"))
                    .chain((0..u.enums.len()).map(|i| self.scoped(|| self.enumeration(i))))
                    .collect(),
            ),
            self.list(
                std::iter::once(self.sym("decls"))
                    .chain(u.decls.iter().map(|d| self.scoped(|| self.decl(d))))
                    .collect(),
            ),
        ]
    }
}

// Output format.
//
// Function-like macros go in the `macros` section, in place of a `macro` entry:
//
//   (fmacro NAME (PARAM ...) BODY LOC)
//       The body is a C expression. Macro parameters become `(param ..)`. Every other
//       identifier becomes `(id ..)`.
// Nodes:
//         (lit N)                   integer, character and floating literal (N is a number)
//         (str "s")                 string literal
//         (param NAME)  (id NAME)   macro parameter / other identifier
//         (op SYM ARG)              unary, SYM is + neg ~ !
//         (op SYM A B)              binary, SYM is + - * / % << >> & bor ^ && lor == != < <= > >=
//         (if C A B)                conditional `C ? A : B`
//         (cast TYPE E)             TYPE is the same type sexp used in `decls`
//         (sizeof TYPE)             sizeof(TYPE)
//         (call NAME ARG ...)       call of a named function
//       `bor` and `lor` stand for `|` and `||`
//       LOC is `(loc FILE LINE)`.
//
//   (fmacro-skipped NAME REASON)
//       The macro cannot be translated: variadic, uses `#` or `##`, is empty, is not an
//       expression (statements, declarations) or uses an unsupported construct.
//
// Static inline functions produce one C source text and a list of the functions in it:
//
//   (shim SOURCE (NAME ...))
//       SOURCE starts with `#include` of the unit's header, using the same `<>` or `""`
//       form as the request. For each NAME it defines
//           RET capy_shim_NAME(TYPE __capy_a0, ...) { return (NAME)(__capy_a0, ...); }
//       (no `return` for void).
//
//   (shim-skipped NAME REASON)
//       The function has no shim: variadic, or a parameter or return type that cannot be
//       spelled in C (an anonymous struct, union or enum, or an unsupported type).
//

fn fm_binop(op: BinOp) -> &'static str {
    use BinOp::*;
    match op {
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Rem => "%",
        Shl => "<<",
        Shr => ">>",
        BitAnd => "&",
        BitOr => "bor",
        BitXor => "^",
        LAnd => "&&",
        LOr => "lor",
        Eq => "==",
        Ne => "!=",
        Lt => "<",
        Le => "<=",
        Gt => ">",
        Ge => ">=",
    }
}

fn strip_typed(e: &Expr) -> &Expr {
    match e {
        Expr::Typed(_, x) => strip_typed(x),
        e => e,
    }
}

fn decay(t: &Type) -> Type {
    match t {
        Type::Array(x, _) => Type::Ptr(x.clone()),
        Type::Func(_) => Type::Ptr(Box::new(t.clone())),
        _ => t.clone(),
    }
}

fn c_int_name(k: IntKind) -> &'static str {
    use IntKind::*;
    match k {
        Char => "char",
        SChar => "signed char",
        UChar => "unsigned char",
        Short => "short",
        UShort => "unsigned short",
        Int => "int",
        UInt => "unsigned int",
        Long => "long",
        ULong => "unsigned long",
        LongLong => "long long",
        ULongLong => "unsigned long long",
        Int128 => "__int128",
        UInt128 => "unsigned __int128",
    }
}

fn c_float_name(k: FloatKind) -> Option<&'static str> {
    Some(match k {
        FloatKind::Half => "_Float16",
        FloatKind::BFloat16 => "__bf16",
        FloatKind::Float => "float",
        FloatKind::Double => "double",
        FloatKind::LongDouble => "long double",
        FloatKind::Float128 => "_Float128",
    })
}

impl<E: Emit> Ser<'_, E> {
    pub fn fmacro_expr(&self, e: &Expr, params: &[Rc<str>]) -> Result<E::V, String> {
        let r = |x: &Expr| self.fmacro_expr(x, params);
        Ok(match e {
            Expr::Typed(..) => r(strip_typed(e))?,
            Expr::Int(v, _) | Expr::Char(v, _) => lst![self, self.sym("lit"), self.int(*v)],
            Expr::Float(x, _) => lst![self, self.sym("lit"), self.float(*x)],
            Expr::Bool(b) => lst![self, self.sym("lit"), self.int(*b as i128)],
            Expr::Str(bytes) => {
                let text = String::from_utf8(bytes.clone())
                    .map_err(|_| "string literal is not UTF-8".to_string())?;
                lst![self, self.sym("str"), self.str(text)]
            }
            Expr::Ident(n) => lst![
                self,
                self.sym(if params.contains(n) { "param" } else { "id" }),
                self.str(n)
            ],
            Expr::Unary(op, x) => {
                let o = match op {
                    UnOp::Plus => "+",
                    UnOp::Neg => "neg",
                    UnOp::BitNot => "~",
                    UnOp::Not => "!",
                    _ => {
                        return Err(format!(
                            "unary '{}' is not allowed in a macro body",
                            op.name()
                        ));
                    }
                };
                lst![self, self.sym("op"), self.sym(o), r(x)?]
            }
            Expr::Binary(op, l, rhs) => lst![
                self,
                self.sym("op"),
                self.sym(fm_binop(*op)),
                r(l)?,
                r(rhs)?
            ],
            Expr::Cond(c, a, b) => lst![self, self.sym("if"), r(c)?, r(a)?, r(b)?],
            Expr::Cast(t, x) => lst![self, self.sym("cast"), self.ty(t), r(x)?],
            Expr::SizeofType(t) => lst![self, self.sym("sizeof"), self.ty(t)],
            Expr::Call(f, args) => {
                let Expr::Ident(n) = strip_typed(f) else {
                    return Err("call through an expression that is not a name".into());
                };
                if params.contains(n) {
                    return Err(format!("call of macro parameter '{n}'"));
                }
                let mut v = vec![self.sym("call"), self.str(n)];
                for a in args {
                    v.push(r(a)?);
                }
                self.list(v)
            }
            Expr::Assign(..) => return Err("assignment is not allowed in a macro body".into()),
            Expr::PostInc(..) => return Err("increment is not allowed in a macro body".into()),
            Expr::Member(..) | Expr::Arrow(..) | Expr::Index(..) => {
                return Err("member access or subscript is not allowed".into());
            }
            Expr::Comma(..) => return Err("comma operator is not allowed".into()),
            Expr::StmtExpr(_) => return Err("statement expression is not allowed".into()),
            Expr::CompoundLit(..) => return Err("compound literal is not allowed".into()),
            Expr::Generic(..) => return Err("_Generic is not allowed in a macro body".into()),
            Expr::OffsetOf(..) => return Err("offsetof is not allowed in a macro body".into()),
            Expr::SizeofExpr(_) => return Err("sizeof of an expression is not translated".into()),
            Expr::AlignofType(_) => return Err("_Alignof is not translated".into()),
            Expr::Nullptr => return Err("nullptr is not translated".into()),
            Expr::Unsupported(why) => return Err(why.to_string()),
        })
    }

    pub fn shim_section(&self, header: &str, angle: bool) -> Vec<E::V> {
        let (open, close) = if angle { ('<', '>') } else { ('"', '"') };
        let mut src = format!("#include {open}{header}{close}\n");
        let mut names = Vec::new();
        let mut skipped = Vec::new();
        let mut seen = HashSet::new();
        for d in &self.u.decls {
            let Decl::Func(f) = d else { continue };
            if !f.inline
                || f.storage == Storage::Extern
                || !f.has_body
                || !seen.insert(f.name.clone())
            {
                continue;
            }
            match self.shim_fn(f) {
                Ok(text) => {
                    src.push_str(&text);
                    src.push('\n');
                    names.push(self.str(&*f.name));
                }
                Err(why) => skipped.push(lst![
                    self,
                    self.sym("shim-skipped"),
                    self.str(&*f.name),
                    self.str(why)
                ]),
            }
        }
        let mut out = vec![lst![
            self,
            self.sym("shim"),
            self.str(src),
            self.list(names)
        ]];
        out.extend(skipped);
        out
    }

    /// The wrapper definition for one function.
    fn shim_fn(&self, f: &FuncDecl) -> Result<String, String> {
        if f.variadic {
            return Err("variadic".into());
        }
        let no_spelling = || {
            "a parameter or the return type has no C spelling (anonymous struct or enum, or unsupported type)".to_string()
        };
        let mut ps = Vec::new();
        for (i, p) in f.params.iter().enumerate() {
            ps.push(
                self.c_decl(&decay(&p.ty), &format!("__capy_a{i}"))
                    .ok_or_else(no_spelling)?,
            );
        }
        let params = if ps.is_empty() {
            "void".to_string()
        } else {
            ps.join(", ")
        };
        let head = self
            .c_decl(&f.ret, &format!("capy_shim_{}({params})", f.name))
            .ok_or_else(no_spelling)?;
        let args: Vec<String> = (0..f.params.len())
            .map(|i| format!("__capy_a{i}"))
            .collect();
        let call = format!("({})({})", f.name, args.join(", "));
        Ok(if matches!(f.ret, Type::Void) {
            format!("{head} {{ {call}; }}")
        } else {
            format!("{head} {{ return {call}; }}")
        })
    }

    fn c_decl(&self, t: &Type, inner: &str) -> Option<String> {
        let join = |base: &str| {
            if inner.is_empty() {
                base.to_string()
            } else {
                format!("{base} {inner}")
            }
        };
        Some(match t {
            Type::Void => join("void"),
            Type::Bool => join("_Bool"),
            Type::Int(k) => join(c_int_name(*k)),
            Type::BitInt(n, signed) => join(&format!(
                "{}_BitInt({n})",
                if *signed { "" } else { "unsigned " }
            )),
            Type::Float(k) => join(c_float_name(*k)?),
            Type::Complex(k) => {
                let base = match k {
                    FloatKind::Float | FloatKind::Double | FloatKind::LongDouble => {
                        c_float_name(*k)?
                    }
                    _ => return None,
                };
                join(&format!("{base} _Complex"))
            }
            Type::Ptr(x) => {
                let p = format!("*{inner}");
                let p = if matches!(**x, Type::Array(..) | Type::Func(_)) {
                    format!("({p})")
                } else {
                    p
                };
                return self.c_decl(x, &p);
            }
            Type::Array(x, len) => {
                let n = match len {
                    ArrLen::Fixed(n) => n.to_string(),
                    ArrLen::Incomplete => String::new(),
                    ArrLen::Variable => return None,
                };
                return self.c_decl(x, &format!("{inner}[{n}]"));
            }
            Type::Func(f) => {
                let mut ps = Vec::new();
                for p in &f.params {
                    ps.push(self.c_decl(&decay(p), "")?);
                }
                if f.variadic {
                    ps.push("...".into());
                }
                if ps.is_empty() && f.prototyped {
                    ps.push("void".into());
                }
                return self.c_decl(&f.ret, &format!("{inner}({})", ps.join(", ")));
            }
            Type::Record(id) => {
                let r = &self.u.records[*id];
                let kw = match r.kind {
                    RecKind::Struct => "struct",
                    RecKind::Union => "union",
                };
                join(&format!("{kw} {}", r.tag.as_deref()?))
            }
            Type::Enum(id) => join(&format!("enum {}", self.u.enums[*id].tag.as_deref()?)),
            Type::Typedef(n) => join(&**n),
            Type::VaList => join("__builtin_va_list"),
            Type::Vector(..) | Type::Atomic(_) | Type::NullPtr | Type::Unsupported(_) => {
                return None;
            }
        })
    }
}
