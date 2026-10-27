use crate::probe::Target;
use crate::token::Loc;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum IntKind {
    Char,
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Int128,
    UInt128,
}

impl IntKind {
    pub fn is_unsigned_fixed(self) -> Option<bool> {
        Some(match self {
            IntKind::Char => return None,
            IntKind::SChar
            | IntKind::Short
            | IntKind::Int
            | IntKind::Long
            | IntKind::LongLong
            | IntKind::Int128 => false,
            _ => true,
        })
    }

    pub fn to_unsigned(self) -> IntKind {
        match self {
            IntKind::Char | IntKind::SChar => IntKind::UChar,
            IntKind::Short => IntKind::UShort,
            IntKind::Int => IntKind::UInt,
            IntKind::Long => IntKind::ULong,
            IntKind::LongLong => IntKind::ULongLong,
            IntKind::Int128 => IntKind::UInt128,
            k => k,
        }
    }

    pub fn to_signed(self) -> IntKind {
        match self {
            IntKind::UChar => IntKind::SChar,
            IntKind::UShort => IntKind::Short,
            IntKind::UInt => IntKind::Int,
            IntKind::ULong => IntKind::Long,
            IntKind::ULongLong => IntKind::LongLong,
            IntKind::UInt128 => IntKind::Int128,
            k => k,
        }
    }

    pub fn rank(self) -> u8 {
        match self {
            IntKind::Char | IntKind::SChar | IntKind::UChar => 1,
            IntKind::Short | IntKind::UShort => 2,
            IntKind::Int | IntKind::UInt => 3,
            IntKind::Long | IntKind::ULong => 4,
            IntKind::LongLong | IntKind::ULongLong => 5,
            IntKind::Int128 | IntKind::UInt128 => 6,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum FloatKind {
    Half,
    BFloat16,
    Float,
    Double,
    LongDouble,
    Float128,
}

pub type RecId = usize;
pub type EnumId = usize;

#[derive(Clone, PartialEq, Debug)]
pub enum ArrLen {
    Fixed(u64),
    Incomplete,
    /// Variable-length or otherwise non-constant.
    Variable,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FuncType {
    pub ret: Type,
    pub params: Vec<Type>,
    pub variadic: bool,
    pub prototyped: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Void,
    Bool,
    Int(IntKind),
    BitInt(u32, bool),
    Float(FloatKind),
    Complex(FloatKind),
    Ptr(Box<Type>),
    Array(Box<Type>, ArrLen),
    Func(Rc<FuncType>),
    Record(RecId),
    Enum(EnumId),
    Typedef(Rc<str>),
    Vector(Box<Type>, u64),
    Atomic(Box<Type>),
    VaList,
    NullPtr,
    Unsupported(Rc<str>),
}

impl Type {
    pub fn ptr(self) -> Type {
        Type::Ptr(Box::new(self))
    }
    pub fn is_unsupported(&self) -> bool {
        matches!(self, Type::Unsupported(_))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RecKind {
    Struct,
    Union,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: Option<Rc<str>>,
    pub ty: Type,
    pub bits: Option<u32>,
    pub align_attr: Option<u64>,
    pub packed: bool,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct FieldLayout {
    /// Byte offset of the field (or of its storage unit for bit-fields).
    pub offset: u64,
    /// For bit-fields: bit offset from the start of the record, width, and unit size in bytes.
    pub bit: Option<BitLayout>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct BitLayout {
    pub bit_offset: u64,
    pub width: u32,
    pub unit_size: u64,
}

#[derive(Clone, Debug, Default)]
pub struct Layout {
    pub size: u64,
    pub align: u64,
    pub fields: Vec<FieldLayout>,
    /// The layout could not be computed faithfully (for example an unsupported member).
    pub unsupported: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Record {
    pub kind: RecKind,
    pub tag: Option<Rc<str>>,
    pub fields: Vec<Field>,
    pub complete: bool,
    pub packed: bool,
    pub align_attr: Option<u64>,
    pub pack: Option<u64>,
    pub layout: Option<Layout>,
    pub loc: Loc,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub tag: Option<Rc<str>>,
    pub fixed: Option<Type>,
    pub consts: Vec<(Rc<str>, i128)>,
    pub complete: bool,
    pub loc: Loc,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Storage {
    None,
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
    ThreadLocal,
}

#[derive(Clone, Debug, Default)]
pub struct Attrs {
    pub packed: bool,
    pub aligned: Option<u64>,
    pub mode: Option<String>,
    pub vector_size: Option<u64>,
    pub ext_vector: Option<u64>,
    pub noreturn: bool,
    pub deprecated: bool,
    pub unavailable: bool,
    pub weak_import: bool,
    pub visibility: Option<String>,
    pub cleanup: bool,
}

impl Attrs {
    pub fn merge(&mut self, o: Attrs) {
        self.packed |= o.packed;
        self.aligned = self.aligned.max(o.aligned);
        self.mode = o.mode.or(self.mode.take());
        self.vector_size = o.vector_size.or(self.vector_size);
        self.ext_vector = o.ext_vector.or(self.ext_vector);
        self.noreturn |= o.noreturn;
        self.deprecated |= o.deprecated;
        self.unavailable |= o.unavailable;
        self.weak_import |= o.weak_import;
        self.visibility = o.visibility.or(self.visibility.take());
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: Option<Rc<str>>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Decl {
    Typedef { name: Rc<str>, ty: Type, loc: Loc },
    Record(RecId),
    Enum(EnumId),
    Func(FuncDecl),
    Var(VarDecl),
}

#[derive(Clone, Debug)]
pub struct FuncDecl {
    pub name: Rc<str>,
    pub ret: Type,
    pub params: Vec<Param>,
    pub variadic: bool,
    pub storage: Storage,
    pub inline: bool,
    pub asm_label: Option<String>,
    pub attrs: Attrs,
    pub body: Option<Vec<crate::ast::Stmt>>,
    pub has_body: bool,
    pub loc: Loc,
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub name: Rc<str>,
    pub ty: Type,
    pub storage: Storage,
    pub asm_label: Option<String>,
    pub init: Option<crate::consteval::ConstVal>,
    pub is_const: bool,
    pub attrs: Attrs,
    pub loc: Loc,
}

#[derive(Clone, Debug)]
pub enum Ident {
    Typedef(Type),
    Var(Type),
    EnumConst(i128, EnumId),
    Func(Type),
}

#[derive(Clone, Debug)]
pub struct Abi {
    pub char_signed: bool,
    pub short: u64,
    pub int: u64,
    pub long: u64,
    pub longlong: u64,
    pub ptr: u64,
    pub long_double: u64,
    pub long_double_align: u64,
    pub float128_ok: bool,
    pub wchar: u64,
    pub wchar_signed: bool,
    pub weak_wide_align: bool,
    pub os: String,
    pub arch: String,
}

impl Abi {
    pub fn from_target(t: &Target) -> Abi {
        let sz = |m: &str, d: u64| {
            t.macro_value(m)
                .and_then(|v| v.trim().parse().ok())
                .unwrap_or(d)
        };
        let ptr = sz("__SIZEOF_POINTER__", 8);
        let long_double = sz("__SIZEOF_LONG_DOUBLE__", 16);
        let wchar_signed = t.macro_value("__WCHAR_UNSIGNED__").is_none();
        Abi {
            char_signed: t.macro_value("__CHAR_UNSIGNED__").is_none(),
            short: sz("__SIZEOF_SHORT__", 2),
            int: sz("__SIZEOF_INT__", 4),
            long: sz("__SIZEOF_LONG__", 8),
            longlong: sz("__SIZEOF_LONG_LONG__", 8),
            ptr,
            long_double,
            long_double_align: long_double.min(16),
            float128_ok: t.macro_value("__SIZEOF_FLOAT128__").is_some() || t.arch == "x86_64",
            wchar: sz("__SIZEOF_WCHAR_T__", 4),
            wchar_signed,
            weak_wide_align: t.arch == "x86" || t.arch == "i386" || t.arch == "i686",
            os: t.os.clone(),
            arch: t.arch.clone(),
        }
    }

    pub fn host_default() -> Abi {
        Abi {
            char_signed: true,
            short: 2,
            int: 4,
            long: 8,
            longlong: 8,
            ptr: 8,
            long_double: 16,
            long_double_align: 16,
            float128_ok: true,
            wchar: 4,
            wchar_signed: true,
            weak_wide_align: false,
            os: "unknown".into(),
            arch: "x86_64".into(),
        }
    }

    pub fn int_size(&self, k: IntKind) -> u64 {
        match k {
            IntKind::Char | IntKind::SChar | IntKind::UChar => 1,
            IntKind::Short | IntKind::UShort => self.short,
            IntKind::Int | IntKind::UInt => self.int,
            IntKind::Long | IntKind::ULong => self.long,
            IntKind::LongLong | IntKind::ULongLong => self.longlong,
            IntKind::Int128 | IntKind::UInt128 => 16,
        }
    }

    pub fn is_unsigned(&self, k: IntKind) -> bool {
        k.is_unsigned_fixed().unwrap_or(!self.char_signed)
    }

    pub fn float_size(&self, k: FloatKind) -> u64 {
        match k {
            FloatKind::Half | FloatKind::BFloat16 => 2,
            FloatKind::Float => 4,
            FloatKind::Double => 8,
            FloatKind::LongDouble => self.long_double,
            FloatKind::Float128 => 16,
        }
    }

    pub fn float_align(&self, k: FloatKind) -> u64 {
        match k {
            FloatKind::LongDouble => self.long_double_align,
            FloatKind::Double if self.weak_wide_align => 4,
            k => self.float_size(k),
        }
    }
}

pub struct Unit {
    pub abi: Abi,
    pub records: Vec<Record>,
    pub enums: Vec<EnumDef>,
    pub typedefs: HashMap<Rc<str>, Type>,
    pub decls: Vec<Decl>,
    pub scopes: Vec<HashMap<Rc<str>, Ident>>,
    /// Tag namespaces, per scope depth.
    pub tags: Vec<HashMap<Rc<str>, TagRef>>,
    pub files: Vec<String>,
    pub diags: Vec<(crate::pp::Severity, String, Loc)>,
}

#[derive(Clone, Copy, Debug)]
pub enum TagRef {
    Rec(RecId),
    Enum(EnumId),
}

impl Unit {
    pub fn new(abi: Abi) -> Unit {
        Unit {
            abi,
            records: Vec::new(),
            enums: Vec::new(),
            typedefs: HashMap::new(),
            decls: Vec::new(),
            scopes: vec![HashMap::new()],
            tags: vec![HashMap::new()],
            files: Vec::new(),
            diags: Vec::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Ident> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }

    pub fn lookup_tag(&self, name: &str) -> Option<TagRef> {
        self.tags.iter().rev().find_map(|s| s.get(name).copied())
    }

    pub fn is_typedef_name(&self, name: &str) -> bool {
        matches!(self.lookup(name), Some(Ident::Typedef(_)))
    }

    pub fn resolve(&self, t: &Type) -> Type {
        let mut t = t.clone();
        for _ in 0..64 {
            match &t {
                Type::Typedef(n) => match self.typedefs.get(n) {
                    Some(x) => t = x.clone(),
                    None => return Type::Unsupported(format!("unknown typedef {n}").into()),
                },
                Type::Atomic(x) => t = (**x).clone(),
                _ => return t,
            }
        }
        Type::Unsupported("typedef cycle".into())
    }

    pub fn file_name(&self, id: u32) -> &str {
        self.files.get(id as usize).map_or("?", |s| s.as_str())
    }

    pub fn diag(&mut self, sev: crate::pp::Severity, loc: Loc, msg: impl Into<String>) {
        self.diags.push((sev, msg.into(), loc));
    }
}
