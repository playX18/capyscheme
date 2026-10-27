use std::collections::BTreeSet;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Punct {
    LBracket,
    RBracket,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Dot,
    Arrow,
    Inc,
    Dec,
    Amp,
    Star,
    Plus,
    Minus,
    Tilde,
    Bang,
    Slash,
    Percent,
    Shl,
    Shr,
    Lt,
    Gt,
    Le,
    Ge,
    EqEq,
    Ne,
    Caret,
    Pipe,
    AmpAmp,
    PipePipe,
    Question,
    Colon,
    ColonColon,
    Semi,
    Ellipsis,
    Assign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    PlusAssign,
    MinusAssign,
    ShlAssign,
    ShrAssign,
    AmpAssign,
    CaretAssign,
    PipeAssign,
    Comma,
    Hash,
    HashHash,
}

impl Punct {
    pub fn spelling(self) -> &'static str {
        use Punct::*;
        match self {
            LBracket => "[",
            RBracket => "]",
            LParen => "(",
            RParen => ")",
            LBrace => "{",
            RBrace => "}",
            Dot => ".",
            Arrow => "->",
            Inc => "++",
            Dec => "--",
            Amp => "&",
            Star => "*",
            Plus => "+",
            Minus => "-",
            Tilde => "~",
            Bang => "!",
            Slash => "/",
            Percent => "%",
            Shl => "<<",
            Shr => ">>",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
            EqEq => "==",
            Ne => "!=",
            Caret => "^",
            Pipe => "|",
            AmpAmp => "&&",
            PipePipe => "||",
            Question => "?",
            Colon => ":",
            ColonColon => "::",
            Semi => ";",
            Ellipsis => "...",
            Assign => "=",
            StarAssign => "*=",
            SlashAssign => "/=",
            PercentAssign => "%=",
            PlusAssign => "+=",
            MinusAssign => "-=",
            ShlAssign => "<<=",
            ShrAssign => ">>=",
            AmpAssign => "&=",
            CaretAssign => "^=",
            PipeAssign => "|=",
            Comma => ",",
            Hash => "#",
            HashHash => "##",
        }
    }
}

/// Feature-test builtins usable in `#if`: `__has_attribute(x)` and friends.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Query {
    HasAttribute,
    HasCAttribute,
    HasCppAttribute,
    HasBuiltin,
    HasFeature,
    HasExtension,
    HasDeclspecAttribute,
    HasWarning,
    HasConstexprBuiltin,
    HasTargetFeature,
    IsTargetArch,
    IsTargetVendor,
    IsTargetOs,
    IsTargetEnvironment,
    IsTargetVariantOs,
    IsTargetVariantEnvironment,
    BuildingModule,
    IsIdentifier,
}

impl Query {
    /// The spelling the compiler knows this query by.
    pub fn name(self) -> &'static str {
        use Query::*;
        match self {
            HasAttribute => "__has_attribute",
            HasCAttribute => "__has_c_attribute",
            HasCppAttribute => "__has_cpp_attribute",
            HasBuiltin => "__has_builtin",
            HasFeature => "__has_feature",
            HasExtension => "__has_extension",
            HasDeclspecAttribute => "__has_declspec_attribute",
            HasWarning => "__has_warning",
            HasConstexprBuiltin => "__has_constexpr_builtin",
            HasTargetFeature => "__has_target_feature",
            IsTargetArch => "__is_target_arch",
            IsTargetVendor => "__is_target_vendor",
            IsTargetOs => "__is_target_os",
            IsTargetEnvironment => "__is_target_environment",
            IsTargetVariantOs => "__is_target_variant_os",
            IsTargetVariantEnvironment => "__is_target_variant_environment",
            BuildingModule => "__building_module",
            IsIdentifier => "__is_identifier",
        }
    }
}

/// Reserved words (with spelling variants folded together) and preprocessor-special names.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Word {
    // storage classes and function specifiers
    Typedef,
    Extern,
    Static,
    Register,
    Auto,
    ThreadLocal,
    Constexpr,
    Inline,
    Noreturn,
    // qualifiers
    Const,
    Volatile,
    Restrict,
    Atomic,
    /// Nullability annotations, calling conventions and ARC ownership: no effect on the ABI.
    IgnoredQualifier,
    Extension,
    // type specifiers
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    /// `bool` (a keyword only in C23, otherwise a library macro or typedef).
    BoolName,
    Complex,
    Imaginary,
    Int128,
    Int128T,
    UInt128T,
    Float16,
    BFloat16,
    Float32,
    Float64,
    Float128,
    VaList,
    BitInt,
    Struct,
    Union,
    Enum,
    Typeof,
    AutoType,
    Alignas,
    Alignof,
    StaticAssert,
    Sizeof,
    Generic,
    // statements
    If,
    Else,
    While,
    Do,
    For,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Return,
    Goto,
    Asm,
    // constants
    True,
    False,
    Nullptr,
    NullptrT,
    FuncName,
    // attributes
    Attribute,
    Declspec,
    // builtins with their own syntax
    BuiltinOffsetof,
    BuiltinVaArg,
    BuiltinTypesCompatible,
    // preprocessor
    VaArgs,
    VaOpt,
    Defined,
    HasInclude,
    HasIncludeNext,
    HasEmbed,
    PragmaOperator,
    Line,
    File,
    BaseFile,
    Counter,
    IncludeLevel,
    Date,
    Time,
    Query(Query),
    /// Internal marker the preprocessor emits for `#pragma pack`.
    CapyPragmaPack,
}

impl Word {
    pub fn lookup(s: &str) -> Option<Word> {
        use Word::*;
        Some(match s {
            "typedef" => Typedef,
            "extern" => Extern,
            "static" => Static,
            "register" => Register,
            "auto" => Auto,
            "_Thread_local" | "thread_local" | "__thread" => ThreadLocal,
            "constexpr" => Constexpr,
            "inline" | "__inline" | "__inline__" => Inline,
            "_Noreturn" => Noreturn,
            "const" | "__const" | "__const__" => Const,
            "volatile" | "__volatile" | "__volatile__" => Volatile,
            "restrict" | "__restrict" | "__restrict__" => Restrict,
            "_Atomic" => Atomic,
            "_Nonnull"
            | "_Nullable"
            | "_Null_unspecified"
            | "_Nullable_result"
            | "__nonnull"
            | "__nullable"
            | "__null_unspecified"
            | "__cdecl"
            | "__stdcall"
            | "__fastcall"
            | "__unaligned"
            | "__ptr32"
            | "__ptr64"
            | "__kindof"
            | "__unsafe_unretained"
            | "__strong"
            | "__weak"
            | "__autoreleasing" => IgnoredQualifier,
            "__extension__" => Extension,
            "void" => Void,
            "char" => Char,
            "short" => Short,
            "int" => Int,
            "long" => Long,
            "float" => Float,
            "double" => Double,
            "signed" | "__signed" | "__signed__" => Signed,
            "unsigned" => Unsigned,
            "_Bool" => Bool,
            "bool" => BoolName,
            "_Complex" | "__complex__" | "__complex" => Complex,
            "_Imaginary" => Imaginary,
            "__int128" => Int128,
            "__int128_t" => Int128T,
            "__uint128_t" => UInt128T,
            "_Float16" | "__fp16" => Float16,
            "__bf16" => BFloat16,
            "_Float32" => Float32,
            "_Float64" | "_Float32x" => Float64,
            "_Float128" | "__float128" | "_Float64x" => Float128,
            "__builtin_va_list" | "__builtin_ms_va_list" => VaList,
            "_BitInt" => BitInt,
            "struct" => Struct,
            "union" => Union,
            "enum" => Enum,
            "typeof" | "__typeof__" | "__typeof" | "typeof_unqual" | "__typeof_unqual__" => Typeof,
            "__auto_type" => AutoType,
            "alignas" | "_Alignas" => Alignas,
            "alignof" | "_Alignof" | "__alignof__" | "__alignof" => Alignof,
            "static_assert" | "_Static_assert" => StaticAssert,
            "sizeof" => Sizeof,
            "_Generic" => Generic,
            "if" => If,
            "else" => Else,
            "while" => While,
            "do" => Do,
            "for" => For,
            "switch" => Switch,
            "case" => Case,
            "default" => Default,
            "break" => Break,
            "continue" => Continue,
            "return" => Return,
            "goto" => Goto,
            "asm" | "__asm__" | "__asm" => Asm,
            "true" => True,
            "false" => False,
            "nullptr" => Nullptr,
            "nullptr_t" => NullptrT,
            "__func__" | "__FUNCTION__" | "__PRETTY_FUNCTION__" => FuncName,
            "__attribute__" | "__attribute" => Attribute,
            "__declspec" => Declspec,
            "__builtin_offsetof" => BuiltinOffsetof,
            "__builtin_va_arg" => BuiltinVaArg,
            "__builtin_types_compatible_p" => BuiltinTypesCompatible,
            "__VA_ARGS__" => VaArgs,
            "__VA_OPT__" => VaOpt,
            "defined" => Defined,
            "__has_include" => HasInclude,
            "__has_include_next" => HasIncludeNext,
            "__has_embed" => HasEmbed,
            "_Pragma" => PragmaOperator,
            "__LINE__" => Line,
            "__FILE__" => File,
            "__BASE_FILE__" => BaseFile,
            "__COUNTER__" => Counter,
            "__INCLUDE_LEVEL__" => IncludeLevel,
            "__DATE__" => Date,
            "__TIME__" => Time,
            "__has_attribute" => Word::Query(self::Query::HasAttribute),
            "__has_c_attribute" => Word::Query(self::Query::HasCAttribute),
            "__has_cpp_attribute" => Word::Query(self::Query::HasCppAttribute),
            "__has_builtin" => Word::Query(self::Query::HasBuiltin),
            "__has_feature" => Word::Query(self::Query::HasFeature),
            "__has_extension" => Word::Query(self::Query::HasExtension),
            "__has_declspec_attribute" => Word::Query(self::Query::HasDeclspecAttribute),
            "__has_warning" => Word::Query(self::Query::HasWarning),
            "__has_constexpr_builtin" => Word::Query(self::Query::HasConstexprBuiltin),
            "__has_target_feature" => Word::Query(self::Query::HasTargetFeature),
            "__is_target_arch" => Word::Query(self::Query::IsTargetArch),
            "__is_target_vendor" => Word::Query(self::Query::IsTargetVendor),
            "__is_target_os" => Word::Query(self::Query::IsTargetOs),
            "__is_target_environment" => Word::Query(self::Query::IsTargetEnvironment),
            "__is_target_variant_os" => Word::Query(self::Query::IsTargetVariantOs),
            "__is_target_variant_environment" => {
                Word::Query(self::Query::IsTargetVariantEnvironment)
            }
            "__building_module" => Word::Query(self::Query::BuildingModule),
            "__is_identifier" => Word::Query(self::Query::IsIdentifier),
            "__capy_pragma_pack" => CapyPragmaPack,
            _ => return None,
        })
    }

    /// Words that the preprocessor evaluates itself, so `defined(X)` must treat them as defined.
    pub fn is_pp_builtin(self) -> bool {
        use Word::*;
        matches!(
            self,
            HasInclude
                | HasIncludeNext
                | HasEmbed
                | Line
                | File
                | BaseFile
                | Counter
                | IncludeLevel
                | Date
                | Time
                | PragmaOperator
                | Query(_)
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kind {
    Ident,
    /// A pp-number; classified as integer/float later by the constant evaluator.
    Number,
    /// Character constant, including any `u8`/`u`/`U`/`L` prefix.
    Char,
    /// String literal, including any prefix.
    Str,
    Punct(Punct),
    /// A stray byte that is not part of any other token.
    Other,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Loc {
    pub file: u32,
    pub line: u32,
    pub col: u32,
}

/// Names of macros that must not be expanded again while rescanning this token.
pub type HideSet = Option<Rc<BTreeSet<Rc<str>>>>;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: Kind,
    pub text: Rc<str>,
    /// Set for identifiers that spell a reserved word or preprocessor-special name.
    pub word: Option<Word>,
    pub loc: Loc,
    /// First token on its (logical) source line.
    pub bol: bool,
    /// Whitespace or a comment precedes this token.
    pub space: bool,
    pub hide: HideSet,
}

impl Token {
    pub fn new(kind: Kind, text: impl Into<Rc<str>>) -> Token {
        let text: Rc<str> = text.into();
        let word = if kind == Kind::Ident {
            Word::lookup(&text)
        } else {
            None
        };
        Token {
            kind,
            text,
            word,
            loc: Loc::default(),
            bol: false,
            space: false,
            hide: None,
        }
    }

    pub fn punct_tok(p: Punct) -> Token {
        Token::new(Kind::Punct(p), p.spelling())
    }

    pub fn ident(text: impl Into<Rc<str>>) -> Token {
        Token::new(Kind::Ident, text)
    }

    pub fn punct(&self) -> Option<Punct> {
        match self.kind {
            Kind::Punct(p) => Some(p),
            _ => None,
        }
    }

    pub fn is_punct(&self, p: Punct) -> bool {
        self.kind == Kind::Punct(p)
    }

    pub fn is_word(&self, w: Word) -> bool {
        self.word == Some(w)
    }

    pub fn is_ident(&self) -> bool {
        self.kind == Kind::Ident
    }

    pub fn hidden(&self, name: &str) -> bool {
        self.hide.as_ref().is_some_and(|h| h.contains(name))
    }
}

pub fn hide_union(a: &HideSet, b: &HideSet) -> HideSet {
    match (a, b) {
        (None, x) | (x, None) => x.clone(),
        (Some(x), Some(y)) => {
            if Rc::ptr_eq(x, y) {
                return Some(x.clone());
            }
            Some(Rc::new(x.union(y).cloned().collect()))
        }
    }
}

pub fn hide_add(a: &HideSet, name: &Rc<str>) -> HideSet {
    let mut set = a.as_ref().map(|s| (**s).clone()).unwrap_or_default();
    set.insert(name.clone());
    Some(Rc::new(set))
}

pub fn hide_intersect(a: &HideSet, b: &HideSet) -> HideSet {
    match (a, b) {
        (Some(x), Some(y)) => {
            let s: BTreeSet<Rc<str>> = x.intersection(y).cloned().collect();
            if s.is_empty() { None } else { Some(Rc::new(s)) }
        }
        _ => None,
    }
}
