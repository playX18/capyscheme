use crate::expander::synclo::Environment;
use crate::expander::synclo::EnvironmentRef;
use crate::expander::synclo::expand;
use crate::expander::synclo::get_current_toplevel_environment;
use crate::expander::synclo::syntax_caddr;
use crate::expander::synclo::syntax_cadr;
use crate::expander::synclo::syntax_car;
use crate::expander::synclo::syntax_cddr;
use crate::expander::synclo::syntax_cdr;
use crate::expander::synclo::syntax_kind;
use crate::expander::synclo::syntax_length;
use crate::expander::synclo::unwrap_syntax;
use crate::fluid;
use crate::runtime::Context;
use crate::runtime::modules::mangle_library_spec;
use crate::runtime::value::*;
use crate::static_symbols;
use core::fmt;
use rsgc::alloc::array::Array;
use rsgc::alloc::array::ArrayRef;
use rsgc::{Gc, Global, Rootable, Trace};
use std::path::PathBuf;
use std::sync::OnceLock;

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Library<'gc> {
    pub header: ScmHeader,
    pub name: Value<'gc>,
    pub environment: EnvironmentRef<'gc>,
    pub imports: ArrayRef<'gc, Import<'gc>>,
    pub exports: Gc<'gc, HashTable<'gc>>,
    pub body: Value<'gc>,
}

unsafe impl<'gc> Tagged for Library<'gc> {
    const TC8: TypeCode8 = TypeCode8::MODULE;
}

pub type LibraryRef<'gc> = Gc<'gc, Library<'gc>>;

static LIBRARIES: OnceLock<Global<Rootable!(Gc<'_, HashTable<'_>>)>> = OnceLock::new();

pub fn libraries<'gc>(ctx: Context<'gc>) -> Gc<'gc, HashTable<'gc>> {
    *LIBRARIES
        .get_or_init(|| {
            let table = HashTable::new(&ctx, HashTableType::Eq, 32, 0.75);

            Global::new(table)
        })
        .fetch(&ctx)
}

pub fn find_library<'gc>(ctx: Context<'gc>, spec: Value<'gc>) -> Option<LibraryRef<'gc>> {
    let libraries = libraries(ctx);

    libraries.get(ctx, spec).map(|lib| lib.downcast())
}

fluid!(
    /// The current library being expanded.
    pub current_library = Value::new(false);
);

fn make_library_renamer<'gc>(ctx: Context<'gc>, prefix: Value<'gc>) -> Value<'gc> {
    NativeProc::new(ctx, Some([prefix]), |ctx, args, closure| {
        if args.len() != 1 {
            return Return::Err(Str::new(&ctx, "invalid number of arguments", true).into());
        }
        let id = args[0];
        let prefix = closure.unwrap()[0];
        Return::Ok(Symbol::from_str(ctx, &format!("{prefix}{id}")).into())
    })
    .into()
}

pub fn make_r7rs_toplevel_env<'gc>(ctx: Context<'gc>, prefix: Value<'gc>) -> EnvironmentRef<'gc> {
    let renamer = make_library_renamer(ctx, prefix);

    Environment::new(ctx, None, None, renamer)
}

pub fn make_library<'gc>(ctx: Context<'gc>, spec: Value<'gc>) -> LibraryRef<'gc> {
    let env = make_r7rs_toplevel_env(ctx, mangle_library_spec(ctx, spec));
    let obj = Gc::new(
        &ctx,
        Library {
            header: ScmHeader::with_type_bits(TypeCode8::MODULE.bits() as _),
            name: spec,
            environment: env,
            exports: HashTable::new(&ctx, HashTableType::Eq, 32, 0.75).into(),
            imports: Array::from_array(&ctx, &[]),
            body: Value::null(),
        },
    );

    libraries(ctx).put(ctx, spec, obj.clone());

    obj
}

pub fn library_environment<'gc>(
    ctx: Context<'gc>,
    spec: Value<'gc>,
) -> Option<EnvironmentRef<'gc>> {
    find_library(ctx, spec).map(|lib| lib.environment)
}

pub fn library_exports<'gc>(
    ctx: Context<'gc>,
    spec: Value<'gc>,
) -> Option<Gc<'gc, HashTable<'gc>>> {
    find_library(ctx, spec).map(|lib| lib.exports)
}

pub fn library_exists<'gc>(ctx: Context<'gc>, spec: Value<'gc>) -> bool {
    find_library(ctx, spec).is_some()
}

pub fn with_library<'gc, F, R>(ctx: Context<'gc>, spec: Value<'gc>, f: F) -> R
where
    F: FnOnce(Context<'gc>, LibraryRef<'gc>) -> R,
{
    let lib = find_library(ctx, spec).expect("Library not found");
    let old = get_current_library(ctx);
    set_current_library(ctx, lib.into());
    let r = f(ctx, lib);
    set_current_library(ctx, old);
    r
}

pub fn expand_library<'gc>(ctx: Context<'gc>, form: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
    let spec = form.cadr();
    make_library(ctx, spec);

    with_library(ctx, spec, move |ctx, _| {
        let mut decls = form.cddr();
        let mut forms = Value::null();
        while !syntax_kind(decls, |x| x.is_null()) {
            forms = eval_library_decl(ctx, syntax_car(ctx, decls))?.append(ctx, forms);
            decls = syntax_cdr(ctx, decls);
        }

        expand_toplevel(ctx, forms)
    })
}

static_symbols!(
    SYM_BEGIN = "begin"
    SYM_IMPORT = "import"
    SYM_EXPORT = "export"
    SYM_COND_EXPAND = "cond-expand"
    SYM_INCLUDE = "include"
    SYM_INCLUDE_LIB_DECLS = "include-library-declarations"
    SYM_PREFIX = "prefix"
    SYM_ONLY = "only"
    SYM_EXCEPT = "except"
    SYM_RENAME = "rename"
);

fn eval_library_decl<'gc>(ctx: Context<'gc>, decl: Value<'gc>) -> Result<Value<'gc>, Value<'gc>> {
    let x = syntax_car(ctx, decl);
    let x = unwrap_syntax(ctx, x);
    if x == sym_begin(ctx).into() {
        Ok(decl.cdr())
    } else {
        todo!()
    }
}

pub fn expand_toplevel<'gc>(
    ctx: Context<'gc>,
    forms: Value<'gc>,
) -> Result<Value<'gc>, Value<'gc>> {
    let forms = forms.try_map(ctx, |form| {
        expand(ctx, form, get_current_toplevel_environment(ctx).downcast())
    })?;
    Ok(forms)
}

pub fn library_import<'gc>(
    ctx: Context<'gc>,
    spec: Value<'gc>,
) -> Result<Gc<'gc, HashTable<'gc>>, Value<'gc>> {
    let exports = library_exports(ctx, spec).unwrap();
    let env = library_environment(ctx, spec).unwrap();

    let new_exports = HashTable::new(&ctx, HashTableType::Eq, 32, 0.75);
    for (nickname, id) in exports.iter() {
        let name = env.get(ctx, id)?.unwrap();
        new_exports.put(ctx, nickname, name);
    }

    Ok(new_exports)
}

#[derive(Trace, Clone, Copy)]
#[collect(no_drop)]
pub enum Import<'gc> {
    /// Import a library with specified spec.
    Library(Value<'gc>),
    Prefix(Value<'gc>, ImportRef<'gc>),
    Only(Value<'gc>, ImportRef<'gc>),
    Except(Value<'gc>, ImportRef<'gc>),
    Rename(Value<'gc>, ImportRef<'gc>),
}

impl<'gc> fmt::Display for Import<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Import::Library(spec) => write!(f, "{spec}"),
            Import::Prefix(prefix, import) => write!(f, "(prefix {import} {prefix})"),
            Import::Only(orig, import) => write!(f, "(only {import} {orig})"),
            Import::Except(orig, import) => write!(f, "(except {import} {orig})"),
            Import::Rename(orig, import) => write!(f, "(rename {import} {orig})"),
        }
    }
}

pub type ImportRef<'gc> = Gc<'gc, Import<'gc>>;

pub fn import_to_filename<'gc>(
    spec: Value<'gc>,
    ext: Option<&str>,
    append_dirs: &[PathBuf],
    prepend_dirs: &[PathBuf],
) -> PathBuf {
    let file_ext = ext.unwrap_or("sld");

    let mut filename = String::new();

    spec.for_each(|part| {
        filename.push('/');
        filename.push_str(&part.to_string());
    });
    filename.push('.');
    filename.push_str(file_ext);

    let filename = &filename[1..];
    let dir = &[PathBuf::from("./")];

    for dir in prepend_dirs
        .iter()
        .chain(dir.iter())
        .chain(append_dirs.iter())
    {
        let f = dir.join(filename);

        if f.exists() {
            return f;
        }
    }

    check_system_path(filename)
}

fn check_system_path<'gc>(filename: &str) -> PathBuf {
    let env_dir = std::env::var("CAPY_SYSTEM_PATH").unwrap_or_else(|_| String::from("./"));
    let mut path = PathBuf::from(env_dir);
    path.push(filename);
    if path.exists() {
        path
    } else {
        PathBuf::from(filename)
    }
}

pub fn read_import<'gc>(ctx: Context<'gc>, spec: Value<'gc>) -> Result<ImportRef<'gc>, Value<'gc>> {
    if spec.is_null() {
        return Err(Str::new(&ctx, "import spec cannot be null", true).into());
    }

    let prefix = syntax_car(ctx, spec);

    if prefix == sym_prefix(ctx).into() {
        if syntax_length(ctx, spec) != 2 {
            let lib_spec = syntax_cadr(ctx, spec);
            let prefix = syntax_caddr(ctx, spec);
            if !syntax_kind(prefix, |x| x.is::<Symbol>()) {
                return Err(Str::new(&ctx, "prefix must be a symbol", true).into());
            }

            return Ok(Gc::new(
                &ctx,
                Import::Prefix(prefix, read_import(ctx, lib_spec)?),
            ));
        } else {
            return Err(Str::new(&ctx, "invalid prefix import syntax", true).into());
        }
    } else if prefix == sym_only(ctx).into() {
        if syntax_length(ctx, spec) >= 3 {
            let lib_spec = syntax_cadr(ctx, spec);
            let orig = unwrap_syntax(ctx, syntax_cddr(ctx, spec));
            let mut only = syntax_cddr(ctx, spec);

            while !syntax_kind(only, |x| x.is_null()) {
                if !syntax_kind(syntax_car(ctx, only), |x| x.is::<Symbol>()) {
                    return Err(Str::new(&ctx, "only must be a symbol", true).into());
                }
                only = syntax_cdr(ctx, only);
            }

            return Ok(Gc::new(
                &ctx,
                Import::Only(orig, read_import(ctx, lib_spec)?),
            ));
        } else {
            return Err(Str::new(&ctx, "invalid only import syntax", true).into());
        }
    } else if prefix == sym_except(ctx).into() {
        if syntax_length(ctx, spec) >= 3 {
            let lib_spec = syntax_cadr(ctx, spec);
            let orig = unwrap_syntax(ctx, syntax_caddr(ctx, spec));
            let mut except = syntax_caddr(ctx, spec);

            while !syntax_kind(except, |x| x.is_null()) {
                if !syntax_kind(syntax_car(ctx, except), |x| x.is::<Symbol>()) {
                    return Err(Str::new(&ctx, "except must be a symbol", true).into());
                }
                except = syntax_cdr(ctx, except);
            }

            return Ok(Gc::new(
                &ctx,
                Import::Except(orig, read_import(ctx, lib_spec)?),
            ));
        } else {
            return Err(Str::new(&ctx, "invalid except import syntax", true).into());
        }
    } else if prefix == sym_rename(ctx).into() {
        if syntax_length(ctx, spec) >= 3 {
            let lib_spec = syntax_cadr(ctx, spec);
            let orig = unwrap_syntax(ctx, syntax_caddr(ctx, spec));
            let mut rename = syntax_caddr(ctx, spec);

            while !syntax_kind(rename, |x| x.is_null()) {
                if !syntax_kind(syntax_car(ctx, rename), |x| x.is::<Symbol>()) {
                    return Err(Str::new(&ctx, "rename must be a symbol", true).into());
                }
                rename = syntax_cdr(ctx, rename);
            }

            return Ok(Gc::new(
                &ctx,
                Import::Rename(orig, read_import(ctx, lib_spec)?),
            ));
        } else {
            return Err(Str::new(&ctx, "invalid rename import syntax", true).into());
        }
    }

    Ok(Gc::new(&ctx, Import::Library(unwrap_syntax(ctx, spec))))
}
