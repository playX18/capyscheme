//! Scheme Native Interface wrapper around [`capy_cparse`].

use capy_cparse::driver::{Options, probe_target, process_header};
use capy_cparse::emit::{Builder, Emit};
use capy_sni::{Env, Ref, onload, scheme};

struct SchemeEmit<'a, 'env> {
    env: &'a mut Env<'env>,
    failed: Option<Ref<'env>>,
}

impl<'env> SchemeEmit<'_, 'env> {
    fn keep(&mut self, r: Result<Ref<'env>, Ref<'env>>) -> Ref<'env> {
        match r {
            Ok(v) => v,
            Err(e) => {
                self.failed.get_or_insert(e);
                Ref::null()
            }
        }
    }
}

impl<'env> Emit for SchemeEmit<'_, 'env> {
    type V = Ref<'env>;

    fn int(&mut self, v: i128) -> Ref<'env> {
        match i64::try_from(v) {
            Ok(v) => self.env.int64(v),
            Err(_) => self.env.int128(v),
        }
    }

    fn float(&mut self, v: f64) -> Ref<'env> {
        self.env.flonum(v)
    }

    fn str(&mut self, v: &str) -> Ref<'env> {
        self.env.string_utf8(v)
    }

    fn bytes(&mut self, v: &[u8]) -> Ref<'env> {
        let r = self.env.bytevector_from(v);
        self.keep(r)
    }

    fn sym(&mut self, v: &'static str) -> Ref<'env> {
        let r = self.env.intern_symbol(v);
        self.keep(r)
    }

    fn bool(&mut self, v: bool) -> Ref<'env> {
        self.env.bool(v)
    }

    fn list(&mut self, items: Vec<Ref<'env>>) -> Ref<'env> {
        if self.failed.is_some() {
            return Ref::null();
        }
        self.env.list(&items)
    }

    fn enter(&mut self) {
        if self.env.push_local_frame(64).is_err() {
            self.failed
                .get_or_insert_with(|| self.env.make_message_condition("out of local references"));
        }
    }

    fn leave(&mut self, v: Ref<'env>) -> Ref<'env> {
        self.env.pop_local_frame(v)
    }
}

fn field<'env>(env: &mut Env<'env>, request: Ref<'env>, key: &str) -> Option<Vec<Ref<'env>>> {
    let mut rest = request;
    while env.is_pair(rest) {
        let entry = env.car(rest).ok()?;
        rest = env.cdr(rest).ok()?;
        if !env.is_pair(entry) {
            continue;
        }
        let head = env.car(entry).ok()?;
        if env.is_symbol(head) && env.value_to_utf8(head).ok()? == key {
            return Some(list_items(env, env.cdr(entry).ok()?));
        }
    }
    None
}

fn list_items<'env>(env: &mut Env<'env>, mut list: Ref<'env>) -> Vec<Ref<'env>> {
    let mut out = Vec::new();
    while env.is_pair(list) {
        let (Ok(head), Ok(tail)) = (env.car(list), env.cdr(list)) else {
            break;
        };
        out.push(head);
        list = tail;
    }
    out
}

fn text<'env>(env: &mut Env<'env>, v: Ref<'env>) -> Option<String> {
    if env.is_string(v) {
        env.string_to_utf8(v).ok()
    } else {
        None
    }
}

/// Builds options from `((header "x.h") (angle #t) (include-dirs "d" ...)
/// (defines ("NAME" "BODY"|#f) ...) (std "c23") (cc "cc") (inline-bodies #t))`.
fn options<'env>(env: &mut Env<'env>, request: Ref<'env>) -> Result<Options, String> {
    let strings = |env: &mut Env<'env>, key: &str| -> Vec<String> {
        field(env, request, key)
            .unwrap_or_default()
            .into_iter()
            .filter_map(|v| text(env, v))
            .collect()
    };
    let one = |env: &mut Env<'env>, key: &str| -> Option<String> {
        let v = *field(env, request, key)?.first()?;
        text(env, v)
    };
    let flag = |env: &mut Env<'env>, key: &str, default: bool| -> bool {
        match field(env, request, key).and_then(|v| v.first().copied()) {
            Some(v) if env.is_bool(v) => env.to_bool(v),
            _ => default,
        }
    };
    let mut defines = Vec::new();
    for d in field(env, request, "defines").unwrap_or_default() {
        let parts = list_items(env, d);
        let Some(name) = parts.first().and_then(|&n| text(env, n)) else {
            continue;
        };
        defines.push((name, parts.get(1).and_then(|&b| text(env, b))));
    }
    Ok(Options {
        header: one(env, "header").ok_or("request has no (header ...)")?,
        angle: flag(env, "angle", true),
        include_dirs: strings(env, "include-dirs")
            .into_iter()
            .map(Into::into)
            .collect(),
        defines,
        std: one(env, "std"),
        cc: one(env, "cc"),
        inline_bodies: flag(env, "inline-bodies", true),
    })
}

/// Runs `build` with a [`Builder`] over `env` and returns its value, or the first failure.
fn emit<'env>(
    env: &mut Env<'env>,
    build: impl FnOnce(&Builder<SchemeEmit<'_, 'env>>) -> Result<Ref<'env>, String>,
) -> Result<Ref<'env>, Ref<'env>> {
    let b = Builder::new(SchemeEmit { env, failed: None });
    let built = build(&b);
    let SchemeEmit { env, failed } = b.into_inner();
    if let Some(e) = failed {
        return Err(e);
    }
    built.map_err(|msg| env.string_utf8(&msg))
}

/// `(c-process-header request)`: `request` is `((header "x.h") (angle #t) ...)`.
///
/// Returns `(c-result (deps ...) (errors N) (c-unit ...))`.
#[scheme(name = "c-process-header", module = "capy c native")]
fn c_process_header<'env>(env: &mut Env<'env>, request: Ref<'env>) -> Result<Ref<'env>, Ref<'env>> {
    let opts = match options(env, request) {
        Ok(o) => o,
        Err(msg) => return Err(env.string_utf8(&msg)),
    };
    emit(env, |b| {
        let out = process_header(b, &opts)?;
        Ok(out.into_result(b))
    })
}

/// `(c-probe-target cc std)`: compiler identity and search directories as `(c-target ...)`.
#[scheme(name = "c-probe-target", module = "capy c native")]
fn c_probe_target<'env>(
    env: &mut Env<'env>,
    cc: Option<String>,
    std: Option<String>,
) -> Result<Ref<'env>, Ref<'env>> {
    emit(env, |b| probe_target(b, cc.as_deref(), std.as_deref()))
}

#[scheme(name = "c-native-version", module = "capy c native")]
fn c_native_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[onload(frame = 16)]
fn init(env: &mut Env<'_>) {
    scheme_register_c_process_header(env);
    scheme_register_c_probe_target(env);
    scheme_register_c_native_version(env);
}
