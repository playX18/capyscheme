//! Compile-only smoke for #[onload] / #[cps] / #[scheme(cps)] / typed cps::call.
#![allow(dead_code)]

use capy_sni::{Env, Ref, Scm, TryFromScm, cps, onload, scheme};

#[scheme(name = "add", module = "smoke")]
fn add(x: i32, y: i32) -> i32 {
    x + y
}

/// Scheme-callable native that nests via CPS.
#[scheme(cps, name = "add-via-cps", module = "smoke")]
fn add_via_cps(env: &mut Env<'_>, x: i32, y: i32) -> i32 {
    let _ = env.push_local_frame(16);
    if let Some(proc) = env.public_ref("smoke", "add") {
        let r = cps::call(proc, &[env.fixnum(x), env.fixnum(y)]);
        let n = i32::try_from_scm(env, r).unwrap_or(-1);
        let _ = env.pop_local_frame(Ref::null());
        return n;
    }
    let _ = env.pop_local_frame(Ref::null());
    -1
}

#[onload(frame = 8)]
fn init(env: &mut Env<'_>) {
    scheme_register_add(env);
    scheme_register_add_via_cps(env);
}

#[cps]
fn demo(scm: &Scm) -> i32 {
    let _ = env.push_local_frame(16);
    scheme_register_add(env);
    scheme_register_add_via_cps(env);
    if let Some(proc) = env.public_ref("smoke", "add") {
        let r = cps::call(proc, &[env.fixnum(40), env.fixnum(2)]);
        let n = i32::try_from_scm(env, r).unwrap_or(-1);
        let _caught = cps::guard(
            |_exn| -> i32 { 1i32 },
            || -> i32 {
                let _ = cps::call(proc, &[env.fixnum(1), env.fixnum(1)]);
                0i32
            },
        );
        let _ = env.call_in_native(|| 0i32);
        let _ = env.pop_local_frame(Ref::null());
        return n;
    }
    let _ = env.pop_local_frame(Ref::null());
    -1
}

fn _ensure_used() {
    let _ = demo as fn(&Scm) -> i32;
    let _ = init as fn(&mut Env<'_>);
    let _ = add_via_cps as fn(&mut Env<'_>, i32, i32) -> i32;
}
