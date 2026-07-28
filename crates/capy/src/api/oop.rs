//! Class, record, and generic SNI APIs.

use libc::c_char;

use crate::heap::object::builtin_class_ids;
use crate::prelude::*;
use crate::runtime::class::{
    ClassCategory, ClassDescriptor, class_table, generic_descriptor_from_value, try_scheme_instance,
};
use crate::runtime::value::Tuple;

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};
use super::exceptions::set_type_error;
use super::util::{call_public, values_from_refs};

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_rtd(
    env: *mut SniEnv<'static>,
    name: ScmRef,
    parent: ScmRef,
    uid: ScmRef,
    sealed: bool,
    opaque: bool,
    fields: ScmRef,
) -> ScmRef {
    if env.is_null() || name.is_null() || fields.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let parent_v = if parent.is_null() {
        Value::new(false)
    } else {
        unsafe { ref_value(parent) }
    };
    let uid_v = if uid.is_null() {
        Value::new(false)
    } else {
        unsafe { ref_value(uid) }
    };
    call_public(
        e,
        "make-record-type-descriptor",
        [
            unsafe { ref_value(name) },
            parent_v,
            uid_v,
            Value::new(sealed),
            Value::new(opaque),
            unsafe { ref_value(fields) },
        ],
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_rcd(
    env: *mut SniEnv<'static>,
    rtd: ScmRef,
    parent_rcd: ScmRef,
    protocol: ScmRef,
) -> ScmRef {
    if env.is_null() || rtd.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let rtd_v = unsafe { ref_value(rtd) };
    if !rtd_v.is_recod_type_descriptor(e.ctx) {
        set_type_error(e, "sni_make_rcd", "record-type descriptor", rtd_v);
        return SCM_REF_NULL;
    }
    let parent = if parent_rcd.is_null() {
        Value::new(false)
    } else {
        unsafe { ref_value(parent_rcd) }
    };
    let proto = if protocol.is_null() {
        Value::new(false)
    } else {
        unsafe { ref_value(protocol) }
    };
    call_public(
        e,
        "make-record-constructor-descriptor",
        [rtd_v, parent, proto],
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_record_constructor(env: *mut SniEnv<'static>, rcd: ScmRef) -> ScmRef {
    if env.is_null() || rcd.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let rcd_v = unsafe { ref_value(rcd) };
    if !rcd_v.is_record_constructor_descriptor(e.ctx) {
        set_type_error(
            e,
            "sni_record_constructor",
            "record-constructor descriptor",
            rcd_v,
        );
        return SCM_REF_NULL;
    }
    call_public(e, "record-constructor", [rcd_v])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_record_predicate(env: *mut SniEnv<'static>, rtd: ScmRef) -> ScmRef {
    if env.is_null() || rtd.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let rtd_v = unsafe { ref_value(rtd) };
    if !rtd_v.is_recod_type_descriptor(e.ctx) {
        set_type_error(e, "sni_record_predicate", "record-type descriptor", rtd_v);
        return SCM_REF_NULL;
    }
    call_public(e, "record-predicate", [rtd_v])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_record_accessor(
    env: *mut SniEnv<'static>,
    rtd: ScmRef,
    field: i32,
) -> ScmRef {
    if env.is_null() || rtd.is_null() || field < 0 {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let rtd_v = unsafe { ref_value(rtd) };
    if !rtd_v.is_recod_type_descriptor(e.ctx) {
        set_type_error(e, "sni_record_accessor", "record-type descriptor", rtd_v);
        return SCM_REF_NULL;
    }
    call_public(e, "record-accessor", [rtd_v, Value::new(field)])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_record_mutator(
    env: *mut SniEnv<'static>,
    rtd: ScmRef,
    field: i32,
) -> ScmRef {
    if env.is_null() || rtd.is_null() || field < 0 {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let rtd_v = unsafe { ref_value(rtd) };
    if !rtd_v.is_recod_type_descriptor(e.ctx) {
        set_type_error(e, "sni_record_mutator", "record-type descriptor", rtd_v);
        return SCM_REF_NULL;
    }
    call_public(e, "record-mutator", [rtd_v, Value::new(field)])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_is_record(_env: *mut SniEnv<'static>, obj: ScmRef) -> bool {
    if obj.is_null() {
        return false;
    }
    // Need a context for is_record — use a dummy via trying tuple tag.
    let v = unsafe { ref_value::<'_>(obj) };
    v.is::<Tuple>()
        && v.downcast::<Tuple>().len() > 0
        && v.downcast::<Tuple>()[0].get().is::<Tuple>()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_class(
    env: *mut SniEnv<'static>,
    name: ScmRef,
    slots: ScmRef,
    supers: ScmRef,
) -> ScmRef {
    if env.is_null() || name.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let slots_v = if slots.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(slots) }
    };
    let supers_v = if supers.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(supers) }
    };
    call_public(
        e,
        "make-class",
        [unsafe { ref_value(name) }, slots_v, supers_v],
    )
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_abstract_class(
    env: *mut SniEnv<'static>,
    name: ScmRef,
    supers: ScmRef,
) -> ScmRef {
    if env.is_null() || name.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let name_v = unsafe { ref_value(name) };
    let name_str = if let Some(sym) = name_v.try_as::<Symbol>() {
        sym.to_string()
    } else if let Some(s) = name_v.try_as::<Str>() {
        s.to_string()
    } else {
        return SCM_REF_NULL;
    };

    let mut super_ids = Vec::new();
    let mut ls = if supers.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(supers) }
    };
    while ls.is_pair() {
        let c = ls.car();
        if let Some(cls) = c.try_as::<ClassDescriptor>() {
            super_ids.push(cls.id());
        }
        ls = ls.cdr();
    }
    if super_ids.is_empty() {
        // Default to <object> if available.
        if let Some(obj) = e.ctx.public_ref("capy", "%builtin-class") {
            // Prefer calling register with OBJECT builtin id.
            let _ = obj;
        }
        use crate::heap::object::ClassId;
        super_ids.push(ClassId::new(builtin_class_ids::OBJECT).expect("object class id"));
    }

    match class_table(e.ctx).register_dynamic_with_slots(
        e.ctx,
        &name_str,
        ClassCategory::Abstract,
        &super_ids,
        &[],
    ) {
        Ok(cls) => e.make_local(cls.into()),
        Err(_) => SCM_REF_NULL,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_instance(
    env: *mut SniEnv<'static>,
    class_: ScmRef,
    n: i32,
    initargs: *const ScmRef,
) -> ScmRef {
    if env.is_null() || class_.is_null() || n < 0 {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let class_v = unsafe { ref_value(class_) };
    if !class_v.is::<ClassDescriptor>() {
        set_type_error(e, "sni_make_instance", "class", class_v);
        return SCM_REF_NULL;
    }
    let args = values_from_refs(initargs, n as usize);
    let mut call_args = Vec::with_capacity(args.len() + 1);
    call_args.push(class_v);
    call_args.extend(args);
    call_public(e, "make-instance", call_args)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_slot_ref(
    env: *mut SniEnv<'static>,
    obj: ScmRef,
    slot: ScmRef,
    unbound: ScmRef,
) -> ScmRef {
    if env.is_null() || obj.is_null() || slot.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let obj_v = unsafe { ref_value(obj) };
    if try_scheme_instance(e.ctx, obj_v).is_none() {
        set_type_error(e, "sni_slot_ref", "instance", obj_v);
        return SCM_REF_NULL;
    }
    let slot_v = unsafe { ref_value(slot) };
    if !slot_v.is::<Symbol>() {
        set_type_error(e, "sni_slot_ref", "symbol", slot_v);
        return SCM_REF_NULL;
    }
    if unbound.is_null() {
        call_public(e, "slot-ref", [obj_v, slot_v])
    } else {
        call_public(
            e,
            "slot-ref",
            [obj_v, slot_v, unsafe { ref_value(unbound) }],
        )
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_slot_set(
    env: *mut SniEnv<'static>,
    obj: ScmRef,
    slot: ScmRef,
    value: ScmRef,
) -> libc::c_int {
    if env.is_null() || obj.is_null() || slot.is_null() || value.is_null() {
        return -1;
    }
    let e = unsafe { env_mut(env) };
    let obj_v = unsafe { ref_value(obj) };
    if try_scheme_instance(e.ctx, obj_v).is_none() {
        set_type_error(e, "sni_slot_set", "instance", obj_v);
        return -1;
    }
    let slot_v = unsafe { ref_value(slot) };
    if !slot_v.is::<Symbol>() {
        set_type_error(e, "sni_slot_set", "symbol", slot_v);
        return -1;
    }
    let r = call_public(e, "slot-set!", [obj_v, slot_v, unsafe { ref_value(value) }]);
    if r.is_null() && crate::runtime::sni::exception::check() {
        -1
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_class_of(env: *mut SniEnv<'static>, obj: ScmRef) -> ScmRef {
    if env.is_null() || obj.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    call_public(e, "class-of", [unsafe { ref_value(obj) }])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_builtin_class(
    env: *mut SniEnv<'static>,
    name: *const c_char,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let Some(name) = (unsafe { safe_cstr(name) }) else {
        return SCM_REF_NULL;
    };
    // Accept both "object" and "<object>"; %builtin-class matches unbracketed names.
    let name = name
        .strip_prefix('<')
        .and_then(|n| n.strip_suffix('>'))
        .unwrap_or(name);
    let sym = Symbol::from_str(e.ctx, name);
    call_public(e, "%builtin-class", [sym.into()])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_generic(
    env: *mut SniEnv<'static>,
    name: ScmRef,
    max_dispatch_args: i32,
) -> ScmRef {
    if env.is_null() || name.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let name_v = unsafe { ref_value(name) };
    if !name_v.is::<Symbol>() {
        set_type_error(e, "sni_make_generic", "symbol", name_v);
        return SCM_REF_NULL;
    }
    call_public(e, "make-generic", [name_v, Value::new(max_dispatch_args)])
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_add_method(
    env: *mut SniEnv<'static>,
    generic: ScmRef,
    specializers: ScmRef,
    required_argc: i32,
    body: ScmRef,
    locked: bool,
) -> libc::c_int {
    if env.is_null() || generic.is_null() || specializers.is_null() || body.is_null() {
        return -1;
    }
    let e = unsafe { env_mut(env) };
    let generic_v = unsafe { ref_value(generic) };
    if generic_descriptor_from_value(e.ctx, generic_v).is_none() {
        set_type_error(e, "sni_add_method", "generic procedure", generic_v);
        return -1;
    }
    let body_v = unsafe { ref_value(body) };
    if !body_v.is::<Closure>() {
        set_type_error(e, "sni_add_method", "procedure", body_v);
        return -1;
    }
    let r = call_public(
        e,
        "add-method!",
        [
            generic_v,
            unsafe { ref_value(specializers) },
            Value::new(required_argc),
            body_v,
            Value::new(locked),
        ],
    );
    if r.is_null() && crate::runtime::sni::exception::check() {
        -1
    } else {
        0
    }
}
