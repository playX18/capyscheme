//! Specializers that a method may name in place of a class.
//!
//! An `eql` specializer selects the one object it was interned for. Interning
//! gives one specializer per object, so `eq?` on specializers follows `eqv?`
//! on their objects.

use super::builtin::builtin_id;
use super::flags::LayoutFamily;
use crate::heap::alloc::{Array, ArrayRef};
use crate::heap::cell::Lock;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace};
use crate::prelude::*;
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::conversions::ClassTagged;
use crate::runtime::objectmodel::{Value, WeakTable};

/// The specializer `(eql object)`.
#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct EqlSpecializer<'gc> {
    object: Value<'gc>,
    /// The class of `object` at intern time. A method with an eql position
    /// keeps this class id in its class part, so class-keyed code still sees
    /// the class.
    class: ClassId,
    /// Methods that name this specializer, appended as they are added.
    direct_methods: Lock<ArrayRef<'gc, Value<'gc>>>,
}

// SAFETY: `gc` for `EqlSpecializer` upholds all trait invariants
unsafe impl<'gc> ClassTagged for EqlSpecializer<'gc> {
    const TYPE_NAME: &'static str = "eql-specializer";
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::EQL_SPECIALIZER];
    const FAMILY: Option<LayoutFamily> = Some(LayoutFamily::EqlSpecializer);
}

// Every interned specializer, keyed by its object. The keys are weak: an
// object nothing else refers to is dropped with its specializer.
global!(EQL_SPECIALIZERS<'gc>: Gc<'gc, WeakTable<'gc>> = (ctx) WeakTable::new(ctx, 16, 0.75););

impl<'gc> EqlSpecializer<'gc> {
    fn new(ctx: MutationContext<'gc>, object: Value<'gc>) -> Gc<'gc, Self> {
        // An object without a class id is not a value a program can name,
        // so the top class stands in for it.
        let class = object
            .class_id()
            .or(ClassId::new(builtin_class_ids::TOP))
            .expect("the top class id is nonzero");
        ctx.allocate_with(
            Self {
                object,
                class,
                direct_methods: Lock::new(Array::new(ctx, 0, |_, _| {
                    unreachable!("zero-length direct method arrays are never initialized")
                })),
            },
            class_header_word(builtin_id(builtin_class_ids::EQL_SPECIALIZER)),
        )
    }

    /// The interned specializer for `object`, created on first use.
    pub fn intern(ctx: MutationContext<'gc>, object: Value<'gc>) -> Gc<'gc, Self> {
        let table = eql_specializers(ctx);
        if let Some(existing) = table.get(ctx, object) {
            return existing.downcast::<Self>();
        }
        // Keys compare by bits, which is `eqv?` for everything but numbers
        // that are not fixnums (bignums, flonums, ratios): two such objects
        // with one value are `eqv?` but differ in bits, so scan for them.
        if object.number().is_some() {
            let mut found = None;
            table.for_each(ctx, |key, value| {
                if found.is_none() && key.eqv(object) {
                    found = Some(value);
                }
            });
            if let Some(existing) = found {
                return existing.downcast::<Self>();
            }
        }
        let specializer = Self::new(ctx, object);
        table.put(ctx, object, Value::from(specializer));
        specializer
    }

    /// The object this specializer selects.
    pub fn object(&self) -> Value<'gc> {
        self.object
    }

    /// The class of the object at intern time.
    pub fn class_id(&self) -> ClassId {
        self.class
    }

    /// The methods that name this specializer.
    pub fn direct_methods(&self) -> ArrayRef<'gc, Value<'gc>> {
        self.direct_methods.get()
    }

    /// Record `method` as one that names this specializer.
    pub(crate) fn add_direct_method(
        ctx: MutationContext<'gc>,
        specializer: Gc<'gc, Self>,
        method: Value<'gc>,
    ) {
        let current = specializer.direct_methods.get();
        if current.contains(&method) {
            return;
        }
        let appended = Array::new(ctx, current.len() + 1, |_, index| {
            if index < current.len() {
                current[index]
            } else {
                method
            }
        });
        barrier::field!(Gc::write(ctx, specializer), EqlSpecializer, direct_methods)
            .unlock()
            .set(appended);
    }

    /// Forget `method` as one that names this specializer (the counterpart of
    /// [`Self::add_direct_method`]).
    pub(crate) fn remove_direct_method(
        ctx: MutationContext<'gc>,
        specializer: Gc<'gc, Self>,
        method: Value<'gc>,
    ) {
        let current = specializer.direct_methods.get();
        if !current.contains(&method) {
            return;
        }
        let kept: Vec<Value<'gc>> = current.iter().copied().filter(|m| *m != method).collect();
        barrier::field!(Gc::write(ctx, specializer), EqlSpecializer, direct_methods)
            .unlock()
            .set(Array::from_slice(ctx, &kept));
    }
}
