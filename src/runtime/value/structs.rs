use crate::{
    list,
    runtime::{Context, errors::Violation},
};

use super::*;
use rsgc::{
    Collect, EnsureGCInfo, GCInfoIndexForT, Trace, Visitor,
    alloc::array::Array,
    gc::{GCInfo, GCInfoIndex, GLOBAL_GC_INFO_TABLE},
    generic_static::Namespace,
    vmkit::prelude::{GCMetadata, TraceCallback},
};
use std::sync::atomic::{AtomicU16, Ordering};

#[derive(Collect)]
#[collect(no_drop)]
pub struct RecordTypeDescriptor<'gc> {
    pub name: Value<'gc>,
    pub parent: Option<Gc<'gc, Self>>,
    pub uid: Value<'gc>,
    pub sealed: bool,
    pub opaque: bool,
    pub fields: Gc<'gc, Array<(bool, Value<'gc>)>>,
}

impl<'gc> RecordTypeDescriptor<'gc> {
    pub fn new<'a>(
        mc: Context<'gc>,
        name: Value<'gc>,
        parent: Value<'gc>,
        uid: Value<'gc>,
        sealed: Value<'gc>,
        opaque: Value<'gc>,
        fields: Value<'gc>,
    ) -> Result<Gc<'gc, Self>, Violation<'gc>> {
        if !name.is::<Symbol>() {
            return Err(Violation::WrongTypeArgument {
                who: Some("make-record-type-descriptor".into()),
                position: 1,
                expected: "symbol".into(),
                got: name,
                args: list![mc, name, parent, uid, sealed, opaque, fields],
            });
        }

        if !fields.is::<Vector>() {
            return Err(Violation::WrongTypeArgument {
                who: Some("make-record-type-descriptor".into()),
                position: 6,
                expected: "vector".into(),
                got: fields,
                args: list![mc, name, parent, uid, sealed, opaque, fields],
            });
        }

        if !parent.is::<RecordTypeDescriptor>() {
            return Err(Violation::WrongTypeArgument {
                who: Some("make-record-type-descriptor".into()),
                position: 2,
                expected: "record-type descriptor or #f".into(),
                got: parent,
                args: list![mc, name, parent, uid, sealed, opaque, fields],
            });
        } else if parent.downcast::<RecordTypeDescriptor>().sealed {
            return Err(Violation::AssertionViolation {
                who: Some("make-record-type-descriptor".into()),
                message: "attempted to extend sealed record-type".into(),
                irritants: list!(mc, parent),
            });
        }
        let parent = if parent.is_bool() {
            None
        } else {
            Some(parent.downcast::<RecordTypeDescriptor>())
        };
        let opaque = opaque.as_bool() || parent.map_or(false, |p| p.opaque);

        let fieldsv = fields.downcast::<Vector>();
        let err = || Violation::AssertionViolation {
            who: Some("make-record-type-descriptor".into()),
            message: "malformed field specifiers".into(),
            irritants: list!(mc, fields),
        };
        for field in fieldsv.iter() {
            if field.is_pair() && field.list_length() == 2 {
                let mutability = field.car();
                let name = field.cdr().car();
                if !mutability.is::<Symbol>() {
                    return Err(err());
                }

                if !name.is::<Symbol>() {
                    return Err(err());
                }

                let mutability = mutability.downcast::<Symbol>();
                if *mutability != "mutable" && *mutability != "immutable" {
                    return Err(err());
                }
            } else {
                return Err(err());
            }
        }

        let fields = Array::with_fn(&mc, fields.downcast::<Vector>().len(), |i| {
            let field = fieldsv[i];
            let mutability = field.car();
            let name = field.cdr().car();
            let mutable = *mutability.downcast::<Symbol>() == "mutable";
            (mutable, name)
        });

        if uid.not() {
            Ok(Gc::new(
                &mc,
                Self {
                    name,
                    parent,
                    uid: Value::new(false),
                    sealed: sealed.as_bool(),
                    opaque,
                    fields,
                },
            ))
        } else {
            todo!("UID generation")
        }
    }

    pub fn name(&self) -> Gc<'gc, Symbol<'gc>> {
        self.name.downcast()
    }

    pub fn parent(&self) -> Option<Gc<'gc, Self>> {
        self.parent
    }

    pub fn uid(&self) -> Value<'gc> {
        self.uid
    }

    pub fn sealed(&self) -> bool {
        self.sealed
    }

    pub fn opaque(&self) -> bool {
        self.opaque
    }

    pub fn fields(&self) -> Gc<'gc, Array<(bool, Value<'gc>)>> {
        self.fields
    }

    pub fn is_ancestor(parent: Gc<'gc, Self>, rtd: Gc<'gc, Self>) -> bool {
        let mut rtd = Some(rtd);

        while let Some(current) = rtd {
            if Gc::ptr_eq(current, parent) {
                return true;
            }
            rtd = current.parent;
        }

        false
    }

    pub fn inherited_field_count(&self) -> usize {
        let mut count = 0;
        let mut rtd = self.parent;

        while let Some(current) = rtd {
            count += current.fields.len();
            rtd = current.parent;
        }

        count
    }

    pub fn total_field_count(&self) -> usize {
        self.inherited_field_count() + self.fields.len()
    }
}

unsafe impl<'gc> Tagged for RecordTypeDescriptor<'gc> {
    const TC8: TypeCode8 = TypeCode8::RECORD_TYPE_DESCRIPTOR;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct RecordConstructorDescriptor<'gc> {
    pub rtd: Gc<'gc, RecordTypeDescriptor<'gc>>,
    pub protocol: Value<'gc>,
    pub custom_protocol: bool,
    pub parent: Option<Gc<'gc, Self>>,
}

pub struct Record<'gc> {
    pub rtd: Gc<'gc, RecordTypeDescriptor<'gc>>,
    fields: [Value<'gc>; 0],
}

unsafe impl<'gc> Tagged for Record<'gc> {
    const TC8: TypeCode8 = TypeCode8::RECORD;
}

unsafe impl<'gc> Trace for Record<'gc> {
    fn trace(&mut self, visitor: &mut Visitor<'_>) {
        self.rtd.trace(visitor);
        unsafe {
            let fields = std::slice::from_raw_parts_mut(
                self.fields.as_mut_ptr(),
                self.rtd.total_field_count(),
            );

            for field in fields {
                field.trace(visitor);
            }
        }
    }
}

unsafe impl<'gc> EnsureGCInfo<'gc> for Record<'gc> {
    fn ensure_gc_info() -> rsgc::gc::GCInfoIndex {
        let registered_index = ValuesNamespace::generic_static::<GCInfoIndexForT<Record<'gc>>>();
        let index = registered_index.index.load(Ordering::Relaxed);

        if index != 0 {
            return unsafe { GCInfoIndex::from_raw(index) };
        }

        register_record_info(&registered_index.index)
    }
}

#[cold]
fn register_record_info(index: &AtomicU16) -> GCInfoIndex {
    GLOBAL_GC_INFO_TABLE.register_new_gc_info(
        index,
        GCInfo {
            weak_callback: |_, _| {},
            id: 0,
            gc_metadata: GCMetadata {
                alignment: std::mem::align_of::<Value<'static>>(),
                compute_alignment: None,
                compute_size: Some(|object| {
                    let record = unsafe { object.as_address().as_ref::<Record<'static>>() };
                    // The size of the fields array
                    record.rtd.total_field_count() * std::mem::size_of::<Value<'static>>()
                }),
                instance_size: std::mem::size_of::<Record<'static>>(), // Base size of Record struct (rtd field)
                trace: TraceCallback::TraceObject(|object, tracer| {
                    let record = unsafe { object.as_address().as_mut_ref::<Record<'static>>() };
                    let mut visitor =
                        unsafe { Visitor::new(tracer, Some(object.as_object_unchecked())) };
                    record.trace(&mut visitor);
                }),
            },
        },
    )
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct RecordType<'gc> {
    pub name: Value<'gc>,
    pub rtd: Gc<'gc, RecordTypeDescriptor<'gc>>,
    pub rcd: Gc<'gc, RecordConstructorDescriptor<'gc>>,
}

unsafe impl<'gc> Tagged for RecordType<'gc> {
    const TC8: TypeCode8 = TypeCode8::RECORD_TYPE;
}
