/*use std::{alloc::Layout, cell::Cell};

use rsgc::{
    Gc, Trace,
    alloc::ArrayRef,
    mmtk::{AllocationSemantics, util::Address},
    object::{GCObject, VTable},
};

use crate::runtime::{
    Context,
    value::{ScmHeader, TypeCode8, Value},
};

bitflags::bitflags! {
    #[derive(Copy, Clone, PartialEq, Eq)]
    pub struct StructureFlags: u32 {
        const NONFAIL_CTOR = 0x1;
        const SYSTEM_OPAQUE = 0x2;
        const AUTHENTIC = 0x4;
        const SEALED = 0x8;
    }
}

unsafe impl<'gc> Trace for StructureFlags {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct StructType<'gc> {
    pub header: ScmHeader,

    pub num_slots: u16,
    pub num_islots: u16,
    pub name_pos: u16,

    pub flags: Cell<StructureFlags>,

    pub name: Value<'gc>,
    pub accessor: Value<'gc>,
    pub mutator: Value<'gc>,
    pub prefab_key: Value<'gc>,

    pub uninit_val: Value<'gc>,

    pub props: ArrayRef<'gc, Value<'gc>>,
    pub proc_attr: Value<'gc>,
    pub guard: Value<'gc>,

    pub parent_types: ArrayRef<'gc, Gc<'gc, Self>>,
}

#[repr(C)]
pub struct Struct<'gc> {
    pub(crate) header: ScmHeader,
    pub stype: Gc<'gc, StructType<'gc>>,
    pub slots: [Value<'gc>; 0],
}

unsafe impl<'gc> Trace for Struct<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        (Self::VT.trace)(GCObject::from_address(Address::from_ref(self)), visitor)
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> Struct<'gc> {
    pub const VT: &'static VTable = &VTable {
        alignment: align_of::<Value<'static>>(),
        trace: |object, visitor| unsafe {
            let struct_ = object.to_address().as_mut_ref::<Struct>();

            let data = struct_.slots.as_mut_ptr();

            for i in 0..struct_.stype.num_slots as usize {
                let slot = data.add(i);
                visitor.trace(&mut *slot);
            }
        },
        weak_proc: |_, _| {},
        instance_size: 0,
        compute_size: Some(|object| unsafe {
            let struct_ = object.to_address().as_ref::<Struct>();

            size_of::<Struct>() + (struct_.stype.num_slots as usize * size_of::<Value>())
        }),
        compute_alignment: None,
    };

    pub fn new(
        ctx: Context<'gc>,
        stype: Gc<'gc, StructType<'gc>>,
        args: &[Value<'gc>],
    ) -> Gc<'gc, Self> {
        let c = stype.num_slots as usize;

        let size = size_of::<Self>() + (c * size_of::<Value>());

        let layout = Layout::from_size_align(size, align_of::<Value>()).unwrap();
        let instance =
            ctx.mc
                .allocate_with_layout::<Self>(layout, Self::VT, AllocationSemantics::Default);

        unsafe {
            let uninit = instance.as_mut_ptr();

            uninit.write(Struct {
                header: ScmHeader::with_type_bits(TypeCode8::RECORD.bits() as _),
                stype,
                slots: [],
            });

            let slots = uninit
                .as_mut()
                .expect("must not be null")
                .slots
                .as_mut_ptr();

            let mut j = c;
            let mut i = args.len();

            for p in (0..=stype.name_pos as usize).rev() {
                let (mut ns, mut nis) = if p != 0 {
                    (
                        stype.parent_types[p].num_slots - stype.parent_types[p - 1].num_slots,
                        stype.parent_types[p].num_islots - stype.parent_types[p - 1].num_islots,
                    )
                } else {
                    (
                        stype.parent_types[p].num_slots,
                        stype.parent_types[p].num_islots,
                    )
                };

                ns -= nis;

                while ns != 0 {
                    ns -= 1;
                    j -= 1;
                    slots.add(j).write(stype.parent_types[p].uninit_val);
                }

                while nis != 0 {
                    nis -= 1;
                    j -= 1;
                    i -= 1;
                    slots.add(j).write(args[i]);
                }
            }

            instance.assume_init()
        }
    }
}
*/

use std::cell::Cell;

use rsgc::{Gc, Trace, cell::Lock, object::VTable};

use crate::runtime::{
    Context,
    value::{ScmHeader, Symbol, Tagged, TypeCode8, Value},
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct RecordTypeDescriptor<'gc> {
    header: ScmHeader,
    pub name: Value<'gc>,
    pub parent: Option<Gc<'gc, Self>>,
    pub uid: Value<'gc>,
    pub sealed: Cell<bool>,
    pub opaque: Cell<bool>,
    pub fields: Value<'gc>,
}

impl<'gc> RecordTypeDescriptor<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        name: Value<'gc>,
        parent: Option<Gc<'gc, Self>>,
        uid: Value<'gc>,
        sealed: bool,
        opaque: bool,
        fields: Value<'gc>,
    ) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            RecordTypeDescriptor {
                header: ScmHeader::with_type_bits(TypeCode8::RECORD_TYPE_DESCRIPTOR.bits() as _),
                name,
                parent,
                uid,
                sealed: Cell::new(sealed),
                opaque: Cell::new(opaque),
                fields,
            },
        )
    }

    pub fn name(&self) -> Value<'gc> {
        self.name.clone()
    }

    pub fn parent(&self) -> Option<Gc<'gc, Self>> {
        self.parent.clone()
    }

    pub fn uid(&self) -> Value<'gc> {
        self.uid.clone()
    }

    pub fn sealed(&self) -> bool {
        self.sealed.get()
    }

    pub fn opaque(&self) -> bool {
        self.opaque.get()
    }

    pub fn fields(&self) -> Value<'gc> {
        self.fields.clone()
    }

    pub fn is_ancestor(self: Gc<'gc, Self>, other: Gc<'gc, Self>) -> bool {
        let mut rtd = Some(self);

        while let Some(desc) = rtd {
            if Gc::ptr_eq(desc, other) {
                return true;
            }

            rtd = desc.parent();
        }

        false
    }

    pub fn inherited_field_count(&self) -> usize {
        let mut rtd = self.parent();
        let mut count = 0;

        while let Some(desc) = rtd {
            count += desc.fields().list_length();
            rtd = desc.parent();
        }

        count
    }

    pub fn total_field_count(&self) -> usize {
        self.inherited_field_count() + self.fields().list_length()
    }

    pub fn is_mutable_field(&self, k: usize) -> bool {
        self.fields()
            .list_ref(k)
            .map_or(false, |field| field.car() != Value::new(false))
    }

    pub fn from_rust<'a>(
        ctx: Context<'gc>,
        name: &str,
        parent: Option<Gc<'gc, Self>>,
        uid: Value<'gc>,
        sealed: bool,
        opaque: bool,
        fields: impl IntoIterator<Item = (bool, &'a str)>,
    ) -> Gc<'gc, Self> {
        let name = Symbol::from_str(ctx, name);

        let fields = fields
            .into_iter()
            .map(|(mutable, name)| {
                Value::cons(ctx, Value::new(mutable), Symbol::from_str(ctx, name).into())
            })
            .collect::<Vec<_>>();

        let mut ls = Value::null();

        for field in fields.iter().rev() {
            ls = Value::cons(ctx, *field, ls);
        }

        RecordTypeDescriptor::new(ctx, name.into(), parent, uid, sealed, opaque, ls)
    }
}

unsafe impl<'gc> Tagged for RecordTypeDescriptor<'gc> {
    const TC8: TypeCode8 = TypeCode8::RECORD_TYPE_DESCRIPTOR;
}

impl<'gc> Value<'gc> {
    pub fn is_rtd(&self) -> bool {
        self.is::<RecordTypeDescriptor>()
    }
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct RecordCtorDescriptor<'gc> {
    header: ScmHeader,
    pub rtd: Gc<'gc, RecordTypeDescriptor<'gc>>,
    pub protocol: Value<'gc>,
    pub custom_protocol: Cell<bool>,
    pub parent: Option<Gc<'gc, Self>>,
}

impl<'gc> RecordCtorDescriptor<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        rtd: Gc<'gc, RecordTypeDescriptor<'gc>>,
        protocol: Value<'gc>,
        custom_protocol: bool,
        parent: Option<Gc<'gc, Self>>,
    ) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            RecordCtorDescriptor {
                header: ScmHeader::with_type_bits(
                    TypeCode8::RECORD_CONSTRUCTOR_DESCRIPTOR.bits() as _
                ),
                rtd,
                protocol,
                custom_protocol: Cell::new(custom_protocol),
                parent,
            },
        )
    }

    pub fn rtd(&self) -> Gc<'gc, RecordTypeDescriptor<'gc>> {
        self.rtd.clone()
    }

    pub fn protocol(&self) -> Value<'gc> {
        self.protocol.clone()
    }

    pub fn custom_protocol(&self) -> bool {
        self.custom_protocol.get()
    }

    pub fn parent(&self) -> Option<Gc<'gc, Self>> {
        self.parent.clone()
    }
}

#[repr(C)]
pub struct Record<'gc> {
    header: ScmHeader,
    pub rtd: Gc<'gc, RecordTypeDescriptor<'gc>>,
    pub slots: [Lock<Value<'gc>>; 0],
}

impl<'gc> Record<'gc> {
    pub const VT: &'static VTable = &VTable {
        alignment: std::mem::align_of::<Lock<Value<'static>>>(),
        trace: |object, visitor| unsafe {
            let record = object.to_address().as_mut_ref::<Record>();

            let data = record.slots.as_mut_ptr();

            for i in 0..record.rtd.total_field_count() {
                let slot = data.add(i);
                slot.as_mut().unwrap().get_mut().trace(visitor);
            }
        },
        weak_proc: |_, _| {},
        instance_size: 0,
        compute_size: Some(|object| unsafe {
            let record = object.to_address().as_ref::<Record>();

            std::mem::size_of::<Record>()
                + (record.rtd.total_field_count() * std::mem::size_of::<Lock<Value>>())
        }),
        compute_alignment: None,
    };
}
