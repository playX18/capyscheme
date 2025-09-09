use rsgc::{Gc, Trace, barrier::Unlock, cell::Lock, object::VTable};
use std::{alloc::Layout, cmp::Ordering};

use crate::{
    global, native_fn,
    runtime::{
        Context,
        modules::define,
        value::{ByteVector, IntoValue, ScmHeader, Str, Symbol, Tagged, TypeCode8, Value},
    },
};

pub const VTABLE_BASE_LAYOUT: &str = "pwuhuhpwphuhphuh";

pub const VTABLE_INDEX_LAYOUT: usize = 0;
pub const VTABLE_INDEX_FLAGS: usize = 1;
pub const VTABLE_INDEX_INSTANCE_FINALIZE: usize = 2;
pub const VTABLE_INDEX_INSTANCE_PRINTER: usize = 3;
pub const VTABLE_INDEX_NAME: usize = 4;
pub const VTABLE_INDEX_SIZE: usize = 5;
pub const VTABLE_INDEX_UNBOXED_FIELDS: usize = 6;
pub const VTABLE_INDEX_RESERVED_7: usize = 7;
pub const VTABLE_OFFSET_USER: usize = 8;

pub const VTABLE_APPLICABLE_BASE_LAYOUT: &str = "pw";
pub const VTABLE_APPLICABLE_WITH_SETTER_LAYOUT: &str = "pwpw";

pub const APPLICABLE_STRUCT_INDEX_PROCEDURE: usize = 0;
pub const APPLICABLE_STRUCT_INDEX_SETTER: usize = 1;

pub const VTABLE_FLAG_VALIDATED: u64 = 1u64 << 0;
pub const VTABLE_FLAG_VTABLE: u64 = 1u64 << 1;
pub const VTABLE_FLAG_APPLICABLE_VTABLE: u64 = 1u64 << 2;
pub const VTABLE_FLAG_APPLICABLE: u64 = 1u64 << 3;
pub const VTABLE_FLAG_SETTER_VTABLE: u64 = 1u64 << 4;
pub const VTABLE_FLAG_SETTER: u64 = 1u64 << 5;
pub const VTABLE_FLAG_RESERVED_0: u64 = 1u64 << 6;
pub const VTABLE_FLAG_RESERVED_1: u64 = 1u64 << 7;
pub const VTABLE_FLAG_SMOB_0: u64 = 1u64 << 8;
pub const VTABLE_FLAG_GOOPS_0: u64 = 1u64 << 9;
pub const VTABLE_FLAG_GOOPS_1: u64 = 1u64 << 10;
pub const VTABLE_FLAG_GOOPS_2: u64 = 1u64 << 11;
pub const VTABLE_FLAG_GOOPS_3: u64 = 1u64 << 12;
pub const VTABLE_FLAG_GOOPS_4: u64 = 1u64 << 13;
pub const VTABLE_FLAG_RESERVED_2: u64 = 1u64 << 14;
pub const VTABLE_FLAG_RESERVED_3: u64 = 1u64 << 15;
pub const VTABLE_USER_FLAG_SHIFT: u32 = 16;

impl<'gc> Value<'gc> {
    pub fn is_struct(&self) -> bool {
        self.is_cell() && self.has_typ8(TypeCode8::RECORD)
    }
}

pub type StructRef<'gc> = Gc<'gc, Struct<'gc>>;

#[repr(C)]
pub struct Struct<'gc> {
    pub(crate) header: ScmHeader,
    pub(crate) vtable: StructRef<'gc>,
    pub(crate) slots: [Lock<Value<'gc>>; 0],
}

unsafe impl<'gc> Trace for Struct<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        let nfields = self.vtable.vtable_size();

        visitor.trace(&mut self.vtable);

        for i in 0..nfields {
            unsafe {
                if self.vtable.vtable_is_boxed_field(i) {
                    let ptr = (self.slots.as_ptr() as *mut Lock<Value<'gc>>).add(i);
                    visitor.trace(&mut *ptr);
                }
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> Struct<'gc> {
    pub const VT: &'static VTable = &VTable {
        alignment: align_of::<Value>(),
        compute_alignment: None,
        compute_size: Some(|object| unsafe {
            let s = object.to_address().as_ref::<Struct>();

            (s.size() * size_of::<Value>()) + size_of::<Struct>()
        }),
        trace: |object, visitor| unsafe {
            object.to_address().as_mut_ref::<Struct>().trace(visitor);
        },
        weak_proc: |_, _| {},
        instance_size: 0,
    };

    pub fn equal(self: StructRef<'gc>, other: StructRef<'gc>) -> bool {
        if !Gc::ptr_eq(self.vtable, other.vtable) {
            return false;
        }

        let size = self.size();

        for i in 0..size {
            let f1 = self.data_ref(i);
            let f2 = self.data_ref(i);

            if f1 != f2 {
                if !self.vtable_is_boxed_field(i) {
                    return false;
                }

                if f1 != self.as_ptr() as u64 && f2 != other.as_ptr() as u64 {
                    let v1 = self.slot_ref(i);
                    let v2 = self.slot_ref(i);

                    if !v1.r5rs_equal(v2) {
                        return false;
                    }
                }
            }
        }

        true
    }

    pub(crate) fn new(ctx: Context<'gc>, vtable: StructRef<'gc>, size: usize) -> Gc<'gc, Self> {
        let this = ctx.allocate_with_layout::<Self>(
            unsafe {
                Layout::from_size_align_unchecked(
                    size_of::<Self>() + (size * size_of::<Value>()),
                    align_of::<Value>(),
                )
            },
            Self::VT,
            rsgc::mmtk::AllocationSemantics::Default,
        );
        unsafe {
            let ptr = this.as_gcobj().to_address().to_mut_ptr::<Struct>();
            ptr.write(Self {
                header: ScmHeader::with_type_bits(TypeCode8::RECORD.bits() as _),
                vtable,
                slots: [],
            });

            let slots_ptr = (*ptr).slots.as_ptr() as *mut Lock<Value<'gc>>;
            for i in 0..size {
                slots_ptr.add(i).write(Lock::new(Value::undefined()));
            }

            this.assume_init()
        }
    }

    pub(crate) fn init(
        self: StructRef<'gc>,
        ctx: Context<'gc>,
        layout: Gc<'gc, Symbol<'gc>>,
        inits: impl AsRef<[Value<'gc>]>,
    ) {
        let nfields = self.size();

        let mut inits_idx = 0;
        let inits = inits.as_ref();
        for n in 0..nfields {
            if inits_idx == inits.len() || layout.get(n * 2 + 1).unwrap() == 'h' {
                if self.vtable.vtable_is_boxed_field(n) {
                    self.slot_set(ctx, n, Value::new(false));
                } else {
                    self.data_set(n, 0);
                }
            } else {
                let val = inits[inits_idx];
                inits_idx += 1;
                if self.vtable.vtable_is_boxed_field(n) {
                    self.slot_set(ctx, n, val);
                } else {
                    let Some(num) = val.number() else { todo!() };

                    let u64 = num.coerce_exact_integer_to_u64();

                    self.data_set(n, u64);
                }
            }
        }
    }

    pub fn is_vtable(&self) -> bool {
        self.vtable.vtable_flag_is_set(VTABLE_FLAG_VTABLE)
            && self.vtable.vtable_flag_is_set(VTABLE_FLAG_VALIDATED)
    }

    fn inherit_vtable_magic(
        self: StructRef<'gc>,
        ctx: Context<'gc>,
        vtable: StructRef<'gc>,
    ) -> Result<(), Value<'gc>> {
        if !is_valid_vtable_layout(vtable.vtable_layout().downcast()) {
            todo!()
        }

        set_vtable_access_fields(ctx, self);

        let olayout = self.vtable_layout().downcast::<Symbol>().to_str(&ctx);

        let req = required_vtable_fields(ctx);
        if req.len() <= olayout.len()
            && olayout.compare(req, false, 0, req.len(), 0, req.len()) == Some(Ordering::Equal)
        {
            self.vtable_set_flags(VTABLE_FLAG_VTABLE);
        }

        self.vtable_set_flags(VTABLE_FLAG_VALIDATED);

        Ok(())
    }

    pub fn slot_ref(&self, index: usize) -> Value<'gc> {
        assert!(index < self.slot_count());
        unsafe { self.slots.as_ptr().add(index).read().get() }
    }

    pub fn slot_set(self: StructRef<'gc>, ctx: Context<'gc>, index: usize, value: Value<'gc>) {
        assert!(index < self.slot_count());
        unsafe {
            let _ = Gc::write(&ctx, self);
            self.slots
                .as_ptr()
                .add(index)
                .as_ref()
                .unwrap()
                .unlock_unchecked()
                .set(value);
        }
    }

    pub fn slot_count(&self) -> usize {
        unsafe {
            self.vtable
                .slots
                .as_ptr()
                .add(VTABLE_INDEX_SIZE)
                .cast::<usize>()
                .read()
        }
    }

    pub fn data_ref(&self, index: usize) -> u64 {
        assert!(index < self.slot_count());
        unsafe { self.slots.as_ptr().add(index).cast::<u64>().read() }
    }

    pub fn data_set(&self, index: usize, value: u64) {
        assert!(index < self.slot_count());
        unsafe { (self.slots.as_ptr() as *mut u64).add(index).write(value) }
    }

    pub fn vtable_unboxed_fields(&self) -> &'gc [u8] {
        let bv = self.slot_ref(VTABLE_INDEX_UNBOXED_FIELDS);
        if bv.is::<ByteVector>() {
            bv.downcast::<ByteVector>().as_slice()
        } else {
            &[]
        }
    }

    pub fn vtable_is_boxed_field(&self, ix: usize) -> bool {
        let bv = self.vtable_unboxed_fields();
        bv[ix / 8] & (1 << (ix % 8)) == 0
    }

    pub fn vtable_size(&self) -> usize {
        self.data_ref(VTABLE_INDEX_SIZE) as usize
    }

    pub fn vtable_layout(&self) -> Value<'gc> {
        self.slot_ref(VTABLE_INDEX_LAYOUT)
    }

    pub fn vtable(&self) -> StructRef<'gc> {
        self.vtable
    }

    pub fn size(&self) -> usize {
        self.vtable.vtable_size()
    }

    pub fn flags(&self) -> u64 {
        self.vtable.vtable_flags()
    }

    pub fn vtable_flags(&self) -> u64 {
        self.vtable.data_ref(VTABLE_INDEX_FLAGS)
    }

    pub fn vtable_set_flags(&self, flags: u64) {
        let old = self.vtable_flags();
        self.vtable.data_set(VTABLE_INDEX_FLAGS, old | flags);
    }

    pub fn vtable_flag_is_set(&self, flag: u64) -> bool {
        self.vtable_flags() & flag != 0
    }

    pub fn is_applicable(&self) -> bool {
        self.vtable_flag_is_set(VTABLE_FLAG_APPLICABLE)
    }

    pub fn is_setter(&self) -> bool {
        self.vtable_flag_is_set(VTABLE_FLAG_SETTER)
    }

    pub fn procedure(&self) -> Value<'gc> {
        assert!(self.is_applicable());
        self.slot_ref(APPLICABLE_STRUCT_INDEX_PROCEDURE)
    }

    pub fn setter(&self) -> Value<'gc> {
        assert!(self.is_setter());
        self.slot_ref(APPLICABLE_STRUCT_INDEX_SETTER)
    }

    pub fn set_procedure(self: StructRef<'gc>, ctx: Context<'gc>, proc: Value<'gc>) {
        assert!(self.is_applicable());
        self.slot_set(ctx, APPLICABLE_STRUCT_INDEX_PROCEDURE, proc);
    }

    pub fn set_setter(self: StructRef<'gc>, ctx: Context<'gc>, setter: Value<'gc>) {
        assert!(self.is_setter());
        self.slot_set(ctx, APPLICABLE_STRUCT_INDEX_SETTER, setter);
    }

    pub fn vtable_name(&self) -> Value<'gc> {
        self.slot_ref(VTABLE_INDEX_NAME)
    }

    pub fn name(&self) -> Value<'gc> {
        self.vtable.vtable_name()
    }
}

pub fn make_struct_layout<'gc>(
    ctx: Context<'gc>,
    fields: Gc<'gc, Str<'gc>>,
) -> Result<Value<'gc>, Value<'gc>> {
    if fields.len() % 2 != 0 {
        todo!()
    }

    let mut i = 0;
    while i < fields.len() {
        let typ = fields.get(i).unwrap();
        let spec = fields.get(i + 1).unwrap();

        if !matches!(typ, 'u' | 'p') {
            todo!()
        }

        if !matches!(spec, 'w' | 'h' | 'r') {
            todo!()
        }

        i += 2;
    }

    Ok(Symbol::from_string(ctx, fields).into())
}

pub fn is_valid_vtable_layout<'gc>(fields: Gc<'gc, Symbol<'gc>>) -> bool {
    if fields.len() % 2 != 0 {
        return false;
    }

    let mut i = 0;
    while i < fields.len() {
        let c = fields.get(i).unwrap();
        match c {
            'u' | 'p' => match fields.get(i + 1) {
                Some('w') | Some('h') => {}
                _ => return false,
            },

            _ => return false,
        }

        i += 2;
    }

    true
}

pub fn make_vtable_vtable<'gc>(
    ctx: Context<'gc>,
    fields: Gc<'gc, Str<'gc>>,
) -> Result<Value<'gc>, Value<'gc>> {
    let layout = make_struct_layout(ctx, fields)?.downcast::<Symbol>();

    if !is_valid_vtable_layout(layout) {
        todo!()
    }

    let nfields = layout.len() / 2;
    unsafe {
        let s = ctx.allocate_with_layout::<Struct>(
            Layout::from_size_align_unchecked(
                size_of::<Struct>() + (nfields * size_of::<Value>()),
                align_of::<Value>(),
            ),
            &Struct::VT,
            rsgc::mmtk::AllocationSemantics::Default,
        );

        let ptr = s.as_gcobj().to_address().to_mut_ptr::<Struct>();

        ptr.write(Struct {
            header: ScmHeader::with_type_bits(TypeCode8::RECORD.bits() as _),
            vtable: s.assume_init(),
            slots: [],
        });

        let s = s.assume_init();
        (s.slots.as_ptr() as *mut u64)
            .add(VTABLE_INDEX_SIZE)
            .write(nfields as _);
        s.slot_set(ctx, VTABLE_INDEX_LAYOUT, layout.into());
        set_vtable_access_fields(ctx, s);
        s.vtable_set_flags(VTABLE_FLAG_VTABLE | VTABLE_FLAG_VALIDATED);
        s.data_set(VTABLE_INDEX_INSTANCE_FINALIZE, 0);
        s.slot_set(ctx, VTABLE_INDEX_INSTANCE_PRINTER, Value::new(false));
        s.slot_set(ctx, VTABLE_INDEX_NAME, Value::new(false));
        s.data_set(VTABLE_INDEX_RESERVED_7, 0);

        for n in VTABLE_OFFSET_USER..nfields {
            if s.vtable_is_boxed_field(n) {
                s.data_set(n, 0);
            } else {
                s.slot_set(ctx, n, Value::new(false));
            }
        }

        Ok(s.into())
    }
}

fn set_vtable_access_fields<'gc>(ctx: Context<'gc>, vt: StructRef<'gc>) {
    let layout = vt.vtable_layout().downcast::<Symbol>();

    let nfields = layout.len() / 2;

    let bitvec_size = (nfields + 7) / 8;
    let bv = ByteVector::new::<true>(&ctx, bitvec_size);

    let mut field = 0;
    while field < nfields {
        let typ = layout.get(field * 2).unwrap();
        if typ == 'u' {
            unsafe {
                bv.as_slice_mut_unchecked()[field / 8] |= 1 << (field % 8);
            }
        }
        field += 1;
    }

    vt.vtable_set_flags(0);
    vt.data_set(VTABLE_INDEX_SIZE, nfields as _);
    vt.slot_set(ctx, VTABLE_INDEX_UNBOXED_FIELDS, bv.into());
}

unsafe impl<'gc> Tagged for Struct<'gc> {
    const TC8: TypeCode8 = TypeCode8::RECORD;
    const TYPE_NAME: &'static str = "struct";
}

global!(
    REQUIRED_VTABLE_FIELDS<'gc>: Gc<'gc, Str<'gc>> = (ctx) Str::new(&ctx, VTABLE_BASE_LAYOUT, true);
    REQUIRED_APPLICABLE_FIELDS<'gc>: Gc<'gc, Str<'gc>> = (ctx) Str::new(&ctx, VTABLE_APPLICABLE_BASE_LAYOUT, true);
    REQUIRED_APPLICABLE_WITH_SETTER_FIELDS<'gc>: Gc<'gc, Str<'gc>> = (ctx) Str::new(&ctx, VTABLE_APPLICABLE_WITH_SETTER_LAYOUT, true);

    STANDARD_VTABLE<'gc>: StructRef<'gc> = (ctx) make_vtable_vtable(ctx, *required_vtable_fields(ctx)).unwrap().downcast();

);

pub fn init_structs<'gc>(ctx: Context<'gc>) {
    define(
        ctx,
        "standard-vtable-fields",
        (*required_vtable_fields(ctx)).into(),
    );

    let standard_vtable = *standard_vtable(ctx);
    let name = Symbol::from_str(ctx, "<standard-vtable>");
    standard_vtable.slot_set(ctx, VTABLE_INDEX_NAME, name.into());

    define(ctx, "<standard-vtable>", standard_vtable.into());

    register_struct_fns(ctx);

    define(
        ctx,
        "vtable-offset-user",
        VTABLE_OFFSET_USER.into_value(ctx),
    );
}

pub fn make_struct<'gc>(
    ctx: Context<'gc>,
    vtable: StructRef<'gc>,
    init: impl AsRef<[Value<'gc>]>,
) -> Result<StructRef<'gc>, Value<'gc>> {
    if !vtable.vtable_flag_is_set(VTABLE_FLAG_VTABLE) {
        todo!()
    }

    let size = vtable.vtable_size();

    let init = init.as_ref();
    let s = Struct::new(ctx, vtable, size);

    s.init(ctx, vtable.vtable_layout().downcast(), init);

    if vtable.vtable_flag_is_set(VTABLE_FLAG_VTABLE) && s.vtable_layout() != Value::new(false) {
        s.inherit_vtable_magic(ctx, vtable)?;
    }

    Ok(s)
}

native_fn!(
    register_struct_fns:

    pub ("allocate-struct") fn allocate_struct<'gc>(nctx, vtable: StructRef<'gc>, nfields: usize) -> Result<StructRef<'gc>, Value<'gc>> {
        if !vtable.is_vtable() {
            todo!()
        }

        if nfields != vtable.vtable_size() {
            todo!()
        }

        let ret = Struct::new(nctx.ctx, vtable, nfields);
        ret.init(nctx.ctx, vtable.vtable_layout().downcast(), &[]);

        nctx.return_(Ok(ret))
    }

    pub ("make-struct/simple") fn make_simple_struct<'gc>(nctx, vtable: StructRef<'gc>, init: &'gc [Value<'gc>]) -> Result<StructRef<'gc>, Value<'gc>> {
        if !vtable.is_vtable() {
            todo!()
        }
        println!("vtable_size={}, init.len()={}", vtable.vtable_size(), init.len());
        if vtable.vtable_size() != init.len() {
            todo!()
        }

        let ret = Struct::new(nctx.ctx, vtable, init.len());
        for i in 0..init.len() {
            if !vtable.vtable_is_boxed_field(i) {
                todo!()
            }

            ret.slot_set(nctx.ctx, i, init[i]);
        }

        nctx.return_(Ok(ret))
    }

    pub ("make-struct") fn make_struct_scm<'gc>(nctx, vtable: StructRef<'gc>, init: &'gc [Value<'gc>]) -> Result<StructRef<'gc>, Value<'gc>> {
        if !vtable.is_vtable() {
            todo!()
        }

        let ret = Struct::new(nctx.ctx, vtable, vtable.vtable_size());

        ret.init(nctx.ctx, vtable.vtable_layout().downcast(), init);

        nctx.return_(Ok(ret))
    }

    pub ("make-vtable") fn make_vtable<'gc>(nctx, fields: Gc<'gc, Str<'gc>>, printer: Option<Value<'gc>>) -> Result<StructRef<'gc>, Value<'gc>> {
        let printer = printer.unwrap_or(Value::new(false));

        let ctx = nctx.ctx;
        let layout = match make_struct_layout(ctx, fields) {
            Ok(layout) => layout,
            Err(err) => return nctx.return_(Err(err)),
        };

        nctx.return_(make_struct(ctx, *standard_vtable(ctx), &[layout.into(), printer]))
    }

    pub ("struct-ref") fn struct_ref<'gc>(nctx, s: StructRef<'gc>, index: usize) -> Result<Value<'gc>, Value<'gc>> {
        if index >= s.size() {
            todo!()
        }
        if !s.vtable_is_boxed_field(index) {
            todo!()
        }
        nctx.return_(Ok(s.slot_ref(index)))
    }

    pub ("struct-set!") fn struct_set<'gc>(nctx, s: StructRef<'gc>, index: usize, value: Value<'gc>) -> Result<(), Value<'gc>> {
        if index >= s.size() {
            todo!()
        }
        if !s.vtable_is_boxed_field(index) {
            todo!()
        }
        s.slot_set(nctx.ctx, index, value);
        nctx.return_(Ok(()))
    }

    pub ("struct-ref/unboxed") fn struct_ref_unboxed<'gc>(nctx, s: StructRef<'gc>, index: usize) -> Result<u64, Value<'gc>> {
        if index >= s.size() {
            todo!()
        }
        if s.vtable_is_boxed_field(index) {
            todo!()
        }
        nctx.return_(Ok(s.data_ref(index)))
    }

    pub ("struct-set!/unboxed") fn struct_set_unboxed<'gc>(nctx, s: StructRef<'gc>, index: usize, value: u64) -> Result<(), Value<'gc>> {
        if index >= s.size() {
            todo!()
        }
        if s.vtable_is_boxed_field(index) {
            todo!()
        }
        s.data_set(index, value);
        nctx.return_(Ok(()))
    }

    pub ("struct?") fn is_struct<'gc>(nctx, val: Value<'gc>) -> bool {
        println!("is_struct? {val}={}", val.is::<Struct>());
        nctx.return_(val.is::<Struct>())
    }

    pub ("struct-vtable") fn struct_vtable<'gc>(nctx, s: StructRef<'gc>) -> Result<StructRef<'gc>, Value<'gc>> {
        nctx.return_(Ok(s.vtable()))
    }

    pub ("struct-vtable-name") fn struct_vtable_name<'gc>(nctx, s: StructRef<'gc>) -> Result<Value<'gc>, Value<'gc>> {
        if !s.is_vtable() {
            todo!()
        }
        nctx.return_(Ok(s.vtable_name()))
    }

    pub ("set-struct-vtable-name!") fn set_struct_vtable_name<'gc>(nctx, s: StructRef<'gc>, name: Gc<'gc, Symbol<'gc>>) -> Result<(), Value<'gc>> {
        if !s.is_vtable() {
            todo!()
        }
        s.slot_set(nctx.ctx, VTABLE_INDEX_NAME, name.into());
        nctx.return_(Ok(()))
    }



);
