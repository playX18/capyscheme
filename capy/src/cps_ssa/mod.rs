use std::{
    collections::{HashMap, HashSet},
    io::{self, Cursor, Read, Write},
};

use capy_derive::Trace;

use crate::{
    cps::{packed::Primitive, term::BranchHint},
    rsgc::{
        Gc,
        alloc::{Array, array::ArrayRef},
    },
    runtime::{
        Context,
        fasl::{FASLReader, FASLWriter},
        value::{Str, Symbol, Value, Vector},
    },
};

mod lower;
pub mod pretty;

pub use lower::{LowerError, lower_cps, lower_reified};

pub const FORMAT_MAGIC: [u8; 4] = *b"CSSA";
pub const FORMAT_VERSION: u32 = 4;

pub type Procs<'gc> = ArrayRef<'gc, Proc<'gc>>;
pub type Blocks<'gc> = ArrayRef<'gc, Block<'gc>>;
pub type Captures<'gc> = ArrayRef<'gc, Capture>;
pub type BlockParams<'gc> = ArrayRef<'gc, BlockParam>;
pub type Insts<'gc> = ArrayRef<'gc, Inst<'gc>>;
pub type ValueIds<'gc> = ArrayRef<'gc, ValueId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default, Trace)]
#[collect(no_drop)]
pub struct ProcId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default, Trace)]
#[collect(no_drop)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default, Trace)]
#[collect(no_drop)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default, Trace)]
#[collect(no_drop)]
pub struct ConstId(pub u32);

impl ProcId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl BlockId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl ValueId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl ConstId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum ProcKind {
    Function,
    Continuation,
}

#[derive(Debug, Clone, Copy, Trace)]
#[collect(no_drop)]
pub enum ImmediateConst {
    Null,
    Bool(bool),
    Int32(i32),
    Flonum(f64),
    Char(char),
    Undefined,
    Unspecified,
    Void,
    Eof,
    BrokenWeakPointer,
}

impl PartialEq for ImmediateConst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null)
            | (Self::Undefined, Self::Undefined)
            | (Self::Unspecified, Self::Unspecified)
            | (Self::Void, Self::Void)
            | (Self::Eof, Self::Eof)
            | (Self::BrokenWeakPointer, Self::BrokenWeakPointer) => true,
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Int32(lhs), Self::Int32(rhs)) => lhs == rhs,
            (Self::Flonum(lhs), Self::Flonum(rhs)) => lhs.to_bits() == rhs.to_bits(),
            (Self::Char(lhs), Self::Char(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Eq for ImmediateConst {}

impl ImmediateConst {
    pub fn from_value<'gc>(value: Value<'gc>) -> Option<Self> {
        if value.is_null() {
            Some(Self::Null)
        } else if value.is_bool() {
            Some(Self::Bool(value.as_bool()))
        } else if value.is_int32() {
            Some(Self::Int32(value.as_int32()))
        } else if value.is_flonum() {
            Some(Self::Flonum(value.as_flonum()))
        } else if value.is_char() {
            Some(Self::Char(value.char()))
        } else if value == Value::undefined() {
            Some(Self::Undefined)
        } else if value.is_unspecified() {
            Some(Self::Unspecified)
        } else if value.is_void() {
            Some(Self::Void)
        } else if value.is_eof() {
            Some(Self::Eof)
        } else if value.is_bwp() {
            Some(Self::BrokenWeakPointer)
        } else {
            None
        }
    }

    pub fn to_value<'gc>(self) -> Value<'gc> {
        match self {
            Self::Null => Value::null(),
            Self::Bool(value) => Value::new(value),
            Self::Int32(value) => Value::new(value),
            Self::Flonum(value) => Value::new(value),
            Self::Char(value) => Value::new(value),
            Self::Undefined => Value::undefined(),
            Self::Unspecified => Value::unspecified(),
            Self::Void => Value::void(),
            Self::Eof => Value::eof(),
            Self::BrokenWeakPointer => Value::bwp(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum ConstRef {
    Inline(ImmediateConst),
    Pool(ConstId),
}

pub(crate) enum ClassifiedConst<'gc> {
    Inline(ImmediateConst),
    Heap(Value<'gc>),
}

pub(crate) fn classify_const_value<'gc>(value: Value<'gc>) -> ClassifiedConst<'gc> {
    match ImmediateConst::from_value(value) {
        Some(value) => ClassifiedConst::Inline(value),
        None => ClassifiedConst::Heap(value),
    }
}

#[derive(Clone, Trace)]
#[collect(no_drop)]
pub struct ConstPool<'gc> {
    pub entries: Gc<'gc, Vector<'gc>>,
}

impl<'gc> ConstPool<'gc> {
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.len() == 0
    }

    pub fn get(&self, id: ConstId) -> Option<Value<'gc>> {
        self.entries.as_slice().get(id.index()).copied()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct Capture {
    pub index: u32,
    pub binding_name: ConstRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct BlockParam {
    pub value: ValueId,
    pub name: Option<ConstRef>,
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct Inst<'gc> {
    pub result: Option<ValueId>,
    pub source: Option<ConstRef>,
    pub kind: InstKind<'gc>,
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum InstKind<'gc> {
    Const(ConstRef),
    PackRest {
        items: ValueIds<'gc>,
    },
    PrimCall {
        prim: Primitive,
        args: ValueIds<'gc>,
    },
    RestRef {
        rest: ValueId,
        index: u32,
    },
    RestLength {
        rest: ValueId,
        skip: u32,
    },
    RestToList {
        rest: ValueId,
        skip: u32,
    },
    RestIsEmpty {
        rest: ValueId,
        skip: u32,
    },
    RestIsNotEmpty {
        rest: ValueId,
        skip: u32,
    },
    MakeClosure {
        proc: ProcId,
        is_cont: bool,
        meta: Option<ConstRef>,
        slots: u32,
    },
    InitClosure {
        closure: ValueId,
        captures: ValueIds<'gc>,
    },
    ClosureRef {
        closure: ValueId,
        index: u32,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum TailTarget {
    Direct { proc: ProcId, closure: ValueId },
    Indirect { closure: ValueId },
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub enum Terminator<'gc> {
    Jump {
        block: BlockId,
        args: ValueIds<'gc>,
    },
    Branch {
        cond: ValueId,
        then_block: BlockId,
        then_args: ValueIds<'gc>,
        else_block: BlockId,
        else_args: ValueIds<'gc>,
        hints: [BranchHint; 2],
    },
    TailContinue {
        callee: TailTarget,
        args: ValueIds<'gc>,
        source: Option<ConstRef>,
    },
    TailApp {
        callee: TailTarget,
        retk: ValueId,
        args: ValueIds<'gc>,
        source: Option<ConstRef>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct Block<'gc> {
    pub id: BlockId,
    pub name: Option<ConstRef>,
    pub params: BlockParams<'gc>,
    pub insts: Insts<'gc>,
    pub term: Terminator<'gc>,
    pub cold: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Trace)]
#[collect(no_drop)]
pub struct Proc<'gc> {
    pub id: ProcId,
    pub kind: ProcKind,
    pub binding_name: ConstRef,
    pub display_name: Option<ConstRef>,
    pub source: Option<ConstRef>,
    pub meta: Option<ConstRef>,
    pub cold: bool,
    pub noinline: bool,
    pub entry: BlockId,
    pub self_param: ValueId,
    pub retk_param: Option<ValueId>,
    pub arg_params: ValueIds<'gc>,
    pub rest_param: Option<ValueId>,
    pub captures: Captures<'gc>,
    pub blocks: Blocks<'gc>,
}

#[derive(Clone, Trace)]
#[collect(no_drop)]
pub struct Module<'gc> {
    pub format_version: u32,
    pub entry: ProcId,
    pub procs: Procs<'gc>,
    pub const_pool: ConstPool<'gc>,
}

#[derive(Debug)]
pub enum SerializationError {
    Io(io::Error),
    BadMagic([u8; 4]),
    BadVersion(u32),
    InvalidConstPool(&'static str),
    InvalidTag { kind: &'static str, tag: u8 },
    MalformedVarint,
    InvalidModule(String),
}

impl std::fmt::Display for SerializationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(err) => write!(f, "{err}"),
            Self::BadMagic(magic) => write!(f, "invalid CPS-SSA magic: {magic:?}"),
            Self::BadVersion(version) => write!(f, "unsupported CPS-SSA version: {version}"),
            Self::InvalidConstPool(message) => write!(f, "{message}"),
            Self::InvalidTag { kind, tag } => write!(f, "invalid {kind} tag: {tag}"),
            Self::MalformedVarint => f.write_str("malformed varint in CPS-SSA stream"),
            Self::InvalidModule(message) => f.write_str(message),
        }
    }
}

impl std::error::Error for SerializationError {}

impl From<io::Error> for SerializationError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<ValidationError> for SerializationError {
    fn from(value: ValidationError) -> Self {
        Self::InvalidModule(value.message)
    }
}

#[derive(Debug)]
pub enum ConstDecodeError {
    MissingConst(ConstId),
}

impl std::fmt::Display for ConstDecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingConst(id) => write!(f, "missing const {:?}", id),
        }
    }
}

impl std::error::Error for ConstDecodeError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
    pub message: String,
}

impl ValidationError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ValidationError {}

impl<'gc> Module<'gc> {
    pub fn write_to_writer<W: Write>(
        &self,
        ctx: Context<'gc>,
        mut writer: W,
    ) -> Result<(), SerializationError> {
        writer.write_all(&FORMAT_MAGIC)?;
        writer.write_all(&self.format_version.to_le_bytes())?;
        write_varint(&mut writer, self.entry.0 as u64)?;
        write_varint(&mut writer, self.procs.len() as u64)?;
        for proc in self.procs.iter() {
            write_proc(&mut writer, proc)?;
        }
        let consts_fasl = encode_const_vector(ctx, self.const_pool.entries)?;
        write_bytes(&mut writer, &consts_fasl)?;
        Ok(())
    }

    pub fn read_from_reader<R: Read>(
        ctx: Context<'gc>,
        mut reader: R,
    ) -> Result<Gc<'gc, Self>, SerializationError> {
        let mut magic = [0; 4];
        reader.read_exact(&mut magic)?;
        if magic != FORMAT_MAGIC {
            return Err(SerializationError::BadMagic(magic));
        }

        let mut version = [0; 4];
        reader.read_exact(&mut version)?;
        let version = u32::from_le_bytes(version);
        if version != FORMAT_VERSION {
            return Err(SerializationError::BadVersion(version));
        }

        let entry = ProcId(read_varint_u32(&mut reader)?);
        let proc_count = read_varint_usize(&mut reader)?;
        let mut procs = Vec::with_capacity(proc_count);
        for _ in 0..proc_count {
            procs.push(read_proc(ctx, &mut reader)?);
        }

        let fasl_bytes = read_bytes(&mut reader)?;
        let module = Gc::new(
            *ctx,
            Self {
                format_version: version,
                entry,
                procs: Array::from_slice(*ctx, &procs),
                const_pool: ConstPool {
                    entries: decode_const_vector(ctx, &fasl_bytes)?,
                },
            },
        );
        module.validate()?;
        Ok(module)
    }

    pub fn decode_const(&self, const_ref: ConstRef) -> Result<Value<'gc>, ConstDecodeError> {
        match const_ref {
            ConstRef::Inline(value) => Ok(value.to_value()),
            ConstRef::Pool(id) => self
                .const_pool
                .get(id)
                .ok_or(ConstDecodeError::MissingConst(id)),
        }
    }

    pub fn validate(&self) -> Result<(), ValidationError> {
        let proc_index = self
            .procs
            .iter()
            .map(|proc| (proc.id, proc))
            .collect::<HashMap<_, _>>();

        if !proc_index.contains_key(&self.entry) {
            return Err(ValidationError::new(format!(
                "entry proc {:?} is not present in module",
                self.entry
            )));
        }

        for proc in self.procs.iter() {
            validate_name_ref(self, proc.binding_name, "proc binding_name")?;
            if let Some(display_name) = proc.display_name {
                validate_const_ref(display_name, self.const_pool.len())?;
            }
            if let Some(source) = proc.source {
                validate_const_ref(source, self.const_pool.len())?;
            }
            if let Some(meta) = proc.meta {
                validate_const_ref(meta, self.const_pool.len())?;
            }
            for capture in proc.captures.iter() {
                validate_name_ref(self, capture.binding_name, "capture binding_name")?;
            }

            let block_index = proc
                .blocks
                .iter()
                .map(|block| (block.id, block))
                .collect::<HashMap<_, _>>();

            let Some(entry) = block_index.get(&proc.entry) else {
                return Err(ValidationError::new(format!(
                    "proc {:?} entry block {:?} is missing",
                    proc.id, proc.entry
                )));
            };

            if let Some(name) = entry.name {
                validate_name_ref(self, name, "block name")?;
            }

            let entry_params = entry
                .params
                .iter()
                .map(|param| param.value)
                .collect::<Vec<_>>();
            if entry_params.first().copied() != Some(proc.self_param) {
                return Err(ValidationError::new(format!(
                    "proc {:?} self_param must be the first entry param",
                    proc.id
                )));
            }

            match proc.kind {
                ProcKind::Function => {
                    let Some(retk) = proc.retk_param else {
                        return Err(ValidationError::new(format!(
                            "function proc {:?} must have retk_param",
                            proc.id
                        )));
                    };
                    if !entry_params.contains(&retk) {
                        return Err(ValidationError::new(format!(
                            "function proc {:?} retk_param must be an entry param",
                            proc.id
                        )));
                    }
                }
                ProcKind::Continuation => {
                    if proc.retk_param.is_some() {
                        return Err(ValidationError::new(format!(
                            "continuation proc {:?} must not have retk_param",
                            proc.id
                        )));
                    }
                }
            }

            for arg in proc.arg_params.iter() {
                if !entry_params.contains(arg) {
                    return Err(ValidationError::new(format!(
                        "proc {:?} arg param {:?} must be owned by the entry block",
                        proc.id, arg
                    )));
                }
            }
            if let Some(rest) = proc.rest_param
                && !entry_params.contains(&rest)
            {
                return Err(ValidationError::new(format!(
                    "proc {:?} rest param {:?} must be owned by the entry block",
                    proc.id, rest
                )));
            }

            let mut defs = HashSet::new();
            for block in proc.blocks.iter() {
                if let Some(name) = block.name {
                    validate_name_ref(self, name, "block name")?;
                }
                for param in block.params.iter() {
                    if let Some(name) = param.name {
                        validate_name_ref(self, name, "block param name")?;
                    }
                    if !defs.insert(param.value) {
                        return Err(ValidationError::new(format!(
                            "proc {:?} defines SSA value {:?} more than once",
                            proc.id, param.value
                        )));
                    }
                }

                for inst in block.insts.iter() {
                    if let Some(source) = inst.source {
                        validate_const_ref(source, self.const_pool.len())?;
                    }
                    let needs_result = matches!(
                        inst.kind,
                        InstKind::Const(_)
                            | InstKind::PackRest { .. }
                            | InstKind::PrimCall { .. }
                            | InstKind::RestRef { .. }
                            | InstKind::RestLength { .. }
                            | InstKind::RestToList { .. }
                            | InstKind::RestIsEmpty { .. }
                            | InstKind::RestIsNotEmpty { .. }
                            | InstKind::MakeClosure { .. }
                            | InstKind::ClosureRef { .. }
                    );
                    if needs_result != inst.result.is_some() {
                        return Err(ValidationError::new(format!(
                            "proc {:?} has invalid instruction result shape in block {:?}",
                            proc.id, block.id
                        )));
                    }
                    if let Some(result) = inst.result
                        && !defs.insert(result)
                    {
                        return Err(ValidationError::new(format!(
                            "proc {:?} defines SSA value {:?} more than once",
                            proc.id, result
                        )));
                    }
                }
            }

            let uses_defined = |value: ValueId, defs: &HashSet<ValueId>| defs.contains(&value);

            for block in proc.blocks.iter() {
                for inst in block.insts.iter() {
                    match &inst.kind {
                        InstKind::RestIsEmpty { .. } | InstKind::RestIsNotEmpty { .. } => {}
                        InstKind::Const(const_ref) => {
                            validate_const_ref(*const_ref, self.const_pool.len())?;
                        }
                        InstKind::MakeClosure { meta, .. } => {
                            if let Some(meta) = meta {
                                validate_const_ref(*meta, self.const_pool.len())?;
                            }
                        }
                        InstKind::PackRest { items } => {
                            for item in items.iter() {
                                if !uses_defined(*item, &defs) {
                                    return Err(ValidationError::new(format!(
                                        "proc {:?} uses undefined value {:?}",
                                        proc.id, item
                                    )));
                                }
                            }
                        }
                        InstKind::PrimCall { args, .. } => {
                            for arg in args.iter() {
                                if !uses_defined(*arg, &defs) {
                                    return Err(ValidationError::new(format!(
                                        "proc {:?} uses undefined value {:?}",
                                        proc.id, arg
                                    )));
                                }
                            }
                        }
                        InstKind::RestRef { rest, .. }
                        | InstKind::RestLength { rest, .. }
                        | InstKind::RestToList { rest, .. } => {
                            if !uses_defined(*rest, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} uses undefined rest value {:?}",
                                    proc.id, rest
                                )));
                            }
                        }
                        InstKind::InitClosure { closure, captures } => {
                            if !uses_defined(*closure, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} uses undefined closure value {:?}",
                                    proc.id, closure
                                )));
                            }
                            for capture in captures.iter() {
                                if !uses_defined(*capture, &defs) {
                                    return Err(ValidationError::new(format!(
                                        "proc {:?} uses undefined capture value {:?}",
                                        proc.id, capture
                                    )));
                                }
                            }
                        }
                        InstKind::ClosureRef { closure, .. } => {
                            if !uses_defined(*closure, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} uses undefined closure value {:?}",
                                    proc.id, closure
                                )));
                            }
                        }
                    }
                }

                match &block.term {
                    Terminator::Jump {
                        block: target,
                        args,
                    } => {
                        let Some(target_block) = block_index.get(target) else {
                            return Err(ValidationError::new(format!(
                                "proc {:?} jumps to missing block {:?}",
                                proc.id, target
                            )));
                        };
                        if target_block.params.len() != args.len() {
                            return Err(ValidationError::new(format!(
                                "proc {:?} jumps to {:?} with wrong arity: expected {}, got {}",
                                proc.id,
                                target,
                                target_block.params.len(),
                                args.len()
                            )));
                        }
                        for arg in args.iter() {
                            if !uses_defined(*arg, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} jumps with undefined value {:?}",
                                    proc.id, arg
                                )));
                            }
                        }
                    }
                    Terminator::Branch {
                        cond,
                        then_block,
                        then_args,
                        else_block,
                        else_args,
                        ..
                    } => {
                        if !uses_defined(*cond, &defs) {
                            return Err(ValidationError::new(format!(
                                "proc {:?} branches on undefined value {:?}",
                                proc.id, cond
                            )));
                        }
                        let Some(then_target) = block_index.get(then_block) else {
                            return Err(ValidationError::new(format!(
                                "proc {:?} branches to missing block {:?}",
                                proc.id, then_block
                            )));
                        };
                        let Some(else_target) = block_index.get(else_block) else {
                            return Err(ValidationError::new(format!(
                                "proc {:?} branches to missing block {:?}",
                                proc.id, else_block
                            )));
                        };
                        if then_target.params.len() != then_args.len() {
                            return Err(ValidationError::new(format!(
                                "proc {:?} branches to {:?} with wrong arity: expected {}, got {}",
                                proc.id,
                                then_block,
                                then_target.params.len(),
                                then_args.len()
                            )));
                        }
                        if else_target.params.len() != else_args.len() {
                            return Err(ValidationError::new(format!(
                                "proc {:?} branches to {:?} with wrong arity: expected {}, got {}",
                                proc.id,
                                else_block,
                                else_target.params.len(),
                                else_args.len()
                            )));
                        }
                        for arg in then_args.iter().chain(else_args.iter()) {
                            if !uses_defined(*arg, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} branches with undefined value {:?}",
                                    proc.id, arg
                                )));
                            }
                        }
                    }
                    Terminator::TailContinue {
                        callee,
                        args,
                        source,
                    } => {
                        if let Some(source) = source {
                            validate_const_ref(*source, self.const_pool.len())?;
                        }
                        validate_tail_target(
                            proc.id,
                            callee,
                            &proc_index,
                            ProcKind::Continuation,
                            &defs,
                        )?;
                        for arg in args.iter() {
                            if !uses_defined(*arg, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} continues with undefined value {:?}",
                                    proc.id, arg
                                )));
                            }
                        }
                    }
                    Terminator::TailApp {
                        callee,
                        retk,
                        args,
                        source,
                    } => {
                        if let Some(source) = source {
                            validate_const_ref(*source, self.const_pool.len())?;
                        }
                        validate_tail_target(
                            proc.id,
                            callee,
                            &proc_index,
                            ProcKind::Function,
                            &defs,
                        )?;
                        if !uses_defined(*retk, &defs) {
                            return Err(ValidationError::new(format!(
                                "proc {:?} tail-apps with undefined retk {:?}",
                                proc.id, retk
                            )));
                        }
                        for arg in args.iter() {
                            if !uses_defined(*arg, &defs) {
                                return Err(ValidationError::new(format!(
                                    "proc {:?} tail-apps with undefined value {:?}",
                                    proc.id, arg
                                )));
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

pub(crate) fn const_ref_is_name<'gc>(module: &Module<'gc>, const_ref: ConstRef) -> bool {
    match module.decode_const(const_ref) {
        Ok(value) => value.try_as::<Str>().is_some() || value.try_as::<Symbol>().is_some(),
        Err(_) => false,
    }
}

fn validate_tail_target<'gc>(
    proc_id: ProcId,
    target: &TailTarget,
    proc_index: &HashMap<ProcId, &Proc<'gc>>,
    expected_kind: ProcKind,
    defs: &HashSet<ValueId>,
) -> Result<(), ValidationError> {
    match target {
        TailTarget::Direct { proc, closure } => {
            let Some(target_proc) = proc_index.get(proc) else {
                return Err(ValidationError::new(format!(
                    "proc {:?} tail-calls missing proc {:?}",
                    proc_id, proc
                )));
            };
            if target_proc.kind != expected_kind {
                return Err(ValidationError::new(format!(
                    "proc {:?} tail-calls proc {:?} with wrong kind",
                    proc_id, proc
                )));
            }
            if !defs.contains(closure) {
                return Err(ValidationError::new(format!(
                    "proc {:?} tail-calls with undefined closure value {:?}",
                    proc_id, closure
                )));
            }
        }
        TailTarget::Indirect { closure } => {
            if !defs.contains(closure) {
                return Err(ValidationError::new(format!(
                    "proc {:?} tail-calls with undefined closure value {:?}",
                    proc_id, closure
                )));
            }
        }
    }
    Ok(())
}

fn validate_name_ref<'gc>(
    module: &Module<'gc>,
    const_ref: ConstRef,
    what: &'static str,
) -> Result<(), ValidationError> {
    validate_const_ref(const_ref, module.const_pool.len())?;
    if !const_ref_is_name(module, const_ref) {
        return Err(ValidationError::new(format!(
            "{what} must resolve to a string or symbol"
        )));
    }
    Ok(())
}

fn validate_const_ref(const_ref: ConstRef, pool_len: usize) -> Result<(), ValidationError> {
    if let ConstRef::Pool(id) = const_ref
        && id.index() >= pool_len
    {
        return Err(ValidationError::new(format!(
            "const ref {:?} is out of bounds for const pool size {}",
            id, pool_len
        )));
    }
    Ok(())
}

pub fn write_to_writer<'gc, W: Write>(
    module: Gc<'gc, Module<'gc>>,
    ctx: Context<'gc>,
    writer: W,
) -> Result<(), SerializationError> {
    module.write_to_writer(ctx, writer)
}

pub fn read_from_reader<'gc, R: Read>(
    ctx: Context<'gc>,
    reader: R,
) -> Result<Gc<'gc, Module<'gc>>, SerializationError> {
    Module::read_from_reader(ctx, reader)
}

pub fn decode_const<'gc>(
    module: Gc<'gc, Module<'gc>>,
    const_ref: ConstRef,
) -> Result<Value<'gc>, ConstDecodeError> {
    module.decode_const(const_ref)
}

pub fn validate(module: Gc<'_, Module<'_>>) -> Result<(), ValidationError> {
    module.validate()
}

fn write_proc<W: Write>(writer: &mut W, proc: &Proc<'_>) -> Result<(), SerializationError> {
    write_varint(writer, proc.id.0 as u64)?;
    write_u8(writer, encode_proc_kind(proc.kind))?;
    write_const_ref(writer, proc.binding_name)?;
    write_option(writer, proc.display_name, write_const_ref)?;
    write_option(writer, proc.source, write_const_ref)?;
    write_option(writer, proc.meta, write_const_ref)?;
    write_bool(writer, proc.cold)?;
    write_bool(writer, proc.noinline)?;
    write_varint(writer, proc.entry.0 as u64)?;
    write_varint(writer, proc.self_param.0 as u64)?;
    write_option(writer, proc.retk_param, write_value_id)?;
    write_array(writer, proc.arg_params.as_slice(), write_value_id_ref)?;
    write_option(writer, proc.rest_param, write_value_id)?;
    write_array(writer, proc.captures.as_slice(), write_capture)?;
    write_array(writer, proc.blocks.as_slice(), write_block)?;
    Ok(())
}

fn read_proc<'gc, R: Read>(
    ctx: Context<'gc>,
    reader: &mut R,
) -> Result<Proc<'gc>, SerializationError> {
    Ok(Proc {
        id: ProcId(read_varint_u32(reader)?),
        kind: decode_proc_kind(read_u8(reader)?)?,
        binding_name: read_const_ref(reader)?,
        display_name: read_option(reader, read_const_ref)?,
        source: read_option(reader, read_const_ref)?,
        meta: read_option(reader, read_const_ref)?,
        cold: read_bool(reader)?,
        noinline: read_bool(reader)?,
        entry: BlockId(read_varint_u32(reader)?),
        self_param: ValueId(read_varint_u32(reader)?),
        retk_param: read_option(reader, read_value_id)?,
        arg_params: read_array(ctx, reader, read_value_id)?,
        rest_param: read_option(reader, read_value_id)?,
        captures: read_array(ctx, reader, read_capture)?,
        blocks: read_array(ctx, reader, |reader| read_block(ctx, reader))?,
    })
}

fn write_block<W: Write>(writer: &mut W, block: &Block<'_>) -> Result<(), SerializationError> {
    write_varint(writer, block.id.0 as u64)?;
    write_option(writer, block.name, write_const_ref)?;
    write_array(writer, block.params.as_slice(), write_block_param)?;
    write_array(writer, block.insts.as_slice(), write_inst)?;
    write_terminator(writer, &block.term)?;
    write_bool(writer, block.cold)?;
    Ok(())
}

fn read_block<'gc, R: Read>(
    ctx: Context<'gc>,
    reader: &mut R,
) -> Result<Block<'gc>, SerializationError> {
    Ok(Block {
        id: BlockId(read_varint_u32(reader)?),
        name: read_option(reader, read_const_ref)?,
        params: read_array(ctx, reader, read_block_param)?,
        insts: read_array(ctx, reader, |reader| read_inst(ctx, reader))?,
        term: read_terminator(ctx, reader)?,
        cold: read_bool(reader)?,
    })
}

fn write_capture<W: Write>(writer: &mut W, capture: &Capture) -> Result<(), SerializationError> {
    write_varint(writer, capture.index as u64)?;
    write_const_ref(writer, capture.binding_name)?;
    Ok(())
}

fn read_capture<R: Read>(reader: &mut R) -> Result<Capture, SerializationError> {
    Ok(Capture {
        index: read_varint_u32(reader)?,
        binding_name: read_const_ref(reader)?,
    })
}

fn write_block_param<W: Write>(
    writer: &mut W,
    param: &BlockParam,
) -> Result<(), SerializationError> {
    write_value_id(writer, param.value)?;
    write_option(writer, param.name, write_const_ref)?;
    Ok(())
}

fn read_block_param<R: Read>(reader: &mut R) -> Result<BlockParam, SerializationError> {
    Ok(BlockParam {
        value: read_value_id(reader)?,
        name: read_option(reader, read_const_ref)?,
    })
}

fn write_inst<W: Write>(writer: &mut W, inst: &Inst<'_>) -> Result<(), SerializationError> {
    write_option(writer, inst.result, write_value_id)?;
    write_option(writer, inst.source, write_const_ref)?;
    match &inst.kind {
        InstKind::Const(const_ref) => {
            write_u8(writer, 0)?;
            write_const_ref(writer, *const_ref)?;
        }
        InstKind::PackRest { items } => {
            write_u8(writer, 1)?;
            write_array(writer, items.as_slice(), write_value_id_ref)?;
        }
        InstKind::PrimCall { prim, args } => {
            write_u8(writer, 2)?;
            write_string(writer, prim.name())?;
            write_array(writer, args.as_slice(), write_value_id_ref)?;
        }
        InstKind::RestRef { rest, index } => {
            write_u8(writer, 3)?;
            write_value_id(writer, *rest)?;
            write_varint(writer, *index as u64)?;
        }
        InstKind::RestLength { rest, skip } => {
            write_u8(writer, 4)?;
            write_value_id(writer, *rest)?;
            write_varint(writer, *skip as u64)?;
        }
        InstKind::RestToList { rest, skip } => {
            write_u8(writer, 5)?;
            write_value_id(writer, *rest)?;
            write_varint(writer, *skip as u64)?;
        }
        InstKind::RestIsEmpty { rest, skip } => {
            write_u8(writer, 6)?;
            write_value_id(writer, *rest)?;
            write_varint(writer, *skip as u64)?;
        }
        InstKind::RestIsNotEmpty { rest, skip } => {
            write_u8(writer, 7)?;
            write_value_id(writer, *rest)?;
            write_varint(writer, *skip as u64)?;
        }
        InstKind::MakeClosure {
            proc,
            is_cont,
            meta,
            slots,
        } => {
            write_u8(writer, 8)?;
            write_varint(writer, proc.0 as u64)?;
            write_bool(writer, *is_cont)?;
            write_option(writer, *meta, write_const_ref)?;
            write_varint(writer, *slots as u64)?;
        }
        InstKind::InitClosure { closure, captures } => {
            write_u8(writer, 9)?;
            write_value_id(writer, *closure)?;
            write_array(writer, captures.as_slice(), write_value_id_ref)?;
        }
        InstKind::ClosureRef { closure, index } => {
            write_u8(writer, 10)?;
            write_value_id(writer, *closure)?;
            write_varint(writer, *index as u64)?;
        }
    }
    Ok(())
}

fn read_inst<'gc, R: Read>(
    ctx: Context<'gc>,
    reader: &mut R,
) -> Result<Inst<'gc>, SerializationError> {
    let result = read_option(reader, read_value_id)?;
    let source = read_option(reader, read_const_ref)?;
    let tag = read_u8(reader)?;
    let kind = match tag {
        0 => InstKind::Const(read_const_ref(reader)?),
        1 => InstKind::PackRest {
            items: read_array(ctx, reader, read_value_id)?,
        },
        2 => {
            let prim_name = read_string(reader)?;
            let prim = Primitive::from_name(&prim_name).ok_or_else(|| {
                SerializationError::InvalidModule(format!("unknown primitive `{prim_name}`"))
            })?;
            InstKind::PrimCall {
                prim,
                args: read_array(ctx, reader, read_value_id)?,
            }
        }
        3 => InstKind::RestRef {
            rest: read_value_id(reader)?,
            index: read_varint_u32(reader)?,
        },
        4 => InstKind::RestLength {
            rest: read_value_id(reader)?,
            skip: read_varint_u32(reader)?,
        },
        5 => InstKind::RestToList {
            rest: read_value_id(reader)?,
            skip: read_varint_u32(reader)?,
        },
        6 => InstKind::RestIsEmpty {
            rest: read_value_id(reader)?,
            skip: read_varint_u32(reader)?,
        },
        7 => InstKind::RestIsNotEmpty {
            rest: read_value_id(reader)?,
            skip: read_varint_u32(reader)?,
        },
        8 => InstKind::MakeClosure {
            proc: ProcId(read_varint_u32(reader)?),
            is_cont: read_bool(reader)?,
            meta: read_option(reader, read_const_ref)?,
            slots: read_varint_u32(reader)?,
        },
        9 => InstKind::InitClosure {
            closure: read_value_id(reader)?,
            captures: read_array(ctx, reader, read_value_id)?,
        },
        10 => InstKind::ClosureRef {
            closure: read_value_id(reader)?,
            index: read_varint_u32(reader)?,
        },
        _ => {
            return Err(SerializationError::InvalidTag {
                kind: "instruction",
                tag,
            });
        }
    };
    Ok(Inst {
        result,
        source,
        kind,
    })
}

fn write_terminator<W: Write>(
    writer: &mut W,
    term: &Terminator<'_>,
) -> Result<(), SerializationError> {
    match term {
        Terminator::Jump { block, args } => {
            write_u8(writer, 0)?;
            write_varint(writer, block.0 as u64)?;
            write_array(writer, args.as_slice(), write_value_id_ref)?;
        }
        Terminator::Branch {
            cond,
            then_block,
            then_args,
            else_block,
            else_args,
            hints,
        } => {
            write_u8(writer, 1)?;
            write_value_id(writer, *cond)?;
            write_varint(writer, then_block.0 as u64)?;
            write_array(writer, then_args.as_slice(), write_value_id_ref)?;
            write_varint(writer, else_block.0 as u64)?;
            write_array(writer, else_args.as_slice(), write_value_id_ref)?;
            write_u8(writer, encode_branch_hint(hints[0]))?;
            write_u8(writer, encode_branch_hint(hints[1]))?;
        }
        Terminator::TailContinue {
            callee,
            args,
            source,
        } => {
            write_u8(writer, 2)?;
            write_tail_target(writer, callee)?;
            write_array(writer, args.as_slice(), write_value_id_ref)?;
            write_option(writer, *source, write_const_ref)?;
        }
        Terminator::TailApp {
            callee,
            retk,
            args,
            source,
        } => {
            write_u8(writer, 3)?;
            write_tail_target(writer, callee)?;
            write_value_id(writer, *retk)?;
            write_array(writer, args.as_slice(), write_value_id_ref)?;
            write_option(writer, *source, write_const_ref)?;
        }
    }
    Ok(())
}

fn read_terminator<'gc, R: Read>(
    ctx: Context<'gc>,
    reader: &mut R,
) -> Result<Terminator<'gc>, SerializationError> {
    let tag = read_u8(reader)?;
    match tag {
        0 => Ok(Terminator::Jump {
            block: BlockId(read_varint_u32(reader)?),
            args: read_array(ctx, reader, read_value_id)?,
        }),
        1 => Ok(Terminator::Branch {
            cond: read_value_id(reader)?,
            then_block: BlockId(read_varint_u32(reader)?),
            then_args: read_array(ctx, reader, read_value_id)?,
            else_block: BlockId(read_varint_u32(reader)?),
            else_args: read_array(ctx, reader, read_value_id)?,
            hints: [
                decode_branch_hint(read_u8(reader)?)?,
                decode_branch_hint(read_u8(reader)?)?,
            ],
        }),
        2 => Ok(Terminator::TailContinue {
            callee: read_tail_target(reader)?,
            args: read_array(ctx, reader, read_value_id)?,
            source: read_option(reader, read_const_ref)?,
        }),
        3 => Ok(Terminator::TailApp {
            callee: read_tail_target(reader)?,
            retk: read_value_id(reader)?,
            args: read_array(ctx, reader, read_value_id)?,
            source: read_option(reader, read_const_ref)?,
        }),
        _ => Err(SerializationError::InvalidTag {
            kind: "terminator",
            tag,
        }),
    }
}

fn write_tail_target<W: Write>(
    writer: &mut W,
    target: &TailTarget,
) -> Result<(), SerializationError> {
    match target {
        TailTarget::Direct { proc, closure } => {
            write_u8(writer, 0)?;
            write_varint(writer, proc.0 as u64)?;
            write_value_id(writer, *closure)?;
        }
        TailTarget::Indirect { closure } => {
            write_u8(writer, 1)?;
            write_value_id(writer, *closure)?;
        }
    }
    Ok(())
}

fn read_tail_target<R: Read>(reader: &mut R) -> Result<TailTarget, SerializationError> {
    let tag = read_u8(reader)?;
    match tag {
        0 => Ok(TailTarget::Direct {
            proc: ProcId(read_varint_u32(reader)?),
            closure: read_value_id(reader)?,
        }),
        1 => Ok(TailTarget::Indirect {
            closure: read_value_id(reader)?,
        }),
        _ => Err(SerializationError::InvalidTag {
            kind: "tail target",
            tag,
        }),
    }
}

fn write_const_ref<W: Write>(
    writer: &mut W,
    const_ref: ConstRef,
) -> Result<(), SerializationError> {
    match const_ref {
        ConstRef::Inline(value) => {
            write_u8(writer, 0)?;
            write_immediate_const(writer, value)?;
        }
        ConstRef::Pool(id) => {
            write_u8(writer, 1)?;
            write_varint(writer, id.0 as u64)?;
        }
    }
    Ok(())
}

fn read_const_ref<R: Read>(reader: &mut R) -> Result<ConstRef, SerializationError> {
    match read_u8(reader)? {
        0 => Ok(ConstRef::Inline(read_immediate_const(reader)?)),
        1 => Ok(ConstRef::Pool(ConstId(read_varint_u32(reader)?))),
        tag => Err(SerializationError::InvalidTag {
            kind: "const ref",
            tag,
        }),
    }
}

fn write_immediate_const<W: Write>(
    writer: &mut W,
    value: ImmediateConst,
) -> Result<(), SerializationError> {
    match value {
        ImmediateConst::Null => write_u8(writer, 0)?,
        ImmediateConst::Bool(false) => write_u8(writer, 1)?,
        ImmediateConst::Bool(true) => write_u8(writer, 2)?,
        ImmediateConst::Int32(value) => {
            write_u8(writer, 3)?;
            writer.write_all(&value.to_le_bytes())?;
        }
        ImmediateConst::Flonum(value) => {
            write_u8(writer, 4)?;
            writer.write_all(&value.to_bits().to_le_bytes())?;
        }
        ImmediateConst::Char(value) => {
            write_u8(writer, 5)?;
            writer.write_all(&(value as u32).to_le_bytes())?;
        }
        ImmediateConst::Undefined => write_u8(writer, 6)?,
        ImmediateConst::Unspecified => write_u8(writer, 7)?,
        ImmediateConst::Void => write_u8(writer, 8)?,
        ImmediateConst::Eof => write_u8(writer, 9)?,
        ImmediateConst::BrokenWeakPointer => write_u8(writer, 10)?,
    }
    Ok(())
}

fn read_immediate_const<R: Read>(reader: &mut R) -> Result<ImmediateConst, SerializationError> {
    let tag = read_u8(reader)?;
    match tag {
        0 => Ok(ImmediateConst::Null),
        1 => Ok(ImmediateConst::Bool(false)),
        2 => Ok(ImmediateConst::Bool(true)),
        3 => {
            let mut buf = [0; 4];
            reader.read_exact(&mut buf)?;
            Ok(ImmediateConst::Int32(i32::from_le_bytes(buf)))
        }
        4 => {
            let mut buf = [0; 8];
            reader.read_exact(&mut buf)?;
            Ok(ImmediateConst::Flonum(f64::from_bits(u64::from_le_bytes(
                buf,
            ))))
        }
        5 => {
            let mut buf = [0; 4];
            reader.read_exact(&mut buf)?;
            let value = char::from_u32(u32::from_le_bytes(buf)).ok_or_else(|| {
                SerializationError::InvalidModule("invalid char in immediate const".to_string())
            })?;
            Ok(ImmediateConst::Char(value))
        }
        6 => Ok(ImmediateConst::Undefined),
        7 => Ok(ImmediateConst::Unspecified),
        8 => Ok(ImmediateConst::Void),
        9 => Ok(ImmediateConst::Eof),
        10 => Ok(ImmediateConst::BrokenWeakPointer),
        _ => Err(SerializationError::InvalidTag {
            kind: "immediate const",
            tag,
        }),
    }
}

fn write_value_id<W: Write>(writer: &mut W, value: ValueId) -> Result<(), SerializationError> {
    write_varint(writer, value.0 as u64)
}

fn write_value_id_ref<W: Write>(writer: &mut W, value: &ValueId) -> Result<(), SerializationError> {
    write_value_id(writer, *value)
}

fn read_value_id<R: Read>(reader: &mut R) -> Result<ValueId, SerializationError> {
    Ok(ValueId(read_varint_u32(reader)?))
}

fn write_option<W: Write, T>(
    writer: &mut W,
    value: Option<T>,
    mut write_value: impl FnMut(&mut W, T) -> Result<(), SerializationError>,
) -> Result<(), SerializationError> {
    match value {
        Some(value) => {
            write_u8(writer, 1)?;
            write_value(writer, value)?;
        }
        None => write_u8(writer, 0)?,
    }
    Ok(())
}

fn read_option<R: Read, T>(
    reader: &mut R,
    mut read_value: impl FnMut(&mut R) -> Result<T, SerializationError>,
) -> Result<Option<T>, SerializationError> {
    match read_u8(reader)? {
        0 => Ok(None),
        1 => read_value(reader).map(Some),
        tag => Err(SerializationError::InvalidTag {
            kind: "option",
            tag,
        }),
    }
}

fn write_array<W: Write, T>(
    writer: &mut W,
    values: &[T],
    mut write_value: impl FnMut(&mut W, &T) -> Result<(), SerializationError>,
) -> Result<(), SerializationError> {
    write_varint(writer, values.len() as u64)?;
    for value in values {
        write_value(writer, value)?;
    }
    Ok(())
}

fn read_array<'gc, R: Read, T: Clone>(
    ctx: Context<'gc>,
    reader: &mut R,
    mut read_value: impl FnMut(&mut R) -> Result<T, SerializationError>,
) -> Result<ArrayRef<'gc, T>, SerializationError>
where
    T: crate::rsgc::Trace,
{
    let len = read_varint_usize(reader)?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(read_value(reader)?);
    }
    Ok(Array::from_slice(*ctx, &values))
}

fn write_bytes<W: Write>(writer: &mut W, bytes: &[u8]) -> Result<(), SerializationError> {
    write_varint(writer, bytes.len() as u64)?;
    writer.write_all(bytes)?;
    Ok(())
}

fn read_bytes<R: Read>(reader: &mut R) -> Result<Vec<u8>, SerializationError> {
    let len = read_varint_usize(reader)?;
    let mut bytes = vec![0; len];
    reader.read_exact(&mut bytes)?;
    Ok(bytes)
}

fn write_string<W: Write>(writer: &mut W, value: &str) -> Result<(), SerializationError> {
    write_bytes(writer, value.as_bytes())
}

fn read_string<R: Read>(reader: &mut R) -> Result<String, SerializationError> {
    let bytes = read_bytes(reader)?;
    String::from_utf8(bytes).map_err(|err| {
        SerializationError::InvalidModule(format!("invalid UTF-8 in CPS-SSA stream: {err}"))
    })
}

fn write_u8<W: Write>(writer: &mut W, value: u8) -> Result<(), SerializationError> {
    writer.write_all(&[value])?;
    Ok(())
}

fn read_u8<R: Read>(reader: &mut R) -> Result<u8, SerializationError> {
    let mut buf = [0; 1];
    reader.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn write_bool<W: Write>(writer: &mut W, value: bool) -> Result<(), SerializationError> {
    write_u8(writer, value as u8)
}

fn read_bool<R: Read>(reader: &mut R) -> Result<bool, SerializationError> {
    match read_u8(reader)? {
        0 => Ok(false),
        1 => Ok(true),
        tag => Err(SerializationError::InvalidTag { kind: "bool", tag }),
    }
}

fn write_varint<W: Write>(writer: &mut W, mut value: u64) -> Result<(), SerializationError> {
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        writer.write_all(&[byte])?;
        if value == 0 {
            return Ok(());
        }
    }
}

fn read_varint<R: Read>(reader: &mut R) -> Result<u64, SerializationError> {
    let mut value = 0u64;
    let mut shift = 0u32;
    loop {
        let byte = read_u8(reader)?;
        value |= ((byte & 0x7f) as u64) << shift;
        if (byte & 0x80) == 0 {
            return Ok(value);
        }
        shift += 7;
        if shift >= 64 {
            return Err(SerializationError::MalformedVarint);
        }
    }
}

fn read_varint_u32<R: Read>(reader: &mut R) -> Result<u32, SerializationError> {
    let value = read_varint(reader)?;
    u32::try_from(value).map_err(|_| SerializationError::InvalidModule("varint exceeds u32".into()))
}

fn read_varint_usize<R: Read>(reader: &mut R) -> Result<usize, SerializationError> {
    let value = read_varint(reader)?;
    usize::try_from(value)
        .map_err(|_| SerializationError::InvalidModule("varint exceeds usize".into()))
}

fn encode_proc_kind(kind: ProcKind) -> u8 {
    match kind {
        ProcKind::Function => 0,
        ProcKind::Continuation => 1,
    }
}

fn decode_proc_kind(tag: u8) -> Result<ProcKind, SerializationError> {
    match tag {
        0 => Ok(ProcKind::Function),
        1 => Ok(ProcKind::Continuation),
        _ => Err(SerializationError::InvalidTag {
            kind: "proc kind",
            tag,
        }),
    }
}

fn encode_branch_hint(hint: BranchHint) -> u8 {
    match hint {
        BranchHint::Normal => 0,
        BranchHint::Hot => 1,
        BranchHint::Cold => 2,
    }
}

fn decode_branch_hint(tag: u8) -> Result<BranchHint, SerializationError> {
    match tag {
        0 => Ok(BranchHint::Normal),
        1 => Ok(BranchHint::Hot),
        2 => Ok(BranchHint::Cold),
        _ => Err(SerializationError::InvalidTag {
            kind: "branch hint",
            tag,
        }),
    }
}

fn encode_const_vector<'gc>(
    ctx: Context<'gc>,
    value: Gc<'gc, Vector<'gc>>,
) -> Result<Vec<u8>, io::Error> {
    let mut buffer = Vec::new();
    let writer = FASLWriter::new(ctx, &mut buffer);
    writer.write(value.into())?;
    Ok(buffer)
}

fn decode_const_vector<'gc>(
    ctx: Context<'gc>,
    bytes: &[u8],
) -> Result<Gc<'gc, Vector<'gc>>, SerializationError> {
    let value = FASLReader::new(ctx, Cursor::new(bytes)).read()?;
    value
        .try_as::<Vector>()
        .ok_or(SerializationError::InvalidConstPool(
            "serialized const pool root must be a Scheme vector",
        ))
}

#[cfg(test)]
mod tests;
