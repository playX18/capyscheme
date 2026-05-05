use std::{
    collections::{HashMap, HashSet},
    io::{self, BufReader, BufWriter, Read, Write},
};

use crate::{
    cps::{
        linear::{
            Block, BlockId, BranchTarget, ClosureKind, CodeId, Instruction, LinearAtom,
            LinearAtoms, LinearProgram, Procedure, ProcedureKind, RestPredicate, SwitchCase,
            SwitchCaseValue, SwitchKind, Terminator, ValueId, ValueSource, ValueSources,
        },
        packed::Primitive,
        term::BranchHint,
    },
    rsgc::alloc::{Array, ArrayRef},
    runtime::{
        Context,
        fasl::{FASLReader, FASLWriter},
        value::{Symbol, Value, Vector},
    },
};

const MAGIC: &[u8; 8] = b"CAPYCSC\0";
const VERSION: u32 = 2;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct LinearArtifactOptions {
    pub(crate) backtraces: bool,
}

pub(crate) fn write_linear_program<'gc, W: Write>(
    ctx: Context<'gc>,
    writer: W,
    program: &LinearProgram<'gc>,
    options: LinearArtifactOptions,
) -> io::Result<()> {
    let pool = ValuePool::from_program(program)?;
    let mut fasl = Vec::new();
    write_value_pool(ctx, &mut fasl, &pool.values)?;

    let mut writer = ProgramWriter {
        writer: BufWriter::new(writer),
        pool: &pool,
    };
    writer.write_bytes(MAGIC)?;
    writer.write_u32(VERSION)?;
    writer.write_options(options)?;
    writer.write_usize(fasl.len())?;
    writer.write_bytes(&fasl)?;
    writer.write_program(program)?;
    writer.flush()
}

pub(crate) fn read_linear_program<'gc, R: Read>(
    ctx: Context<'gc>,
    reader: R,
) -> io::Result<(LinearArtifactOptions, LinearProgram<'gc>)> {
    let mut reader = ProgramReader {
        ctx,
        reader: BufReader::new(reader),
        values: Vec::new(),
    };
    reader.read_header()?;
    let options = reader.read_options()?;

    let fasl_len = reader.read_usize()?;
    let mut fasl = vec![0; fasl_len];
    reader.reader.read_exact(&mut fasl)?;
    reader.values = read_value_pool(ctx, &fasl)?;

    Ok((options, reader.read_program()?))
}

fn write_value_pool<'gc, W: Write>(
    ctx: Context<'gc>,
    writer: W,
    values: &[Value<'gc>],
) -> io::Result<()> {
    let vector = Vector::from_slice(*ctx, values);
    FASLWriter::new(ctx, writer).write(vector.into())
}

fn read_value_pool<'gc>(ctx: Context<'gc>, bytes: &[u8]) -> io::Result<Vec<Value<'gc>>> {
    let value = FASLReader::new(ctx, bytes).read()?;
    let vector = value
        .try_as::<Vector>()
        .ok_or_else(|| invalid_data("LCPS value pool is not a vector"))?;
    Ok(vector.as_slice().to_vec())
}

struct ValuePool<'gc> {
    values: Vec<Value<'gc>>,
    ids: HashMap<Value<'gc>, u32>,
}

impl<'gc> ValuePool<'gc> {
    fn from_program(program: &LinearProgram<'gc>) -> io::Result<Self> {
        let mut pool = Self {
            values: Vec::new(),
            ids: HashMap::new(),
        };
        pool.collect_program(program)?;
        Ok(pool)
    }

    fn intern(&mut self, value: Value<'gc>) -> io::Result<()> {
        if self.ids.contains_key(&value) {
            return Ok(());
        }

        let index = u32::try_from(self.values.len())
            .map_err(|_| invalid_input("LCPS value pool has more than u32::MAX entries"))?;
        self.values.push(value);
        self.ids.insert(value, index);
        Ok(())
    }

    fn index(&self, value: Value<'gc>) -> io::Result<u32> {
        self.ids
            .get(&value)
            .copied()
            .ok_or_else(|| invalid_input("LCPS value was not collected into the value pool"))
    }

    fn collect_program(&mut self, program: &LinearProgram<'gc>) -> io::Result<()> {
        for procedure in &program.procedures {
            self.collect_procedure(procedure)?;
        }
        Ok(())
    }

    fn collect_procedure(&mut self, procedure: &Procedure<'gc>) -> io::Result<()> {
        self.intern(procedure.name)?;
        self.intern(procedure.source)?;
        self.intern(procedure.meta)?;
        self.collect_sources(&procedure.sources)?;

        for block in &procedure.blocks {
            self.collect_block(block)?;
        }
        Ok(())
    }

    fn collect_sources(&mut self, sources: &ValueSources<'gc>) -> io::Result<()> {
        let mut sources = sources.iter().collect::<Vec<_>>();
        sources.sort_by_key(|(value_id, _)| value_id.0);

        for (_, source) in sources {
            self.intern(source.name)?;
            self.intern(source.id)?;
        }
        Ok(())
    }

    fn collect_block(&mut self, block: &Block<'gc>) -> io::Result<()> {
        self.intern(block.source)?;

        for instruction in &block.instructions {
            self.collect_instruction(instruction)?;
        }
        self.collect_terminator(&block.terminator)
    }

    fn collect_instruction(&mut self, instruction: &Instruction<'gc>) -> io::Result<()> {
        match instruction {
            Instruction::Const { value, .. } => self.intern(*value),
            Instruction::MakeClosure { .. } => Ok(()),
            Instruction::ClosureRef { closure, .. } => self.collect_atom(*closure),
            Instruction::ClosureSet { closure, value, .. } => {
                self.collect_atom(*closure)?;
                self.collect_atom(*value)
            }
            Instruction::CacheRef {
                cache_key, source, ..
            } => {
                self.collect_atom(*cache_key)?;
                self.intern(*source)
            }
            Instruction::CacheSet {
                cache_key,
                value,
                source,
                ..
            } => {
                self.collect_atom(*cache_key)?;
                self.collect_atom(*value)?;
                self.intern(*source)
            }
            Instruction::PrimCall { args, source, .. } => {
                self.collect_atoms(args)?;
                self.intern(*source)
            }
            Instruction::RestToList { source, .. }
            | Instruction::RestRef { source, .. }
            | Instruction::RestLength { source, .. }
            | Instruction::RestPredicate { source, .. } => self.intern(*source),
        }
    }

    fn collect_terminator(&mut self, terminator: &Terminator<'gc>) -> io::Result<()> {
        match terminator {
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => {
                self.collect_atom(*callee)?;
                self.collect_atom(*retk)?;
                self.collect_atoms(args)?;
                self.intern(*source)
            }
            Terminator::TailCall {
                callee,
                args,
                source,
            } => {
                self.collect_atom(*callee)?;
                self.collect_atoms(args)?;
                self.intern(*source)
            }
            Terminator::Jump { args, .. } => self.collect_atoms(args),
            Terminator::Branch {
                test,
                consequent,
                alternative,
                ..
            } => {
                self.collect_atom(*test)?;
                self.collect_branch_target(consequent)?;
                self.collect_branch_target(alternative)
            }
            Terminator::Switch {
                scrutinee,
                cases,
                default,
                ..
            } => {
                self.collect_atom(*scrutinee)?;
                for case in cases.iter() {
                    self.collect_switch_case(case)?;
                }
                self.collect_branch_target(default)
            }
        }
    }

    fn collect_switch_case(&mut self, case: &SwitchCase<'gc>) -> io::Result<()> {
        if let SwitchCaseValue::Symbol { value, .. } = case.value {
            self.intern(value)?;
        }
        self.collect_branch_target(&case.target)
    }

    fn collect_branch_target(&mut self, target: &BranchTarget<'gc>) -> io::Result<()> {
        match target {
            BranchTarget::Local { args, .. } => self.collect_atoms(args),
            BranchTarget::Reified { continuation, args } => {
                self.collect_atom(*continuation)?;
                self.collect_atoms(args)
            }
        }
    }

    fn collect_atoms(&mut self, atoms: &LinearAtoms<'gc>) -> io::Result<()> {
        for atom in atoms.iter() {
            self.collect_atom(*atom)?;
        }
        Ok(())
    }

    fn collect_atom(&mut self, atom: LinearAtom<'gc>) -> io::Result<()> {
        match atom {
            LinearAtom::Constant(value) => self.intern(value),
            LinearAtom::Local(_) => Ok(()),
        }
    }
}

struct ProgramWriter<'a, 'gc, W: Write> {
    writer: BufWriter<W>,
    pool: &'a ValuePool<'gc>,
}

impl<'gc, W: Write> ProgramWriter<'_, 'gc, W> {
    fn flush(mut self) -> io::Result<()> {
        self.writer.flush()
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> io::Result<()> {
        self.writer.write_all(bytes)
    }

    fn write_u8(&mut self, value: u8) -> io::Result<()> {
        self.write_bytes(&[value])
    }

    fn write_bool(&mut self, value: bool) -> io::Result<()> {
        self.write_u8(u8::from(value))
    }

    fn write_u32(&mut self, value: u32) -> io::Result<()> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_i32(&mut self, value: i32) -> io::Result<()> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_u64(&mut self, value: u64) -> io::Result<()> {
        self.write_bytes(&value.to_le_bytes())
    }

    fn write_usize(&mut self, value: usize) -> io::Result<()> {
        self.write_u64(
            u64::try_from(value).map_err(|_| invalid_input("usize does not fit in u64"))?,
        )
    }

    fn write_len(&mut self, len: usize) -> io::Result<()> {
        self.write_u32(
            u32::try_from(len).map_err(|_| invalid_input("LCPS sequence length exceeds u32"))?,
        )
    }

    fn write_str(&mut self, value: &str) -> io::Result<()> {
        self.write_len(value.len())?;
        self.write_bytes(value.as_bytes())
    }

    fn write_value_ref(&mut self, value: Value<'gc>) -> io::Result<()> {
        self.write_u32(self.pool.index(value)?)
    }

    fn write_code_id(&mut self, code: CodeId) -> io::Result<()> {
        self.write_u32(code.0)
    }

    fn write_value_id(&mut self, value: ValueId) -> io::Result<()> {
        self.write_u32(value.0)
    }

    fn write_block_id(&mut self, block: BlockId) -> io::Result<()> {
        self.write_usize(block.0)
    }

    fn write_optional_value_id(&mut self, value: Option<ValueId>) -> io::Result<()> {
        self.write_bool(value.is_some())?;
        if let Some(value) = value {
            self.write_value_id(value)?;
        }
        Ok(())
    }

    fn write_options(&mut self, options: LinearArtifactOptions) -> io::Result<()> {
        self.write_bool(options.backtraces)
    }

    fn write_program(&mut self, program: &LinearProgram<'gc>) -> io::Result<()> {
        self.write_code_id(program.entry)?;
        self.write_len(program.procedures.len())?;
        for procedure in &program.procedures {
            self.write_procedure(procedure)?;
        }
        Ok(())
    }

    fn write_procedure(&mut self, procedure: &Procedure<'gc>) -> io::Result<()> {
        self.write_code_id(procedure.code)?;
        self.write_procedure_kind(procedure.kind)?;
        self.write_value_id(procedure.binding)?;
        self.write_value_ref(procedure.name)?;
        self.write_value_ref(procedure.source)?;
        self.write_value_ref(procedure.meta)?;
        self.write_optional_value_id(procedure.return_cont)?;
        self.write_value_ids(&procedure.params)?;
        self.write_optional_value_id(procedure.variadic)?;
        self.write_value_ids(&procedure.free_vars)?;
        self.write_sources(&procedure.sources)?;
        self.write_block_id(procedure.entry)?;
        self.write_len(procedure.blocks.len())?;
        for block in &procedure.blocks {
            self.write_block(block)?;
        }
        Ok(())
    }

    fn write_sources(&mut self, sources: &ValueSources<'gc>) -> io::Result<()> {
        let mut entries = sources.iter().collect::<Vec<_>>();
        entries.sort_by_key(|(value_id, _)| value_id.0);

        self.write_len(entries.len())?;
        for (value_id, source) in entries {
            self.write_value_id(*value_id)?;
            self.write_value_ref(source.name)?;
            self.write_value_ref(source.id)?;
            self.write_u32(source.set_count)?;
            self.write_u32(source.ref_count)?;
        }
        Ok(())
    }

    fn write_block(&mut self, block: &Block<'gc>) -> io::Result<()> {
        self.write_block_id(block.id)?;
        self.write_value_ids(&block.params)?;
        self.write_optional_value_id(block.variadic)?;
        self.write_len(block.instructions.len())?;
        for instruction in &block.instructions {
            self.write_instruction(instruction)?;
        }
        self.write_terminator(&block.terminator)?;
        self.write_value_ref(block.source)
    }

    fn write_value_ids(&mut self, ids: &ArrayRef<'gc, ValueId>) -> io::Result<()> {
        self.write_len(ids.len())?;
        for id in ids.iter() {
            self.write_value_id(*id)?;
        }
        Ok(())
    }

    fn write_atoms(&mut self, atoms: &LinearAtoms<'gc>) -> io::Result<()> {
        self.write_len(atoms.len())?;
        for atom in atoms.iter() {
            self.write_atom(*atom)?;
        }
        Ok(())
    }

    fn write_atom(&mut self, atom: LinearAtom<'gc>) -> io::Result<()> {
        match atom {
            LinearAtom::Constant(value) => {
                self.write_u8(0)?;
                self.write_value_ref(value)
            }
            LinearAtom::Local(value) => {
                self.write_u8(1)?;
                self.write_value_id(value)
            }
        }
    }

    fn write_instruction(&mut self, instruction: &Instruction<'gc>) -> io::Result<()> {
        match instruction {
            Instruction::Const { dst, value } => {
                self.write_u8(0)?;
                self.write_value_id(*dst)?;
                self.write_value_ref(*value)
            }
            Instruction::MakeClosure {
                dst,
                code,
                kind,
                free_count,
            } => {
                self.write_u8(1)?;
                self.write_value_id(*dst)?;
                self.write_code_id(*code)?;
                self.write_closure_kind(*kind)?;
                self.write_usize(*free_count)
            }
            Instruction::ClosureRef {
                dst,
                closure,
                index,
            } => {
                self.write_u8(2)?;
                self.write_value_id(*dst)?;
                self.write_atom(*closure)?;
                self.write_usize(*index)
            }
            Instruction::ClosureSet {
                closure,
                index,
                value,
            } => {
                self.write_u8(3)?;
                self.write_atom(*closure)?;
                self.write_usize(*index)?;
                self.write_atom(*value)
            }
            Instruction::CacheRef {
                dst,
                cache_key,
                source,
            } => {
                self.write_u8(4)?;
                self.write_value_id(*dst)?;
                self.write_atom(*cache_key)?;
                self.write_value_ref(*source)
            }
            Instruction::CacheSet {
                dst,
                cache_key,
                value,
                source,
            } => {
                self.write_u8(5)?;
                self.write_value_id(*dst)?;
                self.write_atom(*cache_key)?;
                self.write_atom(*value)?;
                self.write_value_ref(*source)
            }
            Instruction::PrimCall {
                dst,
                prim,
                args,
                source,
            } => {
                self.write_u8(6)?;
                self.write_value_id(*dst)?;
                self.write_primitive(*prim)?;
                self.write_atoms(args)?;
                self.write_value_ref(*source)
            }
            Instruction::RestToList { dst, rest, source } => {
                self.write_u8(7)?;
                self.write_value_id(*dst)?;
                self.write_value_id(*rest)?;
                self.write_value_ref(*source)
            }
            Instruction::RestRef {
                dst,
                rest,
                index,
                source,
            } => {
                self.write_u8(8)?;
                self.write_value_id(*dst)?;
                self.write_value_id(*rest)?;
                self.write_usize(*index)?;
                self.write_value_ref(*source)
            }
            Instruction::RestLength {
                dst,
                rest,
                skip,
                source,
            } => {
                self.write_u8(9)?;
                self.write_value_id(*dst)?;
                self.write_value_id(*rest)?;
                self.write_usize(*skip)?;
                self.write_value_ref(*source)
            }
            Instruction::RestPredicate {
                dst,
                rest,
                predicate,
                skip,
                source,
            } => {
                self.write_u8(10)?;
                self.write_value_id(*dst)?;
                self.write_value_id(*rest)?;
                self.write_rest_predicate(*predicate)?;
                self.write_usize(*skip)?;
                self.write_value_ref(*source)
            }
        }
    }

    fn write_branch_target(&mut self, target: &BranchTarget<'gc>) -> io::Result<()> {
        match target {
            BranchTarget::Local { block, args } => {
                self.write_u8(0)?;
                self.write_block_id(*block)?;
                self.write_atoms(args)
            }
            BranchTarget::Reified { continuation, args } => {
                self.write_u8(1)?;
                self.write_atom(*continuation)?;
                self.write_atoms(args)
            }
        }
    }

    fn write_switch_case(&mut self, case: &SwitchCase<'gc>) -> io::Result<()> {
        self.write_switch_case_value(case.value)?;
        self.write_branch_target(&case.target)
    }

    fn write_switch_case_value(&mut self, value: SwitchCaseValue<'gc>) -> io::Result<()> {
        match value {
            SwitchCaseValue::Integer(value) => {
                self.write_u8(0)?;
                self.write_i32(value)
            }
            SwitchCaseValue::Symbol { hash, value } => {
                self.write_u8(1)?;
                self.write_u64(hash)?;
                self.write_value_ref(value)
            }
        }
    }

    fn write_terminator(&mut self, terminator: &Terminator<'gc>) -> io::Result<()> {
        match terminator {
            Terminator::Call {
                callee,
                retk,
                args,
                source,
            } => {
                self.write_u8(0)?;
                self.write_atom(*callee)?;
                self.write_atom(*retk)?;
                self.write_atoms(args)?;
                self.write_value_ref(*source)
            }
            Terminator::TailCall {
                callee,
                args,
                source,
            } => {
                self.write_u8(1)?;
                self.write_atom(*callee)?;
                self.write_atoms(args)?;
                self.write_value_ref(*source)
            }
            Terminator::Jump { target, args } => {
                self.write_u8(2)?;
                self.write_block_id(*target)?;
                self.write_atoms(args)
            }
            Terminator::Branch {
                test,
                consequent,
                alternative,
                hints,
            } => {
                self.write_u8(3)?;
                self.write_atom(*test)?;
                self.write_branch_target(consequent)?;
                self.write_branch_target(alternative)?;
                self.write_branch_hint(hints[0])?;
                self.write_branch_hint(hints[1])
            }
            Terminator::Switch {
                kind,
                scrutinee,
                cases,
                default,
            } => {
                self.write_u8(4)?;
                self.write_switch_kind(*kind)?;
                self.write_atom(*scrutinee)?;
                self.write_len(cases.len())?;
                for case in cases.iter() {
                    self.write_switch_case(case)?;
                }
                self.write_branch_target(default)
            }
        }
    }

    fn write_procedure_kind(&mut self, kind: ProcedureKind) -> io::Result<()> {
        self.write_u8(match kind {
            ProcedureKind::Function => 0,
            ProcedureKind::Continuation => 1,
        })
    }

    fn write_closure_kind(&mut self, kind: ClosureKind) -> io::Result<()> {
        self.write_u8(match kind {
            ClosureKind::Function => 0,
            ClosureKind::Continuation => 1,
        })
    }

    fn write_rest_predicate(&mut self, predicate: RestPredicate) -> io::Result<()> {
        self.write_u8(match predicate {
            RestPredicate::Null => 0,
            RestPredicate::Pair => 1,
            RestPredicate::List => 2,
        })
    }

    fn write_branch_hint(&mut self, hint: BranchHint) -> io::Result<()> {
        self.write_u8(match hint {
            BranchHint::Normal => 0,
            BranchHint::Hot => 1,
            BranchHint::Cold => 2,
        })
    }

    fn write_switch_kind(&mut self, kind: SwitchKind) -> io::Result<()> {
        match kind {
            SwitchKind::Eq => self.write_u8(0),
            SwitchKind::Fixnum => self.write_u8(1),
            SwitchKind::Numeric => self.write_u8(2),
            SwitchKind::Char => self.write_u8(3),
            SwitchKind::CharEq => self.write_u8(4),
            SwitchKind::SymbolEq { mask } => {
                self.write_u8(5)?;
                self.write_u64(mask)
            }
        }
    }

    fn write_primitive(&mut self, primitive: Primitive) -> io::Result<()> {
        self.write_str(primitive.name())
    }
}

struct ProgramReader<'gc, R: Read> {
    ctx: Context<'gc>,
    reader: BufReader<R>,
    values: Vec<Value<'gc>>,
}

impl<'gc, R: Read> ProgramReader<'gc, R> {
    fn read_header(&mut self) -> io::Result<()> {
        let mut magic = [0; MAGIC.len()];
        self.reader.read_exact(&mut magic)?;
        if &magic != MAGIC {
            return Err(invalid_data("invalid LCPS .csc magic"));
        }

        let version = self.read_u32()?;
        if version != VERSION {
            return Err(invalid_data(format!(
                "unsupported LCPS .csc version {version}"
            )));
        }
        Ok(())
    }

    fn read_u8(&mut self) -> io::Result<u8> {
        let mut buf = [0; 1];
        self.reader.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    fn read_bool(&mut self) -> io::Result<bool> {
        match self.read_u8()? {
            0 => Ok(false),
            1 => Ok(true),
            tag => Err(invalid_data(format!("invalid boolean tag {tag}"))),
        }
    }

    fn read_u32(&mut self) -> io::Result<u32> {
        let mut buf = [0; 4];
        self.reader.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    fn read_i32(&mut self) -> io::Result<i32> {
        let mut buf = [0; 4];
        self.reader.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    fn read_u64(&mut self) -> io::Result<u64> {
        let mut buf = [0; 8];
        self.reader.read_exact(&mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    fn read_usize(&mut self) -> io::Result<usize> {
        usize::try_from(self.read_u64()?)
            .map_err(|_| invalid_data("serialized usize does not fit this platform"))
    }

    fn read_len(&mut self) -> io::Result<usize> {
        Ok(self.read_u32()? as usize)
    }

    fn read_string(&mut self) -> io::Result<String> {
        let len = self.read_len()?;
        let mut buf = vec![0; len];
        self.reader.read_exact(&mut buf)?;
        String::from_utf8(buf)
            .map_err(|err| invalid_data(format!("invalid UTF-8 in LCPS string: {err}")))
    }

    fn read_value_ref(&mut self) -> io::Result<Value<'gc>> {
        let index = self.read_u32()? as usize;
        self.values
            .get(index)
            .copied()
            .ok_or_else(|| invalid_data(format!("LCPS value pool index {index} is out of bounds")))
    }

    fn read_code_id(&mut self) -> io::Result<CodeId> {
        Ok(CodeId(self.read_u32()?))
    }

    fn read_value_id(&mut self) -> io::Result<ValueId> {
        Ok(ValueId(self.read_u32()?))
    }

    fn read_block_id(&mut self) -> io::Result<BlockId> {
        Ok(BlockId(self.read_usize()?))
    }

    fn read_optional_value_id(&mut self) -> io::Result<Option<ValueId>> {
        if self.read_bool()? {
            Ok(Some(self.read_value_id()?))
        } else {
            Ok(None)
        }
    }

    fn read_options(&mut self) -> io::Result<LinearArtifactOptions> {
        Ok(LinearArtifactOptions {
            backtraces: self.read_bool()?,
        })
    }

    fn read_program(&mut self) -> io::Result<LinearProgram<'gc>> {
        let entry = self.read_code_id()?;
        let procedure_count = self.read_len()?;
        let mut procedures = Vec::with_capacity(procedure_count);
        for procedure_index in 0..procedure_count {
            procedures.push(self.read_procedure(procedure_index)?);
        }

        Ok(LinearProgram {
            entry,
            procedures: Array::from_slice(*self.ctx, &procedures),
        })
    }

    fn read_procedure(&mut self, procedure_index: usize) -> io::Result<Procedure<'gc>> {
        let code = self.read_code_id()?;
        let kind = self.read_procedure_kind()?;
        let binding = self.read_value_id()?;
        let name = self.read_value_ref()?;
        let source = self.read_value_ref()?;
        let meta = self.read_value_ref()?;
        let return_cont = self.read_optional_value_id()?;
        let params = self.read_value_ids()?;
        let variadic = self.read_optional_value_id()?;
        let free_vars = self.read_value_ids()?;
        let sources = self.read_sources()?;
        let entry = self.read_block_id()?;
        let block_count = self.read_len()?;
        let mut blocks = Vec::with_capacity(block_count);
        for _ in 0..block_count {
            blocks.push(self.read_block()?);
        }

        let mut procedure = Procedure {
            code,
            kind,
            binding,
            name,
            source,
            meta,
            return_cont,
            params,
            variadic,
            free_vars,
            sources,
            entry,
            blocks: Array::from_slice(*self.ctx, &blocks),
        };
        complete_sources(self.ctx, procedure_index, &mut procedure);
        Ok(procedure)
    }

    fn read_sources(&mut self) -> io::Result<ValueSources<'gc>> {
        let entry_count = self.read_len()?;
        let mut sources = HashMap::with_capacity(entry_count);
        for _ in 0..entry_count {
            let value_id = self.read_value_id()?;
            let name = self.read_value_ref()?;
            let id = self.read_value_ref()?;
            let set_count = self.read_u32()?;
            let ref_count = self.read_u32()?;
            let source = ValueSource::new(name, id, set_count, ref_count);

            if sources.insert(value_id, source).is_some() {
                return Err(invalid_data(format!(
                    "duplicate LCPS source entry for value id {:?}",
                    value_id
                )));
            }
        }
        Ok(sources)
    }

    fn read_block(&mut self) -> io::Result<Block<'gc>> {
        let id = self.read_block_id()?;
        let params = self.read_value_ids()?;
        let variadic = self.read_optional_value_id()?;
        let instruction_count = self.read_len()?;
        let mut instructions = Vec::with_capacity(instruction_count);
        for _ in 0..instruction_count {
            instructions.push(self.read_instruction()?);
        }
        let terminator = self.read_terminator()?;
        let source = self.read_value_ref()?;

        Ok(Block {
            id,
            params,
            variadic,
            instructions: Array::from_slice(*self.ctx, &instructions),
            terminator,
            source,
        })
    }

    fn read_value_ids(&mut self) -> io::Result<ArrayRef<'gc, ValueId>> {
        let count = self.read_len()?;
        let mut ids = Vec::with_capacity(count);
        for _ in 0..count {
            ids.push(self.read_value_id()?);
        }
        Ok(Array::from_slice(*self.ctx, &ids))
    }

    fn read_atoms(&mut self) -> io::Result<LinearAtoms<'gc>> {
        let count = self.read_len()?;
        let mut atoms = Vec::with_capacity(count);
        for _ in 0..count {
            atoms.push(self.read_atom()?);
        }
        Ok(Array::from_slice(*self.ctx, &atoms))
    }

    fn read_atom(&mut self) -> io::Result<LinearAtom<'gc>> {
        match self.read_u8()? {
            0 => Ok(LinearAtom::Constant(self.read_value_ref()?)),
            1 => Ok(LinearAtom::Local(self.read_value_id()?)),
            tag => Err(invalid_data(format!("unknown LCPS atom tag {tag}"))),
        }
    }

    fn read_instruction(&mut self) -> io::Result<Instruction<'gc>> {
        match self.read_u8()? {
            0 => Ok(Instruction::Const {
                dst: self.read_value_id()?,
                value: self.read_value_ref()?,
            }),
            1 => Ok(Instruction::MakeClosure {
                dst: self.read_value_id()?,
                code: self.read_code_id()?,
                kind: self.read_closure_kind()?,
                free_count: self.read_usize()?,
            }),
            2 => Ok(Instruction::ClosureRef {
                dst: self.read_value_id()?,
                closure: self.read_atom()?,
                index: self.read_usize()?,
            }),
            3 => Ok(Instruction::ClosureSet {
                closure: self.read_atom()?,
                index: self.read_usize()?,
                value: self.read_atom()?,
            }),
            4 => Ok(Instruction::CacheRef {
                dst: self.read_value_id()?,
                cache_key: self.read_atom()?,
                source: self.read_value_ref()?,
            }),
            5 => Ok(Instruction::CacheSet {
                dst: self.read_value_id()?,
                cache_key: self.read_atom()?,
                value: self.read_atom()?,
                source: self.read_value_ref()?,
            }),
            6 => Ok(Instruction::PrimCall {
                dst: self.read_value_id()?,
                prim: self.read_primitive()?,
                args: self.read_atoms()?,
                source: self.read_value_ref()?,
            }),
            7 => Ok(Instruction::RestToList {
                dst: self.read_value_id()?,
                rest: self.read_value_id()?,
                source: self.read_value_ref()?,
            }),
            8 => Ok(Instruction::RestRef {
                dst: self.read_value_id()?,
                rest: self.read_value_id()?,
                index: self.read_usize()?,
                source: self.read_value_ref()?,
            }),
            9 => Ok(Instruction::RestLength {
                dst: self.read_value_id()?,
                rest: self.read_value_id()?,
                skip: self.read_usize()?,
                source: self.read_value_ref()?,
            }),
            10 => Ok(Instruction::RestPredicate {
                dst: self.read_value_id()?,
                rest: self.read_value_id()?,
                predicate: self.read_rest_predicate()?,
                skip: self.read_usize()?,
                source: self.read_value_ref()?,
            }),
            tag => Err(invalid_data(format!("unknown LCPS instruction tag {tag}"))),
        }
    }

    fn read_branch_target(&mut self) -> io::Result<BranchTarget<'gc>> {
        match self.read_u8()? {
            0 => Ok(BranchTarget::Local {
                block: self.read_block_id()?,
                args: self.read_atoms()?,
            }),
            1 => Ok(BranchTarget::Reified {
                continuation: self.read_atom()?,
                args: self.read_atoms()?,
            }),
            tag => Err(invalid_data(format!(
                "unknown LCPS branch target tag {tag}"
            ))),
        }
    }

    fn read_switch_case(&mut self) -> io::Result<SwitchCase<'gc>> {
        Ok(SwitchCase {
            value: self.read_switch_case_value()?,
            target: self.read_branch_target()?,
        })
    }

    fn read_switch_case_value(&mut self) -> io::Result<SwitchCaseValue<'gc>> {
        match self.read_u8()? {
            0 => Ok(SwitchCaseValue::Integer(self.read_i32()?)),
            1 => Ok(SwitchCaseValue::Symbol {
                hash: self.read_u64()?,
                value: self.read_value_ref()?,
            }),
            tag => Err(invalid_data(format!(
                "unknown LCPS switch case value tag {tag}"
            ))),
        }
    }

    fn read_terminator(&mut self) -> io::Result<Terminator<'gc>> {
        match self.read_u8()? {
            0 => Ok(Terminator::Call {
                callee: self.read_atom()?,
                retk: self.read_atom()?,
                args: self.read_atoms()?,
                source: self.read_value_ref()?,
            }),
            1 => Ok(Terminator::TailCall {
                callee: self.read_atom()?,
                args: self.read_atoms()?,
                source: self.read_value_ref()?,
            }),
            2 => Ok(Terminator::Jump {
                target: self.read_block_id()?,
                args: self.read_atoms()?,
            }),
            3 => Ok(Terminator::Branch {
                test: self.read_atom()?,
                consequent: self.read_branch_target()?,
                alternative: self.read_branch_target()?,
                hints: [self.read_branch_hint()?, self.read_branch_hint()?],
            }),
            4 => {
                let kind = self.read_switch_kind()?;
                let scrutinee = self.read_atom()?;
                let case_count = self.read_len()?;
                let mut cases = Vec::with_capacity(case_count);
                for _ in 0..case_count {
                    cases.push(self.read_switch_case()?);
                }
                Ok(Terminator::Switch {
                    kind,
                    scrutinee,
                    cases: Array::from_slice(*self.ctx, &cases),
                    default: self.read_branch_target()?,
                })
            }
            tag => Err(invalid_data(format!("unknown LCPS terminator tag {tag}"))),
        }
    }

    fn read_procedure_kind(&mut self) -> io::Result<ProcedureKind> {
        match self.read_u8()? {
            0 => Ok(ProcedureKind::Function),
            1 => Ok(ProcedureKind::Continuation),
            tag => Err(invalid_data(format!("unknown LCPS procedure kind {tag}"))),
        }
    }

    fn read_closure_kind(&mut self) -> io::Result<ClosureKind> {
        match self.read_u8()? {
            0 => Ok(ClosureKind::Function),
            1 => Ok(ClosureKind::Continuation),
            tag => Err(invalid_data(format!("unknown LCPS closure kind {tag}"))),
        }
    }

    fn read_rest_predicate(&mut self) -> io::Result<RestPredicate> {
        match self.read_u8()? {
            0 => Ok(RestPredicate::Null),
            1 => Ok(RestPredicate::Pair),
            2 => Ok(RestPredicate::List),
            tag => Err(invalid_data(format!("unknown LCPS rest predicate {tag}"))),
        }
    }

    fn read_branch_hint(&mut self) -> io::Result<BranchHint> {
        match self.read_u8()? {
            0 => Ok(BranchHint::Normal),
            1 => Ok(BranchHint::Hot),
            2 => Ok(BranchHint::Cold),
            tag => Err(invalid_data(format!("unknown LCPS branch hint {tag}"))),
        }
    }

    fn read_switch_kind(&mut self) -> io::Result<SwitchKind> {
        match self.read_u8()? {
            0 => Ok(SwitchKind::Eq),
            1 => Ok(SwitchKind::Fixnum),
            2 => Ok(SwitchKind::Numeric),
            3 => Ok(SwitchKind::Char),
            4 => Ok(SwitchKind::CharEq),
            5 => Ok(SwitchKind::SymbolEq {
                mask: self.read_u64()?,
            }),
            tag => Err(invalid_data(format!("unknown LCPS switch kind {tag}"))),
        }
    }

    fn read_primitive(&mut self) -> io::Result<Primitive> {
        let name = self.read_string()?;
        Primitive::from_name(&name)
            .ok_or_else(|| invalid_data(format!("unknown LCPS primitive {name}")))
    }
}

fn complete_sources<'gc>(
    ctx: Context<'gc>,
    procedure_index: usize,
    procedure: &mut Procedure<'gc>,
) {
    let mut ids = required_source_ids(procedure)
        .into_iter()
        .collect::<Vec<_>>();
    ids.sort_by_key(|id| id.0);

    for id in ids {
        procedure
            .sources
            .entry(id)
            .or_insert_with(|| generated_source(ctx, procedure_index, id));
    }
}

fn required_source_ids<'gc>(procedure: &Procedure<'gc>) -> HashSet<ValueId> {
    let mut ids = HashSet::new();

    ids.insert(procedure.binding);
    ids.extend(procedure.return_cont);
    ids.extend(procedure.params.iter().copied());
    ids.extend(procedure.variadic);
    ids.extend(procedure.free_vars.iter().copied());

    for block in &procedure.blocks {
        ids.extend(block.params.iter().copied());
        ids.extend(block.variadic);

        for instruction in &block.instructions {
            add_instruction_source_ids(&mut ids, instruction);
        }
        add_terminator_source_ids(&mut ids, &block.terminator);
    }

    ids
}

fn add_instruction_source_ids<'gc>(ids: &mut HashSet<ValueId>, instruction: &Instruction<'gc>) {
    match instruction {
        Instruction::Const { dst, .. } | Instruction::MakeClosure { dst, .. } => {
            ids.insert(*dst);
        }
        Instruction::ClosureRef { dst, closure, .. } => {
            ids.insert(*dst);
            add_atom_source_ids(ids, *closure);
        }
        Instruction::ClosureSet { closure, value, .. } => {
            add_atom_source_ids(ids, *closure);
            add_atom_source_ids(ids, *value);
        }
        Instruction::CacheRef { dst, cache_key, .. } => {
            ids.insert(*dst);
            add_atom_source_ids(ids, *cache_key);
        }
        Instruction::CacheSet {
            dst,
            cache_key,
            value,
            ..
        } => {
            ids.insert(*dst);
            add_atom_source_ids(ids, *cache_key);
            add_atom_source_ids(ids, *value);
        }
        Instruction::PrimCall { dst, args, .. } => {
            ids.insert(*dst);
            add_atoms_source_ids(ids, args);
        }
        Instruction::RestToList { dst, rest, .. }
        | Instruction::RestRef { dst, rest, .. }
        | Instruction::RestLength { dst, rest, .. }
        | Instruction::RestPredicate { dst, rest, .. } => {
            ids.insert(*dst);
            ids.insert(*rest);
        }
    }
}

fn add_terminator_source_ids<'gc>(ids: &mut HashSet<ValueId>, terminator: &Terminator<'gc>) {
    match terminator {
        Terminator::Call {
            callee, retk, args, ..
        } => {
            add_atom_source_ids(ids, *callee);
            add_atom_source_ids(ids, *retk);
            add_atoms_source_ids(ids, args);
        }
        Terminator::TailCall { callee, args, .. } => {
            add_atom_source_ids(ids, *callee);
            add_atoms_source_ids(ids, args);
        }
        Terminator::Jump { args, .. } => add_atoms_source_ids(ids, args),
        Terminator::Branch {
            test,
            consequent,
            alternative,
            ..
        } => {
            add_atom_source_ids(ids, *test);
            add_branch_target_source_ids(ids, consequent);
            add_branch_target_source_ids(ids, alternative);
        }
        Terminator::Switch {
            scrutinee,
            cases,
            default,
            ..
        } => {
            add_atom_source_ids(ids, *scrutinee);
            for case in cases.iter() {
                add_branch_target_source_ids(ids, &case.target);
            }
            add_branch_target_source_ids(ids, default);
        }
    }
}

fn add_branch_target_source_ids<'gc>(ids: &mut HashSet<ValueId>, target: &BranchTarget<'gc>) {
    match target {
        BranchTarget::Local { args, .. } => add_atoms_source_ids(ids, args),
        BranchTarget::Reified { continuation, args } => {
            add_atom_source_ids(ids, *continuation);
            add_atoms_source_ids(ids, args);
        }
    }
}

fn add_atoms_source_ids<'gc>(ids: &mut HashSet<ValueId>, atoms: &LinearAtoms<'gc>) {
    for atom in atoms.iter() {
        add_atom_source_ids(ids, *atom);
    }
}

fn add_atom_source_ids<'gc>(ids: &mut HashSet<ValueId>, atom: LinearAtom<'gc>) {
    if let LinearAtom::Local(id) = atom {
        ids.insert(id);
    }
}

fn generated_source<'gc>(
    ctx: Context<'gc>,
    procedure_index: usize,
    value_id: ValueId,
) -> ValueSource<'gc> {
    let name = Symbol::from_str_uninterned(
        *ctx,
        &format!("lcps:p{procedure_index}:v{}", value_id.0),
        None,
    );
    let id = i32::try_from(value_id.0)
        .map(Value::new)
        .unwrap_or_else(|_| Value::new(false));

    ValueSource::new(name.into(), id, 0, 0)
}

fn invalid_data(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

fn invalid_input(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, message.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;
    use std::sync::Mutex;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(f);
    }

    fn sym<'gc>(ctx: Context<'gc>, name: &str) -> Value<'gc> {
        Symbol::from_str_uninterned(*ctx, name, None).into()
    }

    fn value_source<'gc>(
        ctx: Context<'gc>,
        name: &str,
        id: i32,
        set_count: u32,
        ref_count: u32,
    ) -> ValueSource<'gc> {
        ValueSource::new(sym(ctx, name), Value::new(id), set_count, ref_count)
    }

    #[test]
    fn round_trips_representative_linear_program() {
        with_ctx(|ctx| {
            let program = representative_program(ctx);
            let options = LinearArtifactOptions { backtraces: true };
            let mut bytes = Vec::new();

            write_linear_program(ctx, &mut bytes, &program, options).unwrap();
            let (decoded_options, decoded) = read_linear_program(ctx, bytes.as_slice()).unwrap();

            assert_eq!(decoded_options, options);
            assert_eq!(decoded.entry, CodeId(0));
            assert_eq!(decoded.procedures.len(), 2);

            let procedure = &decoded.procedures[0];
            assert_eq!(procedure.code, CodeId(0));
            assert_eq!(procedure.kind, ProcedureKind::Function);
            assert_eq!(procedure.binding, ValueId(0));
            assert_value_string(procedure.name, "entry");
            assert_value_string(procedure.source, "entry-source");
            assert_value_string(procedure.meta, "entry-meta");
            assert_eq!(procedure.return_cont, Some(ValueId(1)));
            assert_eq!(procedure.params.as_slice(), &[ValueId(2), ValueId(3)]);
            assert_eq!(procedure.variadic, Some(ValueId(4)));
            assert_eq!(procedure.free_vars.as_slice(), &[ValueId(5)]);
            assert_eq!(procedure.entry, BlockId(0));
            assert_eq!(procedure.blocks.len(), 4);

            assert_source_metadata(procedure.sources[&ValueId(0)], "entry-binding", 100, 2, 7);
            assert_source_metadata(procedure.sources[&ValueId(1)], "retk", 101, 0, 3);
            assert_source_metadata(procedure.sources[&ValueId(2)], "arg0", 102, 1, 4);
            assert_source_metadata(procedure.sources[&ValueId(4)], "rest", 104, 0, 5);
            assert_source_metadata(procedure.sources[&ValueId(5)], "free", 105, 1, 6);
            assert_eq!(
                procedure.sources[&ValueId(0)],
                procedure.sources[&ValueId(6)]
            );

            for id in required_source_ids(procedure) {
                assert!(
                    procedure.sources.contains_key(&id),
                    "missing source for {id:?}"
                );
            }
            let generated = procedure.sources[&ValueId(3)];
            assert_eq!(generated.name.to_string(), "lcps:p0:v3");
            assert_eq!(generated.id, Value::new(3));
            assert_eq!(generated.set_count, 0);
            assert_eq!(generated.ref_count, 0);

            let block0 = &procedure.blocks[0];
            assert_eq!(block0.id, BlockId(0));
            assert_eq!(block0.params.as_slice(), &[ValueId(2), ValueId(3)]);
            assert_eq!(block0.variadic, Some(ValueId(4)));
            assert_eq!(block0.instructions.len(), 11);
            assert_value_string(block0.source, "block0-source");

            match &block0.instructions[0] {
                Instruction::Const { dst, value } => {
                    assert_eq!(*dst, ValueId(6));
                    assert_eq!(*value, Value::new(42));
                }
                other => panic!("unexpected const instruction: {other:?}"),
            }
            match &block0.instructions[1] {
                Instruction::MakeClosure {
                    dst,
                    code,
                    kind,
                    free_count,
                } => {
                    assert_eq!(*dst, ValueId(7));
                    assert_eq!(*code, CodeId(1));
                    assert_eq!(*kind, ClosureKind::Continuation);
                    assert_eq!(*free_count, 2);
                }
                other => panic!("unexpected make-closure instruction: {other:?}"),
            }
            match &block0.instructions[6] {
                Instruction::PrimCall {
                    dst,
                    prim,
                    args,
                    source,
                } => {
                    assert_eq!(*dst, ValueId(11));
                    assert_eq!(*prim, Primitive::Car);
                    assert_eq!(
                        args.as_slice(),
                        &[
                            LinearAtom::Local(ValueId(2)),
                            LinearAtom::Constant(Value::null())
                        ]
                    );
                    assert_value_string(*source, "prim-source");
                }
                other => panic!("unexpected prim-call instruction: {other:?}"),
            }
            match &block0.instructions[10] {
                Instruction::RestPredicate {
                    dst,
                    rest,
                    predicate,
                    skip,
                    source,
                } => {
                    assert_eq!(*dst, ValueId(15));
                    assert_eq!(*rest, ValueId(4));
                    assert_eq!(*predicate, RestPredicate::Pair);
                    assert_eq!(*skip, 2);
                    assert_value_string(*source, "rest-predicate-source");
                }
                other => panic!("unexpected rest-predicate instruction: {other:?}"),
            }
            match &block0.terminator {
                Terminator::Branch {
                    test,
                    consequent,
                    alternative,
                    hints,
                } => {
                    assert_eq!(*test, LinearAtom::Local(ValueId(15)));
                    assert_eq!(*hints, [BranchHint::Hot, BranchHint::Cold]);
                    assert_reified_target(consequent, ValueId(1), &[ValueId(11)]);
                    assert_local_target(alternative, BlockId(1), &[ValueId(2)]);
                }
                other => panic!("unexpected branch terminator: {other:?}"),
            }

            match &procedure.blocks[1].terminator {
                Terminator::Switch {
                    kind,
                    scrutinee,
                    cases,
                    default,
                } => {
                    assert_eq!(*kind, SwitchKind::SymbolEq { mask: 0xff });
                    assert_eq!(*scrutinee, LinearAtom::Local(ValueId(16)));
                    assert_eq!(cases.len(), 2);
                    match cases[0].value {
                        SwitchCaseValue::Symbol { hash, value } => {
                            assert_eq!(hash, 0x1234);
                            assert_value_string(value, "case-symbol");
                        }
                        ref other => panic!("unexpected switch case value: {other:?}"),
                    }
                    assert_local_target(&cases[0].target, BlockId(2), &[ValueId(16)]);
                    assert_eq!(cases[1].value, SwitchCaseValue::Integer(7));
                    assert_reified_target(&cases[1].target, ValueId(1), &[]);
                    assert_local_target(default, BlockId(3), &[ValueId(2)]);
                }
                other => panic!("unexpected switch terminator: {other:?}"),
            }

            match &procedure.blocks[2].terminator {
                Terminator::Call {
                    callee,
                    retk,
                    args,
                    source,
                } => {
                    assert_eq!(*callee, LinearAtom::Local(ValueId(0)));
                    assert_eq!(*retk, LinearAtom::Local(ValueId(1)));
                    assert_eq!(args.as_slice(), &[LinearAtom::Local(ValueId(2))]);
                    assert_value_string(*source, "call-source");
                }
                other => panic!("unexpected call terminator: {other:?}"),
            }
            match &procedure.blocks[3].terminator {
                Terminator::TailCall {
                    callee,
                    args,
                    source,
                } => {
                    assert_constant_symbol(*callee, "tail-callee");
                    assert_eq!(args.as_slice(), &[LinearAtom::Local(ValueId(6))]);
                    assert_value_string(*source, "tail-source");
                }
                other => panic!("unexpected tail-call terminator: {other:?}"),
            }

            let continuation = &decoded.procedures[1];
            assert_eq!(continuation.kind, ProcedureKind::Continuation);
            assert_eq!(continuation.return_cont, None);
            assert_eq!(continuation.params.as_slice(), &[ValueId(101)]);
            match &continuation.blocks[0].terminator {
                Terminator::Jump { target, args } => {
                    assert_eq!(*target, BlockId(0));
                    assert_eq!(args.as_slice(), &[LinearAtom::Local(ValueId(101))]);
                }
                other => panic!("unexpected jump terminator: {other:?}"),
            }
        });
    }

    #[test]
    fn rejects_invalid_magic() {
        with_ctx(|ctx| {
            let err = read_linear_program(ctx, b"not-csc!".as_slice()).unwrap_err();
            assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        });
    }

    fn representative_program<'gc>(ctx: Context<'gc>) -> LinearProgram<'gc> {
        let shared_entry_source = value_source(ctx, "entry-binding", 100, 2, 7);
        let entry_sources = [
            (ValueId(0), shared_entry_source),
            (ValueId(1), value_source(ctx, "retk", 101, 0, 3)),
            (ValueId(2), value_source(ctx, "arg0", 102, 1, 4)),
            (ValueId(4), value_source(ctx, "rest", 104, 0, 5)),
            (ValueId(5), value_source(ctx, "free", 105, 1, 6)),
            (ValueId(6), shared_entry_source),
        ]
        .into_iter()
        .collect();

        let entry_blocks = Array::from_slice(
            *ctx,
            &[
                Block {
                    id: BlockId(0),
                    params: Array::from_slice(*ctx, &[ValueId(2), ValueId(3)]),
                    variadic: Some(ValueId(4)),
                    instructions: Array::from_slice(
                        *ctx,
                        &[
                            Instruction::Const {
                                dst: ValueId(6),
                                value: Value::new(42),
                            },
                            Instruction::MakeClosure {
                                dst: ValueId(7),
                                code: CodeId(1),
                                kind: ClosureKind::Continuation,
                                free_count: 2,
                            },
                            Instruction::ClosureRef {
                                dst: ValueId(8),
                                closure: LinearAtom::Local(ValueId(7)),
                                index: 0,
                            },
                            Instruction::ClosureSet {
                                closure: LinearAtom::Local(ValueId(7)),
                                index: 1,
                                value: LinearAtom::Local(ValueId(5)),
                            },
                            Instruction::CacheRef {
                                dst: ValueId(9),
                                cache_key: LinearAtom::Constant(sym(ctx, "cache-key")),
                                source: sym(ctx, "cache-ref-source"),
                            },
                            Instruction::CacheSet {
                                dst: ValueId(10),
                                cache_key: LinearAtom::Local(ValueId(9)),
                                value: LinearAtom::Constant(sym(ctx, "cache-value")),
                                source: sym(ctx, "cache-set-source"),
                            },
                            Instruction::PrimCall {
                                dst: ValueId(11),
                                prim: Primitive::Car,
                                args: Array::from_slice(
                                    *ctx,
                                    &[
                                        LinearAtom::Local(ValueId(2)),
                                        LinearAtom::Constant(Value::null()),
                                    ],
                                ),
                                source: sym(ctx, "prim-source"),
                            },
                            Instruction::RestToList {
                                dst: ValueId(12),
                                rest: ValueId(4),
                                source: sym(ctx, "rest-list-source"),
                            },
                            Instruction::RestRef {
                                dst: ValueId(13),
                                rest: ValueId(4),
                                index: 1,
                                source: sym(ctx, "rest-ref-source"),
                            },
                            Instruction::RestLength {
                                dst: ValueId(14),
                                rest: ValueId(4),
                                skip: 1,
                                source: sym(ctx, "rest-length-source"),
                            },
                            Instruction::RestPredicate {
                                dst: ValueId(15),
                                rest: ValueId(4),
                                predicate: RestPredicate::Pair,
                                skip: 2,
                                source: sym(ctx, "rest-predicate-source"),
                            },
                        ],
                    ),
                    terminator: Terminator::Branch {
                        test: LinearAtom::Local(ValueId(15)),
                        consequent: BranchTarget::Reified {
                            continuation: LinearAtom::Local(ValueId(1)),
                            args: Array::from_slice(*ctx, &[LinearAtom::Local(ValueId(11))]),
                        },
                        alternative: BranchTarget::Local {
                            block: BlockId(1),
                            args: Array::from_slice(*ctx, &[LinearAtom::Local(ValueId(2))]),
                        },
                        hints: [BranchHint::Hot, BranchHint::Cold],
                    },
                    source: sym(ctx, "block0-source"),
                },
                Block {
                    id: BlockId(1),
                    params: Array::from_slice(*ctx, &[ValueId(16)]),
                    variadic: None,
                    instructions: Array::from_slice(*ctx, &[]),
                    terminator: Terminator::Switch {
                        kind: SwitchKind::SymbolEq { mask: 0xff },
                        scrutinee: LinearAtom::Local(ValueId(16)),
                        cases: Array::from_slice(
                            *ctx,
                            &[
                                SwitchCase {
                                    value: SwitchCaseValue::Symbol {
                                        hash: 0x1234,
                                        value: sym(ctx, "case-symbol"),
                                    },
                                    target: BranchTarget::Local {
                                        block: BlockId(2),
                                        args: Array::from_slice(
                                            *ctx,
                                            &[LinearAtom::Local(ValueId(16))],
                                        ),
                                    },
                                },
                                SwitchCase {
                                    value: SwitchCaseValue::Integer(7),
                                    target: BranchTarget::Reified {
                                        continuation: LinearAtom::Local(ValueId(1)),
                                        args: Array::from_slice(*ctx, &[]),
                                    },
                                },
                            ],
                        ),
                        default: BranchTarget::Local {
                            block: BlockId(3),
                            args: Array::from_slice(*ctx, &[LinearAtom::Local(ValueId(2))]),
                        },
                    },
                    source: sym(ctx, "block1-source"),
                },
                Block {
                    id: BlockId(2),
                    params: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    instructions: Array::from_slice(*ctx, &[]),
                    terminator: Terminator::Call {
                        callee: LinearAtom::Local(ValueId(0)),
                        retk: LinearAtom::Local(ValueId(1)),
                        args: Array::from_slice(*ctx, &[LinearAtom::Local(ValueId(2))]),
                        source: sym(ctx, "call-source"),
                    },
                    source: sym(ctx, "block2-source"),
                },
                Block {
                    id: BlockId(3),
                    params: Array::from_slice(*ctx, &[]),
                    variadic: None,
                    instructions: Array::from_slice(*ctx, &[]),
                    terminator: Terminator::TailCall {
                        callee: LinearAtom::Constant(sym(ctx, "tail-callee")),
                        args: Array::from_slice(*ctx, &[LinearAtom::Local(ValueId(6))]),
                        source: sym(ctx, "tail-source"),
                    },
                    source: sym(ctx, "block3-source"),
                },
            ],
        );

        let continuation = Procedure {
            code: CodeId(1),
            kind: ProcedureKind::Continuation,
            binding: ValueId(100),
            name: sym(ctx, "k"),
            source: sym(ctx, "k-source"),
            meta: Value::new(false),
            return_cont: None,
            params: Array::from_slice(*ctx, &[ValueId(101)]),
            variadic: None,
            free_vars: Array::from_slice(*ctx, &[]),
            sources: [
                (ValueId(100), value_source(ctx, "k-binding", 200, 0, 1)),
                (ValueId(101), value_source(ctx, "k-arg", 201, 0, 2)),
            ]
            .into_iter()
            .collect(),
            entry: BlockId(0),
            blocks: Array::from_slice(
                *ctx,
                &[Block {
                    id: BlockId(0),
                    params: Array::from_slice(*ctx, &[ValueId(101)]),
                    variadic: None,
                    instructions: Array::from_slice(*ctx, &[]),
                    terminator: Terminator::Jump {
                        target: BlockId(0),
                        args: Array::from_slice(*ctx, &[LinearAtom::Local(ValueId(101))]),
                    },
                    source: sym(ctx, "k-block-source"),
                }],
            ),
        };

        LinearProgram {
            entry: CodeId(0),
            procedures: Array::from_slice(
                *ctx,
                &[
                    Procedure {
                        code: CodeId(0),
                        kind: ProcedureKind::Function,
                        binding: ValueId(0),
                        name: sym(ctx, "entry"),
                        source: sym(ctx, "entry-source"),
                        meta: sym(ctx, "entry-meta"),
                        return_cont: Some(ValueId(1)),
                        params: Array::from_slice(*ctx, &[ValueId(2), ValueId(3)]),
                        variadic: Some(ValueId(4)),
                        free_vars: Array::from_slice(*ctx, &[ValueId(5)]),
                        sources: entry_sources,
                        entry: BlockId(0),
                        blocks: entry_blocks,
                    },
                    continuation,
                ],
            ),
        }
    }

    fn assert_source_metadata<'gc>(
        source: ValueSource<'gc>,
        name: &str,
        id: i32,
        set_count: u32,
        ref_count: u32,
    ) {
        assert_eq!(source.name.to_string(), name);
        assert_eq!(source.id, Value::new(id));
        assert_eq!(source.set_count, set_count);
        assert_eq!(source.ref_count, ref_count);
    }

    fn assert_value_string<'gc>(value: Value<'gc>, expected: &str) {
        assert_eq!(value.to_string(), expected);
    }

    fn assert_constant_symbol<'gc>(atom: LinearAtom<'gc>, expected: &str) {
        match atom {
            LinearAtom::Constant(value) => assert_value_string(value, expected),
            other => panic!("expected constant symbol, got {other:?}"),
        }
    }

    fn assert_local_target<'gc>(target: &BranchTarget<'gc>, block: BlockId, args: &[ValueId]) {
        match target {
            BranchTarget::Local {
                block: actual_block,
                args: actual_args,
            } => {
                assert_eq!(*actual_block, block);
                assert_eq!(
                    actual_args.as_slice(),
                    args.iter()
                        .copied()
                        .map(LinearAtom::Local)
                        .collect::<Vec<_>>()
                        .as_slice()
                );
            }
            other => panic!("expected local target, got {other:?}"),
        }
    }

    fn assert_reified_target<'gc>(
        target: &BranchTarget<'gc>,
        continuation: ValueId,
        args: &[ValueId],
    ) {
        match target {
            BranchTarget::Reified {
                continuation: actual_continuation,
                args: actual_args,
            } => {
                assert_eq!(*actual_continuation, LinearAtom::Local(continuation));
                assert_eq!(
                    actual_args.as_slice(),
                    args.iter()
                        .copied()
                        .map(LinearAtom::Local)
                        .collect::<Vec<_>>()
                        .as_slice()
                );
            }
            other => panic!("expected reified target, got {other:?}"),
        }
    }
}
