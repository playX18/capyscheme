//! Devirtualization of calls
//!
//! `CallGeneric` resolves the callee through the runtime's method cache at
//! every call. When the callee operand is a closure whose code the IR already
//! knows - the result of a `CheckCode` that passed, or a fresh
//! `AllocClosure` - the call can name that code block directly. This is the
//! case inlining leaves behind when it refuses a site (cold, over budget) or
//! after a `procedure?`-style guard, so higher-order code keeps its dynamic
//! call shape but loses the cache lookup.

use super::PassOutcome;
use crate::runtime::codegen::opt::ir::func::{Func, ValueDef};
use crate::runtime::codegen::opt::ir::inst::Op;

pub(super) fn run(func: &mut Func) -> PassOutcome {
    let mut changed = false;
    let mut work = 0;
    for block in func.blocks.keys().collect::<Vec<_>>() {
        let insts = func.blocks[block].insts.clone();
        for inst in insts {
            work += 1;
            let Op::CallGeneric(values) = func.insts[inst].op else {
                continue;
            };
            let Some(&callee) = func.args(inst).first() else {
                continue;
            };
            let ValueDef::Inst(def) = func.value_def(callee) else {
                continue;
            };
            let code = match func.insts[def].op {
                // The check passed, so the closure really runs this code.
                Op::CheckCode(code) => code,
                // A fresh closure's code is the one it was built for.
                Op::AllocClosure(code) => code,
                _ => continue,
            };
            func.insts[inst].op = Op::CallKnown(code, values);
            changed = true;
        }
    }
    PassOutcome { changed, work }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::codegen::opt::ir::func::{Func, ValueDef};
    use crate::runtime::codegen::opt::ir::inst::{ReturnValues, Terminator};
    use crate::runtime::codegen::opt::ir::types::{CodeRef, Type};
    use crate::runtime::codegen::opt::ir::verify::verify;
    use crate::runtime::codegen::opt::passes::testutil::{const_any, frame, ret};
    use crate::runtime::objectmodel::Value as SchemeValue;
    use cranelift_entity::EntityRef;

    #[test]
    fn checked_closure_call_becomes_known() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let closure = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let code = CodeRef::new(0);
        let check_frame = frame(&mut func, closure);
        let checked = func
            .push_inst(
                entry,
                Op::CheckCode(code),
                &[closure],
                Some(Type::Closure(Some(code))),
                Some(check_frame),
            )
            .expect("a check has a result");
        let call_frame = frame(&mut func, closure);
        let call = func
            .push_inst(
                entry,
                Op::CallGeneric(ReturnValues::Error),
                &[checked],
                Some(Type::Any),
                Some(call_frame),
            )
            .expect("a call has a result");
        ret(&mut func, entry, call);

        let ValueDef::Inst(call_inst) = func.value_def(call) else {
            panic!("the call is an instruction result");
        };
        verify(&func).expect("the test function is valid");
        let outcome = run(&mut func);
        assert!(outcome.changed);
        assert_eq!(
            func.insts[call_inst].op,
            Op::CallKnown(code, ReturnValues::Error),
        );
        assert!(matches!(
            func.blocks[entry].term,
            Some(Terminator::Return(_))
        ));
    }

    #[test]
    fn unknown_callee_stays_generic() {
        let mut func: Func<'static> = Func::new();
        let entry = func.entry;
        let callee = func.append_param(entry, Type::Any);
        let closure = const_any(&mut func, entry, SchemeValue::from_i32(0).bits());
        let call_frame = frame(&mut func, closure);
        let call = func
            .push_inst(
                entry,
                Op::CallGeneric(ReturnValues::Error),
                &[callee],
                Some(Type::Any),
                Some(call_frame),
            )
            .expect("a call has a result");
        ret(&mut func, entry, call);

        let ValueDef::Inst(call_inst) = func.value_def(call) else {
            panic!("the call is an instruction result");
        };
        verify(&func).expect("the test function is valid");
        let outcome = run(&mut func);
        assert!(!outcome.changed);
        assert_eq!(
            func.insts[call_inst].op,
            Op::CallGeneric(ReturnValues::Error),
        );
    }
}
