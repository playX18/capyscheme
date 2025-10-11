use std::collections::{HashMap, HashSet};

use crate::{
    expander::core::{Term, TermKind, TermRef},
    runtime::value::Value,
};

struct DeclarativeToplevels<'gc> {
    dynamic: HashSet<Value<'gc>>,
    assigned: HashSet<(Value<'gc>, Value<'gc>)>,
    defined: HashSet<(Value<'gc>, Value<'gc>)>,
}

fn compute_declarative_toplvels<'gc>(t: TermRef<'gc>) -> DeclarativeToplevels<'gc> {
    fn rec<'gc>(t: &Term<'gc>, dt: &mut DeclarativeToplevels<'gc>) {
        match t.kind {
            TermKind::ToplevelSet(module, name, expr) => {
                if module != Value::new(false) {
                    dt.assigned.insert((module, name));
                } else {
                    dt.dynamic.insert(name);
                }

                rec(&expr, dt);
            }

            TermKind::Define(module, name, expr) => {
                if module != Value::new(false) {
                    let ht = if dt.defined.contains(&(module, name)) {
                        &mut dt.assigned
                    } else {
                        &mut dt.defined
                    };
                    ht.insert((module, name));
                } else {
                    dt.dynamic.insert(name);
                }
            }

            TermKind::Call(proc, args) => {
                rec(&proc, dt);
                args.iter().for_each(|arg| rec(arg, dt));
            }

            TermKind::Fix(fix) => {
                fix.rhs.iter().for_each(|rhs| rec(&rhs.body, dt));
                rec(&fix.body, dt);
            }

            TermKind::LSet(_, expr) => {
                rec(&expr, dt);
            }

            TermKind::ModuleSet(_, _, _, expr) => {
                rec(&expr, dt);
            }

            TermKind::PrimCall(_, args) => {
                args.iter().for_each(|arg| rec(arg, dt));
            }

            TermKind::Proc(proc) => {
                rec(&proc.body, dt);
            }

            TermKind::Receive(_, _, producer, consumer) => {
                rec(&producer, dt);
                rec(&consumer, dt);
            }

            TermKind::Seq(seq) => {
                seq.iter().for_each(|expr| rec(expr, dt));
            }

            _ => (),
        }
    }

    let mut dt = DeclarativeToplevels {
        dynamic: HashSet::new(),
        assigned: HashSet::new(),
        defined: HashSet::new(),
    };

    rec(&t, &mut dt);
    dt
}
