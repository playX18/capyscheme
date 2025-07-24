use crate::il::term::LVar;

use super::*;
use im::{HashMap, HashSet};
use term::*;

/// Converts continuations into functions aka reifies them.
///
/// Not all continuations are reified. Continuations that do not escape
/// are not reified and thus can be lowered to direct jumps later in the compilation pipeline.
pub fn reify_continuations(term: P<Term>) -> P<Term> {
    let _escaping = escaping_continuations(&term);
    todo!()
}

/* 
fn rewrite(term: &P<Term>, state: &State) -> P<Term> {

}*/


pub struct State {
    continuations: HashMap<P<LVar>, P<Cont>>,
    escaping: HashSet<P<LVar>>,
}

/// Convert set of continuations that escape.
pub fn escaping_continuations(term: &Term) -> State {
    let mut state = State {
        continuations: HashMap::new(),
        escaping: HashSet::new(),
    };

    fn rec(term: &Term, state: &mut State) {
        match term {
            Term::Letk(continuations, body) => {
                for cont in continuations {
                    state
                        .continuations
                        .insert(cont.binding.clone(), cont.clone());
                    rec(&cont.body, state);
                }

                rec(body, state);
            }

            Term::Let(_, exp, body) => match exp {
                Expression::PrimCall(_, args, _) => {
                    for arg in args {
                        let Atom::Local(var) = arg else {
                            continue;
                        };
                        if state.continuations.contains_key(var) {
                            state.escaping.insert(var.clone());
                        }
                    }
                    rec(&body, state);
                }
            },

            Term::Fix(funcs, body) => {
                for func in funcs {
                    rec(&func.body, state);
                }

                rec(body, state);
            }

            Term::Continue(_, args, _) => {
                for arg in args {
                    let Atom::Local(var) = arg else {
                        continue;
                    };
                    if state.continuations.contains_key(var) {
                        state.escaping.insert(var.clone());
                    }
                }
            }

            _ => {}
        }
    }

    rec(&term, &mut state);

    state
}
