use cranelift_entity::EntityList;

use crate::cps::graph::term::*;

impl<'gc> Graph<'gc> {
    pub fn reduce(&mut self) {
        log::debug!("reduce: starting reduction pass");
        let mut worklist = Vec::new();

        self.scan_for_redexes(&mut worklist);
        log::debug!("reduce: initial worklist has {} redexes", worklist.len());

        while let Some(redex) = worklist.pop() {
            log::debug!("reduce: processing {redex:?}");
            match redex {
                Redex::Inline(var) => {
                    self.inline(var, |redex| worklist.push(redex));
                }

                Redex::Dead(var) => {
                    let def = self[var].def;
                    log::debug!("reduce: eliminating dead {var} (def={def:?})");

                    match def {
                        VarDef::Cont(cont) => {
                            let tletk = self[cont].parent.unwrap();
                            let TermKind::Letk(mut conts, body) = self[tletk].kind else {
                                unreachable!();
                            };

                            log::trace!("reduce: deleting occurrences in dead cont {cont} body");
                            for occ in self.occurrences_in(self[cont].body()) {
                                self.delete(occ, &mut |redex| worklist.push(redex));
                            }

                            let sconts = conts.as_slice(&self.cont_pool);
                            if sconts.len() == 1 {
                                debug_assert_eq!(sconts[0], cont);
                                log::trace!("reduce: splicing out single-cont letk {tletk:?}");
                                self.splice(tletk, body);
                            } else {
                                log::trace!("reduce: removing {cont} from multi-cont letk");
                                let new_conts = sconts
                                    .iter()
                                    .copied()
                                    .filter(|&c| c != cont)
                                    .collect::<Vec<_>>();
                                conts.clear(&mut self.cont_pool);
                                conts = EntityList::from_slice(&new_conts, &mut self.cont_pool);
                                self[tletk].kind = TermKind::Letk(conts, body);
                            }
                        }

                        VarDef::Func(func) => {
                            let tfix = self[func].parent.unwrap();
                            let TermKind::Fix(mut funcs, body) = self[tfix].kind else {
                                unreachable!();
                            };

                            log::trace!("reduce: deleting occurrences in dead func {func} body");
                            for occ in self.occurrences_in(self[func].body()) {
                                self.delete(occ, &mut |redex| worklist.push(redex));
                            }

                            let sfuncs = funcs.as_slice(&self.func_pool);
                            if sfuncs.len() == 1 {
                                debug_assert_eq!(sfuncs[0], func);
                                log::trace!("reduce: splicing out single-func fix {tfix:?}");
                                self.splice(tfix, body);
                            } else {
                                log::trace!("reduce: removing {func} from multi-func fix");
                                let new_funcs = sfuncs
                                    .iter()
                                    .copied()
                                    .filter(|&f| f != func)
                                    .collect::<Vec<_>>();
                                funcs.clear(&mut self.func_pool);
                                funcs = EntityList::from_slice(&new_funcs, &mut self.func_pool);
                                self[tfix].kind = TermKind::Fix(funcs, body);
                            }
                        }

                        VarDef::Let(t) => {
                            let TermKind::Let(_, expr, body) = self[t].kind else {
                                unreachable!();
                            };

                            log::trace!("reduce: deleting occurrences in dead let {t:?}");
                            for occ in self.occurrences_in_expr(&expr) {
                                self.delete(occ, &mut |redex| worklist.push(redex));
                            }

                            self.splice(t, body);
                        }

                        _ => (),
                    }
                }
            }
        }
        log::debug!("reduce: reduction pass complete");
    }

    /// Initial scan for redexes. This simply iterates over all variables
    /// and checks if they are dead or can be inlined.
    fn scan_for_redexes(&self, worklist: &mut Vec<Redex>) {
        log::trace!(
            "scan_for_redexes: scanning {} variables",
            self.variables.len()
        );
        for var_id in self.variables.keys() {
            if let Some(redex) = self.kill_or_inline(var_id) {
                log::trace!("scan_for_redexes: found {redex:?}");
                worklist.push(redex);
            }
        }
    }
}
