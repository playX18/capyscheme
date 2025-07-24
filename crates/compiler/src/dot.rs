use std::fmt::{self};

use crate::{ast::INTERNER, cps::*};

pub struct DotForCPS<'a, W: fmt::Write> {
    graph: &'a Graph,
    root: FuncId,
    pub writer: W,
}

impl<'a, W: fmt::Write> DotForCPS<'a, W> {
    pub fn new(graph: &'a Graph, writer: W) -> Self {
        let root = graph.entrypoint.expect("graph does not have root");

        Self {
            graph,
            root,
            writer,
        }
    }

    pub fn add_node(
        &mut self,
        id: impl GraphNode,
        label: impl fmt::Display,
        shape: impl fmt::Display,
    ) -> fmt::Result {
        let id = id.to_dot_node();
        writeln!(self.writer, "  {id} [label=\"{label}\", shape={shape}];")
    }

    pub fn add_edge(
        &mut self,
        src: impl GraphNode,
        dst: impl GraphNode,
        label: Option<&str>,
    ) -> fmt::Result {
        let label = label
            .map(|label| format!("{label}"))
            .unwrap_or(String::new());
        if label.is_empty() {
            writeln!(
                self.writer,
                "  {} -> {};\n",
                src.to_dot_node(),
                dst.to_dot_node()
            )
        } else {
            writeln!(
                self.writer,
                "  {} -> {} [label=\"{label}\"];",
                src.to_dot_node(),
                dst.to_dot_node()
            )
        }
    }

    pub fn func(&mut self, func: FuncId) -> fmt::Result {
        writeln!(self.writer, "  subgraph {func:?} {{")?;
        let id = func;
        let func = &self.graph.functions[id];

        if let Some(name) = func.name {
            writeln!(self.writer, "    label = \"{}\";", INTERNER.resolve(&name))?;
        }

        writeln!(self.writer, "\tstyle=\"filled\";\n\tcolor=\"lightgrey\";")?;

        let ret = func.return_continuation;
        let err = func.error_continuation;

        self.add_node(ret, "ret", "invhouse")?;
        self.add_node(err, "err", "invhouse")?;

        self.rec(func.body)?;
        self.add_edge(id, func.body, None)?;
        writeln!(self.writer, "}}")?;

        Ok(())
    }

    pub fn rec(&mut self, term: TermId) -> fmt::Result {
        let id = term;
        let term = &self.graph.terms[term];

        match term.definition {
            TermDef::Throw { .. } => todo!(),
            TermDef::Case(test, k1, k2) => {
                self.add_node(id, format!("case {test:?} => {k1:?} | {k2:?}"), "box")?;
                self.add_edge(id, test, None)?;
                self.add_edge(id, k1, None)?;
                self.add_edge(id, k2, None)?;
            }

            TermDef::Arity(clause, other) => {
                self.add_node(id, format!("arity {clause:?} | {other:?}"), "box")?;

                self.add_edge(id, clause, None)?;
                if let Some(other) = other {
                    self.add_edge(id, other, None)?
                };
            }
            TermDef::Letv(var, val_id) => {
                let val = &self.graph.values[val_id];
                self.add_node(id, format!("letv {var:?} = {val}"), "box")?;
                // self.add_edge(var, val_id, None)?;
                // self.add_edge(var, id, None)?;
            }

            TermDef::Letk1(k) => {
                let Continuation::Local {
                    args,
                    variadic,
                    body,
                    ..
                } = &self.graph.continuations[k]
                else {
                    unreachable!()
                };
                let args = args.as_slice(&self.graph.var_pool);

                self.add_node(
                    id,
                    format!("letk {k:?} ({args:?} . {variadic:?}) = {body:?}"),
                    "box",
                )?;
                //self.add_edge(k, id, None)?;
                self.add_edge(k, *body, None)?;
                self.rec(*body)?;
            }

            TermDef::LetkRec(_) => todo!("letrec"),
            TermDef::Fix(_fix) => {
                todo!()
            }

            TermDef::Funcall(f, k, h, args) => {
                let args = args.as_slice(&self.graph.var_pool);
                self.add_node(id, format!("{f:?}({k:?}, {h:?}, {args:?})"), "box")?;

                self.add_edge(id, f, None)?;
                self.add_edge(id, k, None)?;
                self.add_edge(id, h, None)?;
            }

            TermDef::PrimCall(name, k, h, args) => {
                let args = args.as_slice(&self.graph.var_pool);
                let name = INTERNER.resolve(&name);
                self.add_node(id, format!("#%{name}({k:?}, {h:?}, {args:?})"), "box")?;

                self.add_edge(id, k, None)?;
                self.add_edge(id, h, None)?;
            }

            TermDef::Continue(k, args) => {
                let args = args.as_slice(&self.graph.var_pool);
                self.add_node(id, format!("conitnue {k:?}({args:?})"), "box")?;

                self.add_edge(id, k, None)?;
            }

            TermDef::Halt => {
                self.add_node(id, "halt", "box")?;
            }

            TermDef::Removed => self.add_node(id, "removed", "box")?,
        }

        if let Some(next) = term.next {
            self.add_edge(id, next, None)?;
            self.rec(next)?;
        }

        Ok(())
    }

    pub fn print(&mut self) -> fmt::Result {
        writeln!(self.writer, "digraph Program {{")?;
        writeln!(self.writer, "  rankdir=TB;\n")?;
        self.func(self.root)?;
        writeln!(self.writer, "}}")
    }
}

pub trait GraphNode {
    fn to_dot_node(&self) -> String;
}

impl GraphNode for Var {
    fn to_dot_node(&self) -> String {
        format!("v{}", self.as_u32())
    }
}

impl GraphNode for ContVar {
    fn to_dot_node(&self) -> String {
        format!("k{}", self.as_u32())
    }
}

impl GraphNode for FuncId {
    fn to_dot_node(&self) -> String {
        format!("func{}", self.as_u32())
    }
}

impl GraphNode for ValueId {
    fn to_dot_node(&self) -> String {
        format!("val{}", self.as_u32())
    }
}

impl GraphNode for TermId {
    fn to_dot_node(&self) -> String {
        format!("term{}", self.as_u32())
    }
}
