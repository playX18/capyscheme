use crate::{
    compiler::cranelift::primitive::Primitive,
    runtime::value::{Str, Symbol, Value},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use super::graph::{
    BoundVar, ContVar, ExprId, ExprKind, FreeVar, FreeVars, FunctionId, FunctionLinks, Graph,
    Subterm, TermId, TermKind,
};

pub fn render_graph<'gc>(graph: &Graph<'gc>, root: TermId) -> String {
    let mut out = String::new();
    render_toplevel(&mut out, graph, root);
    let mut seen = HashSet::new();
    let mut functions = Vec::new();
    collect_functions(graph, root, &mut seen, &mut functions);
    for function in functions {
        render_function(&mut out, graph, function);
        writeln!(out).unwrap();
    }
    out
}

fn collect_functions<'gc>(
    graph: &Graph<'gc>,
    term: TermId,
    seen: &mut HashSet<FunctionId>,
    order: &mut Vec<FunctionId>,
) {
    match graph[term].kind {
        TermKind::LetVal(_, body) => {
            if let Some(body) = graph.read_term_link(body) {
                collect_functions(graph, body, seen, order);
            }
        }
        TermKind::Fix(functions, body) | TermKind::Letk(functions, body) => {
            for function in live_functions(graph, &functions) {
                if seen.insert(function) {
                    order.push(function);
                    if let Some(body) = graph.read_term_link(graph[function].body) {
                        collect_functions(graph, body, seen, order);
                    }
                }
            }
            if let Some(body) = graph.read_term_link(body) {
                collect_functions(graph, body, seen, order);
            }
        }
        _ => {}
    }
}

fn render_function<'gc>(out: &mut String, graph: &Graph<'gc>, function: FunctionId) {
    let data = graph[function];
    let kind = if data.cont.is_some() {
        "function"
    } else {
        "continuation"
    };
    let code_id = if data.cont.is_some() {
        format!("f{}", function.as_u32())
    } else {
        format!("k{}", function.as_u32())
    };

    let mut renderer = FunctionRenderer::new(graph);
    let params = renderer.values(graph.bound_vars_slice(&data.vars));
    let retk = data
        .cont
        .map(|cont| renderer.value(cont))
        .unwrap_or_else(|| "#f".to_string());
    let name = render_value(data.name);

    writeln!(
        out,
        "procedure {kind} {code_id} {name} ({}) retk {retk}:",
        params.join(", ")
    )
    .expect("infallible allocation callback");

    let entry = renderer.alloc_block();
    let params = renderer.param_ids(graph.bound_vars_slice(&data.vars), data.variadic);
    let variadic = data.variadic.map(|var| renderer.value_id(var));
    renderer.convert_block(entry, params, variadic, data.body);
    renderer.finish(out);
}

fn render_toplevel<'gc>(out: &mut String, graph: &Graph<'gc>, root: TermId) {
    let mut renderer = FunctionRenderer::new(graph);
    renderer.is_toplevel = true;
    let entry = renderer.alloc_block();
    let mut lines = Vec::new();
    let successors = renderer.convert_term(root, &mut lines);
    if successors == "Successors: <dead>" && lines.is_empty() {
        return;
    }
    renderer.blocks.push(RenderBlock {
        id: entry,
        params: Vec::new(),
        lines,
        successors,
    });
    writeln!(out, "toplevel:").unwrap();
    renderer.finish(out);
}

struct RenderBlock {
    id: usize,
    params: Vec<String>,
    lines: Vec<String>,
    successors: String,
}

struct FunctionRenderer<'a, 'gc> {
    graph: &'a Graph<'gc>,
    values: HashMap<BoundVar, u32>,
    next_value: u32,
    blocks: Vec<RenderBlock>,
    local_blocks: HashMap<BoundVar, usize>,
    next_block: usize,
    is_toplevel: bool,
}

impl<'a, 'gc> FunctionRenderer<'a, 'gc> {
    fn new(graph: &'a Graph<'gc>) -> Self {
        Self {
            graph,
            values: HashMap::new(),
            next_value: 0,
            blocks: Vec::new(),
            local_blocks: HashMap::new(),
            next_block: 0,
            is_toplevel: false,
        }
    }

    fn value_id(&mut self, var: BoundVar) -> u32 {
        if let Some(id) = self.values.get(&var).copied() {
            return id;
        }
        let id = self.next_value;
        self.next_value += 1;
        self.values.insert(var, id);
        id
    }

    fn value(&mut self, var: BoundVar) -> String {
        format!("v@{}", self.value_id(var))
    }

    fn values(&mut self, vars: &[BoundVar]) -> Vec<String> {
        vars.iter().copied().map(|var| self.value(var)).collect()
    }

    fn param_ids(&mut self, vars: &[BoundVar], variadic: Option<BoundVar>) -> Vec<String> {
        let mut params = self.values(vars);
        if let Some(variadic) = variadic {
            params.push(format!("...{}", self.value(variadic)));
        }
        params
    }

    fn free_var(&mut self, occ: FreeVar) -> String {
        self.value(self.graph.free_binder(occ))
    }

    fn free_vars(&mut self, vars: &FreeVars) -> Vec<String> {
        self.graph
            .free_vars_slice(vars)
            .iter()
            .copied()
            .map(|occ| self.free_var(occ))
            .collect()
    }

    fn alloc_block(&mut self) -> usize {
        let id = self.next_block;
        self.next_block += 1;
        id
    }

    fn convert_block(
        &mut self,
        id: usize,
        params: Vec<String>,
        variadic: Option<u32>,
        link: Subterm,
    ) {
        let Some(term) = self.graph.read_term_link(link) else {
            return;
        };
        let mut lines = Vec::new();
        let successors = self.convert_term(term, &mut lines);
        let mut block_params = params;
        if let Some(variadic) = variadic {
            block_params.push(format!("...v@{variadic}"));
        }
        self.blocks.push(RenderBlock {
            id,
            params: block_params,
            lines,
            successors,
        });
    }

    fn convert_term(&mut self, term: TermId, lines: &mut Vec<String>) -> String {
        match self.graph[term].kind {
            TermKind::LetVal((var, expr), body) => {
                let Some(expr) = self.graph.read_expr_link(expr) else {
                    return "Successors: <dead>".to_string();
                };
                let rendered_expr = self.render_expr(expr);
                lines.push(format!("{} = {}", self.value(var), rendered_expr));
                self.convert_term_link(body, lines)
            }
            TermKind::Fix(functions, body) => {
                if self.is_toplevel {
                    for function in live_functions(self.graph, &functions) {
                        let binder = self.graph[function].var;
                        lines.push(format!(
                            "{} = Fix f{}",
                            self.value(binder),
                            function.as_u32()
                        ));
                    }
                }
                self.convert_term_link(body, lines)
            }
            TermKind::Letk(continuations, body) => {
                let continuations = live_functions(self.graph, &continuations);
                let (reified, local): (Vec<_>, Vec<_>) = continuations
                    .into_iter()
                    .partition(|continuation| self.graph[*continuation].is_reified);

                if self.is_toplevel {
                    for continuation in &reified {
                        let binder = self.graph[*continuation].var;
                        lines.push(format!(
                            "{} = Letk k{}",
                            self.value(binder),
                            continuation.as_u32()
                        ));
                    }
                }

                for continuation in &local {
                    let id = self.alloc_block();
                    self.local_blocks.insert(self.graph[*continuation].var, id);
                }

                for continuation in local {
                    let data = self.graph[continuation];
                    let id = self.local_blocks[&data.var];
                    let params = self.values(self.graph.bound_vars_slice(&data.vars));
                    let variadic = data.variadic.map(|var| self.value_id(var));
                    self.convert_block(id, params, variadic, data.body);
                }

                self.convert_term_link(body, lines)
            }
            TermKind::Continue(cont, args) => {
                let target = self.graph.free_binder(cont);
                let args = self.free_vars(&args).join(", ");
                if let Some(block) = self.local_blocks.get(&target) {
                    format!("Successors: BB{block}({args})")
                } else {
                    format!("Successors: TailCall v@{}({args})", self.value_id(target))
                }
            }
            TermKind::App(callee, args, retk) => {
                let callee = self.free_var(callee);
                let retk = self.free_var(retk);
                let args = self.free_vars(&args).join(", ");
                format!("Successors: Call {callee} {retk}({args})")
            }
            TermKind::Raise(kind, args) => {
                let args = self.free_vars(&args).join(", ");
                format!("Successors: Raise {kind:?}({args})")
            }
            TermKind::If(test, consequent, alternative, hints) => {
                let test = self.free_var(test);
                format!(
                    "Successors: If {test} Then: {}, Else: {} [{:?}, {:?}]",
                    self.branch_target(consequent),
                    self.branch_target(alternative),
                    hints[0],
                    hints[1]
                )
            }
        }
    }

    fn convert_term_link(&mut self, link: Subterm, lines: &mut Vec<String>) -> String {
        let Some(term) = self.graph.read_term_link(link) else {
            return "Successors: <dead>".to_string();
        };
        self.convert_term(term, lines)
    }

    fn branch_target(&mut self, link: Subterm) -> String {
        if let Some((continuation, args)) = self.direct_continue(link) {
            return self.continuation_target(continuation, args);
        }

        let block = self.alloc_block();
        self.convert_block(block, Vec::new(), None, link);
        format!("BB{block}()")
    }

    fn direct_continue(&self, link: Subterm) -> Option<(ContVar, FreeVars)> {
        let term = self.graph.read_term_link(link)?;
        let TermKind::Continue(continuation, args) = self.graph[term].kind else {
            return None;
        };
        Some((continuation, args))
    }

    fn continuation_target(&mut self, continuation: ContVar, args: FreeVars) -> String {
        let continuation = self.graph.free_binder(continuation);
        let args = self.free_vars(&args).join(", ");
        if let Some(block) = self.local_blocks.get(&continuation) {
            format!("BB{block}({args})")
        } else {
            format!("Reified v@{}({args})", self.value_id(continuation))
        }
    }

    fn render_expr(&mut self, expr: ExprId) -> String {
        match self.graph[expr].kind {
            ExprKind::Literal(value) => render_value(value),
            ExprKind::PrimCall(prim, args) => {
                let prim = primitive_name(prim);
                let args = self.free_vars(&args).join(", ");
                format!("{prim}({args})")
            }
        }
    }

    fn finish(&mut self, out: &mut String) {
        let mut predecessors: HashMap<usize, Vec<usize>> = HashMap::new();
        for block in &self.blocks {
            for pred in successor_blocks(&block.successors) {
                predecessors.entry(pred).or_default().push(block.id);
            }
        }

        self.blocks.sort_by_key(|block| block.id);
        for block in &self.blocks {
            writeln!(out, "BB{}: ({})", block.id, block.params.join(", ")).expect("write to string cannot fail");
            if let Some(preds) = predecessors.get(&block.id) {
                let mut pred_ids = preds.clone();
                pred_ids.sort_unstable();
                pred_ids.dedup();
                let rendered = pred_ids
                    .iter()
                    .map(|id| format!("BB{id}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(out, "  Predecessors: {rendered}").unwrap();
            }
            for line in &block.lines {
                writeln!(out, "  {line}").unwrap();
            }
            writeln!(out, "  {}", block.successors).unwrap();
            writeln!(out).unwrap();
        }
    }
}

fn successor_blocks(successors: &str) -> Vec<usize> {
    successors
        .split(|ch: char| !ch.is_ascii_digit())
        .filter_map(|part| part.parse::<usize>().ok())
        .collect()
}

fn live_functions<'gc>(graph: &Graph<'gc>, functions: &FunctionLinks) -> Vec<FunctionId> {
    graph
        .function_links_slice(functions)
        .iter()
        .copied()
        .filter_map(|link| graph.read_function_link(link))
        .collect()
}

fn primitive_name<'gc>(value: Value<'gc>) -> String {
    let name = value.downcast::<Symbol>().to_string();
    Primitive::from_name(&name)
        .map(|prim| prim.to_string())
        .unwrap_or(name)
}

fn render_value<'gc>(value: Value<'gc>) -> String {
    if value == Value::new(false) {
        "#f".to_string()
    } else if value == Value::new(true) {
        "#t".to_string()
    } else if value.is::<Symbol>() {
        value.downcast::<Symbol>().to_string()
    } else if let Some(number) = value.number() {
        number.to_string()
    } else if value.is::<Str>() {
        value.downcast::<Str>().as_str().into_owned()
    } else {
        format!("{value:?}")
    }
}
