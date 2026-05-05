use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    pub fn new(parts: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self(parts.into_iter().map(Into::into).collect())
    }

    pub fn parts(&self) -> &[String] {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct ModuleGraphNode {
    pub name: ModuleName,
    pub runtime_imports: BTreeSet<ModuleName>,
    pub compile_time_imports: BTreeSet<ModuleName>,
    pub requires_eval: bool,
}

impl ModuleGraphNode {
    pub fn new(name: ModuleName) -> Self {
        Self {
            name,
            runtime_imports: BTreeSet::new(),
            compile_time_imports: BTreeSet::new(),
            requires_eval: false,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ModuleGraph {
    nodes: BTreeMap<ModuleName, ModuleGraphNode>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, node: ModuleGraphNode) -> Option<ModuleGraphNode> {
        self.nodes.insert(node.name.clone(), node)
    }

    pub fn get(&self, name: &ModuleName) -> Option<&ModuleGraphNode> {
        self.nodes.get(name)
    }

    pub fn unresolved_runtime_imports(&self) -> Vec<(ModuleName, ModuleName)> {
        let mut missing = Vec::new();
        for node in self.nodes.values() {
            for import in &node.runtime_imports {
                if !self.nodes.contains_key(import) {
                    missing.push((node.name.clone(), import.clone()));
                }
            }
        }
        missing
    }

    pub fn runtime_link_order(&self) -> Result<Vec<ModuleName>, ModuleGraphError> {
        let mut temporary = BTreeSet::new();
        let mut permanent = BTreeSet::new();
        let mut out = Vec::new();

        for name in self.nodes.keys() {
            self.visit_runtime(name, &mut temporary, &mut permanent, &mut out)?;
        }

        Ok(out)
    }

    fn visit_runtime(
        &self,
        name: &ModuleName,
        temporary: &mut BTreeSet<ModuleName>,
        permanent: &mut BTreeSet<ModuleName>,
        out: &mut Vec<ModuleName>,
    ) -> Result<(), ModuleGraphError> {
        if permanent.contains(name) {
            return Ok(());
        }
        if !temporary.insert(name.clone()) {
            return Err(ModuleGraphError::RuntimeImportCycle(name.clone()));
        }

        let node = self
            .nodes
            .get(name)
            .ok_or_else(|| ModuleGraphError::UnresolvedRuntimeImport(name.clone()))?;

        for import in &node.runtime_imports {
            self.visit_runtime(import, temporary, permanent, out)?;
        }

        temporary.remove(name);
        permanent.insert(name.clone());
        out.push(name.clone());
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ModuleGraphError {
    UnresolvedRuntimeImport(ModuleName),
    RuntimeImportCycle(ModuleName),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_link_order_places_imports_first() {
        let core = ModuleName::new(["core"]);
        let app = ModuleName::new(["app"]);
        let mut graph = ModuleGraph::new();
        graph.insert(ModuleGraphNode::new(core.clone()));
        let mut app_node = ModuleGraphNode::new(app.clone());
        app_node.runtime_imports.insert(core.clone());
        graph.insert(app_node);

        assert_eq!(graph.runtime_link_order().unwrap(), vec![core, app]);
    }

    #[test]
    fn unresolved_runtime_imports_are_reported() {
        let app = ModuleName::new(["app"]);
        let missing = ModuleName::new(["missing"]);
        let mut node = ModuleGraphNode::new(app.clone());
        node.runtime_imports.insert(missing.clone());
        let mut graph = ModuleGraph::new();
        graph.insert(node);

        assert_eq!(graph.unresolved_runtime_imports(), vec![(app, missing)]);
    }
}
