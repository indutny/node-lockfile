use node_semver::{Range, Version};
use petgraph::stable_graph::{NodeIndex, StableGraph};

pub trait Package {
    fn name(&self) -> &str;
    fn version(&self) -> &Version;
    fn dependencies(&self) -> Vec<(&String, &Range)>;
}

#[derive(Debug, Clone)]
pub struct Tree<P: Package + Clone> {
    pub package: P,
}

#[derive(Debug, Clone)]
struct Edge {}

pub fn build_package_hierarchy<P: Package + Clone, E>(
    root: NodeIndex,
    graph: StableGraph<P, E>,
) -> Tree<P> {
    Tree {
        package: graph[root].clone(),
    }
}
