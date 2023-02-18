use node_semver::Version;
use petgraph::algo::dominators;
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::{VisitMap, Visitable};
use std::collections::BTreeMap;
use std::ops::Index;

pub trait Package {
    fn name(&self) -> &str;
    fn version(&self) -> &Version;
}

#[derive(Debug, Clone)]
pub struct TreeNode<P: Package + Clone> {
    pub idx: NodeIndex,
    pub package: P,
    pub children: BTreeMap<String, Vec<NodeIndex>>,
}

#[derive(Debug, Clone)]
pub struct Tree<P: Package + Clone> {
    pub root: NodeIndex,
    inner: BTreeMap<NodeIndex, TreeNode<P>>,
}

impl<P: Package + Clone> Index<NodeIndex> for Tree<P> {
    type Output = TreeNode<P>;

    fn index(&self, idx: NodeIndex) -> &TreeNode<P> {
        &self.inner[&idx]
    }
}

impl<P: Package + Clone> Tree<P> {
    pub fn build<E>(graph: StableGraph<P, E>, root: NodeIndex) -> Self {
        let mut tree = Tree {
            root,
            inner: BTreeMap::new(),
        };

        let dominators = dominators::simple_fast(&graph, root);
        let mut visited = graph.visit_map();

        tree.build_subtree(&graph, &dominators, tree.root, &mut visited);
        tree.resolve_conflicts(&graph, tree.root);

        tree
    }

    fn build_subtree<E, V: VisitMap<NodeIndex>>(
        &mut self,
        graph: &StableGraph<P, E>,
        dominators: &dominators::Dominators<NodeIndex>,
        root: NodeIndex,
        visited: &mut V,
    ) -> bool {
        if !visited.visit(root) {
            return false;
        }

        let mut children: BTreeMap<String, Vec<NodeIndex>> = BTreeMap::new();
        for child_idx in dominators.immediately_dominated_by(root) {
            if !self.build_subtree(graph, dominators, child_idx, visited) {
                // Detected loop, break it. First occurence of the package in
                // the loop would be used throughout the loop.
                continue;
            }

            let child_name = graph[child_idx].name().to_string();
            if let Some(versions) = children.get_mut(&child_name) {
                versions.push(child_idx);
            } else {
                children.insert(child_name, vec![child_idx]);
            }
        }

        self.inner.insert(
            root,
            TreeNode {
                idx: root,
                package: graph[root].clone(),
                children,
            },
        );

        true
    }

    fn resolve_conflicts<E>(&mut self, graph: &StableGraph<P, E>, root: NodeIndex) {
        // TODO(indutny): implement me.
    }
}
