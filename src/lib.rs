use petgraph::algo::dominators;
use petgraph::algo::DfsSpace;
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::{EdgeRef, Visitable};
use petgraph::Direction;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::ops::Index;

pub trait Package {
    fn name(&self) -> &str;
}

#[derive(Debug, Copy, Clone, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TreeIndex(usize);

#[derive(Debug, Clone)]
pub struct TreeNode<P: Package + Clone> {
    pub idx: TreeIndex,
    pub graph_idx: NodeIndex,
    pub package: P,
    pub parent: Option<TreeIndex>,
    dependents: HashSet<TreeIndex>,
    pub children: BTreeMap<String, TreeIndex>,
    conflicts: BTreeMap<String, Vec<TreeIndex>>,
}

#[derive(Debug, Clone)]
pub struct Tree<P: Package + Clone> {
    pub root: TreeIndex,
    inner: HashMap<TreeIndex, TreeNode<P>>,
    node_count: usize,
}

impl<P: Package + Clone> Index<TreeIndex> for Tree<P> {
    type Output = TreeNode<P>;

    fn index(&self, idx: TreeIndex) -> &TreeNode<P> {
        &self.inner[&idx]
    }
}

type StableDfsSpace<P, E> = DfsSpace<NodeIndex, <StableGraph<P, E> as Visitable>::Map>;

impl<P: Package + Clone> Tree<P> {
    pub fn build<E>(graph: &StableGraph<P, E>, root: NodeIndex) -> Self {
        let mut tree = Tree {
            root: TreeIndex(0),
            inner: HashMap::new(),
            node_count: 0,
        };

        let dominators = dominators::simple_fast(graph, root);
        let mut idx_converter = HashMap::new();

        tree.build_subtree(graph, &dominators, root, None, &mut idx_converter);
        tree.init_dependents(graph, tree.root, &idx_converter);

        let mut dfs = DfsSpace::new(graph);
        tree.resolve_conflicts(graph, &mut dfs, tree.root);

        tree
    }

    pub fn nodes(&self) -> TreeNodeIterator<'_, P> {
        let mut queue = VecDeque::new();
        queue.push_back(self.root);
        TreeNodeIterator { tree: self, queue }
    }

    fn build_subtree<E>(
        &mut self,
        graph: &StableGraph<P, E>,
        dominators: &dominators::Dominators<NodeIndex>,
        root: NodeIndex,
        parent: Option<TreeIndex>,
        idx_converter: &mut HashMap<NodeIndex, TreeIndex>,
    ) -> bool {
        if idx_converter.contains_key(&root) {
            return false;
        }

        let idx = TreeIndex(self.node_count);
        self.node_count += 1;
        idx_converter.insert(root, idx);

        let mut conflicts: BTreeMap<String, Vec<TreeIndex>> = BTreeMap::new();
        for child_idx in dominators.immediately_dominated_by(root) {
            if !self.build_subtree(graph, dominators, child_idx, Some(idx), idx_converter) {
                // Detected loop, break it. First occurence of the package in
                // the loop would be used throughout the loop.
                continue;
            }

            let child_name = graph[child_idx].name().to_string();
            let child_idx = idx_converter[&child_idx];
            if let Some(versions) = conflicts.get_mut(&child_name) {
                versions.push(child_idx);
            } else {
                conflicts.insert(child_name, vec![child_idx]);
            }
        }

        self.inner.insert(
            idx,
            TreeNode {
                idx,
                graph_idx: root,
                package: graph[root].clone(),
                parent,
                // Will be filled later
                dependents: HashSet::new(),
                conflicts,
                children: BTreeMap::new(),
            },
        );

        true
    }

    fn init_dependents<E>(
        &mut self,
        graph: &StableGraph<P, E>,
        root: TreeIndex,
        idx_converter: &HashMap<NodeIndex, TreeIndex>,
    ) {
        let tree_node = self.inner.get_mut(&root).expect("tree node");
        tree_node.dependents.extend(
            graph
                .edges_directed(tree_node.graph_idx, Direction::Incoming)
                .map(|e| idx_converter[&e.source()]),
        );

        // Satisfy borrow checker
        let conflicts = tree_node
            .conflicts
            .values()
            .flat_map(|v| v.iter())
            .cloned()
            .collect::<Vec<_>>();
        for tree_idx in conflicts {
            self.init_dependents(graph, tree_idx, idx_converter);
        }
    }

    fn resolve_conflicts<E>(
        &mut self,
        graph: &StableGraph<P, E>,
        dfs: &mut StableDfsSpace<P, E>,
        root: TreeIndex,
    ) {
        let mut queue = Vec::new();

        // We need to have two mutable references to node's conflicts so
        // unfortunately this copy of conflicts keys is needed.
        let child_names = self.inner[&root]
            .conflicts
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for name in child_names.into_iter() {
            while self.inner[&root].conflicts[&name].len() > 1 {
                // Select conflicting package with less dependent packages that
                // are not dependencies of the root.
                let (i, least_used) = self.inner[&root].conflicts[&name]
                    .iter()
                    .cloned()
                    .enumerate()
                    .filter(|(_, idx)| !self.inner[idx].dependents.contains(&root))
                    .reduce(|(i, a), (j, b)| {
                        if self.inner[&a].dependents.len() > self.inner[&b].dependents.len() {
                            (j, b)
                        } else {
                            (i, a)
                        }
                    })
                    .expect("least used duplicate");

                // Remove package from conflicts
                self.inner
                    .get_mut(&root)
                    .expect("root")
                    .conflicts
                    .get_mut(&name)
                    .expect("child package")
                    .remove(i);

                // Duplicate package into node's conflicts that are also
                // ancestors of the `least_used` (i.e. the subtrees that use
                // `least_used`).
                self.duplicate(graph, dfs, root, least_used);
            }

            assert_eq!(self.inner[&root].conflicts[&name].len(), 1);
            queue.push(self.inner[&root].conflicts[&name][0]);
        }

        // Populate `children` by draining `conflicts`
        {
            let root = self.inner.get_mut(&root).expect("root");

            while let Some((name, conflicts)) = root.conflicts.pop_last() {
                assert_eq!(conflicts.len(), 1);
                root.children.insert(name, conflicts[0]);
            }
        }

        for child_idx in queue {
            self.resolve_conflicts(graph, dfs, child_idx);
        }
    }

    fn duplicate<E>(
        &mut self,
        graph: &StableGraph<P, E>,
        dfs: &mut StableDfsSpace<P, E>,
        root: TreeIndex,
        dep: TreeIndex,
    ) {
        // Find conflicts of `root` that are still ancestors of `dep`
        let targets = self.inner[&root]
            .conflicts
            .values()
            .flatten()
            .cloned()
            .filter(|&child| self.is_ancestor(graph, dfs, child, dep))
            .collect::<Vec<_>>();

        let dep = self.inner.remove(&dep).expect("removed dependency");

        // Put a copy of dep into each such child.
        for child in targets {
            let mut dep = dep.clone();
            let name = dep.package.name().to_string();

            let idx = TreeIndex(self.node_count);
            self.node_count += 1;
            dep.idx = idx;
            dep.parent = Some(child);

            // Make sure that dependents are within the child's subtree
            dep.dependents
                .retain(|&dependent| self.is_ancestor(graph, dfs, child, dependent));

            let child = self.inner.get_mut(&child).expect("child");
            if let Some(versions) = child.conflicts.get_mut(&name) {
                versions.push(dep.idx);
            } else {
                child.conflicts.insert(name, vec![dep.idx]);
            }

            for versions in dep.conflicts.values() {
                for idx in versions {
                    self.inner.get_mut(idx).expect("child").parent = Some(dep.idx);
                }
            }

            self.inner.insert(idx, dep);
        }
    }

    fn is_ancestor<E>(
        &self,
        graph: &StableGraph<P, E>,
        dfs: &mut StableDfsSpace<P, E>,
        ancestor: TreeIndex,
        node: TreeIndex,
    ) -> bool {
        petgraph::algo::has_path_connecting(
            graph,
            self.inner[&ancestor].graph_idx,
            self.inner[&node].graph_idx,
            Some(dfs),
        )
    }
}

pub struct TreeNodeIterator<'a, P: Package + Clone> {
    tree: &'a Tree<P>,
    queue: VecDeque<TreeIndex>,
}

impl<'a, P: Package + Clone> Iterator for TreeNodeIterator<'a, P> {
    type Item = &'a TreeNode<P>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(idx) = self.queue.pop_front() {
            let res = &self.tree[idx];
            self.queue.extend(res.children.values());
            Some(res)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone)]
    struct Node {
        name: String,
        version: String,
    }

    #[derive(Debug, Clone)]
    struct Edge {}

    impl Package for Node {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Node {
        fn new(name: &str, version: &str) -> Self {
            Self {
                name: name.to_string(),
                version: version.to_string(),
            }
        }
    }

    fn render_tree(tree: &Tree<Node>) -> Vec<(String, Vec<String>)> {
        let mut res = Vec::new();
        for node in tree.nodes() {
            let mut children = node
                .children
                .values()
                .map(|&idx| format!("{}@{}", tree[idx].package.name, tree[idx].package.version))
                .collect::<Vec<_>>();
            children.sort();

            // verify that parent is in the tree
            if let Some(parent) = node.parent {
                assert!(tree.inner.contains_key(&parent));
            }

            res.push((
                format!("{}@{}", node.package.name, node.package.version),
                children,
            ));
        }
        res
    }

    #[test]
    fn it_promotes_shared_dependencies() {
        let mut graph = StableGraph::new();

        let root = graph.add_node(Node::new("root", "1.0.0"));
        let a = graph.add_node(Node::new("a", "1.0.0"));
        graph.add_edge(root, a, Edge {});
        let b = graph.add_node(Node::new("b", "1.0.0"));
        graph.add_edge(root, b, Edge {});
        let shared = graph.add_node(Node::new("shared", "1.0.0"));
        graph.add_edge(a, shared, Edge {});
        graph.add_edge(b, shared, Edge {});

        assert_eq!(
            render_tree(&Tree::build(&graph, root)),
            vec![
                (
                    "root@1.0.0".into(),
                    vec!["a@1.0.0".into(), "b@1.0.0".into(), "shared@1.0.0".into()]
                ),
                ("a@1.0.0".into(), vec![]),
                ("b@1.0.0".into(), vec![]),
                ("shared@1.0.0".into(), vec![]),
            ],
        );
    }

    #[test]
    fn it_demotes_conflicts() {
        let mut graph = StableGraph::new();

        let root = graph.add_node(Node::new("root", "1.0.0"));
        let a = graph.add_node(Node::new("a", "1.0.0"));
        graph.add_edge(root, a, Edge {});
        let b = graph.add_node(Node::new("b", "1.0.0"));
        graph.add_edge(root, b, Edge {});
        let shared = graph.add_node(Node::new("shared", "1.0.0"));
        graph.add_edge(a, shared, Edge {});
        graph.add_edge(b, shared, Edge {});
        let leaf = graph.add_node(Node::new("leaf", "1.0.0"));
        graph.add_edge(shared, leaf, Edge {});
        let shared2 = graph.add_node(Node::new("shared", "2.0.0"));
        graph.add_edge(root, shared2, Edge {});

        assert_eq!(
            render_tree(&Tree::build(&graph, root)),
            vec![
                (
                    "root@1.0.0".into(),
                    vec!["a@1.0.0".into(), "b@1.0.0".into(), "shared@2.0.0".into()]
                ),
                ("a@1.0.0".into(), vec!["shared@1.0.0".into()]),
                ("b@1.0.0".into(), vec!["shared@1.0.0".into()]),
                ("shared@2.0.0".into(), vec![]),
                ("shared@1.0.0".into(), vec!["leaf@1.0.0".into()]),
                ("shared@1.0.0".into(), vec!["leaf@1.0.0".into()]),

                // These are actually the same tree node, but it doesn't matter!
                ("leaf@1.0.0".into(), vec![]),
                ("leaf@1.0.0".into(), vec![]),
            ],
        );
    }
}
