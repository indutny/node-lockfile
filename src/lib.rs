use petgraph::algo::dominators;
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::collections::{BTreeMap, HashMap, VecDeque};
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
    pub dependents: Vec<TreeIndex>,
    pub children: BTreeMap<String, Vec<TreeIndex>>,
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
        tree.resolve_conflicts(tree.root);

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

        let mut children: BTreeMap<String, Vec<TreeIndex>> = BTreeMap::new();
        for child_idx in dominators.immediately_dominated_by(root) {
            if !self.build_subtree(graph, dominators, child_idx, Some(idx), idx_converter) {
                // Detected loop, break it. First occurence of the package in
                // the loop would be used throughout the loop.
                continue;
            }

            let child_name = graph[child_idx].name().to_string();
            let child_idx = idx_converter[&child_idx];
            if let Some(versions) = children.get_mut(&child_name) {
                versions.push(child_idx);
            } else {
                children.insert(child_name, vec![child_idx]);
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
                dependents: Vec::new(),
                children,
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
        let children = tree_node
            .children
            .values()
            .flat_map(|v| v.iter())
            .cloned()
            .collect::<Vec<_>>();
        for tree_idx in children {
            self.init_dependents(graph, tree_idx, idx_converter);
        }
    }

    fn resolve_conflicts(&mut self, root: TreeIndex) {
        let mut queue = Vec::new();

        // We need to have two mutable references to node's children so
        // unfortunately this copy of children keys is needed.
        let child_names = self.inner[&root]
            .children
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for name in child_names.into_iter() {
            while self.inner[&root].children[&name].len() > 1 {
                // Select conflicting package with less dependent packages.
                let (i, least_used) = self.inner[&root].children[&name]
                    .iter()
                    .cloned()
                    .enumerate()
                    .reduce(|(i, a), (j, b)| {
                        if self.inner[&a].dependents.len() > self.inner[&b].dependents.len() {
                            (j, b)
                        } else {
                            (i, a)
                        }
                    })
                    .expect("least used duplicate");

                // Remove package
                self.inner
                    .get_mut(&root)
                    .expect("root")
                    .children
                    .get_mut(&name)
                    .expect("child package")
                    .remove(i);

                // Duplicate package into node's children that are also
                // ancestors of the `least_used` (i.e. the subtrees that use
                // `least_used`).
                self.duplicate(root, least_used);
            }

            assert_eq!(self.inner[&root].children[&name].len(), 1);
            queue.push(self.inner[&root].children[&name][0]);
        }

        for child_idx in queue {
            self.resolve_conflicts(child_idx);
        }
    }

    fn duplicate(&mut self, root: TreeIndex, dep: TreeIndex) {
        // Find children of `root` that are still ancestors of `dep`
        let targets = self.inner[&root]
            .children
            .values()
            .flatten()
            .cloned()
            .filter(|&child| self.is_ancestor(child, dep))
            .collect::<Vec<_>>();

        let dep = self.inner.remove(&dep).expect("removed dependency");

        // Put a copy of dep into each such child.
        for child in targets {
            let mut dep = dep.clone();
            let name = dep.package.name().to_string();
            dep.parent = Some(child);

            let idx = TreeIndex(self.node_count);
            self.node_count += 1;
            dep.idx = idx;

            // Make sure that dependents are within the child's subtree
            dep.dependents
                .retain(|&dependent| self.is_ancestor(child, dependent));

            let child = self.inner.get_mut(&child).expect("child");
            if let Some(versions) = child.children.get_mut(&name) {
                versions.push(dep.idx);
            } else {
                child.children.insert(name, vec![dep.idx]);
            }
        }
    }

    fn is_ancestor(&self, ancestor: TreeIndex, node: TreeIndex) -> bool {
        let mut current = node;
        while let Some(parent) = self.inner[&current].parent {
            if parent == ancestor {
                return true;
            }
            current = parent;
        }
        false
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
            for versions in res.children.values() {
                self.queue.extend(versions);
            }
            Some(res)
        } else {
            None
        }
    }
}
