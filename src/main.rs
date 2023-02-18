use clap::Parser;
use node_lockfile::{Package, Tree};
use node_semver::{Range, Version};
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::{VisitMap, Visitable};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, VecDeque};
use std::fs;

#[derive(Serialize, Deserialize, Debug, Clone)]
struct NpmPackage {
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    version: Version,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    dependencies: BTreeMap<String, Range>,
    #[serde(flatten)]
    extra: BTreeMap<String, Value>,
}

#[derive(Serialize, Deserialize, Debug)]
struct NpmLock {
    name: String,
    version: Version,
    packages: BTreeMap<String, NpmPackage>,
    #[serde(flatten)]
    extra: BTreeMap<String, Value>,
}

/// Simple program to greet a person
#[derive(Parser, Debug)]
struct Args {
    /// Path to lockfile
    #[arg(short, long)]
    lockfile: String,
}

impl Package for NpmPackage {
    fn name(&self) -> &str {
        self.name.as_ref().expect("package name")
    }

    fn version(&self) -> &Version {
        &self.version
    }
}

struct Edge {}

fn main() {
    let args = Args::parse();

    let contents = fs::read_to_string(args.lockfile).expect("lockfile");
    let lock: NpmLock = serde_json::from_str(&contents).expect("valid JSON");

    let prefix_re = Regex::new(r"^.*node_modules/").unwrap();

    let mut graph: StableGraph<NpmPackage, Edge> = StableGraph::new();
    let mut indices: BTreeMap<String, BTreeMap<Version, NodeIndex>> = BTreeMap::new();

    // Load packages and put them into graph.
    for (path, mut package) in lock.packages.into_iter() {
        let name = prefix_re.replace(&path, "").into_owned();
        let version = package.version.clone();
        let name = package.name.get_or_insert(name).to_owned();

        if let Some(by_version) = indices.get(&name) {
            if by_version.contains_key(&version) {
                // No duplicates
                continue;
            }
        } else {
            indices.insert(name.clone(), BTreeMap::new());
        }

        let idx = graph.add_node(package);
        indices.get_mut(&name).unwrap().insert(version, idx);
    }
    let root_idx = indices[&lock.name][&lock.version];

    // Connect edges
    let mut q = VecDeque::new();
    let mut visited = graph.visit_map();
    q.push_back(root_idx);
    while let Some(idx) = q.pop_front() {
        if !visited.visit(idx) {
            continue;
        }

        let package = &graph[idx];
        let edges = package
            .dependencies
            .iter()
            .map(|(name, range)| {
                let versions = &indices[name];
                let (_, &dep_idx) = versions.iter().find(|(v, _)| range.satisfies(v)).unwrap();
                q.push_back(dep_idx);

                (idx, dep_idx, Edge {})
            })
            .collect::<Vec<_>>();

        for (source, target, weight) in edges {
            graph.add_edge(source, target, weight);
        }
    }

    // Build hierarchy
    let tree = Tree::build(graph, root_idx);
    println!("{tree:?}");
}
