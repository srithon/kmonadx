use ahash::RandomState;
use bimap::hash::BiHashMap;
use petgraph::algo::{toposort, Cycle};
use petgraph::stable_graph::{DefaultIx, NodeIndex as GraphNodeIndex, StableDiGraph};

use core::hash::Hash;

type HashMap<L, R> = BiHashMap<L, R, RandomState, RandomState>;

type NodeIndex = GraphNodeIndex<DefaultIx>;

/// A directed graph which maintains connections between vertices. Each vertex has an associated
/// key and value. Users may look up vertices by the key or by the index within the graph, which is
/// exposed by the add_node function.
#[derive(Debug)]
pub struct DependencyGraph<K: Hash + Eq, V> {
    // store V nodes directly in the graph
    graph: StableDiGraph<V, ()>,
    // maps some key type K to indices within the graph
    lookup_table: HashMap<K, NodeIndex>,
}

impl<K: Hash + Eq, V> DependencyGraph<K, V> {
    pub fn new() -> DependencyGraph<K, V> {
        DependencyGraph {
            graph: StableDiGraph::new(),
            lookup_table: HashMap::default(),
        }
    }

    pub fn add_node(&mut self, key: K, value: V) -> NodeIndex {
        let index = self.graph.add_node(value);
        self.lookup_table.insert(key, index);
        index
    }

    pub fn lookup_node_by_key(&self, key: &K) -> Option<&V> {
        self.lookup_table
            .get_by_left(key)
            .map(|&index| self.graph.node_weight(index))
            .flatten()
    }

    pub fn lookup_node_by_index(&self, index: NodeIndex) -> &V {
        self.graph
            .node_weight(index)
            .expect("The node must exist in the graph.")
    }

    pub fn contains_node(&self, key: &K) -> bool {
        self.lookup_table.contains_left(key)
    }

    /// Adds a dependency from `dependent_key` to `dependency_key`.
    /// Panics if either `dependent_key` or `dependent_key` is not already in the graph
    pub fn add_dep_by_key(&mut self, dependent_key: &K, dependency_key: &K) {
        if let Some(&dependent_index) = self.lookup_table.get_by_left(dependent_key) {
            if let Some(&dependency_index) = self.lookup_table.get_by_left(dependency_key) {
                self.graph.add_edge(dependent_index, dependency_index, ());
            }
        }
    }

    /// Adds a dependency from `dependent_key` to `dependency_key`.
    /// Panics if either `dependent_key` or `dependent_key` is not already in the graph
    pub fn add_dep_by_index(&mut self, dependent_index: NodeIndex, dependency_index: NodeIndex) {
        self.graph.add_edge(dependent_index, dependency_index, ());
    }

    /// Iterates over the nodes in the graph such that nodes are processed after the nodes they
    /// depend on. If there is a cycle in the dependencies, returns the error.
    /// TODO: do more with the Err
    pub fn toposort(&self) -> Result<impl Iterator<Item = (&K, &V)>, Cycle<NodeIndex>> {
        toposort(&self.graph, None).map(move |list| {
            list.into_iter().map(move |index| {
                (
                    self.lookup_table
                        .get_by_right(&index)
                        .expect("Index must be in the table"),
                    self.graph
                        .node_weight(index)
                        .expect("Cannot have None's in the graph"),
                )
            })
        })
    }
}
