use ahash::RandomState;
use bimap::hash::BiHashMap;
use petgraph::algo::{toposort, Cycle};
use petgraph::stable_graph::{DefaultIx, NodeIndex as GraphNodeIndex, StableDiGraph};

use std::cell::UnsafeCell;

type HashMap<L, R> = BiHashMap<L, R, RandomState, RandomState>;

pub type NodeIndex = GraphNodeIndex<DefaultIx>;

/// A directed graph which maintains connections between vertices. Each vertex has an associated
/// key and value. Users may look up vertices by the key or by the index within the graph, which is
/// exposed by the add_node function.
#[derive(Debug)]
pub struct DependencyGraph<V> {
    // store V nodes directly in the graph
    graph: UnsafeCell<StableDiGraph<V, ()>>,
    // maps some key type K to indices within the graph
    lookup_table: HashMap<String, NodeIndex>,
}

impl<V> DependencyGraph<V> {
    pub fn new() -> DependencyGraph<V> {
        DependencyGraph {
            graph: UnsafeCell::new(StableDiGraph::new()),
            lookup_table: HashMap::default(),
        }
    }

    pub fn add_node(&mut self, key: String, value: V) -> NodeIndex {
        let index = self.graph.get_mut().add_node(value);
        self.lookup_table.insert(key, index);

        index
    }

    fn get_shared_graph_reference(&self) -> &StableDiGraph<V, ()> {
        unsafe { &*self.graph.get() }
    }

    fn get_exclusive_graph_reference(&self) -> &mut StableDiGraph<V, ()> {
        unsafe { &mut *self.graph.get() }
    }

    pub fn lookup_node_by_key(&self, key: impl AsRef<str>) -> Option<&V> {
        self.lookup_table
            .get_by_left(key.as_ref())
            .map(|&index| self.get_shared_graph_reference().node_weight(index))
            .flatten()
    }

    pub fn lookup_node_by_index(&self, index: NodeIndex) -> &V {
        self.get_shared_graph_reference()
            .node_weight(index)
            .expect("The node must exist in the graph.")
    }

    pub fn lookup_key_by_index(&self, index: &NodeIndex) -> Option<&String> {
        self.lookup_table.get_by_right(index)
    }

    pub fn lookup_index_by_key(&self, key: impl AsRef<str>) -> Option<&NodeIndex> {
        self.lookup_table.get_by_left(key.as_ref())
    }

    pub fn contains_node(&self, key: impl AsRef<str>) -> bool {
        self.lookup_table.contains_left(key.as_ref())
    }

    /// Adds a dependency from `dependent_key` to `dependency_key`.
    /// Panics if either `dependent_key` or `dependent_key` is not already in the graph
    pub fn add_dep_by_key(&self, dependent_key: impl AsRef<str>, dependency_key: impl AsRef<str>) {
        if let Some(&dependent_index) = self.lookup_table.get_by_left(dependent_key.as_ref()) {
            if let Some(&dependency_index) = self.lookup_table.get_by_left(dependency_key.as_ref())
            {
                self.get_exclusive_graph_reference().add_edge(
                    dependent_index,
                    dependency_index,
                    (),
                );
            }
        }
    }

    /// Adds a dependency from `dependent_key` to `dependency_key`.
    /// Panics if either `dependent_key` or `dependent_key` is not already in the graph
    pub fn add_dep_by_index(&self, dependent_index: NodeIndex, dependency_index: NodeIndex) {
        self.get_exclusive_graph_reference()
            .add_edge(dependent_index, dependency_index, ());
    }

    /// Iterates over the nodes in the graph such that nodes are processed after the nodes they
    /// depend on. If there is a cycle in the dependencies, returns the error.
    /// TODO: do more with the Err
    pub fn toposort(&self) -> Result<impl Iterator<Item = (&String, &V)>, Cycle<NodeIndex>> {
        toposort(self.get_shared_graph_reference(), None).map(move |list| {
            list.into_iter().map(move |index| {
                let node_weight = self
                    .get_shared_graph_reference()
                    .node_weight(index)
                    .expect("Cannot have None's in the graph");
                (
                    self.lookup_table
                        .get_by_right(&index)
                        .expect(&format!("Index {:?} must be in the table", index)),
                    node_weight,
                )
            })
        })
    }
}
