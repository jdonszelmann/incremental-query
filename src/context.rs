use std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    mem,
    num::NonZeroUsize,
};

use siphasher::sip128::Hasher128;
use tracing::Level;

use crate::query::{QueryColor, QueryMode};

use super::{
    generation::Generation,
    query::{Query, QueryInstance},
    query_parameter::{QueryParameter, TypeErasedQueryParam},
    storage::Storage,
    QueryHasher,
};

// inspired by https://smallcultfollowing.com/babysteps/blog/2015/04/06/modeling-graphs-in-rust-using-vector-indices/
struct Node<'cx> {
    query_instance: QueryInstance<'cx>,

    // 1-based indices in the edges array
    first_edge_index: Option<NonZeroUsize>,
    last_edge_index: Option<NonZeroUsize>,
}

#[derive(Copy, Clone)]
struct Edge {
    // index in the nodes array
    node: usize,
    // 1-based index in the edges array
    next_edge: Option<NonZeroUsize>,
}

struct OutgoingEdgeIterator {
    curr: Option<(Edge, NonZeroUsize)>,
}

impl OutgoingEdgeIterator {
    fn next_node(&mut self, cx: &Context) -> Option<usize> {
        self.next_node_inner(&cx.inner.borrow().edges)
    }

    fn next_node_inner(&mut self, edges: &[Edge]) -> Option<usize> {
        let (Edge { node, next_edge }, _) = self.curr?;
        self.curr = next_edge.map(|e| (edges[e.get()], e));
        Some(node)
    }

    fn next_edge(&mut self, edges: &[Edge]) -> Option<(Edge, NonZeroUsize)> {
        let edge = self.curr;
        self.next_node_inner(edges);
        edge
    }
}

struct CycleDetection {
    history: Vec<u128>,
    lookup: HashSet<u128>,
}
const NO_HASHSET_LEN: usize = 10;

impl CycleDetection {
    pub fn new() -> Self {
        Self {
            history: Vec::new(),
            lookup: HashSet::new(),
        }
    }

    pub fn push_is_cycle(&mut self, input_hash: u128) -> bool {
        if self.lookup.len() < NO_HASHSET_LEN {
            let cycle = self.history.contains(&input_hash);
            if !cycle {
                self.history.push(input_hash);
            }
            cycle
        } else {
            if self.lookup.is_empty() && NO_HASHSET_LEN > 0 {
                for &i in &self.history {
                    self.lookup.insert(i);
                }
            }

            let cycle = self.lookup.insert(input_hash);
            if !cycle {
                self.history.push(input_hash);
            }

            cycle
        }
    }

    pub fn pop(&mut self) -> u128 {
        let v = self.history.pop().expect("some value");
        if !self.lookup.is_empty() && NO_HASHSET_LEN > 0 {
            assert!(self.lookup.remove(&v));
        }
        v
    }

    fn history(&self) -> impl Iterator<Item = u128> {
        self.history.clone().into_iter().rev()
    }
}

struct Inner<'cx> {
    // index in the nodes array
    curr: Option<usize>,

    nodes: Vec<Node<'cx>>,
    edges: Vec<Edge>,

    roots: Vec<u128>,

    // map hash to node index
    lookup: HashMap<u128, usize>,

    cycle_detection: CycleDetection,

    generation: Generation,

    cache_enabled: bool,
}

impl<'cx> Inner<'cx> {
    fn get_node(&self, node: usize) -> &Node<'cx> {
        &self.nodes[node]
    }
    fn get_node_mut(&mut self, node: usize) -> &mut Node<'cx> {
        &mut self.nodes[node]
    }

    fn add_edge_between(&mut self, src: usize, dst: usize) {
        let src_node = &mut self.nodes[src];

        let edge_index = NonZeroUsize::new(self.edges.len()).unwrap();
        self.edges.push(Edge {
            node: dst,
            next_edge: None,
        });

        if src_node.first_edge_index.is_none() {
            src_node.first_edge_index = Some(edge_index);
            src_node.last_edge_index = Some(edge_index);
        } else if let Some(ref mut i) = src_node.last_edge_index {
            assert!(
                self.edges[i.get()].next_edge.is_none(),
                "should be none because it's the last in the chain"
            );
            self.edges[i.get()].next_edge = Some(edge_index);
            *i = edge_index;
        }
    }

    fn outgoing_edges(&self, node: usize) -> OutgoingEdgeIterator {
        outgoing_edges(node, &self.nodes, &self.edges)
    }
}

fn outgoing_edges(node: usize, nodes: &[Node], edges: &[Edge]) -> OutgoingEdgeIterator {
    let node = &nodes[node];
    let first_edge_index = node.first_edge_index;

    OutgoingEdgeIterator {
        curr: first_edge_index.map(|e| (edges[e.get()], e)),
    }
}

/// The context is the most important part of incremental compilation,
/// the structure through which queries are run and that caches query invocations.
pub struct Context<'cx> {
    inner: RefCell<Inner<'cx>>,

    pub storage: &'cx Storage,
}

impl<'cx> Context<'cx> {
    pub fn new(storage: &'cx Storage) -> Self {
        Self {
            storage,
            inner: RefCell::new(Inner {
                curr: None,
                nodes: vec![],
                edges: vec![
                    // dummy edge because edge indexes are 1-based,
                    // never referenced!
                    Edge {
                        node: 0,
                        next_edge: None,
                    },
                ],
                generation: Generation::new(),
                lookup: HashMap::new(),
                cycle_detection: CycleDetection::new(),
                roots: Vec::new(),
                cache_enabled: true,
            }),
        }
    }

    pub fn set_cache_enabled(&self, enabled: bool) {
        self.inner.borrow_mut().cache_enabled = enabled;
    }

    pub fn next_generation(&self) {
        tracing::debug!("GENERATION {}", self.inner.borrow().generation.0);
        self.inner.borrow_mut().generation.next();
    }

    /// Garbage collect the context. Deletes unreachable notes.
    /// Reachability is evaluated based on "root" nodes.
    ///
    /// Some queries are run as part of another query in the query system,
    /// I like to call those queries that run "inside" the query system.
    /// Other queries are run from "outside" the query system after you
    /// created a new [`Context`] and want to run your first query.
    ///
    /// These outside queries are considered root queries, and any node reachable
    /// from them is kept alive by [`gc`](Context::gc).
    ///
    /// After this, all objects are allocated in `new_storage` and you can delete
    /// the old backing storage to actually save space.
    pub fn gc(self, new_storage: &Storage) -> Context<'_> {
        let Inner {
            curr,
            mut nodes,
            mut edges,
            roots,
            lookup,
            cycle_detection,
            generation,
            cache_enabled,
        } = self.inner.into_inner();

        tracing::info!("old edges size: {}", edges.len());
        tracing::info!("old nodes size: {}", nodes.len());
        tracing::info!("old storage size: {}", self.storage.size());

        let mut nodes_index_map = HashMap::new();
        let mut edges_index_map = HashMap::new();
        edges_index_map.insert(0, 0);
        let mut todo = VecDeque::new();

        for i in &roots {
            if let Some(&node) = lookup.get(i) {
                todo.push_back(node);
            }
        }
        while let Some(old_idx) = todo.pop_front() {
            let new_idx = nodes_index_map.len();
            nodes_index_map.insert(old_idx, new_idx);

            let mut outgoing = outgoing_edges(old_idx, &nodes, &edges);
            while let Some(node) = outgoing.next_node_inner(&edges) {
                todo.push_back(node);
            }
        }

        let mut idx = 0;
        nodes.retain_mut(|_| {
            let retain = nodes_index_map.contains_key(&idx);
            idx += 1;
            retain
        });

        for node_idx in 0..nodes.len() {
            let mut outgoing = outgoing_edges(node_idx, &nodes, &edges);
            while let Some((edge, old_idx)) = outgoing.next_edge(&edges) {
                let new_idx = edges_index_map.len();
                edges_index_map.insert(old_idx.get(), new_idx);

                edges[old_idx.get()].node = *nodes_index_map.get(&edge.node).unwrap();
            }
        }

        let mut idx = 0;
        edges.retain_mut(|_| {
            let retain = edges_index_map.contains_key(&idx);
            idx += 1;
            retain
        });

        for node in &mut nodes {
            if let Some(ref mut i) = node.last_edge_index {
                *i = NonZeroUsize::new(*edges_index_map.get(&i.get()).unwrap()).unwrap();
            }
            if let Some(ref mut i) = node.first_edge_index {
                *i = NonZeroUsize::new(*edges_index_map.get(&i.get()).unwrap()).unwrap();
            }
        }

        for edge in &mut edges {
            if let Some(ref mut i) = edge.next_edge {
                *i = NonZeroUsize::new(*edges_index_map.get(&i.get()).unwrap()).unwrap();
            }
        }

        let old_nodes = nodes;
        let mut nodes = Vec::new();

        for n in old_nodes {
            nodes.push(Node {
                query_instance: QueryInstance {
                    input: n.query_instance.input.deep_clone(new_storage),
                    output: n.query_instance.output.map(|i| i.deep_clone(new_storage)),

                    run: n.query_instance.run,
                    name: n.query_instance.name,
                    mode: n.query_instance.mode,
                    generation: n.query_instance.generation,
                    output_hash: n.query_instance.output_hash,
                    color: n.query_instance.color,
                    transitively_has_always_dep: n.query_instance.transitively_has_always_dep,
                },
                first_edge_index: n.first_edge_index,
                last_edge_index: n.last_edge_index,
            });
        }

        tracing::info!("new edges size: {}", edges.len());
        tracing::info!("new nodes size: {}", nodes.len());
        tracing::info!("new storage size: {}", new_storage.size());

        Context {
            inner: RefCell::new(Inner {
                curr,
                nodes,
                edges,
                roots,
                lookup,
                cycle_detection,
                generation,
                cache_enabled,
            }),
            storage: new_storage,
        }
    }

    /// Returns the (approximate) size in bytes of the cache right now.
    ///
    /// The approximation is always at most wrong by a linear factor,
    /// and can be used to determine when to garbage collect. However,
    /// some fields that don't actually grow are not counted.
    pub fn size(&self) -> usize {
        let inner = self.inner.borrow();
        let nodes_size = inner.nodes.len() * mem::size_of::<Node>();
        let edges_size = inner.edges.len() * mem::size_of::<Edge>();
        let arena_size = self.storage.size();
        let lookup_size = inner.lookup.len() * (mem::size_of::<usize>() + mem::size_of::<u128>());

        nodes_size + edges_size + arena_size + lookup_size
    }
    // pub fn deserialize() {}

    // pub fn serialize(&mut self, path: impl) {
    //
    // }

    /// Used in macros
    #[doc(hidden)]
    pub fn hash<Q: Query<'cx>>(&self, query: Q, input: &impl QueryParameter) -> u128 {
        let mut hasher = QueryHasher::new();
        // also hash the query, so that paramters to different
        // queries don't have hash collisions.
        // THIS IS IMPORTANT FOR SAFETY!!
        query.type_id().hash(&mut hasher);
        input.hash_stable(&mut hasher);
        hasher.finish128().as_u128()
    }

    /// Determines whether the node's output is actually up-to-date
    /// and counts as a cache hit
    fn cache_usable(&self, node: usize) -> bool {
        {
            let inner = self.inner.borrow();
            let instance = &inner.get_node(node).query_instance;

            tracing::debug!("check if {instance} is a hit");
            match instance.mode {
                QueryMode::Always => {
                    tracing::debug!("miss (always)");
                    // this dependency should always be rerun, so the cache is never usable
                    return false;
                }
                QueryMode::Generation if inner.generation.is_newer_than(instance.generation) => {
                    tracing::debug!("miss (new generation)");
                    // this dependency should always be rerun, so the cache is never usable
                    return false;
                }
                _ => {
                    // otherwise, unsure, let's find out
                }
            }

            // if the color is green, it's a guaranteed hit
            // at least, if the generation changed just make sure that there
            // aren't any dependent generational queries that used to return Green
            // but over the generation really should have started to return Red.
            // TODO: check if there's a always dep
            if instance.color == QueryColor::Green
                && !inner.generation.is_newer_than(instance.generation)
                && !instance.transitively_has_always_dep
            {
                tracing::debug!("automatic hit");
                return true;
            }
        }

        // we need to rerun if any of the dependencies changed
        // since the last time we ran this query.
        // reasons dependencies can change:
        // * the generation updated and a dependency is generation dependent
        // * a dependency always has to rerun, and it produced a different result than before
        // * any of this happening recursively
        //
        // Thus, for any query, we need to know if deep down it contains any always-changing
        // dependencies, and generation-changing dependencies.
        //
        // Luckily, each dependent is itself a query, which knows for itself how to compute
        // whether it has to be rerun through this same function recursively. The cases above
        // are the base cases. Here, we can iterate over all the dependent queries and simply try
        // to run them all.
        let mut outgoing = self.inner.borrow().outgoing_edges(node);

        while let Some(i) = outgoing.next_node(self) {
            // when we run this query, the very first thing it does is call back into
            // this function but one layer down. We can then hit either a base case or
            // go through this recursive case again.
            self.run_query_instance_of_node(i);

            // after running (possibly a recompute), the result is either
            // red or green. On red, we know that one of the dependencies
            // changed and we need to recompute
            match self.inner.borrow().get_node(i).query_instance.color {
                QueryColor::Green => continue, // however, on green we can continue
                QueryColor::Red => {
                    tracing::debug!("cache miss");
                    return false;
                }
                QueryColor::Unknown => {
                    unreachable!()
                }
            }
        }

        tracing::debug!("cache hit all deps green");

        // if all inputs were green, we can set this node to green too.
        // it won't change (see Cache Promotion)
        self.inner
            .borrow_mut()
            .get_node_mut(node)
            .query_instance
            .color = QueryColor::Green;

        true
    }

    fn try_get_usable_cache(&self, node: usize) -> Option<TypeErasedQueryParam<'cx>> {
        if self.cache_usable(node) {
            // this possibly changed output we retrieve here.
            self.inner.borrow().get_node(node).query_instance.output
        } else {
            None
        }
    }

    fn is_cached(&self, input_hash: u128) -> Option<usize> {
        self.inner.borrow().lookup.get(&input_hash).copied()
    }

    fn run_query_instance_of_node(&self, node: usize) -> TypeErasedQueryParam<'cx> {
        {
            let mut inner = self.inner.borrow_mut();
            if let Some(old) = inner.curr {
                // add an edge between what used to be the current node, and this one
                // since we clearly depend on it.
                // TODO: dedupe?
                inner.add_edge_between(old, node);
            }
        }

        if self.inner.borrow().cache_enabled {
            // we got the id of a node. The node already contains an output. The question is,
            // can we use it? If we can, return that output
            if let Some(output) = self.try_get_usable_cache(node) {
                // Unerase the pointer. We know it's type is Q::Output here
                //
                // Note: This relies on there not being a hash collision between a hash
                // of an instance of Q::Output and some other T::Output
                return output;
            }
        }

        // otherwise, compute it
        let (run, input, old_curr_node, _span) = {
            let mut inner = self.inner.borrow_mut();

            // temporarily make this the "current" node we're working one
            // this is used to register what this query depends on while
            // running it
            let old_curr_node = inner.curr;
            inner.curr = Some(node);
            let node = inner.get_node_mut(node);
            tracing::debug!("yeet dep list of {}", node.query_instance);
            // reset the dependents list,
            // since we're going to be rerunning the query
            // and the dependents might change
            node.first_edge_index = None;
            node.last_edge_index = None;

            let span = tracing::span!(Level::DEBUG, "", "{}", node.query_instance).entered();
            tracing::debug!("run {}", node.query_instance);
            (
                node.query_instance.run,
                node.query_instance.input,
                old_curr_node,
                span,
            )
        };

        // run the actual query (through a pointer, since we don't know
        // anything about the concrete query type here)
        let (new_output, output_hash) = (run)(self, input, &|output_hash| {
            let inner = self.inner.borrow();
            let instance = &inner.get_node(node).query_instance;

            // no need to reallocate if the hash remains the same, the old allocation works
            instance.output.is_none() || output_hash != instance.output_hash
        });

        let mut inner = self.inner.borrow_mut();
        inner.curr = old_curr_node;
        // if the thing we just executed got marked as
        // transitively having an always dep, then the parent also does.
        if let Some(old_curr_node) = old_curr_node {
            let has_transitive_dep = inner
                .get_node(node)
                .query_instance
                .transitively_has_always_dep;

            if inner.get_node(old_curr_node).query_instance.mode == QueryMode::Always {
                inner
                    .get_node_mut(old_curr_node)
                    .query_instance
                    .transitively_has_always_dep = true;
            } else {
                inner
                    .get_node_mut(old_curr_node)
                    .query_instance
                    .transitively_has_always_dep = has_transitive_dep;
            }
        }

        let generation = inner.generation;
        let instance = &mut inner.get_node_mut(node).query_instance;

        tracing::trace!("hash: {} => {}", instance.output_hash, output_hash);

        // update the node based on the result
        instance.color = if instance.output.is_none() {
            assert!(
                new_output.is_some(),
                "should be some because of closure passed to run"
            );
            instance.output = new_output;
            instance.output_hash = output_hash;
            QueryColor::Red
        } else if output_hash == instance.output_hash {
            // if the hash of the output didn't change,
            // then this query is all safe to use wherever you want.
            QueryColor::Green
        } else {
            assert!(
                new_output.is_some(),
                "should be some because of closure passed to run"
            );
            instance.output = new_output;
            instance.output_hash = output_hash;

            QueryColor::Red
        };
        instance.generation = generation;

        // TODO: can be unchecked, we know for sure this is Some
        instance.output.unwrap()
    }

    /// Run a query in the query system. Queries look like functions, but
    /// you cannot actually call them directly. Instead, you should use code
    /// similar to the following example:
    ///
    /// ```rust
    /// # use moment::{Context, define_query, Storage};
    ///
    /// define_query! {
    ///     fn some_query<'cx>(cx: &Context<'cx>, param1: &u64, param2: &u64, param3: &u64) -> u64 {
    /// #       _ = (param2, param3);
    ///         *param1
    ///     }
    /// }
    /// # let (param1, param2, param3) = (1, 2, 3);
    ///
    /// let storage = Storage::new();
    ///
    /// let cx = Context::new(&storage);
    /// let output = cx.query(some_query, (param1, param2, param3));
    /// ```
    ///
    /// again, here `some_query` refers to the query that has to be run.
    /// It's definiton (using [`define_query`](crate::define_query))
    pub fn query<Q: Query<'cx>>(&self, query: Q, input: Q::Input) -> &'cx Q::Output {
        let input_hash = self.hash(query, &input);
        if self
            .inner
            .borrow_mut()
            .cycle_detection
            .push_is_cycle(input_hash)
        {
            let mut data = Vec::new();
            let inner = self.inner.borrow();

            if let Some(i) = inner.lookup.get(&input_hash) {
                let instance = &inner.get_node(*i).query_instance;
                data.push(format!("running query `{}` after running:", instance.name));
            } else {
                data.push(format!("running query `{}` after running:", Q::NAME));
            }

            let mut idx = 0;
            for i in inner.cycle_detection.history() {
                idx += 1;
                if let Some(i) = inner.lookup.get(&i) {
                    let instance = &inner.get_node(*i).query_instance;
                    data.push(format!("{idx}. query `{}`", instance.name));
                } else {
                    data.push(format!("{idx}. query with hash `{i}`"));
                }
            }
            panic!("cycle detected {}", data.join("\n"));
        }

        // if we call this query but no query was currently active
        // then this is a kind of query root. Anything it references
        // should stay alive, and if something is not referenced by
        // a root anymore we can safely garbage collect it.
        if self.inner.borrow().curr.is_none() {
            self.inner.borrow_mut().roots.push(input_hash);
        }

        let node = if let Some(node) = self.is_cached(input_hash) {
            node
        } else {
            // make a new node if we've not seen this query-input combination before.
            // Nodes have a unique input hash. Input hashes also hash the query identifier.
            // A new node represents a new (input, query) pair.

            let input_ref = &*self.storage.alloc(input);
            let query = QueryInstance {
                run: Q::get_run_fn(),
                name: Q::NAME,
                mode: query.mode(),
                generation: Generation::NULL,
                color: QueryColor::Unknown,
                input: TypeErasedQueryParam::new(input_ref),
                output: None,
                output_hash: 0,
                transitively_has_always_dep: query.mode() == QueryMode::Always,
            };

            let mut this = self.inner.borrow_mut();
            let node = this.nodes.len();
            this.nodes.push(Node {
                query_instance: query,
                first_edge_index: None,
                last_edge_index: None,
            });
            this.lookup.insert(input_hash, node);

            node
        };

        // Safety: we run a type erased query instance, but here we know that the
        // query type of the instance is Q
        let res = unsafe { self.run_query_instance_of_node(node).get_ref() };
        assert_eq!(self.inner.borrow_mut().cycle_detection.pop(), input_hash);
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::context::NO_HASHSET_LEN;

    use super::CycleDetection;

    fn cycle(mut c: CycleDetection) {
        assert!(!c.push_is_cycle(1));
        assert!(!c.push_is_cycle(2));
        assert!(!c.push_is_cycle(3));
        assert!(c.push_is_cycle(1));
        assert!(c.push_is_cycle(2));
        assert!(c.push_is_cycle(3));
        c.pop();
        assert!(!c.push_is_cycle(3));
    }

    #[test]
    fn test_cycle() {
        let c = CycleDetection::new();
        cycle(c);
    }

    #[test]
    fn test_long() {
        let mut c = CycleDetection::new();
        for i in 0..(NO_HASHSET_LEN + 10) {
            assert!(!c.push_is_cycle(i as u128));
        }
        assert!(c.push_is_cycle(1));
        assert!(c.push_is_cycle(NO_HASHSET_LEN as u128 + 9));
        c.pop();
        assert!(!c.push_is_cycle(NO_HASHSET_LEN as u128 + 9));
        for _ in 0..(NO_HASHSET_LEN + 9) {
            c.pop();
        }

        cycle(c);
    }

    #[test]
    #[should_panic]
    fn test_empty() {
        let mut c = CycleDetection::new();
        c.pop();
    }
}
