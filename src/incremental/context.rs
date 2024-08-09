use std::{
    any::Any,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    num::NonZeroUsize,
};

use siphasher::sip128::Hasher128;
use tracing::Level;

use crate::incremental::query::{QueryColor, QueryMode};

use super::{
    query::{Query, QueryInstance},
    query_parameter::{QueryParameter, TypeErasedQueryParam},
    storage::Storage,
    QueryHasher,
};

#[derive(Clone, Copy)]
pub struct Generation(u64);

impl Generation {
    // special null generation; never executed
    const NULL: Generation = Generation(0);

    fn new() -> Self {
        // generations start at 1
        Self(1)
    }

    pub fn is_newer_than(&self, older: Self) -> bool {
        older.0 < self.0
    }

    pub fn next(&mut self) {
        self.0 += 1;
    }
}

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
    curr: Option<Edge>,
}

impl OutgoingEdgeIterator {
    fn next_node(&mut self, cx: &Context) -> Option<usize> {
        let Edge { node, next_edge } = self.curr?;
        self.curr = next_edge.map(|e| cx.inner.borrow().edges[e.get()]);
        Some(node)
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

pub struct Inner<'cx> {
    // index in the nodes array
    curr: Option<usize>,

    nodes: Vec<Node<'cx>>,
    edges: Vec<Edge>,

    // map hash to node index
    lookup: HashMap<u128, usize>,

    cycle_detection: CycleDetection,

    generation: Generation,
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
        let node = self.get_node(node);
        let first_edge_index = node.first_edge_index;

        OutgoingEdgeIterator {
            curr: first_edge_index.map(|e| self.edges[e.get()]),
        }
    }
}

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
            }),
        }
    }

    pub fn next_generation(&self) {
        self.inner.borrow_mut().generation.next();
    }

    // pub fn serialize() {}
    // pub fn gc() {}
    // pub fn deserialize() {}

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
            if instance.color == QueryColor::Green {
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
                    return false;
                }
                QueryColor::Unknown => {
                    unreachable!()
                }
            }
        }

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
        // we got the id of a node. The node already contains an output. The question is,
        // can we use it? If we can, return that output
        if let Some(output) = self.try_get_usable_cache(node) {
            // Unerase the pointer. We know it's type is Q::Output here
            //
            // Note: This relies on there not being a hash collision between a hash
            // of an instance of Q::Output and some other T::Output
            return output;
        }

        // otherwise, compute it
        let (run, input, old_curr_node, _span) = {
            let mut inner = self.inner.borrow_mut();

            // temporarily make this the "current" node we're working one
            // this is used to register what this query depends on while
            // running it
            let old_curr_node = inner.curr;
            inner.curr = Some(node);
            if let Some(old) = old_curr_node {
                // add an edge between what used to be the current node, and this one
                // since we clearly depend on it.
                // TODO: dedupe?
                inner.add_edge_between(old, node);
            }

            let node = inner.get_node_mut(node);
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
        let generation = inner.generation;
        let instance = &mut inner.get_node_mut(node).query_instance;

        tracing::debug!("hash: {} => {}", instance.output_hash, output_hash);

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
                data.push(format!("running query {instance}"));
            } else {
                data.push(format!("running query {}", Q::NAME));
            }

            let mut idx = 0;
            for i in inner.cycle_detection.history() {
                idx += 1;
                if let Some(i) = inner.lookup.get(&i) {
                    let instance = &inner.get_node(*i).query_instance;
                    data.push(format!("{idx}. query {instance}"));
                } else {
                    data.push(format!("{idx}. query with hash `{i}`"));
                }
            }
            panic!("cycle detected {}", data.join("\n"));
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
    use crate::incremental::context::NO_HASHSET_LEN;

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
