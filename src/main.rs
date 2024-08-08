#![feature(macro_metavar_expr)]

use bumpalo::Bump;
use incremental::{context::Context, query_parameter::QueryParameter,  QueryHasher};
use tracing_subscriber::EnvFilter;

#[macro_use]
pub mod incremental;

#[derive(Clone, Copy)]
struct CompilerConfig {}

impl QueryParameter for CompilerConfig {
    fn hash_stable(&self, _hasher: &mut QueryHasher) {}
}

define_query! {
    fn compile<'cx>(_ctx: &Context<'cx>, _config: &CompilerConfig) -> () {
        tracing::info!("hi");
    }
}

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let config = CompilerConfig {};

    let arena = Bump::new();
    let ctx = Context::new(&arena);
    ctx.query( compile, (config,));
}

#[cfg(test)]
mod tests {
    use std::{cell::{Cell, RefCell}, collections::VecDeque, hash::Hash, rc::Rc};
    use tracing_subscriber::EnvFilter;

    use bumpalo::Bump;

    use rand::{thread_rng, Rng};
    use crate::incremental::{ context::Context, query::{Query, QueryMode}, query_parameter::QueryParameter,  QueryHasher};


    #[derive(Clone)]
    struct Counter(pub u64, Rc<Cell<usize>>);
    impl Counter {
        // create a new counter that counts how many times it has been add() ed to
        // The metric itself is not hashed and is thus not compared for equality
        // while calling queries
        pub fn new(i: u64) -> Self {
            Self(i, Default::default())
        }

        pub fn add(&self) {
            self.1.set(self.1.get() + 1);
        }

        pub fn get(&self) -> usize {
            self.1.get()
        }
    }

    #[derive(Clone)]
    struct ReturnInOrder<T> {
        data: Rc<RefCell<VecDeque<T>>>,
        id: u64,
    }

    impl<T> ReturnInOrder<T> {
        fn new(inp: impl IntoIterator<Item = T>, id: u64) -> Self {
            Self {
                data: Rc::new(RefCell::new(inp.into_iter().collect())),
                id,
            }
        }

        fn is_empty(&self) -> bool {
            self.data.borrow().len() == 0
        }

        fn next(&self) -> T {
            self.data.borrow_mut().pop_front().expect("empty")
        }
    } 

    impl QueryParameter for Counter {
        fn hash_stable(&self, hasher: &mut QueryHasher) {
            self.0.hash(hasher);
        }
    }
    impl<T> QueryParameter for ReturnInOrder<T> {
        fn hash_stable(&self, hasher: &mut QueryHasher) {
            self.id.hash(hasher);
        }
    }

    fn log() {
        let format = tracing_subscriber::fmt::format()
            .without_time()
            .with_source_location(false);

        tracing_subscriber::fmt()
            .event_format(format)
            .with_env_filter(EnvFilter::from_default_env())
            .try_init()
            .ok();
    }

    #[test]
    fn call_once() {
        define_query! {
            fn test<'cx>(_ctx: &Context<'cx>, input: &Counter) -> () {
                input.add();
            }
        }

        log();
        let arena = Bump::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&arena);
        ctx.query( test, (ctr.clone(),));
        assert_eq!(ctr.get(), 1);
    }

    #[test]
    fn call_twice() {
       define_query! {
            fn called_twice<'cx>(_ctx: &Context<'cx>, input: &Counter) -> () {
               input.add();
            }
        }
        log();

        let arena = Bump::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&arena);

        ctx.query(called_twice, (ctr.clone(),));
        ctx.query(called_twice, (ctr.clone(),));
        assert_eq!(ctr.get(), 1);
    }

    #[test]
    fn impure_flag() {
        define_query! {
            #[rerun(always)] 
            fn always<'cx>(_ctx: &Context<'cx>, _inp: &()) -> () {}
        }
        define_query! {
            fn cache<'cx>(_ctx: &Context<'cx>, _inp: &()) -> () {}
        }
        define_query! {
            #[rerun(generation)]
            fn generation<'cx>(_ctx: &Context<'cx>, _inp: &()) -> () {}
        }
        log();

        assert_eq!(always.mode(), QueryMode::Always);
        assert_eq!(generation.mode(), QueryMode::Generation);
        assert_eq!(cache.mode(), QueryMode::Cache);
    }

    #[test]
    fn impure_rerun() {
        define_query! {
            #[rerun(always)] 
            fn random<'cx>(_ctx: &Context<'cx>,) -> u64 {
                thread_rng().gen_range(0..u64::MAX)
            }
        }
        define_query! {
            fn depends_on_impure<'cx>(ctx: &Context<'cx>, inp: &Counter) -> () {
                inp.add();
                let _dep = ctx.query(random, ());

            }
        }

        log();

        let arena = Bump::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&arena);
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));

        assert_eq!(ctr.get(), 2);
    }

    #[test]
    fn test_intvalue() {
        define_query! {
            #[rerun(always)]
            fn intvalue<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }
        }
        define_query! {
            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }
        }
        define_query! {
            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let arena = Bump::new();

        // note: 2000 is here twice since initvalue is actuall run three times in total.
        // * once for some_other_query's first invocation
        // * once to figure out that some_other_query's second invocation
        // * once to actually run `sign_of` a second time after we figured out that we have to.
        // TODO: can this be optimized?
        let order = ReturnInOrder::new(vec![1000, 2000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&arena);

        ctx.query(some_other_query, (order.clone(), counter1.clone(), counter2.clone()));
        ctx.query(some_other_query, (order.clone(), counter1.clone(), counter2.clone()));

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 2);
    }

    #[test]
    fn test_intvalue_generational() {
        define_query! {
            #[rerun(generation)]
            fn intvalue<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }
        }
        define_query! {
            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }
        }
        define_query! {
            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let arena = Bump::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&arena);

        ctx.query(some_other_query, (order.clone(), counter1.clone(), counter2.clone()));
        ctx.next_generation();
        tracing::info!("gen");
        ctx.query(some_other_query, (order.clone(), counter1.clone(), counter2.clone()));

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 2);
    }

    #[test]
    fn test_intvalue_assume_pure_wrong() {
        define_query! {
            fn intvalue<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }
        }
        define_query! {
            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }
        }
        define_query! {
            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let arena = Bump::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&arena);

        ctx.query(some_other_query, (order.clone(), counter1.clone(), counter2.clone()));
        ctx.query(some_other_query, (order.clone(), counter1.clone(), counter2.clone()));

        assert!(!order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 1);
    }
}
