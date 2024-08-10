#![feature(macro_metavar_expr)]

use siphasher::sip128::SipHasher13;

mod context;
mod generation;
mod query;
mod query_parameter;
mod storage;

pub use context::Context;
pub use query_parameter::QueryParameter;
pub use storage::Storage;

// used in macros, hidden to the user
#[doc(hidden)]
pub use query::{ErasedQueryRun, Query, QueryMode};
#[doc(hidden)]
pub use query_parameter::TypeErasedQueryParam;

pub type QueryHasher = SipHasher13;

#[doc(hidden)]
#[macro_export]
macro_rules! parse_attrs {
    (rerun(always) $($rest:tt)*) => {
        fn mode(&self) -> $crate::QueryMode {
            $crate::query::QueryMode::Always
        }

        parse_attrs!($($rest)*);
    };
    (rerun(generation) $($rest:tt)*) => {
        fn mode(&self) -> $crate::QueryMode {
            $crate::QueryMode::Generation
        }

        parse_attrs!($($rest)*);
    };

    () => {};
}

#[macro_export]
#[doc(hidden)]
macro_rules! tup_or_empty {
    () => {
        ()
    };
    ($($param: tt)*) => {
        ($($param),*,)
    };
}

/// A macro to define a query. 
///
/// A query is a lot like a function, except for the small detail that 
/// you cannot call it. Instead queries expand to an identifier you can pass 
/// to a [`Context`](crate::Context) through which you can execute the function.
///
/// The following example should illustrate its syntax pretty well:
///
/// ```rust
/// # use moment::define_query;
/// define_query! {
///     // note: the lifetime <'cx> is required, (though you can choose a different name)
///     fn some_query<'cx>(
///         // this first parameter is required! though you can change the name it gets.
///         cx: &Context<'cx>,
///         // any number of parameters can follow, but they *must* be of type
///         // &T where T: QueryParameter.
///         param1: &u64, param2: &u64, param3: &u64) -> u64 {
/// #       _ = (param2, param3);
/// #       *param1
///         // ...
///     }
///
///     // more queries can follow
/// }
/// ```
///
/// The output and input of queries are cached, 
/// and its dependencies are automatically tracked.
///
#[macro_export]
macro_rules! define_query {
    (
        $(
            $(#[$($attr: tt)*])*
            fn $name: ident <$lt: lifetime>
            ($cxname: ident: &Context<'cx> $(,$paramname: ident: &$param: ty)* $(,)?)
            -> $ret: ty
            $block: block
        )*
    ) => {
        $(
            #[derive(Copy, Clone)]
            #[allow(non_camel_case_types)]
            struct $name;

            impl<$lt> $crate::Query<$lt> for $name {
                type Input = $crate::tup_or_empty!($($param)*);
                type Output = $ret;

                const NAME: &'static str = stringify!($name);

                fn get_run_fn() -> $crate::ErasedQueryRun {
                    fn run<'cx>(
                        cx: &$crate::Context<'cx>,
                        input: $crate::TypeErasedQueryParam<'cx>,
                        should_alloc: &dyn Fn(u128) -> bool,
                    ) -> (Option<$crate::TypeErasedQueryParam<'cx>>, u128) {
                        // safety: we know thatcx is Q::Input here because this function is generated
                        // together with the definition of the query
                        let input: &$crate::tup_or_empty!($($param)*) = unsafe{input.get_ref()};
                        let output = <$name as $crate::Query<'cx>>::run(cx, input);

                        let output_hash = cx.hash($name, &output);
                        if should_alloc(output_hash) {
                            (Some($crate::TypeErasedQueryParam::new(cx.storage.alloc(output))), output_hash)
                        } else {
                            (None, output_hash)
                        }
                    }

                    run
                }

                $crate::parse_attrs!($($($attr)*)*);

                fn run($cxname: &$crate::Context<'cx>, $crate::tup_or_empty!($($paramname)*): &Self::Input) -> Self::Output $block
            }
        )*
    };
}

#[cfg(test)]
pub fn log() {
    use tracing_subscriber::EnvFilter;

    let format = tracing_subscriber::fmt::format()
        .without_time()
        .with_source_location(false);

    tracing_subscriber::fmt()
        .event_format(format)
        .with_env_filter(EnvFilter::from_default_env())
        .try_init()
        .ok();
}

#[cfg(test)]
mod tests {
    use crate::{log, query::QueryMode};

    #[test]
    fn query_mode() {
        define_query! {
            #[rerun(always)]
            fn always<'cx>(_ctx: &Context<'cx>, _inp: &()) -> () {}
            fn cache<'cx>(_ctx: &Context<'cx>, _inp: &()) -> () {}
            #[rerun(generation)]
            fn generation<'cx>(_ctx: &Context<'cx>, _inp: &()) -> () {}
        }
        log();

        assert_eq!(always.mode(), QueryMode::Always);
        assert_eq!(generation.mode(), QueryMode::Generation);
        assert_eq!(cache.mode(), QueryMode::Cache);
    }

    use std::{
        cell::{Cell, RefCell},
        collections::VecDeque,
        hash::Hash,
        rc::Rc,
    };

    use crate::{query_parameter::QueryParameter, storage::Storage, Context, Query, QueryHasher};
    use rand::{thread_rng, Rng};

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
    impl<T: Clone + 'static> QueryParameter for ReturnInOrder<T> {
        fn hash_stable(&self, hasher: &mut QueryHasher) {
            self.id.hash(hasher);
        }
    }

    #[test]
    fn call_once() {
        define_query! {
            fn test<'cx>(_ctx: &Context<'cx>, input: &Counter) -> () {
                input.add();
            }
        }

        log();
        let storage = Storage::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&storage);
        ctx.query(test, (ctr.clone(),));
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

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&storage);

        ctx.query(called_twice, (ctr.clone(),));
        ctx.query(called_twice, (ctr.clone(),));
        assert_eq!(ctr.get(), 1);
    }

    #[test]
    fn impure_rerun() {
        define_query! {
            #[rerun(always)]
            fn random<'cx>(_ctx: &Context<'cx>,) -> u64 {
                thread_rng().gen_range(0..u64::MAX)
            }

            fn depends_on_impure<'cx>(ctx: &Context<'cx>, inp: &Counter) -> () {
                inp.add();
                let _dep = ctx.query(random, ());

            }
        }

        log();

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&storage);
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

            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let storage = Storage::new();

        // note: 2000 is here twice since initvalue is actuall run three times in total.
        // * once for some_other_query's first invocation
        // * once to figure out that some_other_query's second invocation
        // * once to actually run `sign_of` a second time after we figured out that we have to.
        // TODO: can this be optimized?
        let order = ReturnInOrder::new(vec![1000, 2000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&storage);

        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );
        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );

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

            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let storage = Storage::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&storage);

        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );
        ctx.next_generation();
        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 2);
    }

    #[test]
    fn test_intvalue_many_generation() {
        define_query! {
            #[rerun(generation)]
            fn intvalue<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let storage = Storage::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000, 3000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&storage);

        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );
        ctx.next_generation();
        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );
        ctx.next_generation();
        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 3);
    }

    #[test]
    fn test_intvalue_assume_pure_wrong() {
        define_query! {
            fn intvalue<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter) -> () {
                c1.add();
                ctx.query(sign_of, (r.clone(), c2.clone()));
            }
        }
        log();

        let storage = Storage::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let ctx = Context::new(&storage);

        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );
        ctx.query(
            some_other_query,
            (order.clone(), counter1.clone(), counter2.clone()),
        );

        assert!(!order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 1);
    }

    #[test]
    fn conditional_query() {
        define_query! {
            #[rerun(generation)]
            fn boolean_query<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<bool>) -> bool {
                r.next()
            }

            fn one<'cx>(_ctx: &Context<'cx>) -> u64 {
                1
            }
            fn two<'cx>(_ctx: &Context<'cx>, c: &Counter) -> u64 {
                c.add();
                2
            }
        };
        define_query! {
            fn conditional<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<bool>, c: &Counter) -> u64 {
                if *ctx.query(boolean_query, (r.clone(),)) {
                    *ctx.query(one, ())
                } else {
                    *ctx.query(two, (c.clone(),))
                }
            }
        }

        log();

        let storage = Storage::new();
        let order = ReturnInOrder::new(vec![true, false], 1);
        let counter = Counter::new(0);
        let ctx = Context::new(&storage);

        assert_eq!(ctx.query(conditional, (order.clone(), counter.clone())), &1);
        ctx.next_generation();
        assert_eq!(ctx.query(conditional, (order.clone(), counter.clone())), &2);
        assert_eq!(ctx.query(conditional, (order.clone(), counter.clone())), &2);
        assert_eq!(ctx.query(conditional, (order.clone(), counter.clone())), &2);
        assert_eq!(counter.get(), 1);
        assert!(order.is_empty())
    }

    #[test]
    #[should_panic]
    fn cycle() {
        define_query! {
            fn cyclic<'cx>(ctx: &Context<'cx>, r: &u64) -> bool {
              *ctx.query(cyclic, (*r,))
            }
        }

        log();

        let storage = Storage::new();
        let ctx = Context::new(&storage);
        ctx.query(cyclic, (10,));
    }

    #[test]
    #[should_panic]
    fn long_cycle() {
        define_query! {
            fn e<'cx>(ctx: &Context<'cx>, r: &u64) -> bool {
              *ctx.query(a, (*r,))
            }

            fn d<'cx>(ctx: &Context<'cx>, r: &u64) -> bool {
              *ctx.query(e, (*r,))
            }

            fn c<'cx>(ctx: &Context<'cx>, r: &u64) -> bool {
              *ctx.query(d, (*r,))
            }

            fn b<'cx>(ctx: &Context<'cx>, r: &u64) -> bool {
              *ctx.query(c, (*r,))
            }

            fn a<'cx>(ctx: &Context<'cx>, r: &u64) -> bool {
              *ctx.query(b, (*r,))
            }
        }
        log();

        let storage = Storage::new();
        let ctx = Context::new(&storage);
        ctx.query(a, (10,));
    }

    #[test]
    fn garbage_collect() {
        define_query! {
            fn value_dependent<'cx>(_ctx: &Context<'cx>, r: &i64) -> i64 {
                r * 2
            }

            #[rerun(generation)]
            fn intvalue<'cx>(_ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> bool {
                let v = ctx.query(intvalue, (r.clone(),));
                ctx.query(value_dependent, (*v,)).is_positive()
            }

            fn some_other_query<'cx>(ctx: &Context<'cx>, r: &ReturnInOrder<i64>) -> () {
                ctx.query(sign_of, (r.clone(),));
            }
        }
        log();

        const N: i64 = 1000;

        let storage = Storage::new();

        let mut data = Vec::new();
        for i in 0..N {
            data.push(i * 1000);
        }

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(data, 1);
        let ctx = Context::new(&storage);

        for _ in 0..N {
            ctx.query(some_other_query, (order.clone(),));
            ctx.next_generation();
        }
        assert!(order.is_empty());

        let new_storage = Storage::new();

        tracing::info!("{}", ctx.size());
        let res = ctx.gc(&new_storage);
        drop(storage);
        tracing::info!("{}", res.size());
    }

    #[test]
    fn impure_cache_rerun() {
        define_query! {
            #[rerun(always)]
            fn random<'cx>(_ctx: &Context<'cx>,) -> u64 {
                thread_rng().gen_range(0..u64::MAX)
            }

            fn depends_on_impure<'cx>(ctx: &Context<'cx>, inp: &Counter) -> () {
                inp.add();
                let _dep = ctx.query(random, ());

            }
        }

        log();

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&storage);
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));

        assert_eq!(ctr.get(), 5);
    }

    #[test]
    fn cache_off() {
        define_query! {
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

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let ctx = Context::new(&storage);
        ctx.set_cache_enabled(false);
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));

        assert_eq!(ctr.get(), 5);

        let ctr = Counter::new(0);
        let ctx = Context::new(&storage);
        ctx.set_cache_enabled(true);
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));
        ctx.query(depends_on_impure, (ctr.clone(),));

        assert_eq!(ctr.get(), 1);
    }
}
