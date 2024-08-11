#![feature(macro_metavar_expr)]
#![doc = include_str!("../README.md")]

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
macro_rules! filter_attrs {
    (rerun $($tt:tt)*) => {};
    ($($tt:tt)*) => {$($tt)*};
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

#[macro_export]
#[doc(hidden)]
macro_rules! unit_if_empty {
    () => {
        ()
    };
    (()) => {
        ()
    };
    (ref $($tt: tt)*) => {
        &'cx ($($tt)*)
    };
    ($($tt: tt)*) => {
        $($tt)*
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! define_query_internal {
     (
        @
        // left to parse
        [
            [rerun(always)]
            $(
                [$($unparsed: tt)*]
            )*
        ];
        // attributes passed to function
        [$(
            [$($passedattr: tt)*
        ])*];
        // filtered/parsed attributes
        [$(
            [$($filteredattr: tt)*]
        )*];
        // function
        [$name: ident];
        [$lt: lifetime];
        [$cxname: ident];
        [$lt2: lifetime];
        [$($paramname: ident: &$param: ty),*];
        [$($ret: ty)?];
        [$block: block]
    ) => {
        $crate::define_query_internal!(@
                [
                    $([$($unparsed)*])*
                ];
                [ $([$($passedattr)*])* ];
                [
                    [
                        fn mode(&self) -> $crate::QueryMode {
                            $crate::QueryMode::Always
                        }
                    ]
                    $([$($filteredattr)*])*
                ];
                [$name];
                [$lt];
                [$cxname];
                [$lt2];
                [$($paramname: &$param),*];
                [$($ret)?];
                [$block]
        )
    };
    (
        @
        // left to parse
        [
            [rerun(generation)]
            $(
                [$($unparsed: tt)*]
            )*
        ];
        // attributes passed to function
        [$(
            [$($passedattr: tt)*
        ])*];
        // filtered/parsed attributes
        [$(
            [$($filteredattr: tt)*]
        )*];
        // function
        [$name: ident];
        [$lt: lifetime];
        [$cxname: ident];
        [$lt2: lifetime];
        [$($paramname: ident: &$param: ty),*];
        [$($ret: ty)?];
        [$block: block]
    ) => {
        $crate::define_query_internal!(@
                [
                    $([$($unparsed)*])*
                ];
                [ $([$($passedattr)*])* ];
                [
                    [
                        fn mode(&self) -> $crate::QueryMode {
                            $crate::QueryMode::Generation
                        }
                    ]
                    $([$($filteredattr)*])*
                ];
                [$name];
                [$lt];
                [$cxname];
                [$lt2];
                [$($paramname: &$param),*];
                [$($ret)?];
                [$block]
        )
    };
    (
        @
        // left to parse
        [
            [rerun $($rest:tt)*]
            $(
                [$($unparsed: tt)*]
            )*
        ];
        // attributes passed to function
        [$(
            [$($passedattr: tt)*
        ])*];
        // filtered/parsed attributes
        [$(
            [$($filteredattr: tt)*]
        )*];
        // function
        [$name: ident];
        [$lt: lifetime];
        [$cxname: ident];
        [$lt2: lifetime];
        [$($paramname: ident: &$param: ty),*];
        [$($ret: ty)?];
        [$block: block]
    ) => {
        compile_error!(concat!("#[rerun", stringify!($($rest)*), "] is not valid: valid attributes are #[rerun(always)] and #[rerun(generation)]"));
        $crate::define_query_internal!(@
                [
                    $([$($unparsed)*])*
                ];
                [ $([$($passedattr)*])* ];
                [
                    $([$($filteredattr)*])*
                ];
                [$name];
                [$lt];
                [$cxname];
                [$lt2];
                [$($paramname: &$param),*];
                [$($ret)?];
                [$block]
        )
    };
    (
        @
        // left to parse
        [
            [$($any: tt)*]
            $(
                [$($unparsed: tt)*]
            )*
        ];
        // attributes passed to function
        [$(
            [$($passedattr: tt)*
        ])*];
        // filtered/parsed attributes
        [$(
            [$($filteredattr: tt)*]
        )*];
        // function
        [$name: ident];
        [$lt: lifetime];
        [$cxname: ident];
        [$lt2: lifetime];
        [$($paramname: ident: &$param: ty),*];
        [$($ret: ty)?];
        [$block: block]
    ) => {
        $crate::define_query_internal!(@
                [
                    $([$($unparsed)*])*
                ];
                [ $([$($passedattr)*])* ];
                [ $([$($filteredattr)*])* ];
                [$name];
                [$lt];
                [$cxname];
                [$lt2];
                [$($paramname: &$param),*];
                [$($ret)?];
                [$block]
        )
    };

    (
        @
        // left to parse
        [];
        // attributes passed to function
        [$(
            [$($passedattr: tt)*
        ])*];
        // filtered/parsed attributes
        [$(
            [$($filteredattr: tt)*]
        )*];
        // function
        [$name: ident];
        [$lt: lifetime];
        [$cxname: ident];
        [$lt2: lifetime];
        [$($paramname: ident: &$param: ty),*];
        [$($ret: ty)?];
        [$block: block]
    ) => {
            #[allow(unused_parens)]
            pub fn $name<'cx>(
                cx: &$crate::Context<$lt2>,
                $($paramname: $param),*
            )-> $crate::unit_if_empty!(ref $($ret)?) {
                #[warn(unused_parens)]
                {
                    #[derive(Copy, Clone)]
                    #[allow(non_camel_case_types)]
                    struct Q;

                    impl<$lt> $crate::Query<$lt> for Q {
                        type Input = $crate::tup_or_empty!($($param)*);
                        type Output = $crate::unit_if_empty!($($ret)?);

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
                                let output = <Q as $crate::Query<'cx>>::run(cx, input);

                                let output_hash = cx.hash(Q, &output);
                                if should_alloc(output_hash) {
                                    (Some($crate::TypeErasedQueryParam::new(cx.storage.alloc(output))), output_hash)
                                } else {
                                    (None, output_hash)
                                }
                            }

                            run
                        }

                        $($($filteredattr)*)*

                        fn run($cxname: &$crate::Context<$lt2>, $crate::tup_or_empty!($($paramname)*): &Self::Input) -> Self::Output $block
                    }

                    cx.query(Q, $crate::tup_or_empty!($($paramname)*))
                }
            }
    }
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
/// # use incremental_query::define_query;
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
/// To run a query, including all the cache mechanics, simply call it.
/// The only requirement is that you give it an argument of `&Context<'cx>`.
/// Internally, the function you wrote is actually wrapped in such a way that caching
/// can occur.
///
/// ```rust
/// # use incremental_query::{Context, define_query, Storage};
/// define_query! {
///     fn some_query<'cx>(cx: &Context<'cx>, param1: &u64, param2: &u64, param3: &u64) -> u64 {
/// #       _ = (param2, param3);
/// #       *param1
///         // ...
///     }
/// }
/// # let (param1, param2, param3) = (1, 2, 3);
///
/// let storage = Storage::new();
///
/// let cx = Context::new(&storage);
/// let output = some_query(&cx, param1, param2, param3);
/// ```
///
/// again, here `some_query` refers to the query that has to be run.
/// It's definiton (using [`define_query`](crate::define_query))
#[macro_export]
macro_rules! define_query {
    (
        $(
            $(#[$($attr: tt)*])*

            fn $name: ident <$lt: lifetime>
            ($cxname: ident: &Context<$lt2: lifetime> $(,$paramname: ident: &$param: ty)* $(,)?)
            $(-> $ret: ty)?
            $block: block
        )*
    ) => {
        $(
            $crate::define_query_internal!(@
                [
                    $([$($attr)*])*
                ];
                [];
                [];
                [$name];
                [$lt];
                [$cxname];
                [$lt2];
                [$($paramname: &$param),*];
                [$($ret)?];
                [$block]
            );
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
    use crate::log;

    // #[test]
    // fn query_mode() {
    //     define_query! {
    //         #[rerun(always)]
    //         fn always<'cx>(_cx: &Context<'cx>, _inp: &())  {}
    //         fn cache<'cx>(_cx: &Context<'cx>, _inp: &())  {}
    //         #[rerun(generation)]
    //         fn generation<'cx>(_cx: &Context<'cx>, _inp: &())  {}
    //     }
    //     log();
    //
    //     assert_eq!(always.mode(), QueryMode::Always);
    //     assert_eq!(generation.mode(), QueryMode::Generation);
    //     assert_eq!(cache.mode(), QueryMode::Cache);
    // }

    use std::{
        cell::{Cell, RefCell},
        collections::VecDeque,
        hash::Hash,
        rc::Rc,
    };

    use crate::{query_parameter::QueryParameter, storage::Storage, Context, QueryHasher};
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
            fn test<'cx>(_cx: &Context<'cx>, input: &Counter)  {
                input.add();
            }
        }

        log();
        let storage = Storage::new();

        let ctr = Counter::new(0);
        let cx = Context::new(&storage);
        test(&cx, ctr.clone());
        assert_eq!(ctr.get(), 1);
    }

    #[test]
    fn call_twice() {
        define_query! {
            fn called_twice<'cx>(_cx: &Context<'cx>, input: &Counter) {
               input.add();
            }
        }
        log();

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let cx = Context::new(&storage);

        called_twice(&cx, ctr.clone());
        called_twice(&cx, ctr.clone());
        assert_eq!(ctr.get(), 1);
    }

    #[test]
    fn impure_rerun() {
        define_query! {
            #[rerun(always)]
            fn random<'cx>(_cx: &Context<'cx>) -> u64 {
                thread_rng().gen_range(0..u64::MAX)
            }

            fn depends_on_impure<'cx>(cx: &Context<'cx>, inp: &Counter)  {
                inp.add();
                let _dep = random(cx);
            }
        }

        log();

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let cx = Context::new(&storage);
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());

        assert_eq!(ctr.get(), 2);
    }

    #[test]
    fn test_intvalue() {
        define_query! {
            #[rerun(always)]
            fn intvalue<'cx>(_cx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = intvalue(cx, r.clone());
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter)  {
                c1.add();
                sign_of(cx, r.clone(), c2.clone());
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
        let cx = Context::new(&storage);

        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());
        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 2);
    }

    #[test]
    fn test_intvalue_generational() {
        define_query! {
            #[rerun(generation)]
            fn intvalue<'cx>(_cx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = intvalue(cx, r.clone());
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter)  {
                c1.add();
                sign_of(cx, r.clone(), c2.clone());
            }
        }
        log();

        let storage = Storage::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let cx = Context::new(&storage);

        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());
        cx.next_generation();
        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 2);
    }

    #[test]
    fn test_intvalue_many_generation() {
        define_query! {
            #[rerun(generation)]
            fn intvalue<'cx>(_cx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = intvalue(cx, r.clone());
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter)  {
                c1.add();
                sign_of(cx, r.clone(), c2.clone());
            }
        }
        log();

        let storage = Storage::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000, 3000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let cx = Context::new(&storage);

        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());
        cx.next_generation();
        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());
        cx.next_generation();
        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());

        assert!(order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 3);
    }

    #[test]
    fn test_intvalue_assume_pure_wrong() {
        define_query! {
            fn intvalue<'cx>(_cx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c2: &Counter) -> bool {
                let v = intvalue(cx, r.clone());
                c2.add();

                v.is_positive()
            }

            fn some_other_query<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>, c1: &Counter, c2: &Counter)  {
                c1.add();
                sign_of(cx, r.clone(), c2.clone());
            }
        }
        log();

        let storage = Storage::new();

        // now 2000 is there only once because the query is generational, and the first run in the
        // 2nd generation can be cached.
        let order = ReturnInOrder::new(vec![1000, 2000], 1);
        let counter1 = Counter::new(1);
        let counter2 = Counter::new(2);
        let cx = Context::new(&storage);

        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());
        some_other_query(&cx, order.clone(), counter1.clone(), counter2.clone());

        assert!(!order.is_empty());
        assert_eq!(counter1.get(), 1);
        assert_eq!(counter2.get(), 1);
    }

    #[test]
    fn conditional_query() {
        define_query! {
            #[rerun(generation)]
            fn boolean_query<'cx>(_cx: &Context<'cx>, r: &ReturnInOrder<bool>) -> bool {
                r.next()
            }

            fn one<'cx>(_cx: &Context<'cx>) -> u64 {
                1
            }
            fn two<'cx>(_cx: &Context<'cx>, c: &Counter) -> u64 {
                c.add();
                2
            }

            fn conditional<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<bool>, c: &Counter) -> u64 {
                if *boolean_query(cx, r.clone()) {
                    *one(cx)
                } else {
                    *two(cx, c.clone())
                }
            }
        }

        log();

        let storage = Storage::new();
        let order = ReturnInOrder::new(vec![true, false], 1);
        let counter = Counter::new(0);
        let cx = Context::new(&storage);

        assert_eq!(conditional(&cx, order.clone(), counter.clone()), &1);
        cx.next_generation();
        assert_eq!(conditional(&cx, order.clone(), counter.clone()), &2);
        assert_eq!(conditional(&cx, order.clone(), counter.clone()), &2);
        assert_eq!(conditional(&cx, order.clone(), counter.clone()), &2);
        assert_eq!(counter.get(), 1);
        assert!(order.is_empty())
    }

    #[test]
    #[should_panic]
    fn cycle() {
        define_query! {
            fn cyclic<'cx>(cx: &Context<'cx>, r: &u64) -> bool {
              *cyclic(cx, *r)
            }
        }

        log();

        let storage = Storage::new();
        let cx = Context::new(&storage);
        cyclic(&cx, 10);
    }

    #[test]
    #[should_panic]
    fn long_cycle() {
        define_query! {
            fn e<'cx>(cx: &Context<'cx>, r: &u64) -> bool {
              *a(cx, *r)
            }

            fn d<'cx>(cx: &Context<'cx>, r: &u64) -> bool {
              *e(cx, *r)
            }

            fn c<'cx>(cx: &Context<'cx>, r: &u64) -> bool {
              *d(cx, *r)
            }

            fn b<'cx>(cx: &Context<'cx>, r: &u64) -> bool {
              *c(cx, *r)
            }

            fn a<'cx>(cx: &Context<'cx>, r: &u64) -> bool {
              *b(cx, *r)
            }
        }
        log();

        let storage = Storage::new();
        let cx = Context::new(&storage);
        a(&cx, 10);
    }

    #[test]
    fn garbage_collect() {
        define_query! {
            fn value_dependent<'cx>(_cx: &Context<'cx>, r: &i64) -> i64 {
                r * 2
            }

            #[rerun(generation)]
            fn intvalue<'cx>(_cx: &Context<'cx>, r: &ReturnInOrder<i64>) -> i64 {
                r.next()
            }

            fn sign_of<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>) -> bool {
                let v = intvalue(cx, r.clone());
                value_dependent(cx, *v).is_positive()
            }

            fn some_other_query<'cx>(cx: &Context<'cx>, r: &ReturnInOrder<i64>)  {
                sign_of(cx, r.clone());
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
        let cx = Context::new(&storage);

        for _ in 0..N {
            some_other_query(&cx, order.clone());
            cx.next_generation();
        }
        assert!(order.is_empty());

        let new_storage = Storage::new();

        tracing::info!("{}", cx.size());
        let res = cx.gc(&new_storage);
        drop(storage);
        tracing::info!("{}", res.size());
    }

    #[test]
    fn impure_cache_rerun() {
        define_query! {
            #[rerun(always)]
            fn random<'cx>(_cx: &Context<'cx>) -> u64 {
                thread_rng().gen_range(0..u64::MAX)
            }

            fn depends_on_impure<'cx>(cx: &Context<'cx>, inp: &Counter)  {
                inp.add();
                let _dep = random(&cx);
            }
        }

        log();

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let cx = Context::new(&storage);
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());

        assert_eq!(ctr.get(), 5);
    }

    #[test]
    fn cache_off() {
        define_query! {
            /// Hello, this query is even documented!
            #[inline(always)]
            fn random<'cx>(_cx: &Context<'cx>,) -> u64 {
                thread_rng().gen_range(0..u64::MAX)
            }

            fn depends_on_impure<'cx>(cx: &Context<'cx>, inp: &Counter)  {
                inp.add();
                let _dep = random(cx);

            }
        }

        log();

        let storage = Storage::new();

        let ctr = Counter::new(0);
        let cx = Context::new(&storage);
        cx.set_cache_enabled(false);
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());

        assert_eq!(ctr.get(), 5);

        let ctr = Counter::new(0);
        let cx = Context::new(&storage);
        cx.set_cache_enabled(true);
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());
        depends_on_impure(&cx, ctr.clone());

        assert_eq!(ctr.get(), 1);
    }
}
