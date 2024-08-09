use siphasher::sip128::SipHasher13;

pub mod context;
pub mod query;
pub mod query_parameter;
pub mod storage;

pub type QueryHasher = SipHasher13;

macro_rules! parse_attrs {
    (rerun(always) $($rest:tt)*) => {
        fn mode(&self) -> $crate::incremental::query::QueryMode {
            $crate::incremental::query::QueryMode::Always
        }

        parse_attrs!($($rest)*);
    };
    (rerun(generation) $($rest:tt)*) => {
        fn mode(&self) -> $crate::incremental::query::QueryMode {
            $crate::incremental::query::QueryMode::Generation
        }

        parse_attrs!($($rest)*);
    };

    () => {};
}

macro_rules! tup_or_empty {
    () => {
        ()
    };
    ($($param: tt)*) => {
        ($($param),*,)
    };
}

macro_rules! define_query {
    (
        $(#[$($attr: tt)*])*
        fn $name: ident <$lt: lifetime>
        ($cxname: ident: &Context<'cx>, $($paramname: ident: &$param: ty),* $(,)?)
        -> $ret: ty
        $block: block
    ) => {
        #[derive(Copy, Clone)]
        #[allow(non_camel_case_types)]
        struct $name;

        impl<$lt> $crate::incremental::query::Query<$lt> for $name {
            type Input = tup_or_empty!($($param)*);
            type Output = $ret;

            const NAME: &'static str = stringify!($name);

            fn get_run_fn() -> $crate::incremental::query::ErasedQueryRun {
                fn run<'cx>(
                    cx: &$crate::incremental::context::Context<'cx>,
                    input: $crate::incremental::query_parameter::TypeErasedQueryParam<'cx>,
                    should_alloc: &dyn Fn(u128) -> bool,
                ) -> (Option<$crate::incremental::query_parameter::TypeErasedQueryParam<'cx>>, u128) {
                    // safety: we know thatcx is Q::Input here because this function is generated
                    // together with the definition of the query
                    let input: &tup_or_empty!($($param)*) = unsafe{input.get_ref()};
                    let output = <$name as $crate::incremental::query::Query<'cx>>::run(cx, input);

                    let output_hash = cx.hash($name, &output);
                    if should_alloc(output_hash) {
                        (Some($crate::incremental::query_parameter::TypeErasedQueryParam::new(cx.storage.alloc(output))), output_hash)
                    } else {
                        (None, output_hash)
                    }
                }

                &run
            }

            parse_attrs!($($($attr)*)*);

            fn run($cxname: &$crate::incremental::context::Context<'cx>, tup_or_empty!($($paramname)*): &Self::Input) -> Self::Output $block
        }
    };
}
