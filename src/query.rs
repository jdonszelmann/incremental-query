use std::fmt::Display;

use super::{
    context::Context,
    generation::Generation,
    query_parameter::{QueryParameter, TypeErasedQueryParam},
};

pub trait Query<'cx, T>: 'static + Copy + Clone {
    type Input: QueryParameter + 'cx;
    type Output: QueryParameter + 'cx;
    const NAME: &'static str;

    fn get_run_fn() -> ErasedQueryRun<T>;

    fn mode(&self) -> QueryMode {
        QueryMode::Cache
    }

    fn run(cx: &Context<'cx, T>, i: &Self::Input) -> Self::Output;
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum QueryColor {
    Red,
    Green,
    Unknown,
}

pub type ErasedQueryRun<T = ()> = for<'cx> fn(
    &Context<'cx, T>,
    TypeErasedQueryParam<'cx>,
    &dyn Fn(u128) -> bool,
) -> (Option<TypeErasedQueryParam<'cx>>, u128);

pub struct QueryInstance<'cx, T> {
    pub run: ErasedQueryRun<T>,

    pub name: &'static str,

    pub mode: QueryMode,
    pub generation: Generation,

    // input so we can rerun it
    pub input: TypeErasedQueryParam<'cx>,

    // maybe we know what its output will be
    pub output: Option<TypeErasedQueryParam<'cx>>,

    pub output_hash: u128,

    pub color: QueryColor,
    pub transitively_has_always_dep: bool,
}

impl<T> Display for QueryInstance<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{:.4}", self.name, self.output_hash.to_string())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum QueryMode {
    /// Always rerun this query
    Always,
    /// always run this query once this generation
    /// if the generation number changes, gotta rerun!
    Generation,
    /// cache whenever possible
    Cache,
}
