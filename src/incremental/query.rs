use std::{any::TypeId, fmt::Display};

use super::{
    context::{Context, Generation},
    query_parameter::{QueryParameter, TypeErasedQueryParam},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct QueryId(TypeId);
impl QueryId {
    #[doc(hidden)]
    pub const fn new(id: TypeId) -> Self {
        Self(id)
    }
}

pub trait Query<'cx>: 'static + Copy + Clone {
    type Input: QueryParameter + 'cx;
    type Output: QueryParameter + 'cx;
    const NAME: &'static str;

    fn get_run_fn() -> ErasedQueryRun;

    fn mode(&self) -> QueryMode {
        QueryMode::Cache
    }

    fn run(cx: &Context<'cx>, i: &Self::Input) -> Self::Output;
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum QueryColor {
    Red,
    Green,
    Unknown,
}

pub type ErasedQueryRun = &'static dyn (for<'cx> Fn(
    &Context<'cx>,
    TypeErasedQueryParam<'cx>,
    &dyn Fn(u128) -> bool,
) -> (Option<TypeErasedQueryParam<'cx>>, u128));

pub struct QueryInstance<'cx> {
    pub run: ErasedQueryRun,

    pub name: &'static str,

    pub mode: QueryMode,
    pub generation: Generation,

    // input so we can rerun it
    pub input: TypeErasedQueryParam<'cx>,

    // maybe we know what its output will be
    pub output: Option<TypeErasedQueryParam<'cx>>,

    pub output_hash: u128,

    pub color: QueryColor,
}

impl Display for QueryInstance<'_> {
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
