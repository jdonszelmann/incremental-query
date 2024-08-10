#![feature(macro_metavar_expr)]

use incremental::{Context, QueryHasher, QueryParameter, Storage};
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

    let storage = Storage::new();
    let ctx = Context::new(&storage);
    ctx.query(compile, (config,));
}
