
[<img alt="github" src="https://img.shields.io/badge/github-jdonszelmann/incremental-query?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/jdonszelmann/incremental-query)
[<img alt="crates.io" src="https://img.shields.io/crates/v/incremental-query.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/incremental-query)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-incremental-query?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/incremental-query)
[<img alt="build status" src="https://img.shields.io/github/actions/workflow/status/jdonszelmann/incremental-query/ci.yml?branch=master&style=for-the-badge" height="20">](https://github.com/jdonszelmann/incremental-query/actions)

# Incremental Query

This library wasn't ever supposed to be a library.
It started with me wanting to learn how [rustc's query system](https://rustc-dev-guide.rust-lang.org/query.html) worked, 
and I decided to implement a system very similar to it myself.
Turns out, I'm quite happy with it and started documenting it and now it's this library.

Note: this crate uses a nightly feature: `#![feature(macro_metavar_expr)]`, so it only works on nightly for now.

# What is this?

Core to this library is the concept of a query. Just like in rustc.
A query is just like a normal function, and you can call it as if it is.
There are just some restrictions to what the parameters and return type of the function can be.
However, when you call them, a lot more happens than with a normal function.

The first time, the code in a query runs as normal, but the inputs and outputs are cached and hashed.
Furthermore, any other query you call inside this query (and transitively so) is kept track of.
On subsequent invocations of the same query with the same inputs, the cached result is returned.

This explanation might make it seem like it's just performing memoization. 
However, memoized functions must be [pure](https://en.wikipedia.org/wiki/Pure_function)! 
Otherwise, we cannot decide whether the output is the same by just looking at the inputs.

Instead, using queries, functions don't have to be pure.
Pure functions are still very nice though! We don't have to rerun them as often.
However, if an impure query changes its result, this is noticed by the system, and
any (pure) query depending on it will be automatically rerun when its result is needed.
Also, when an impure query is rerun, and a dependent chain of queries updates, but
at any point we notice that a result didn't change, the chain of reruns is stopped to 
save as much computation time as possible.

You might see how this is useful for incremental compilation. One query reads a file, another parses an ast,
and yet another does a part of typechecking. Rustc has hundreds of queries. If a file hasn't changed, the read file
query notices. Then we don't have to run the parse query again, awesome! Typechecking might depend on multiple files,
but if none of the files it does depend on change, we don't have to rerun it either.

At this moment, this library doesn't yet support serializing the query cache to a file. 
Some day it might. If it did, the cache would be useful for even longer.

The following section is about this library specifically. Even if you're just interested in [how it works](#how-does-it-work),
it might stil be useful to read this so you understand the notation used in the examples.

# How can I use it?

Queries are defined *almost* like normal functions.
The only restriction is that they must be tagged with a `#[query]` attribute.

```rust
use incremental_query::{Context, query, rerun};
# use rand::{thread_rng, Rng};

/// This is a query that retuns a random number.
#[query]
#[rerun(always)]
fn random<'cx>(_cx: &Context<'cx>) -> u64 {
    thread_rng().gen_range(0..u64::MAX)
}
```

> Note: There are some restrictions on what the signature of a query can look like:
> The function *must* have one lifetime argument.
> Furthermore, the first parameter *must* be a reference to a [`Context`]
> Any input *must* be a reference to some `T` which implements [`QueryParameter`].
>
> Also, even though inputs to queries are references, when you invoke a query you must
> provide corresponding *owned values*, not references. The owned values are stored in the cache,
> while the function gets references to those values *that are now in the cache*.

To invoke a query, you can now simply call it:

```
use incremental_query::{Context, Storage};
# use incremental_query::{query, rerun};
# use rand::{thread_rng, Rng};
# 
# 
# /// This is a query that retuns a random number.
# #[query]
# #[rerun(always)]
# fn random<'cx>(_cx: &Context<'cx>) -> u64 {
#     thread_rng().gen_range(0..u64::MAX)
# }
# 
#
// instantiate a storage object that holds the cached data
let storage = Storage::new();
// create a context with it
let cx = Context::new(&storage);

// call the query
println!("{}", random(&cx));
```

When you call a query, you'll get back a reference to the declared return type.
That's again because the actual return value is now stored in the cache. 
You just get a reference into that cache.

## Function purity and generations

You may have noticed the `#[rerun(always)]` attribute on the query defined above.

Without that attribute, the query is expected to be pure. 
That means that, without an attribute, 
a query should not read from a file, or generate random numbers, or call another function that might do that.
However, it may stil call other queries which *are* marked as impure.

A query can be marked as impure in two different ways. Either through `#[rerun(always)]`, 
which will never assume its result stayed the same (in fact, the library won't even cache the output).
These can be quite bad for performance, so there's an alternative: `#[rerun(generation)]`.

The context has a generation counter. It starts at `0`, and you can change it with [`Context::next_generation`](Context::next_generation).
Each cached query also stores in which generation is was cached. Generational queries will be cached as normal,
but will be rerun when the generation number changes.

Thus, you can make all impure queries generational, and when you, through some means know that what they 
depend on may have changed (like files on disk have updated), you can increment the generation number and
the results will be reflected.

For example, here:
```rust
# use incremental_query::{Context, Storage, query, rerun};
#[query]
#[rerun(generation)]
fn boolean_query<'cx>(_cx: &Context<'cx>) -> bool {
    // first return true, then in the 2nd generation false
#   true
}

#[query]
fn one<'cx>(_cx: &Context<'cx>) -> u64 {
    1
}
#[query]
fn two<'cx>(_cx: &Context<'cx>) -> u64 {
    2
}

#[query]
fn conditional<'cx>(cx: &Context<'cx>, ) -> u64 {
    if *boolean_query(cx) {
        *one(cx)
    } else {
        *two(cx)
    }
}

let storage = Storage::new();
let cx = Context::new(&storage);

conditional(&cx);
conditional(&cx);
conditional(&cx);
cx.next_generation();
conditional(&cx);
conditional(&cx);
conditional(&cx);
```

`boolean_query` is marked as generational. Let's say that it returned `true` in the first generation
but `false` in the second. We call `conditional` three times. The first three will all see the result 
to be `true` and will output `1` since the generation is still at `0`, so there's no reason to assume the output may have changed.
After the generation is changed, `conditional` will return `2` on each of the three following invocations.
in total, `boolean_query` is run 2 times here, `one` and `two` are each run once, and `conditional` is run twice. 
All other results are cached.

# How does it work?

You might notice that a lot of the following explanation lines up with the explanation of salsa,
and rust's query system explained in the [rustc dev guide](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html).
That's not a coincidence, as this library started out as me wanting to implement that algorithm.
I'll try my best to explain the algorithm here again, and maybe simplify some of it a little.

> If you've read the rustc dev guide, you may have wondered the following: it uses the word query for two
> different concepts interchangably. It took me a while to realize that.
> First of all, a query is a definition of a query, a function tagged with `#[query]`
> However, in the guide it also refers to the instance of a query. When it says that a query cannot depend on itself,
> it doesn't mean that some `type_of(a)` query cannot execute `type_of(b)`. It just means that you can't make a cycle where
> `type_of(a)` depends recursively on `type_of(a)` again. Similarly, when the guide says a query is cached,
> it means that a query and a given input to it is cached.

When a query is invoked, it actually immediately delegates to the [`Context`]. 
The [`Context`] *might* then later decide to actually run the function. 
The [`Context`] consists of a cache, but also of dependency graph, which is a
[directed acyclic graph or DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph), 
and a datastructure to detect cycles while running queries.

Before a query can be run, it first performs the following checks:

1. Hash the input (together with the query type, to make hashes for different queries unique).
    * If this hash was never seen before, add a new node to the dependency graph and immediately run the query.
      cache its output, and return.
2. If the hash already exist, find the corresponding node in the dependency graph
    * IF the node doesn't have a cached result, run the query and cache the result now. 
3. If it has no cached result, we're still not done because we need to make sure if the cache hasn't been invalidated.
   There are three kinds of queries: always rerunning, generational and pure.
    * The cache result of a pure query can, even though its pure, still be invalidated. 
      That can happen when a dependency of the pure query isn't pure. 
      We solve this using what rustc calls the `try_mark_green` algorithm. More on that later
    * The cache result of generational queries can be invalidated just like a pure query, 
      when a dependency changes. However, its result also gets outdated when the global generation is changed.
    * The cache result of an always rerunning query is never valid. 
      In fact we never even store its output.

## The try mark green algorithm, or recursively evaluating cache validity

For every query instance (that is, a query type, its input, hashed input, possibly cached output, and output hash)
e also store one extra piece of information: is it red or is it green.

This field can be changed by all kinds of things:
1. When a query is actually executed, and its output isn't the same as on the previous execution,
   (or there is no previous output) the query turns red.
2. When a query is actually executed, and its output is the same as on the previous execution,
   the query turns green.
3. When a query is evaluated for cache invalidation, and all its dependencies are green,
   its marked green.

A cache invalidation subsequently goes as follows:

1. If a query is `rerun(always)` its cache is instantly invalid.
2. If a query is `rerun(generational)` its cache is instantly invalid when the generation changed.
3. If a query is already marked green at the start of the validity check, it does not (transitively) 
   depend on any `rerun(always)` query and the generation number hasn't changed, its cache is instantly valid.
4. For all the dependencies of the query, run this check recursively. 
   This might mean running one of the dependent queries, which could change their red/green status through one of the 3 ways outlined above.
   * If after recursively running this algorithm for all dependencies, any of the dependencies is red we have to rerun
     this query, which might change its red/green status.
   * If after recursively running all dependent queries, all of the dependencies are marked green,
     we do not have to rerun it, its cache is valid. Actually, we also immediately mark this query as green itself.

Note that here, for some query `Q`, if we have to rerun a dependency because it *could* have changed, 
but it actually hasn't, then that dependency will be marked green and we won't have to rerun `Q` itself.

## Keeping track of dependencies

Each query instance has a list of dependencies. Whenever a query `Q` is executed,
the context actually updates a field called `curr` that says what the current "parent"
query is. when `Q` runs a query of its own, it will pass through the context again,
and this dependency list is updated.

Note that when a query is rerun, because some dependency changed, we have to wipe its 
dependency list. That's because the dependency list might change! 
To adapt an example of the [rustc dev guide](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html#the-query-dag):

```rust
# use incremental_query::{Context, Storage, query, rerun};
#[query]
#[rerun(generation)]
fn boolean_query<'cx>(_cx: &Context<'cx>) -> bool {
    // first return true, then in the 2nd generation false
#   true
}

#[query]
fn one<'cx>(_cx: &Context<'cx>) -> u64 {
    1
}
#[query]
fn two<'cx>(_cx: &Context<'cx>) -> u64 {
    2
}

#[query]
fn conditional<'cx>(cx: &Context<'cx>, ) -> u64 {
    if *boolean_query(cx) {
        *one(cx)
    } else {
        *two(cx)
    }
}

let storage = Storage::new();
let cx = Context::new(&storage);

conditional(&cx);
cx.next_generation();
conditional(&cx);
```

Here, on the first execution of `conditional`, the dependency list will include `one`.
However, after the first generation, the dependency list no longer includes `one`, and will
instead contain `two`.

## License

Licensed under either of <a href="LICENSE-APACHE">Apache License, Version 2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
