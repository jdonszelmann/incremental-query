use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{
    abort, abort_call_site, emit_call_site_warning, emit_error, emit_warning, proc_macro_error,
};
use quote::{quote, ToTokens};
use syn::parse_quote;
use syn::parse_quote_spanned;
use syn::{
    parse::Parser, parse_macro_input, spanned::Spanned, Attribute, Block, Expr, FnArg,
    Ident, ItemFn, Lifetime, LifetimeParam, MetaNameValue, Pat, PatType, Path, PathArguments,
    ReturnType, Signature, Token, Type, Visibility,
};

struct QueryInputs {
    inputs_dereffed: Vec<PatType>,
    inputs_without_context: Vec<PatType>,
    input_dereffed_types_without_context: Vec<Type>,
    context: Ident,
    context_typaram: Option<Type>,
}

struct QuerySignature {
    constness: Option<Token![const]>,
    asyncness: Option<Token![async]>,
    fn_token: Token![fn],
    ident: Ident,
    query_lifetime: Lifetime,
    inputs: QueryInputs,
    output: Type,
    output_ref: Type,
}

struct Query {
    mode: QueryMode,
    attrs: Vec<Attribute>,
    vis: Visibility,
    sig: QuerySignature,
    block: Box<Block>,
}

fn has_lt_attr(lifetime: &&LifetimeParam) -> bool {
    lifetime
        .attrs
        .iter()
        .filter_map(get_path)
        .any(|i| i.is_ident("lt"))
}

fn pat_as_ident(pat: &Pat) -> Ident {
    match try_pat_as_ident(pat) {
        Ok(i) => i,
        Err(e) => abort!(e, "expected identifier"),
    }
}

fn try_pat_as_ident(pat: &Pat) -> Result<Ident, &Pat> {
    match pat {
        Pat::Ident(i) => Ok(i.ident.clone()),
        x => Err(x),
    }
}

fn is_context(inner: &Type, lifetime: &Lifetime) -> bool {
    match inner {
        Type::Paren(i) => is_context(&i.elem, lifetime),
        Type::Path(p) => {
            if let Some(last) = p.path.segments.last() {
                if last.ident == "Context" {
                    p.qself.is_none()
                } else {
                    false
                }
            } else {
                false
            }
        }
        Type::Ptr(i) => {
            if is_context(&i.elem, lifetime) {
                emit_warning!(
                    i,
                    "did you mean this to be a reference to a context (`&Context<{}>`)",
                    quote! {#lifetime}
                );
            }
            false
        }
        Type::Reference(i) => {
            if is_context(&i.elem, lifetime) {
                emit_warning!(
                    i,
                    "did you mean this to be a reference to a context (`&Context<{}>`)",
                    quote! {#lifetime}
                );
            }
            false
        }
        _ => false,
    }
}

fn is_context_ref(ty: &Type, lifetime: &Lifetime) -> bool {
    match ty {
        Type::Paren(i) => is_context_ref(&i.elem, lifetime),
        Type::Path(i) => {
            if i.path
                .segments
                .last()
                .map(|i| i.ident == "Context")
                .unwrap_or(false)
            {
                emit_warning!(
                    i.path.segments.last().unwrap(),
                    "did you mean &{}<{}>",
                    quote! {#i},
                    quote! {#lifetime}
                );
                false
            } else {
                false
            }
        }
        Type::Ptr(p) => {
            if is_context(&p.elem, lifetime) {
                emit_warning!(
                    p,
                    "did you mean this to be a reference to a context (`&Context<{}>`)",
                    quote! {#lifetime}
                );
            }

            false
        }
        Type::Reference(r) => {
            if is_context(&r.elem, lifetime) {
                r.mutability.is_none()
            } else {
                false
            }
        }
        _ => false,
    }
}

fn get_ty_path(ty: &Type) -> Path {
    match ty {
        Type::Reference(x) => get_ty_path(&x.elem),
        Type::Paren(x) => get_ty_path(&x.elem),
        Type::Path(p) => p.clone().path,
        ty => abort!(ty, "not a path"),
    }
}

fn deref_type(t: &Type) -> Type {
    match t {
        Type::Reference(r) => {
            r.elem.as_ref().clone()
        }
        x => {
            abort!(
                x,
                "expected this input to be a reference `&{}`",
                quote! {#x}
            );
        }
    }
}

fn validate_inputs(inputs: impl IntoIterator<Item = FnArg>, lifetime: &Lifetime) -> QueryInputs {
    let mut new_inputs = Vec::new();

    let mut context = None;
    let mut first_nonself_arg = None;
    let mut idx = 0;

    for i in inputs {
        if let FnArg::Typed(PatType { pat, ty, .. }) = &i {
            if first_nonself_arg.is_none() {
                first_nonself_arg = Some(try_pat_as_ident(pat).map_err(|e| e.clone()));
            }
            if is_context_ref(ty, lifetime) {
                context = Some((pat_as_ident(pat), idx))
            }
            idx += 1;
        } else {
            abort!(i, "queries may not have a receiver type");
        }

        if let FnArg::Typed(x) = i {
            new_inputs.push(x);
        }
    }

    if new_inputs.is_empty() {
        abort_call_site!("queries must have at least one parameter which is `cx: &Context<{}>`")
    }
    if !new_inputs.is_empty() && first_nonself_arg.is_none() {
        abort_call_site!("queries must have at least one parameter which is `cx: &Context<{}>`")
    }

    let (context, idx) = match context {
        Some((context, idx)) => (context, idx),
        _ => {
            emit_call_site_warning!(
                "queries must start with one parameter `cx: &Context<{}>`",
                quote! {#lifetime}
            );
            match first_nonself_arg.unwrap() {
                Ok(i) => (i, 0),
                Err(e) => {
                    abort!(e, "expected identifier for the first parameter of a query (which must have type `Context<{}>`)", quote! {#lifetime});
                }
            }
        }
    };

    if idx != 0 {
        emit_warning!(context, "expected context to be the first argument")
    }

    let inputs_without_context: Vec<_> = new_inputs
        .iter()
        .filter(|p| {
            if let Ok(i) = try_pat_as_ident(&p.pat) {
                i != context
            } else {
                true
            }
        })
        .cloned()
        .collect();

    let input_types_without_context: Vec<_> = inputs_without_context
        .iter()
        .map(|i| i.ty.as_ref().clone())
        .collect();
    let mut input_dereffed_types_without_context = Vec::new();

    // validate that they're all references
    for i in &input_types_without_context {
        input_dereffed_types_without_context.push(deref_type(i))
    }

    let context_ty = get_ty_path(
        &new_inputs
            .iter()
            .find(|i| try_pat_as_ident(&i.pat).as_ref() == Ok(&context))
            .expect("context")
            .ty,
    );
    let PathArguments::AngleBracketed(arguments) = context_ty
        .segments
        .last()
        .expect("path segment")
        .arguments
        .clone()
    else {
        abort!(
            context_ty.segments.last().expect("path segment").arguments,
            "unexpected path segment"
        );
    };
    let generics = arguments.args;

    let mut had_lifetime = false;
    let mut had_generic = None;

    for i in generics {
        match i {
            syn::GenericArgument::Lifetime(l) => {
                if &l != lifetime {
                    abort!(
                        l,
                        "expected `Context<{}>` but found `Context<{}>`",
                        quote! {#lifetime},
                        quote! {#l}
                    );
                }

                if had_lifetime {
                    abort!(
                        l,
                        "expected `Context<{}>` to have only one lifetime argument",
                        quote! {#lifetime}
                    );
                }

                had_lifetime = true;
            }
            syn::GenericArgument::Type(ref t) => {
                if let Some(old) = had_generic.replace(t.clone()) {
                    abort!(t, "expected at most one type argument `Contex<{}>` but found `Context<{}, ..., {}>`", quote!{#old}, quote!{#old}, quote!{#t});
                }
            }
            syn::GenericArgument::Const(c) => abort!(c, "unexpected const argument on `Context`"),
            syn::GenericArgument::AssocType(a) => {
                abort!(a, "unexpected associated type on `Context`")
            }
            syn::GenericArgument::AssocConst(c) => {
                abort!(c, "unexpected associated const on `Context`")
            }
            syn::GenericArgument::Constraint(c) => abort!(c, "unexpected constraint on `Context`"),
            g => abort!(g, "unexpected generic argument on `Context`"),
        }
    }

    let inputs_dereffed = new_inputs
        .iter()
        .map(|i@PatType { attrs, pat, colon_token, ty }| {
            if try_pat_as_ident(&i.pat).as_ref() == Ok(&context) {
                return i.clone();
            }

            let dereffed_ty = deref_type(ty);
            parse_quote!(
                #(#attrs)* #pat #colon_token #dereffed_ty
            )
        })
        .collect();

    QueryInputs {
        inputs_dereffed,
        inputs_without_context,
        input_dereffed_types_without_context,
        context,
        context_typaram: had_generic.clone(),
    }
}

fn validate_sig(
    Signature {
        constness,
        asyncness,
        unsafety,
        abi,
        fn_token,
        ident,
        generics,
        paren_token: _,
        inputs,
        variadic,
        output,
    }: Signature,
) -> QuerySignature {
    let marked_lifetime = generics.lifetimes().find(has_lt_attr);
    let cx_lifetime = generics.lifetimes().find(|i| i.lifetime.ident == "cx");
    let Some(query_lifetime) = marked_lifetime
        .or(cx_lifetime)
        .map(|i| &i.lifetime)
        .cloned()
    else {
        abort!(
            generics,
            "expected `'cx` lifetime or lifetime marked with #[lt] in the generics list"
        )
    };

    if let Some(i) = unsafety {
        abort!(i, "queries can't be unsafe");
    }
    if let Some(i) = abi {
        abort!(i, "queries can't have an explicit abi");
    }
    if let Some(i) = variadic {
        abort!(i, "queries can't be variadic");
    }

    QuerySignature {
        constness,
        asyncness,
        fn_token,
        ident,
        inputs: validate_inputs(inputs, &query_lifetime),
        output: match &output {
            ReturnType::Default => parse_quote!(()),
            ReturnType::Type(_, ty) => *ty.clone(),
        },
        output_ref: match output {
            ReturnType::Default => parse_quote_spanned! {output.span() => & #query_lifetime ()},
            ReturnType::Type(_, ty) => parse_quote!{& #query_lifetime #ty},
        },
        query_lifetime,
    }
}

enum QueryAttr {
    Mode(QueryMode),
}

fn get_string(e: &Expr) -> String {
    match e {
        Expr::Lit(l) => match &l.lit {
            syn::Lit::Str(s) => s.value(),
            l => abort!(l, "expected string literal"),
        },
        e => abort!(e, "expected string literal"),
    }
}

fn parse_rerun(s: &str, span: Span) -> QueryMode {
    match s {
        "always" => QueryMode::Always,
        "generation" => QueryMode::Generation,
        _ => abort!(
            span,
            "unknown query mode, expected `always` or `generation`"
        ),
    }
}

fn parse_attr(attr: &Attribute) -> Option<QueryAttr> {
    match &attr.meta {
        syn::Meta::Path(_) => None,
        syn::Meta::List(ml) => if ml.path.is_ident("rerun") {
            match ml.parse_args::<Ident>() {
                Err(e) => abort!(ml, "{}", e),
                Ok(i) => Some(QueryAttr::Mode(parse_rerun(&i.to_string(), i.span()))),
            }
        } else {
            None
        },
        syn::Meta::NameValue(MetaNameValue { path, value, .. }) => {
            if path.is_ident("rerun") {
                Some(QueryAttr::Mode(parse_rerun(
                    &get_string(value),
                    value.span(),
                )))
            } else {
                None
            }
        }
    }
}

fn validate(
    ItemFn {
        attrs,
        vis,
        sig,
        block,
    }: ItemFn,
) -> Query {
    let mut mode = QueryMode::Cache;

    for attr in attrs.iter().filter_map(parse_attr) {
        match attr {
            QueryAttr::Mode(m) => mode = m,
        }
    }

    Query {
        mode,
        attrs,
        vis,
        sig: validate_sig(sig),
        block,
    }
}

fn get_path(attr: &Attribute) -> Option<&Path> {
    match &attr.meta {
        // ok :)
        syn::Meta::Path(p) => Some(p),
        // uhhh
        syn::Meta::List(_) => None,
        syn::Meta::NameValue(_) => None,
    }
}

fn assert_simple_attr(attr: TokenStream, expected: &str) -> Result<(), syn::Error> {
    let attrs = Parser::parse(Attribute::parse_outer, attr)?;
    for i in attrs {
        match i.meta {
            // ok :)
            syn::Meta::Path(_) => {}
            // uhhh
            syn::Meta::List(ml) if ml.path.is_ident(expected) => {
                emit_error!(ml, "expected an attribute without parameters")
            }
            syn::Meta::NameValue(mnv) if mnv.path.is_ident(expected) => {
                emit_error!(mnv, "expected an attribute without this value")
            }
            // we don't care
            _ => {}
        }
    }
    Ok(())
}

fn tuple_from_types<T: ToTokens>(types: &[T]) -> TokenStream2 {
    match types {
        [] => quote! {()},
        [x] => quote! {(#x,)},
        x => quote! {(#(#x),*)},
    }
}

fn or_unit(ty: &Option<Type>) -> TokenStream2 {
    match ty {
        Some(ty) => quote! {#ty},
        None => quote! {()},
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum QueryMode {
    Always,
    Generation,
    Cache,
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn query(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    if let Err(e) = assert_simple_attr(attr, "query") {
        return e.to_compile_error().into();
    }

    let Query {
        mode,
        attrs,
        vis,
        sig:
            QuerySignature {
                constness,
                asyncness,
                fn_token,
                ident,
                inputs:
                    QueryInputs {
                        inputs_dereffed,
                        context,
                        inputs_without_context,
                        input_dereffed_types_without_context,
                        context_typaram,
                    },
                output,
                output_ref,
                query_lifetime,
            },
        block,
    } = validate(input);

    let query = quote! {incremental_query::Query};
    let erased_query_run = quote! {incremental_query::ErasedQueryRun};
    let input_type = tuple_from_types(&input_dereffed_types_without_context);
    let input_type_dereffed = tuple_from_types(&input_dereffed_types_without_context);

    let string_ident = ident.to_string();
    let data_ty = or_unit(&context_typaram);
    let context_ty = quote! {incremental_query::Context};
    let type_erased_query_param = quote! {incremental_query::TypeErasedQueryParam};
    let mode_ident = quote! {incremental_query::QueryMode};

    let param_names = tuple_from_types(
        &inputs_without_context
            .iter()
            .map(|i| &i.pat)
            .collect::<Vec<_>>(),
    );

    let mode_fn = match mode {
        QueryMode::Always => quote! {
            fn mode(&self) -> #mode_ident {
                #mode_ident::Always
            }
        },
        QueryMode::Generation => quote! {
            fn mode(&self) -> #mode_ident {
                #mode_ident::Generation
            }
        },
        QueryMode::Cache => quote! {
            fn mode(&self) -> #mode_ident {
                #mode_ident::Cache
            }
        },
    };

    quote! {
        #(#attrs)*
        #vis #constness #asyncness #fn_token #ident <#query_lifetime> (#(#inputs_dereffed),*) -> #output_ref {
            #[derive(Copy, Clone)]
            struct Q;

            impl<#query_lifetime> #query<#query_lifetime, #data_ty> for Q {
                type Input = #input_type;
                type Output = #output;

                const NAME: &'static str = #string_ident;

                fn get_run_fn() -> #erased_query_run<#data_ty> {
                    fn run<'cx>(
                        cx: &#context_ty<'cx, #data_ty>,
                        input: #type_erased_query_param<'cx>,
                        should_alloc: &dyn Fn(u128) -> bool,
                    ) -> (Option<#type_erased_query_param<'cx>>, u128) 
                    {
                        let input: &#input_type_dereffed = unsafe{input.get_ref()};
                        let output = <Q as #query<'cx, #data_ty>>::run(cx, input);

                        let output_hash = cx.hash(Q, &output);
                        if should_alloc(output_hash) {
                            (Some(#type_erased_query_param::new(cx.storage.alloc(output))), output_hash)
                        } else {
                            (None, output_hash)
                        }
                    }

                    run
                }

                #mode_fn

                fn run(#context: &#context_ty<#query_lifetime, #data_ty>, #param_names: &Self::Input) -> Self::Output #block
            }

            #context.query(Q, #param_names)
        }

    }
    .into()
}

/// Marker for the query mode. Either `#[rerun(always)]` or `#[rerun(generation)]`.
///
/// Alternatives: `#[rerun = "always"]` and `#[rerun = "generation"]`.
///
/// Defaults to a cached query mode, which you mark by not giving a mode.
#[proc_macro_attribute]
#[proc_macro_error]
pub fn rerun(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
