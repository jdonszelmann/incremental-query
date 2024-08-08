use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr::NonNull,
    rc::Rc,
};

use super::QueryHasher;

/// Query parameters should be hashable etc
pub trait QueryParameter {
    fn hash_stable(&self, hasher: &mut QueryHasher);
}

impl<T> QueryParameter for Option<T>
where
    T: QueryParameter,
{
    fn hash_stable(&self, hasher: &mut QueryHasher) {
        self.is_some().hash(hasher);
        if let Some(i) = self {
            T::hash_stable(i, hasher);
        }
    }
}
impl<T> QueryParameter for Box<T>
where
    T: QueryParameter,
{
    fn hash_stable(&self, hasher: &mut QueryHasher) {
        T::hash_stable(self, hasher)
    }
}
impl<T> QueryParameter for Rc<T>
where
    T: QueryParameter,
{
    fn hash_stable(&self, hasher: &mut QueryHasher) {
        T::hash_stable(self, hasher)
    }
}
impl QueryParameter for () {
    fn hash_stable(&self, _hasher: &mut QueryHasher) {}
}

#[derive(Copy, Clone)]
pub struct TypeErasedQueryParam<'cx> {
    ptr: NonNull<()>,
    phantom: PhantomData<&'cx dyn QueryParameter>,
}

impl<'cx> TypeErasedQueryParam<'cx> {
    pub fn new(inp: &'cx impl QueryParameter) -> Self {
        TypeErasedQueryParam {
            ptr: std::ptr::NonNull::from(inp).cast(),
            phantom: PhantomData,
        }
    }

    // Convert this pointer to a query parameter,
    // into a reference to a concrete query parameter
    //
    // Safety: unsound unless T is the original type of this query parameter
    pub unsafe fn get_ref<T>(&self) -> &'cx T {
        &*self.ptr.as_ptr().cast()
    }
}

macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + count!($($xs)*));
}

macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {
        $sub
    };
}

macro_rules! map {
    (let $name: ident; $e: expr; $tup: ident; $($items: tt)*) => {
        $(
            let $name = replace_expr!($items &$tup.${index()});
            $e;
        )*
    };
}

macro_rules! impl_query_parameter {
    () => {};
    ($first: ident $($rest: ident)*) => {
        impl<$first: QueryParameter, $($rest: QueryParameter),*> QueryParameter for ($first, $($rest),*) {
            fn hash_stable(&self, hasher: &mut $crate::incremental::QueryHasher) {
                hasher.write_usize(count!($first $($rest)*));

                map!(let x; x.hash_stable(hasher); self; $first $($rest)*);
            }
        }

        impl_query_parameter!($($rest)*);
    };
}

impl_query_parameter!(
    T1 T2 T3 T4 T5 T6
);

macro_rules! impl_integers {
    ($($ty: ty => $name: ident),* $(,)?) => {
        $(
            impl QueryParameter for $ty {
                fn hash_stable(&self, hasher: &mut $crate::incremental::QueryHasher) {
                    hasher.$name(*self);
                }
            }
        )*
    };
}

impl_integers!(
    usize => write_usize,
    isize => write_isize,

    u64 => write_u64,
    u32 => write_u32,
    u16 => write_u16,
    u8 => write_u8,

    i64 => write_i64,
    i32 => write_i32,
    i16 => write_i16,
    i8 => write_i8,
);

impl QueryParameter for bool {
    fn hash_stable(&self, hasher: &mut QueryHasher) {
        hasher.write_u8(*self as u8);
    }
}
