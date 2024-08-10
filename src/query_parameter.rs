use std::{
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr::NonNull,
    rc::Rc,
};

use super::{storage::Storage, QueryHasher};

/// Query parameters should be hashable and clonable.
///
/// The hash infrastructure is part of this trait itself, because
/// it's not unlikely you want to hash the elements you pass into
/// queries in a different way than they are hashed by default.
pub trait QueryParameter: Sized + Clone + 'static {
    fn hash_stable(&self, hasher: &mut QueryHasher);

    fn get_clone<'cx>() -> TypeErasedCloneFn<'cx>
    where
        Self: 'cx,
    {
        fn clone<'a, T: QueryParameter>(
            this: TypeErasedQueryParam,
            storage: &'a Storage,
        ) -> TypeErasedQueryParam<'a> {
            // safety: T == Self, this clone function clones self
            let this_ref = unsafe { this.get_ref::<T>() };
            let new_this = this_ref.clone();
            let new_ref = storage.alloc(new_this);
            TypeErasedQueryParam::new(new_ref)
        }

        clone::<Self>
    }
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

type TypeErasedCloneFn<'cx> =
    for<'a> fn(this: TypeErasedQueryParam<'cx>, &'a Storage) -> TypeErasedQueryParam<'a>;

#[derive(Copy, Clone)]
pub struct TypeErasedQueryParam<'cx> {
    ptr: NonNull<()>,
    clone: TypeErasedCloneFn<'cx>,
    phantom: PhantomData<&'cx ()>,
}

impl<'cx> TypeErasedQueryParam<'cx> {
    pub fn new<Q: QueryParameter>(inp: &'cx Q) -> Self {
        TypeErasedQueryParam {
            ptr: std::ptr::NonNull::from(inp).cast(),
            clone: Q::get_clone(),
            phantom: PhantomData,
        }
    }

    pub fn get_ptr(&self) -> NonNull<()> {
        self.ptr
    }

    /// Convert this pointer to a query parameter,
    /// into a reference to a concrete query parameter
    ///
    /// # Safety
    /// unsound unless T is the original type of this query parameter
    pub unsafe fn get_ref<T>(&self) -> &'cx T {
        &*self.ptr.as_ptr().cast()
    }

    pub fn deep_clone<'a>(&self, storage: &'a Storage) -> TypeErasedQueryParam<'a> {
        (self.clone)(*self, storage)
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
            fn hash_stable(&self, hasher: &mut $crate::QueryHasher) {
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
                fn hash_stable(&self, hasher: &mut $crate::QueryHasher) {
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
