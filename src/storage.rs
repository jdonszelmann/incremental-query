use bumpalo::Bump;

use super::query_parameter::QueryParameter;

/// Storage is what stores the inputs and outputs to queries.
///
/// Inside, it has a bump allocator which will grow like a memory leak...
/// That's why there's [`Context::gc`](crate::Context::gc).
///
/// Note: the main thing that grows storage is new generations as that
/// invalidates a lot of the cache. [serializing](crate::Context::serialize)
/// and [deserializing](crate::Context::deserialize) do not store elements and
/// that would be garbage collected otherwise (so a serialize --> deserialize
/// cycle is effectively equivalent to a garbage collect). Thus, you might not
/// need to care about this storage growing too much if you serialize the
/// [`Context`](crate::Context) regularly.
pub struct Storage {
    data: Bump,
}

impl Storage {
    pub fn new() -> Self {
        Self { data: Bump::new() }
    }

    // hidden because of use in macro
    #[doc(hidden)]
    pub fn alloc<T: QueryParameter>(&self, v: T) -> &mut T {
        self.data.alloc(v)
    }

    /// Number of allocated bytes in the storage
    ///
    /// This is much more precise than [`Context::size`](crate::Context::size)
    pub fn size(&self) -> usize {
        self.data.allocated_bytes()
    }
}

impl Default for Storage {
    fn default() -> Self {
        Self::new()
    }
}
