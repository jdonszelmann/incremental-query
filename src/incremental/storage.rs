use bumpalo::Bump;

use super::query_parameter::QueryParameter;

pub struct Storage {
    data: Bump,
}

impl Storage {
    pub fn new() -> Self {
        Self { data: Bump::new() }
    }

    pub fn alloc<T: QueryParameter>(&self, v: T) -> &mut T {
        self.data.alloc(v)
    }

    pub fn size(&self) -> usize {
        self.data.allocated_bytes()
    }
}

impl Default for Storage {
    fn default() -> Self {
        Self::new()
    }
}
