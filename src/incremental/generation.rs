#[derive(Clone, Copy)]
pub struct Generation(pub(super) u64);

impl Generation {
    // special null generation; never executed
    pub const NULL: Generation = Generation(0);

    pub fn new() -> Self {
        // generations start at 1
        Self(1)
    }

    pub fn is_newer_than(&self, older: Self) -> bool {
        older.0 < self.0
    }

    pub fn next(&mut self) {
        self.0 += 1;
    }
}
