use std::cmp::Ordering;

use crate::lattice::{Join, JoinSemiLattice};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Bitmap(u64);

impl Bitmap {
    pub fn zeroed() -> Self {
        Bitmap(0)
    }

    pub fn meet_with(&mut self, other: &Self) {
        self.0 &= other.0
    }
}

impl From<u64> for Bitmap {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl PartialOrd for Bitmap {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else if (self.0 & other.0) == self.0 {
            Some(Ordering::Less)
        } else if (self.0 & other.0) == other.0 {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}

impl Join for Bitmap {
    fn join(&self, other: &Self) -> Self {
        Bitmap(self.0 | other.0)
    }
}

impl JoinSemiLattice for Bitmap {
    fn bot() -> Self {
        Self::zeroed()
    }
}
