use std::cmp::Ordering;

use crate::lattice::JoinSemiLattice;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Bitmap(u64);

impl Bitmap {
    pub fn zeroed() -> Self {
        Bitmap(0)
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

impl JoinSemiLattice for Bitmap {
    fn bot() -> Self {
        Self::zeroed()
    }

    fn join(&self, other: &Self) -> Self {
        Bitmap(self.0 | other.0)
    }
}
