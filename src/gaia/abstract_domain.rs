use std::{fmt::Debug, hash::Hash, mem::MaybeUninit};

use crate::lattice::JoinSemiLattice;

pub trait AbstractDomain: JoinSemiLattice {
    type Term: Debug + Clone;

    fn unify_with(&mut self, other: Self);
    fn unify_term(&mut self, other: &Self::Term);

    fn unify_replace_both(&mut self, other: &mut Self) {
        let right = std::mem::replace(other, Self::bot());
        self.unify_with(right);
        *other = self.join(&Self::bot())
    }
}
