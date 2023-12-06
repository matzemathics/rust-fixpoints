use std::{fmt::Debug, hash::Hash};

use crate::lattice::JoinSemiLattice;

pub trait AbstractDomain: JoinSemiLattice {
    type FunctionSymbol: Eq + Hash + Debug + Clone;

    fn unify_with(&mut self, other: Self);
    fn unify_nested(&mut self, functor: &Self::FunctionSymbol, subterms: &[Option<&Self>]);

    fn unify_replace_both(&mut self, other: &mut Self) {
        let right = std::mem::replace(other, Self::bot());
        self.unify_with(right);
        *other = self.join(&Self::bot());
    }
}
