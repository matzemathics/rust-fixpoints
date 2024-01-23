use crate::lattice::{Join, LocalMinimum, PreOrder};

use super::Variable;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Query<PredicateSymbol, S> {
    pub subst: S,
    pub predicate: PredicateSymbol,
}

impl<AD: PreOrder, PredicateSymbol: Eq> PreOrder for Query<PredicateSymbol, AD> {
    fn leq(&self, other: &Self) -> bool {
        if self.predicate != other.predicate {
            return false;
        }

        self.subst.leq(&other.subst)
    }
}

impl<P, S> AsRef<S> for Query<P, S> {
    fn as_ref(&self) -> &S {
        &self.subst
    }
}

pub trait AbstractSubstitution<F, P>: Join + LocalMinimum<Query<P, Self>> {
    fn apply_eq(&mut self, lhs: Variable, rhs: Variable);
    fn apply_func(&mut self, lhs: Variable, func: &F, rhs: &[Variable]);

    fn extended(&self, num_vars: u16) -> Self;
    fn restrict(&mut self, num_vars: u16);

    fn project(&self, vars: impl Iterator<Item = Variable>) -> Self;
    fn propagate(&mut self, vars: impl Iterator<Item = Variable>, update: Self);

    fn len(&self) -> u16;
}
