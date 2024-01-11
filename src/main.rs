use abstract_domain::AbstractDomain;
use lattice::JoinSemiLattice;

use crate::fixpoint::compute_fixpoint;
use crate::gaia::{Clause, Gaia, Query};

mod abstract_domain;
mod fixpoint;
mod gaia;
mod lattice;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct PredicateSymbol(String);

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct FakeClause;

impl Clause for FakeClause {
    type PredicateSymbol = PredicateSymbol;
    type Shape = ();
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Hash, Clone)]
struct TheNoDomain;

impl JoinSemiLattice for TheNoDomain {
    fn bot() -> Self {
        TheNoDomain
    }

    fn join(&self, _other: &Self) -> Self {
        TheNoDomain
    }
}

impl AbstractDomain for TheNoDomain {
    type Shape = ();

    fn unify_with(&mut self, _other: Self) {}

    fn unify_nested(&mut self, _functor: &(), _subterms: &[Option<&mut Self>]) {}

    fn any() -> Self {
        TheNoDomain
    }
}

fn main() {
    let gaia: Gaia<FakeClause> = gaia::Gaia::init(None);
    let query: Query<_, TheNoDomain> = Query::new(PredicateSymbol("foo".into()), 3);

    let (result, _) = compute_fixpoint(query, gaia);
}
