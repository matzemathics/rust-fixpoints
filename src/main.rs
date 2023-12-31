use abstract_domain::AbstractDomain;
use lattice::JoinSemiLattice;

use crate::fixpoint::compute_fixpoint;
use crate::gaia::{default_substitution, Clause, Gaia, Query};

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
    type FunctionSymbol = u32;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Hash, Clone)]
struct TheNoDomain;

impl JoinSemiLattice for TheNoDomain {
    fn bot() -> Self {
        todo!()
    }

    fn join(&self, other: &Self) -> Self {
        todo!()
    }
}

impl AbstractDomain for TheNoDomain {
    type FunctionSymbol = u32;

    fn unify_with(&mut self, other: Self) {
        todo!()
    }

    fn unify_nested(&mut self, functor: &Self::FunctionSymbol, subterms: &[Option<&Self>]) {
        todo!()
    }
}

fn main() {
    let gaia: Gaia<FakeClause> = gaia::Gaia::init(None);
    let query = Query {
        subst: default_substitution::<TheNoDomain>(3),
        predicate: PredicateSymbol("foo".into()),
    };

    let (result, _) = compute_fixpoint(query, gaia);
}
