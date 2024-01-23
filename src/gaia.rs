use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use crate::fixpoint::MonotoneTransform;
use crate::lattice::{LubIterator, PreOrder};

use self::abstract_substitution::{AbstractSubstitution, Query};

mod abstract_substitution;
mod bit_substitution;

type Variable = u16;

struct NormalizedClause<C: Clause> {
    body_atoms: Vec<(C::PredicateSymbol, Vec<Variable>)>,
    variable_equalities: Vec<(Variable, Variable)>,
    ctor_equalities: Vec<(Variable, C::FunctionSymbol, Vec<Variable>)>,
    num_variables: u16,
}

impl<C: Clause> NormalizedClause<C> {
    fn execute<S>(&self, mut f: impl FnMut(Query<C::PredicateSymbol, S>) -> S, input: &S) -> S
    where
        C::PredicateSymbol: Clone,
        S: AbstractSubstitution<C::FunctionSymbol, C::PredicateSymbol>,
    {
        let mut subst = input.extended(self.num_variables);

        for (pred, vars) in &self.body_atoms {
            let local_subst = subst.project(vars.iter().cloned());
            let query = Query {
                predicate: pred.clone(),
                subst: local_subst,
            };
            subst.propagate(vars.iter().cloned(), f(query));
        }

        for &(lhs, rhs) in &self.variable_equalities {
            subst.apply_eq(lhs, rhs);
        }

        for (lhs, f, vars) in &self.ctor_equalities {
            subst.apply_func(*lhs, f, vars);
        }

        subst.restrict(input.len());
        subst
    }
}

pub struct Gaia<C: Clause> {
    program: HashMap<(C::PredicateSymbol, Variable), Vec<NormalizedClause<C>>>,
}

pub trait Clause: Sized {
    type PredicateSymbol: Hash + Eq + Debug + Clone;
    type FunctionSymbol;
}

fn into_normalized<C: Clause>(c: C) -> ((C::PredicateSymbol, Variable), NormalizedClause<C>) {
    todo!()
}

impl<C: Clause> Gaia<C> {
    pub fn init(src: impl IntoIterator<Item = C>) -> Self {
        let mut program: HashMap<_, Vec<NormalizedClause<C>>> = HashMap::new();
        for (pred, clause) in src.into_iter().map(into_normalized) {
            program.entry(pred).or_default().push(clause);
        }

        Gaia { program }
    }
}

impl<C, S> MonotoneTransform<Query<C::PredicateSymbol, S>> for Gaia<C>
where
    C: Clause,
    S: AbstractSubstitution<C::FunctionSymbol, C::PredicateSymbol>,
{
    type Output = S;

    fn call<F>(&self, mut f: F, query: Query<C::PredicateSymbol, S>) -> Self::Output
    where
        F: FnMut(Query<C::PredicateSymbol, S>) -> Self::Output,
    {
        let min = S::local_minimum(&query);
        self.program
            .get(&(query.predicate, query.subst.len()))
            .into_iter()
            .flatten()
            .map(|c| c.execute(&mut f, &query.subst))
            .lub(min)
    }
}
