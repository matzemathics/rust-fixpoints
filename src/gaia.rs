use std::{collections::HashMap, fmt::Debug};

use crate::abstract_domain::AbstractDomain;
use crate::fixpoint::MonotoneTransform;
use crate::lattice::{JoinSemiLattice, LubIterator};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PredicateSymbol(String);

#[derive(Debug, Clone)]
struct FunctionLike<Symbol> {
    tag: Symbol,
    variables: Vec<u32>,
}

struct Clause<AD: AbstractDomain> {
    body_atoms: Vec<FunctionLike<PredicateSymbol>>,
    variable_equalities: Vec<(u32, u32)>,
    func_equalities: Vec<(u32, FunctionLike<AD::FunctionSymbol>)>,
    num_variables: u32,
}

impl<AD: AbstractDomain + Clone> Clause<AD> {
    fn execute(
        &self,
        mut f: impl FnMut(Query<AD>) -> Substitution<AD>,
        input: &Substitution<AD>,
    ) -> Substitution<AD> {
        let mut subst = input.extended(self.num_variables);

        for atom in &self.body_atoms {
            let local_subst = subst.rename_project(&atom.variables);
            let query = Query {
                predicate: atom.tag.clone(),
                subst: local_subst,
            };
            subst.extend_renamed(&atom.variables, f(query));
        }

        for &(lhs, rhs) in &self.variable_equalities {
            subst.apply_eq_constraint(lhs, rhs);
        }

        for (lhs, rhs) in &self.func_equalities {
            subst.apply_func_constraint(*lhs, rhs);
        }

        subst.restrict(input.len());
        subst
    }
}

struct Gaia<AD: AbstractDomain> {
    program: HashMap<(PredicateSymbol, u32), Vec<Clause<AD>>>,
}

#[derive(Debug)]
struct Substitution<AD>(Vec<AD>);

impl<AD> Substitution<AD> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }
}

impl<AD: JoinSemiLattice> JoinSemiLattice for Substitution<AD> {
    fn bot() -> Self {
        Substitution(Vec::new())
    }

    fn join(&self, other: &Self) -> Self {
        Substitution(
            (0..std::cmp::max(self.0.len(), other.0.len()))
                .map(|i| AD::join_opt(self.0.get(i), other.0.get(i)))
                .collect(),
        )
    }

    fn leq(&self, other: &Self) -> bool {
        if self.0.len() > other.0.len() {
            return false;
        }

        self.0.iter().zip(&other.0).all(|(a, b)| a.leq(b))
    }
}

impl<AD: AbstractDomain + Clone> Substitution<AD> {
    fn extended(&self, num_vars: u32) -> Self {
        let mut res = Vec::with_capacity(num_vars as usize);
        res.extend_from_slice(&self.0);
        res.resize_with(num_vars as usize, AD::bot);
        Substitution(res)
    }

    fn restrict(&mut self, num_vars: u32) {
        self.0.resize_with(num_vars as usize, || {
            panic!("restrict: num_vars must be less then length")
        });
    }

    fn rename_project(&self, vars: &[u32]) -> Self {
        Substitution(vars.iter().map(|&v| self.0[v as usize].clone()).collect())
    }

    fn extend_renamed(&mut self, vars: &[u32], updated: Self) {
        for (var, update) in vars.iter().zip(updated.0) {
            self.0[*var as usize].unify_with(update);
        }
    }

    fn apply_eq_constraint(&mut self, lhs: u32, rhs: u32) {
        if lhs == rhs {
            return;
        }

        let higher = std::cmp::max(lhs, rhs) as usize;
        let lower = std::cmp::min(lhs, rhs) as usize;

        let (left, right) = self.0.split_at_mut(higher);

        left[lower].unify_replace_both(&mut right[0]);
    }

    fn apply_func_constraint(&mut self, lhs: u32, rhs: &FunctionLike<AD::FunctionSymbol>) {
        let (left, right) = self.0.split_at_mut(lhs as usize);
        let (current, right) = right.split_first_mut().unwrap();

        let subterms: Vec<_> = rhs
            .variables
            .iter()
            .map(|v| match *v {
                x if (x > lhs) => Some(&right[(x - lhs - 1) as usize]),
                x if (x == lhs) => None,
                _ => Some(&left[*v as usize]),
            })
            .collect();

        current.unify_nested(&rhs.tag, subterms);
    }
}

struct Query<AD: AbstractDomain> {
    subst: Substitution<AD>,
    predicate: PredicateSymbol,
}

impl<AD: AbstractDomain + Clone> MonotoneTransform<Query<AD>> for Gaia<AD> {
    type Output = Substitution<AD>;

    fn call<F>(&self, mut f: F, query: Query<AD>) -> Self::Output
    where
        F: FnMut(Query<AD>) -> Self::Output,
    {
        self.program
            .get(&(query.predicate, query.subst.len()))
            .into_iter()
            .flatten()
            .map(|c| c.execute(&mut f, &query.subst))
            .lub()
    }
}
