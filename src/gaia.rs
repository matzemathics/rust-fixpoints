use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::repeat_with;
use std::sync::Arc;

use crate::abstract_domain::{AbstractDomain, Shape, Shaped};
use crate::fixpoint::MonotoneTransform;
use crate::lattice::{JoinSemiLattice, LubIterator};

type Variable = u8;

#[derive(Debug, Clone)]
struct NormalizedClause<S> {
    body_atoms: Vec<Shaped<S, Variable>>,
    variable_equalities: Vec<(Variable, Variable)>,
    func_equalities: Vec<(Variable, Shaped<S, Variable>)>,
    num_variables: Variable,
}

impl<S: Shape> NormalizedClause<S> {
    fn execute<AD>(
        &self,
        mut f: impl FnMut(Query<AD>) -> Substitution<AD>,
        upper_bound: Query<AD>,
    ) -> Substitution<AD>
    where
        AD: AbstractDomain<Shape = S> + Clone,
    {
        let mut subst = upper_bound.extended(self.num_variables);

        for atom in &self.body_atoms {
            let query = subst.rename_project(atom);
            subst.extend_renamed(&atom.variables, f(query));
        }

        for &(lhs, rhs) in &self.variable_equalities {
            subst.apply_eq_constraint(lhs, rhs);
        }

        for (lhs, rhs) in &self.func_equalities {
            subst.apply_func_constraint(*lhs, rhs);
        }

        subst.restrict(upper_bound.len());
        subst
    }
}

pub struct Gaia<S> {
    program: Arc<[(S, NormalizedClause<S>)]>,
}

pub trait Clause: Sized {
    type Shape: Shape + Clone + Debug;
}

fn into_normalized<C: Clause>(c: C) -> (C::Shape, NormalizedClause<C>) {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Substitution<AD>(Vec<AD>);

impl<AD> Substitution<AD> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }
}

impl<AD: PartialOrd> PartialOrd for Substitution<AD> {
    fn le(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        self.0.iter().zip(&other.0).all(|(a, b)| a.le(b))
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.0.len() != other.0.len() {
            return None;
        }

        if self == other {
            return Some(Ordering::Equal);
        }

        if self <= other {
            if other <= self {
                None
            } else {
                Some(Ordering::Less)
            }
        } else {
            if other <= self {
                Some(Ordering::Greater)
            } else {
                None
            }
        }
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

    fn apply_func_constraint(&mut self, lhs: u32, rhs: &Shaped<AD::Shape>) {
        let (left, right) = self.0.split_at_mut(lhs as usize);
        let (current, right) = right.split_first_mut().unwrap();

        let subterms: Vec<_> = rhs
            .variables
            .iter()
            .map(|v| match *v {
                x if (x > lhs) => Some(&mut right[(x - lhs - 1) as usize]),
                x if (x == lhs) => None,
                _ => Some(&mut left[*v as usize]),
            })
            .collect();

        current.unify_nested(&rhs.tag, &subterms);
    }
}

pub type Query<AD: AbstractDomain> = Shaped<AD::Shape, AD>;

impl<PredicateSymbol, AD: AbstractDomain> Query<PredicateSymbol, AD> {
    pub fn new(predicate: PredicateSymbol, arity: u32) -> Self {
        Self {
            predicate,
            subst: Substitution(repeat_with(AD::any).take(arity as usize).collect()),
        }
    }
}

impl<AD: PartialOrd, PredicateSymbol: Eq> PartialOrd for Query<PredicateSymbol, AD> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.predicate != other.predicate {
            return None;
        }

        self.subst.partial_cmp(&other.subst)
    }
}

impl<AD> MonotoneTransform<Query<AD>> for Gaia<AD::Shape>
where
    AD: AbstractDomain + Clone,
{
    type Output = Substitution<AD>;

    fn call<F>(&self, mut f: F, query: Query<AD>) -> Self::Output
    where
        F: FnMut(Query<AD>) -> Self::Output,
    {
        self.program
            .iter()
            .filter_map(|(shape, clause)| {
                shape
                    .project_reorder(query)
                    .map(|input| clause.execute(f, input.subterms))
            })
            .lub()
    }
}
