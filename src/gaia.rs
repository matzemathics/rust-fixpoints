use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

use crate::abstract_domain::AbstractDomain;
use crate::fixpoint::MonotoneTransform;
use crate::lattice::{JoinSemiLattice, LubIterator};

#[derive(Debug, Clone)]
struct FunctionLike<Symbol> {
    tag: Symbol,
    variables: Vec<u32>,
}

struct NormalizedClause<C: Clause> {
    body_atoms: Vec<FunctionLike<C::PredicateSymbol>>,
    variable_equalities: Vec<(u32, u32)>,
    func_equalities: Vec<(u32, FunctionLike<C::FunctionSymbol>)>,
    num_variables: u32,
}

impl<C: Clause> NormalizedClause<C> {
    fn execute<AD>(
        &self,
        mut f: impl FnMut(Query<C::PredicateSymbol, AD>) -> Substitution<AD>,
        input: &Substitution<AD>,
    ) -> Substitution<AD>
    where
        AD: AbstractDomain<FunctionSymbol = C::FunctionSymbol> + Clone,
        C::PredicateSymbol: Clone,
    {
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

pub struct Gaia<C: Clause> {
    program: HashMap<(C::PredicateSymbol, u32), Vec<NormalizedClause<C>>>,
}

pub trait Clause: Sized {
    type PredicateSymbol: Hash + Eq + Debug + Clone;
    type FunctionSymbol: Hash + Eq + Debug + Clone;
}

fn into_normalized<C: Clause>(c: C) -> ((C::PredicateSymbol, u32), NormalizedClause<C>) {
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
        if self == other {
            Some(Ordering::Equal)
        } else {
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

        current.unify_nested(&rhs.tag, &subterms);
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Query<PredicateSymbol, AD> {
    pub subst: Substitution<AD>,
    pub predicate: PredicateSymbol,
}

impl<AD: PartialOrd, PredicateSymbol: Eq> PartialOrd for Query<PredicateSymbol, AD> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.predicate != other.predicate {
            return None;
        }

        self.subst.partial_cmp(&other.subst)
    }
}

pub fn default_substitution<AD: AbstractDomain>(arity: u32) -> Substitution<AD> {
    todo!()
}

impl<C, AD> MonotoneTransform<Query<C::PredicateSymbol, AD>> for Gaia<C>
where
    C: Clause,
    AD: AbstractDomain<FunctionSymbol = C::FunctionSymbol> + Clone,
{
    type Output = Substitution<AD>;

    fn call<F>(&self, mut f: F, query: Query<C::PredicateSymbol, AD>) -> Self::Output
    where
        F: FnMut(Query<C::PredicateSymbol, AD>) -> Self::Output,
    {
        self.program
            .get(&(query.predicate, query.subst.len()))
            .into_iter()
            .flatten()
            .map(|c| c.execute(&mut f, &query.subst))
            .lub()
    }
}
