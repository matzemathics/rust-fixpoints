use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;
use std::ops::Index;
use std::ops::Mul;
use std::ops::Sub;

use crate::traits::lattice::Join;
use crate::traits::lattice::JoinSemiLattice;
use crate::traits::lattice::LocalMinimum;
use crate::traits::lattice::PreOrder;

#[derive(Debug)]
pub struct PartialTable<K, V>(HashMap<K, V>);

impl<K, V> Default for PartialTable<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K, V> PartialTable<K, V>
where
    K: Hash + Eq,
    V: PreOrder + LocalMinimum<K>,
{
    pub(crate) fn extend(&mut self, k: K) {
        debug_assert!(!self.0.contains_key(&k));
        let value = V::local_minimum(&k);
        self.0.insert(k, value);
    }

    pub(crate) fn adjust<'a>(&'a mut self, k: &'a K, v: V) -> bool {
        debug_assert!(self.0.contains_key(k));
        let entry = self.0.get_mut(k).unwrap();
        if v.leq(entry) {
            true
        } else {
            *entry = v;
            false
        }
    }
}

impl<K: Hash + Eq, V> PartialTable<K, V> {
    pub fn lookup<'a>(&'a self, k: &K) -> Option<&'a V> {
        self.0.get(k)
    }

    pub(crate) fn dom_contains(&self, k: &K) -> bool {
        self.0.contains_key(k)
    }
}

#[derive(Debug)]
/// The domain of the dependency graph corresponds to the arguments,
/// that are "up to date". Whenever some value is modified in the partial
/// table, everything that transitively depends on that value must be
/// recomputed and is thus removed from the dependency graph. If no
/// element of dg+(a) is suspended, then pt(a) should be the final fix
/// point.
pub(crate) struct DependencyGraph<T>(HashMap<T, Vec<T>>);

impl<T> Default for DependencyGraph<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: Hash + Eq + Clone> DependencyGraph<T> {
    pub(crate) fn remove(&mut self, dep: &T) {
        // everything, which (transitively) depends on anything in `deps`
        // must be recomputed and thus be removed from the graph

        self.0.remove(dep);
        let mut deleted = HashSet::from([dep.clone()]);

        // transitively delete anything dependent on anything deleted before
        loop {
            let mut changed = false;

            self.0.retain(|k, v| {
                if v.iter().any(|d| deleted.contains(d)) {
                    changed |= deleted.insert(k.clone());
                    false
                } else {
                    true
                }
            });

            if !changed {
                break;
            }
        }
    }

    pub(crate) fn extend(&mut self, node: T) {
        self.0.insert(node, Vec::new());
    }

    pub(crate) fn add(&mut self, node: &T, dep: T) {
        self.0
            .get_mut(node)
            .expect("add - node should be in graph")
            .push(dep);
    }

    pub(crate) fn dom_contains(&self, node: &T) -> bool {
        self.0.contains_key(node)
    }
}

pub trait Recursor<T, R> {
    fn recurse(&mut self, arg: T) -> &R;
}

pub trait MonotoneTransform<T> {
    type Output;

    fn call(&self, f: impl Recursor<T, Self::Output>, a: T) -> Self::Output;
}

pub(crate) struct FixComputation<T, R> {
    pub(crate) suspended: Vec<T>,
    pub(crate) dependencies: DependencyGraph<T>,
    pub(crate) partial_table: PartialTable<T, R>,
}

impl<T, R> Default for FixComputation<T, R> {
    fn default() -> Self {
        Self {
            suspended: Default::default(),
            dependencies: Default::default(),
            partial_table: Default::default(),
        }
    }
}

impl<T, R> FixComputation<T, R>
where
    T: Hash + Eq + Clone + Debug,
    R: PreOrder + Clone + LocalMinimum<T>,
{
    pub(crate) fn repeat_computation(
        &mut self,
        alpha: &T,
        tau: &impl MonotoneTransform<T, Output = R>,
    ) {
        if self.dependencies.dom_contains(alpha) || self.suspended.contains(alpha) {
            return;
        }

        if !self.partial_table.dom_contains(alpha) {
            self.partial_table.extend(alpha.clone())
        }

        self.suspended.push(alpha.clone());

        while !self.dependencies.dom_contains(alpha) {
            self.dependencies.extend(alpha.clone());
            let beta = tau.call(self.recursor(alpha, tau), alpha.clone());
            if self.partial_table.adjust(alpha, beta) {
                self.dependencies.remove(alpha);
            }
        }

        let a2 = self.suspended.pop().expect("symmetrical to push");
        debug_assert_eq!(&a2, alpha);
    }

    fn recursor<'a, Tau>(&'a mut self, alpha: &'a T, tau: &'a Tau) -> FixRecursor<'a, T, R, Tau>
    where
        Tau: MonotoneTransform<T, Output = R>,
    {
        FixRecursor {
            c: self,
            alpha,
            tau,
        }
    }
}

struct FixRecursor<'a, T, R, Tau> {
    c: &'a mut FixComputation<T, R>,
    alpha: &'a T,
    tau: &'a Tau,
}

impl<'a, T, R, Tau> Recursor<T, R> for FixRecursor<'a, T, R, Tau>
where
    T: Hash + Eq + Clone + Debug,
    R: PreOrder + Clone + LocalMinimum<T>,
    Tau: MonotoneTransform<T, Output = R>,
{
    fn recurse(&mut self, arg: T) -> &R {
        self.c.repeat_computation(&arg, self.tau);
        let res = self.c.partial_table.lookup(&arg).unwrap();
        if self.c.dependencies.dom_contains(self.alpha) {
            self.c.dependencies.add(self.alpha, arg);
        }
        res
    }
}

pub fn compute_fixpoint<T, R>(
    alpha: T,
    tau: impl MonotoneTransform<T, Output = R>,
) -> PartialTable<T, R>
where
    T: PreOrder + Eq + Hash + Debug + Clone,
    R: PreOrder + Clone + LocalMinimum<T>,
{
    let mut data: FixComputation<T, R> = FixComputation::default();
    data.repeat_computation(&alpha, &tau);
    data.partial_table
}

pub(crate) struct Factorial;

impl<T> MonotoneTransform<T> for Factorial
where
    T: Eq + Mul<Output = T> + Sub<Output = T> + From<u8> + Copy + JoinSemiLattice,
{
    type Output = T;

    fn call(&self, mut f: impl Recursor<T, T>, a: T) -> T {
        if a == T::bot() {
            T::from(1)
        } else {
            a * *f.recurse(a - T::from(1))
        }
    }
}

pub(crate) struct Fib;

impl<T> MonotoneTransform<T> for Fib
where
    T: Eq + Mul<Output = T> + Sub<Output = T> + Add<Output = T> + From<u8> + Copy + JoinSemiLattice,
{
    type Output = T;

    fn call(&self, mut f: impl Recursor<T, T>, a: T) -> T {
        if a == T::bot() || a == T::from(1) {
            T::from(1)
        } else {
            *f.recurse(a - T::from(1)) + *f.recurse(a - T::from(2))
        }
    }
}

#[cfg(test)]
mod test {
    use super::{compute_fixpoint, Factorial};

    #[test]
    fn test_factorial() {
        let table = compute_fixpoint(4u64, Factorial);
        assert_eq!(table.lookup(&4), Some(&24));
    }
}
