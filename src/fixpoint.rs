use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;
use std::ops::Mul;
use std::ops::Sub;

use crate::lattice::BorrowedLubIterator;
use crate::lattice::Join;
use crate::lattice::JoinSemiLattice;
use crate::lattice::LocalMinimum;
use crate::lattice::PreOrder;

#[derive(Debug)]
pub(crate) struct PartialTable<K, V>(HashMap<K, V>);

impl<K, V> Default for PartialTable<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K, V> PartialTable<K, V>
where
    K: PreOrder + Hash + Eq,
    V: Join + Eq + LocalMinimum<K>,
{
    pub(crate) fn extend(&mut self, k: K) {
        debug_assert!(!self.0.contains_key(&k));

        let value = self
            .0
            .iter()
            .filter_map(|(a, b)| if a.leq(&k) { Some(b) } else { None })
            .borrowed_lub(V::local_minimum(&k));

        self.0.insert(k, value);
    }

    pub(crate) fn adjust<'a>(&'a mut self, k: &'a K, v: &'a V) -> impl Iterator<Item = &K> + 'a {
        self.0.iter_mut().filter_map(move |(a, b)| {
            if k.leq(a) {
                let updated = b.join(v);
                if updated != *b {
                    *b = updated;
                    return Some(k);
                }
            }
            None
        })
    }
}

impl<K: Hash + Eq, V> PartialTable<K, V> {
    pub(crate) fn lookup<'a>(&'a self, k: &K) -> Option<&'a V> {
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
    pub(crate) fn remove<'a>(&mut self, deps: impl Iterator<Item = &'a T>)
    where
        T: 'a,
    {
        // everything, which (transitively) depends on anything in `deps`
        // must be recomputed and thus be removed from the graph

        let mut deleted = HashSet::new();
        let deps = deps.collect::<HashSet<_>>();

        // delete all nodes dependent on nodes in `deps`
        self.0.retain(|k, v| {
            if v.iter().any(|d| deps.contains(d)) {
                deleted.insert(k.clone());
                false
            } else {
                true
            }
        });

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

pub(crate) trait MonotoneTransform<T> {
    type Output;

    fn call<F>(&self, f: F, a: T) -> Self::Output
    where
        F: FnMut(T) -> Self::Output;
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
    T: PreOrder + Hash + Eq + Clone + Debug,
    R: Join + Eq + Clone + LocalMinimum<T>,
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
            let beta = tau.call(self.pretended_f(alpha, tau), alpha.clone());
            let modified = self.partial_table.adjust(alpha, &beta);
            self.dependencies.remove(modified);
        }

        let a2 = self.suspended.pop().expect("symmetrical to push");
        debug_assert_eq!(&a2, alpha);
    }

    pub(crate) fn pretended_f<'a>(
        &'a mut self,
        alpha: &'a T,
        tau: &'a impl MonotoneTransform<T, Output = R>,
    ) -> impl FnMut(T) -> R + 'a {
        move |beta| {
            self.repeat_computation(&beta, tau);
            let result = self.partial_table.lookup(&beta).unwrap();
            if self.dependencies.dom_contains(alpha) {
                self.dependencies.add(alpha, beta);
            }
            result.clone()
        }
    }
}

pub(crate) fn compute_fixpoint<T, R>(
    alpha: T,
    tau: impl MonotoneTransform<T, Output = R>,
) -> (PartialTable<T, R>, DependencyGraph<T>)
where
    T: PreOrder + Eq + Hash + Debug + Clone,
    R: Join + Eq + Clone + LocalMinimum<T>,
{
    let mut data: FixComputation<T, R> = FixComputation::default();
    data.repeat_computation(&alpha, &tau);
    (data.partial_table, data.dependencies)
}

pub(crate) struct Factorial;

impl<T> MonotoneTransform<T> for Factorial
where
    T: Eq + Mul<Output = T> + Sub<Output = T> + From<u8> + Copy + JoinSemiLattice,
{
    type Output = T;

    fn call<F>(&self, mut f: F, a: T) -> T
    where
        F: FnMut(T) -> T,
    {
        if a == T::bot() {
            T::from(1)
        } else {
            a * f(a - T::from(1))
        }
    }
}

pub(crate) struct Fib;

impl<T> MonotoneTransform<T> for Fib
where
    T: Eq + Mul<Output = T> + Sub<Output = T> + Add<Output = T> + From<u8> + Copy + JoinSemiLattice,
{
    type Output = T;

    fn call<F>(&self, mut f: F, a: T) -> T
    where
        F: FnMut(T) -> T,
    {
        if a == T::bot() || a == T::from(1) {
            T::from(1)
        } else {
            f(a - T::from(1)) + f(a - T::from(2))
        }
    }
}

#[cfg(test)]
mod test {
    use super::{compute_fixpoint, Factorial};

    #[test]
    fn test_factorial() {
        let (table, _) = compute_fixpoint(4u64, Factorial);
        assert_eq!(table.lookup(&4), Some(&24));
    }
}
