use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    ops::Deref,
};

trait JoinSemiLattice {
    fn bot() -> Self;
    fn join(&self, other: &Self) -> Self;
    fn leq(&self, other: &Self) -> bool;
}

trait LubIterator<T> {
    fn lub(self) -> T;
}

impl<T: JoinSemiLattice, I> LubIterator<T> for I
where
    I: Iterator,
    I::Item: Deref<Target = T>,
{
    fn lub(self) -> T {
        self.fold(T::bot(), |a, b| a.join(&b))
    }
}

impl JoinSemiLattice for u64 {
    fn bot() -> Self {
        0
    }

    fn join(&self, other: &Self) -> Self {
        std::cmp::max(*self, *other)
    }

    fn leq(&self, other: &Self) -> bool {
        self <= other
    }
}

#[derive(Debug)]
struct PartialTable<K, V>(HashMap<K, V>);

impl<K, V> Default for PartialTable<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K: JoinSemiLattice + Hash + Eq, V: JoinSemiLattice + Eq> PartialTable<K, V> {
    fn extend(&mut self, k: K) {
        debug_assert!(!self.0.contains_key(&k));

        let value = self
            .0
            .iter()
            .filter_map(|(a, b)| if a.leq(&k) { Some(b) } else { None })
            .lub();

        self.0.insert(k, value);
    }

    fn adjust<'a>(&'a mut self, k: &'a K, v: &'a V) -> impl Iterator<Item = &K> + 'a {
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
    fn lookup<'a>(&'a self, k: &K) -> Option<&'a V> {
        self.0.get(k)
    }

    fn dom_contains(&self, k: &K) -> bool {
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
struct DependencyGraph<T>(HashMap<T, Vec<T>>);

impl<T> Default for DependencyGraph<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: Hash + Eq + Clone> DependencyGraph<T> {
    fn remove<'a>(&mut self, deps: impl Iterator<Item = &'a T>)
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

    fn extend(&mut self, node: T) {
        self.0.insert(node, Vec::new());
    }

    fn add(&mut self, node: &T, dep: T) {
        self.0
            .get_mut(node)
            .expect("add - node should be in graph")
            .push(dep);
    }

    fn dom_contains(&self, node: &T) -> bool {
        self.0.contains_key(node)
    }
}

trait MonotoneTransform<T, R> {
    fn call<F>(&self, f: F, a: T) -> R
    where
        F: FnMut(T) -> R;
}

struct FixComputation<T, R> {
    suspended: Vec<T>,
    dependencies: DependencyGraph<T>,
    partial_table: PartialTable<T, R>,
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
    T: JoinSemiLattice + Hash + Eq + Clone + Debug,
    R: JoinSemiLattice + Eq + Clone,
{
    fn repeat_computation(&mut self, alpha: &T, tau: &impl MonotoneTransform<T, R>) {
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

    fn pretended_f<'a>(
        &'a mut self,
        alpha: &'a T,
        tau: &'a impl MonotoneTransform<T, R>,
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

fn compute_fixpoint<T, R>(
    alpha: T,
    tau: impl MonotoneTransform<T, R>,
) -> (PartialTable<T, R>, DependencyGraph<T>)
where
    T: JoinSemiLattice + Eq + Hash + Debug + Clone,
    R: JoinSemiLattice + Eq + Clone,
{
    let mut data: FixComputation<T, R> = FixComputation::default();
    data.repeat_computation(&alpha, &tau);
    (data.partial_table, data.dependencies)
}

struct Factorial;

impl MonotoneTransform<u64, u64> for Factorial {
    fn call<F>(&self, mut f: F, a: u64) -> u64
    where
        F: FnMut(u64) -> u64,
    {
        if a == 0 {
            1
        } else {
            a * f(a - 1)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{compute_fixpoint, Factorial};

    #[test]
    fn test_lub() {
        use crate::LubIterator;
        let nums = [1, 4, 2, 42, 1341, 3];
        assert_eq!(nums.iter().lub(), 1341);
    }

    #[test]
    fn test_factorial() {
        let (table, _) = compute_fixpoint(4, Factorial);
        assert_eq!(table.lookup(&4), Some(&24));
    }
}

fn main() {
    let (table, deps) = compute_fixpoint(4, Factorial);
    println!("{table:?}");
    println!("{deps:?}");
}
