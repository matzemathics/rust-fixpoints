use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
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

    #[must_use]
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

    fn lookup<'a>(&'a self, k: &K) -> Option<&'a V> {
        self.0.get(k)
    }

    fn dom_contains(&self, k: &K) -> bool {
        self.0.contains_key(k)
    }
}

#[derive(Debug)]
struct DependencyGraph<T>(PhantomData<T>);

impl<T> Default for DependencyGraph<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: JoinSemiLattice> DependencyGraph<T> {
    fn remove<'a>(&mut self, deps: impl Iterator<Item = &'a T>)
    where
        T: 'a,
    {
        todo!()
    }

    fn extend(&mut self, node: T) {
        todo!()
    }

    fn add(&mut self, node: &T, dep: T) {
        todo!()
    }

    fn dom_contains(&self, node: &T) -> bool {
        todo!()
    }
}

trait MonotoneTransform<T> {
    fn call<F>(&self, f: F, a: T) -> T
    where
        F: FnMut(T) -> T;
}

struct FixComputation<T> {
    suspended: Vec<T>,
    dependencies: DependencyGraph<T>,
    partial_table: PartialTable<T, T>,
}

impl<T> Default for FixComputation<T> {
    fn default() -> Self {
        Self {
            suspended: Default::default(),
            dependencies: Default::default(),
            partial_table: Default::default(),
        }
    }
}

impl<T> FixComputation<T>
where
    T: JoinSemiLattice + Hash + Eq + Clone + Debug,
{
    fn repeat_computation(&mut self, alpha: &T, tau: &impl MonotoneTransform<T>) {
        if self.dependencies.dom_contains(alpha) || self.suspended.contains(alpha) {
            return;
        }

        if !self.partial_table.dom_contains(alpha) {
            self.partial_table.extend(alpha.clone())
        }

        self.suspended.push(alpha.clone());

        while !self.dependencies.dom_contains(&alpha) {
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
        tau: &'a impl MonotoneTransform<T>,
    ) -> impl FnMut(T) -> T + 'a {
        move |beta| {
            self.repeat_computation(&beta, tau);
            let result = self.partial_table.lookup(&beta).unwrap().clone();
            if self.dependencies.dom_contains(alpha) {
                self.dependencies.add(alpha, beta);
            }
            result
        }
    }
}

fn compute_fixpoint<T>(
    alpha: T,
    tau: impl MonotoneTransform<T>,
) -> (PartialTable<T, T>, DependencyGraph<T>)
where
    T: JoinSemiLattice + Eq + Hash + Debug + Clone,
{
    let mut data: FixComputation<T> = FixComputation::default();
    data.repeat_computation(&alpha, &tau);
    (data.partial_table, data.dependencies)
}

mod test {
    #[test]
    fn test_lub() {
        use crate::LubIterator;
        let nums = vec![1, 4, 2, 42, 1341, 3];
        assert_eq!(nums.iter().lub(), 1341);
    }
}

fn main() {
    println!("Hello, world!");
}
