use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    ops::{Add, Deref, Mul, Sub},
};

trait JoinSemiLattice: Sized {
    fn bot() -> Self;
    fn join(&self, other: &Self) -> Self;
    fn leq(&self, other: &Self) -> bool;

    fn join_opt(lhs: Option<&Self>, rhs: Option<&Self>) -> Self {
        match (lhs, rhs) {
            (None, None) => Self::bot(),
            (Some(x), None) => x.join(&Self::bot()),
            (None, Some(x)) => x.join(&Self::bot()),
            (Some(x), Some(y)) => x.join(y),
        }
    }
}

trait BorrowedLubIterator<T> {
    fn borrowed_lub(self) -> T;
}

impl<T: JoinSemiLattice, I> BorrowedLubIterator<T> for I
where
    I: Iterator,
    I::Item: Deref<Target = T>,
{
    fn borrowed_lub(self) -> T {
        self.fold(T::bot(), |a, b| a.join(&b))
    }
}

trait LubIterator<T> {
    fn lub(self) -> T;
}

impl<T: JoinSemiLattice, I> LubIterator<T> for I
where
    I: Iterator<Item = T>,
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

impl JoinSemiLattice for u128 {
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
            .borrowed_lub();

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

trait MonotoneTransform<T> {
    type Output;

    fn call<F>(&self, f: F, a: T) -> Self::Output
    where
        F: FnMut(T) -> Self::Output;
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
    fn repeat_computation(&mut self, alpha: &T, tau: &impl MonotoneTransform<T, Output = R>) {
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

fn compute_fixpoint<T, R>(
    alpha: T,
    tau: impl MonotoneTransform<T, Output = R>,
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

impl<T> MonotoneTransform<T> for Factorial
where
    T: Eq + JoinSemiLattice + Mul<Output = T> + Sub<Output = T> + From<u8> + Copy,
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

struct Fib;

impl<T> MonotoneTransform<T> for Fib
where
    T: Eq + JoinSemiLattice + Mul<Output = T> + Sub<Output = T> + Add<Output = T> + From<u8> + Copy,
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

struct AtomLike {
    predicate: FunctionSymbol,
    variables: Vec<u32>,
}

struct Clause {
    body_atoms: Vec<AtomLike>,
    variable_equalities: Vec<(u32, u32)>,
    func_equalities: Vec<(u32, AtomLike)>,
    num_variables: u32,
}

impl Clause {
    fn execute<AD: AbstractDomain + Clone>(
        &self,
        mut f: impl FnMut(Query<AD>) -> Substitution<AD>,
        input: &Substitution<AD>,
    ) -> Substitution<AD> {
        let mut subst = input.extended(self.num_variables);

        for atom in &self.body_atoms {
            let local_subst = subst.rename_project(&atom.variables);
            let query = Query {
                predicate: atom.predicate.clone(),
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

struct Gaia {
    program: HashMap<(FunctionSymbol, u32), Vec<Clause>>,
}

#[derive(PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Clone)]
struct FunctionSymbol(String);

#[derive(Debug)]
struct Substitution<AD>(Vec<AD>);

trait AbstractDomain: JoinSemiLattice {
    fn unify_with(&mut self, other: Self);
    fn unify_nested(&mut self, functor: &FunctionSymbol, subterms: Vec<Option<&Self>>);

    fn unify_replace_both(&mut self, other: &mut Self) {
        let right = std::mem::replace(other, Self::bot());
        self.unify_with(right);
        *other = self.join(&Self::bot());
    }
}

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

    fn apply_func_constraint(&mut self, lhs: u32, rhs: &AtomLike) {
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

        current.unify_nested(&rhs.predicate, subterms);
    }
}

struct Query<AD> {
    subst: Substitution<AD>,
    predicate: FunctionSymbol,
}

impl<AD: AbstractDomain + Clone> MonotoneTransform<Query<AD>> for Gaia {
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

#[cfg(test)]
mod test {
    use crate::{compute_fixpoint, Factorial};

    #[test]
    fn test_lub() {
        use crate::BorrowedLubIterator;
        let nums = [1 as u64, 4, 2, 42, 1341, 3];
        assert_eq!(nums.iter().borrowed_lub(), 1341);
    }

    #[test]
    fn test_factorial() {
        let (table, _) = compute_fixpoint(4 as u64, Factorial);
        assert_eq!(table.lookup(&4), Some(&24));
    }
}

fn main() {
    let arg = std::env::args().last();
    let alpha: u128 = arg.and_then(|x| x.parse().ok()).unwrap_or(4);
    let (table, deps) = compute_fixpoint(alpha, Fib);
    println!("{table:?}");
    println!("{deps:?}");
    println!("result: {}", table.lookup(&alpha).unwrap());
}
