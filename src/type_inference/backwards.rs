use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::traits::{
    lattice::{Meet, ThreeWayCompare, Top},
    structural::TypeDomain,
};

use super::{fixpoint::Recursor, type_table::TypeTable, TypeAnalysis};

#[derive(Debug, Clone, PartialEq, Eq)]
struct BackType<T> {
    full: T,
    exist_any: bool,
}

impl<T> From<T> for BackType<T> {
    fn from(value: T) -> Self {
        BackType {
            full: value,
            exist_any: false,
        }
    }
}

impl<T: PartialOrd> PartialOrd for BackType<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(
            ThreeWayCompare::init()
                .chain(&self.exist_any, &other.exist_any)?
                .chain(&self.full, &other.full)?
                .finish(),
        )
    }
}

impl<T: Meet + PartialOrd> Meet for BackType<T> {
    fn meet_with(&mut self, other: &Self) {
        self.full.meet_with(&other.full);
        self.exist_any.meet_with(&other.exist_any);
    }
}

impl<T: PartialOrd + Top> Top for BackType<T> {
    fn top() -> Self {
        BackType {
            full: T::top(),
            exist_any: true,
        }
    }
}

impl<T: TypeDomain> TypeDomain for BackType<T> {
    type Functor = T::Functor;

    type Constructor = T::Constructor;

    type Builtin = T::Builtin;

    type Config = T::Config;

    fn init(_config: Self::Config) -> Self {
        unimplemented!()
    }

    fn configure<P>(
        _program: &super::Program<P, Self::Functor, Self::Constructor, Self::Builtin>,
    ) -> Self::Config {
        unimplemented!()
    }

    fn cons(config: &Self::Config, ctor: Self::Constructor, subterms: Vec<Self>) -> Option<Self> {
        todo!()
    }

    fn uncons(&self, func: &Self::Functor) -> Option<Vec<Self>> {
        todo!()
    }

    fn interpret(
        builtin: Self::Builtin,
        tup: crate::util::tup::Tup<Self>,
    ) -> Option<crate::util::tup::Tup<Self>> {
        todo!()
    }
}

impl<K: Eq + Hash, V> Recursor<K, V> for &HashMap<K, V> {
    fn recurse(&mut self, arg: K) -> &V {
        self.get(&arg).unwrap()
    }
}

impl<P, T> TypeAnalysis<P, T>
where
    P: Eq + Hash + Clone,
    T: TypeDomain,
{
    fn backprop(
        &self,
        mut max_types: &HashMap<P, TypeTable<T>>,
        result: &mut HashMap<P, TypeTable<BackType<T>>>,
        predicate: &P,
    ) -> impl Iterator<Item = P> {
        let mut changed = HashSet::new();
        let head = result
            .get(predicate)
            .expect("backprop called with non-existent predicate")
            .clone();

        for clause in self.program.0.get(predicate).into_iter().flatten() {
            let joined = clause.execute_body(&self.config, &mut max_types);
            // reverse-destructure head -> frontier
            let frontier: TypeTable<BackType<T>> = (|_, _| todo!())(joined, &head);

            for atom in &clause.body_atoms {
                // construct head -> matching
                let matching: TypeTable<BackType<T>> =
                    frontier.clone().apply_atom(&self.config, &atom.terms);

                match result.entry(atom.predicate.clone()) {
                    std::collections::hash_map::Entry::Occupied(mut entry) => {
                        if entry.get_mut().union_with(matching) {
                            changed.insert(atom.predicate.clone());
                        }
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(matching);
                        changed.insert(atom.predicate.clone());
                    }
                }
            }
        }

        changed.into_iter()
    }

    fn backwards(&self, dg: HashMap<P, Vec<P>>, output: P, forward: HashMap<P, TypeTable<T>>) {
        let start = forward.get(&output).unwrap().clone();
        let mut work = vec![output.clone()];
        let mut result = HashMap::from([(output, start.map(BackType::from))]);

        while let Some(p) = work.pop() {
            let updated = self.backprop(&forward, &mut result, &p);

            for changed in updated {
                let dependent = dg
                    .get(&changed)
                    .into_iter()
                    .flatten()
                    .filter(|p| !work.contains(&p))
                    .cloned()
                    .collect::<Vec<_>>();

                work.extend(dependent)
            }
        }
    }
}
