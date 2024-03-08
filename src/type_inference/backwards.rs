use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    traits::lattice::Bottom,
    type_terms::{
        flat_type::{FlatType, WildcardType},
        structured_type::StructuredType,
    },
};

use super::{fixpoint::Recursor, type_table::TypeTable, TypeAnalysis};

type BackType = StructuredType<WildcardType>;
type ForwardType = StructuredType<FlatType>;

impl<K: Eq + Hash, V> Recursor<K, V> for &HashMap<K, V> {
    fn recurse(&mut self, arg: K) -> &V {
        self.get(&arg).unwrap()
    }
}

impl<P> TypeAnalysis<P, StructuredType<FlatType>>
where
    P: Eq + Hash + Clone,
{
    fn backprop(
        &self,
        mut max_types: &HashMap<P, TypeTable<ForwardType>>,
        result: &mut HashMap<P, TypeTable<BackType>>,
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
            let frontier: TypeTable<BackType> = (|_, _| todo!())(joined, &head);

            for atom in &clause.body_atoms {
                // construct head -> matching
                let matching: TypeTable<BackType> =
                    frontier.clone().apply_atom(&self.config, &atom.terms, || {
                        StructuredType::from(WildcardType::wildcard())
                    });

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

    fn backwards(
        &self,
        dg: HashMap<P, Vec<P>>,
        output: P,
        forward: HashMap<P, TypeTable<ForwardType>>,
    ) {
        let start = forward.get(&output).unwrap().clone();
        let mut work = vec![output.clone()];
        let mut result = HashMap::from([(output, start.map(|t| t.map(WildcardType::from)))]);

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
