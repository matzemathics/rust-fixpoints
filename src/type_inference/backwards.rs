use std::{
    collections::{HashMap, HashSet}, fmt::Debug, hash::Hash
};

use crate::{
    traits::lattice::Bottom, type_terms::{
        flat_type::{FlatType, WildcardType},
        structured_type::StructuredType,
    }, util::tup::Tup
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
    P: Eq + Hash + Clone + Debug,
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
            .cloned()
            .unwrap_or(TypeTable::empty());

        for clause in self.program.0.get(predicate).into_iter().flatten() {
            let joined = clause.execute_body(&self.config, &mut max_types);
            // reverse-destructure head -> frontier
            let mut frontier: TypeTable<BackType> = TypeTable::empty();

            for usage in &head.rows {
                for produced in &joined.rows {
                    let mut produced: Tup<_> = produced
                        .iter()
                        .map(|t| t.clone().map(WildcardType::wildcarded))
                        .collect();

                    if produced.generalized_unify(usage, &clause.head, BackType::destruct_back_type)
                    {
                        frontier.add_row(produced);
                    }
                }
            }

            for atom in &clause.body_atoms {
                // construct head -> matching
                let matching: TypeTable<BackType> = frontier.clone().apply_atom(
                    &self.config,
                    &atom.terms,
                    StructuredType::from(WildcardType::wildcard()),
                );

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

    pub fn backwards(
        &self,
        dg: HashMap<P, Vec<P>>,
        output: P,
        forward: HashMap<P, TypeTable<ForwardType>>,
    ) -> HashMap<P, TypeTable<BackType>> {
        let start = forward.get(&output).unwrap().clone();
        let mut work = vec![output.clone()];
        let mut result = HashMap::from([(output, start.map(|t| t.map(WildcardType::from)))]);

        while let Some(p) = work.pop() {
            let updated = self.backprop(&forward, &mut result, &p);
            work.extend(updated);
        }

        result
    }
}
