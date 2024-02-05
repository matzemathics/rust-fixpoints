use std::collections::HashMap;

use crate::{
    traits::structural::{RuleModel, TypeDomain},
    type_inference::type_table::TypeTable,
};

use self::model::{BodyAtom, BodyBuiltin, PatClause};

pub mod fixpoint;
pub mod model;
pub mod tup;
pub mod type_table;

impl<M: RuleModel> PatClause<M> {
    fn execute<T: TypeDomain<M>>(
        &self,
        mut recursive: impl FnMut(M::Predicate) -> TypeTable<T>,
    ) -> TypeTable<T> {
        let mut table: TypeTable<T> = TypeTable::new(self.body_variables);

        for BodyAtom { predicate, terms } in &self.body_atoms {
            let other = recursive(predicate.clone());
            table = table.meet(&other, terms);
        }

        for BodyBuiltin { builtin, terms } in &self.body_builtins {
            table.apply_builtin(builtin, terms);
        }

        table.apply_ctor(&self.head)
    }
}

struct Program<M: RuleModel>(HashMap<M::Predicate, Box<[PatClause<M>]>>);

impl<M: RuleModel> Program<M> {
    fn execute<T: TypeDomain<M>>(
        &self,
        p: &M::Predicate,
        mut recursive: impl FnMut(M::Predicate) -> TypeTable<T>,
    ) -> TypeTable<T> {
        self.0
            .get(p)
            .iter()
            .flat_map(|b| b.iter())
            .map(|clause| clause.execute(&mut recursive))
            .fold(TypeTable::empty(), |mut current, next| {
                current.union_with(next);
                current
            })
    }
}
