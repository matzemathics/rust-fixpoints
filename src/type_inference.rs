use std::{collections::HashMap, hash::Hash};

use crate::{
    traits::structural::{ConstModel, TypeDomain},
    type_inference::type_table::TypeTable,
};

use self::{
    fixpoint::{MonotoneTransform, Recursor},
    model::{BodyAtom, BodyBuiltin, PatClause},
};

pub mod fixpoint;
pub mod model;
pub mod tup;
pub mod type_table;

impl<P: Clone, M: ConstModel> PatClause<P, M> {
    fn execute<T: TypeDomain<Model = M>>(
        &self,
        config: &<T as TypeDomain>::Config,
        recursive: &mut impl Recursor<P, TypeTable<T>>,
    ) -> TypeTable<T> {
        let mut table: TypeTable<T> = TypeTable::new(self.body_variables);

        for BodyAtom { predicate, terms } in &self.body_atoms {
            let other = recursive.recurse(predicate.clone());
            table = table.meet(other, terms);
        }

        for BodyBuiltin { builtin, terms } in &self.body_builtins {
            table.apply_builtin(config, builtin, terms);
        }

        table.apply_ctor(config, &self.head)
    }
}

pub struct Program<P, C: ConstModel>(HashMap<P, Vec<PatClause<P, C>>>);

impl<P, C: ConstModel> Program<P, C> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl<P: Eq + Hash, C: ConstModel> Program<P, C> {
    pub fn add_rule(&mut self, predicate: P, clause: PatClause<P, C>) {
        self.0.entry(predicate).or_default().push(clause)
    }
}

impl<P, C> Program<P, C>
where
    P: Eq + Hash + Clone,
    C: ConstModel,
{
    fn execute<T: TypeDomain<Model = C>>(
        &self,
        config: &<T as TypeDomain>::Config,
        p: &P,
        recursive: &mut impl Recursor<P, TypeTable<T>>,
    ) -> TypeTable<T> {
        self.0
            .get(p)
            .iter()
            .flat_map(|b| b.iter())
            .map(|clause| clause.execute(config, recursive))
            .fold(TypeTable::empty(), |mut current, next| {
                current.union_with(next);
                current
            })
    }

    pub fn analyse<T: TypeDomain<Model = C>>(self) -> TypeAnalysis<P, T> {
        TypeAnalysis {
            program: self,
            config: todo!(),
        }
    }
}

pub struct TypeAnalysis<P, T: TypeDomain> {
    program: Program<P, T::Model>,
    config: <T as TypeDomain>::Config,
}

impl<P, T> MonotoneTransform<P> for TypeAnalysis<P, T>
where
    P: Clone + Eq + Hash,
    T: TypeDomain,
{
    type Output = TypeTable<T>;

    fn call(&self, mut f: impl Recursor<P, Self::Output>, a: P) -> Self::Output {
        self.program.execute(&self.config, &a, &mut f)
    }
}
