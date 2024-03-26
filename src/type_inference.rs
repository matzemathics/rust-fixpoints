use std::{collections::HashMap, hash::Hash};

use crate::{traits::structural::TypeDomain, type_inference::type_table::TypeTable};

use self::{
    fixpoint::{MonotoneTransform, Recursor},
    model::{BodyAtom, BodyBuiltin, PatClause},
    tup::UnificationFailure,
};

pub mod backwards;
pub mod fixpoint;
pub mod model;
pub mod tup;
pub mod type_table;

impl<Predicate: Clone, Functor, Constructor, Builtin>
    PatClause<Predicate, Functor, Constructor, Builtin>
{
    fn execute_body<T>(
        &self,
        config: &<T as TypeDomain>::Config,
        recursive: &mut impl Recursor<Predicate, TypeTable<T>>,
    ) -> Result<TypeTable<T>, ((usize, Predicate), Vec<UnificationFailure<T>>)>
    where
        T: TypeDomain<Functor = Functor, Constructor = Constructor, Builtin = Builtin>,
    {
        let mut table: TypeTable<T> = TypeTable::new(self.body_variables);

        for (pos, BodyAtom { predicate, terms }) in self.body_atoms.iter().enumerate() {
            let other = recursive.recurse(predicate.clone());
            table = table
                .meet(other, terms.as_slice())
                .map_err(|f| ((pos, predicate.clone()), f))?;
        }

        for BodyBuiltin { builtin, terms } in &self.body_builtins {
            table.apply_builtin(config, builtin, terms);
        }

        Ok(table)
    }
}

#[derive(Clone)]
pub struct Program<Predicate, Functor, Constructor, Builtin>(
    pub(crate) HashMap<Predicate, Vec<PatClause<Predicate, Functor, Constructor, Builtin>>>,
);

impl<P: Eq + Hash, F, C, B> Program<P, F, C, B> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn add_rule(&mut self, predicate: P, clause: PatClause<P, F, C, B>) {
        self.0.entry(predicate).or_default().push(clause)
    }
}

impl<Predicate, Functor, Constructor, Builtin> Program<Predicate, Functor, Constructor, Builtin>
where
    Predicate: Eq + Hash + Clone,
{
    fn execute<T>(
        &self,
        config: &<T as TypeDomain>::Config,
        p: &Predicate,
        recursive: &mut impl Recursor<Predicate, TypeTable<T>>,
    ) -> TypeTable<T>
    where
        T: TypeDomain<Functor = Functor, Constructor = Constructor, Builtin = Builtin>,
    {
        self.0
            .get(p)
            .iter()
            .flat_map(|b| b.iter())
            .filter_map(|clause| {
                clause
                    .execute_body(config, recursive)
                    .ok()
                    .map(|body| body.apply_ctor(config, &clause.head))
            })
            .fold(TypeTable::empty(), |mut current, next| {
                current.union_with(next);
                current
            })
    }

    pub fn analyse<T>(self) -> TypeAnalysis<Predicate, T>
    where
        T: TypeDomain<Functor = Functor, Constructor = Constructor, Builtin = Builtin>,
    {
        let config = T::configure(&self);
        TypeAnalysis {
            program: self,
            config,
        }
    }
}

#[derive(Clone)]
pub struct TypeAnalysis<P, T: TypeDomain> {
    program: Program<P, T::Functor, T::Constructor, T::Builtin>,
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
