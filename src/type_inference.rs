use std::{array::IntoIter, collections::HashMap, hash::Hash};

use crate::{
    lattice::{Bottom, Join, LubIterator, Meet, Top},
    nemo_model::*,
    type_inference::type_table::TypeTable,
};

use self::type_table::InterpretBuiltin;

mod flat_type;
mod tup;
mod type_table;

pub trait Uncons<F>: Sized {
    fn uncons(&self, func: &F) -> Option<Vec<Self>>;
    fn is_principal(&self, func: &F) -> bool;
}

pub trait Cons<C>: Sized {
    fn cons(ctor: C, subterms: Vec<Self>) -> Option<Self>;
}

enum HeadTerm<C> {
    Var(Variable),
    NemoCtor(C, Vec<HeadTerm<C>>),
}

enum BodyTerm<F> {
    Var(Variable),
    Functor {
        functor: F,
        subterms: Vec<BodyTerm<F>>,
    },
    DontCare,
}

enum BodyAtom<F, P, Builtin> {
    Builtin {
        builtin: Builtin,
        terms: Vec<BodyTerm<F>>,
    },
    Predicate {
        predicate: P,
        terms: Vec<BodyTerm<F>>,
    },
}

struct PatClause<Ctor, Dtor, Predicate, Builtin> {
    head: Vec<HeadTerm<Ctor>>,
    body: Vec<BodyAtom<Dtor, Predicate, Builtin>>,
    body_variables: u16,
}

impl<Ctor, Dtor, Predicate, Builtin> PatClause<Ctor, Dtor, Predicate, Builtin>
where
    Ctor: Clone,
    Dtor: Clone,
    Predicate: Clone,
{
    fn execute<T>(&self, mut recursive: impl FnMut(Predicate) -> TypeTable<T>) -> TypeTable<T>
    where
        T: Cons<Ctor>
            + Cons<Dtor>
            + Uncons<Dtor>
            + Top
            + Bottom
            + Meet
            + Clone
            + InterpretBuiltin<Builtin = Builtin>,
    {
        let mut table: TypeTable<T> = TypeTable::new(self.body_variables);

        for atom in &self.body {
            match atom {
                BodyAtom::Builtin { builtin, terms } => table.apply_builtin(builtin, terms),
                BodyAtom::Predicate { predicate, terms } => {
                    let other = recursive(predicate.clone());
                    table = table.meet(&other, terms);
                }
            }
        }

        table.apply_ctor(&self.head)
    }
}

struct Program<C, F, P, B>(HashMap<P, Box<[PatClause<C, F, P, B>]>>);

impl<Ctor, Dtor, Predicate, Builtin> Program<Ctor, Dtor, Predicate, Builtin>
where
    Ctor: Clone,
    Dtor: Clone,
    Predicate: Clone + Hash + Eq,
{
    fn execute<T>(
        &self,
        p: &Predicate,
        mut recursive: impl FnMut(Predicate) -> TypeTable<T>,
    ) -> TypeTable<T>
    where
        T: Cons<Ctor>
            + Cons<Dtor>
            + Uncons<Dtor>
            + Top
            + Bottom
            + Meet
            + Join
            + Clone
            + InterpretBuiltin<Builtin = Builtin>,
    {
        self.0
            .get(p)
            .iter()
            .flat_map(|b| b.iter())
            .map(|clause| clause.execute(&mut recursive))
            .fold(TypeTable::empty(), |mut current, next| {
                current.union_with(&next);
                current
            })
    }
}
