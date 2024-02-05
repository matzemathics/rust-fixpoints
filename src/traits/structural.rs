use super::lattice::{Bottom, Meet, Top};
use crate::util::tup::Tup;
use std::hash::Hash;

pub(crate) trait Uncons<F>: Sized {
    fn uncons(&self, func: &F) -> Option<Vec<Self>>;
    fn is_principal(&self, func: &F) -> bool;
}

pub(crate) trait Cons<C>: Sized {
    fn cons(ctor: C, subterms: Vec<Self>) -> Option<Self>;
}
pub(crate) trait InterpretBuiltin<Builtin>: Sized {
    fn interpret(builtin: Builtin, tup: Tup<Self>) -> Option<Tup<Self>>;
}

pub(crate) trait RuleModel {
    type Constructor: Clone;
    type Functor: Clone;
    type Builtin: Clone;
    type Predicate: Clone + Hash + Eq;
}

pub(crate) trait TypeDomain<M: RuleModel>:
    Cons<M::Constructor>
    + Cons<M::Functor>
    + Uncons<M::Functor>
    + Top
    + Bottom
    + Meet
    + Clone
    + InterpretBuiltin<M::Builtin>
{
}
