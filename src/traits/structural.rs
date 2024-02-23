use super::lattice::{Meet, Top};
use crate::{type_inference::Program, util::tup::Tup};

pub trait TypeDomain: Top + Meet + Clone + PartialOrd {
    type Functor: Clone;
    type Constructor: Clone + From<Self::Functor>;
    type Builtin: Clone;
    type Config;

    fn init(config: Self::Config) -> Self;
    fn configure<P>(
        program: &Program<P, Self::Functor, Self::Constructor, Self::Builtin>,
    ) -> Self::Config;

    fn cons(config: &Self::Config, ctor: Self::Constructor, subterms: Vec<Self>) -> Option<Self>;
    fn uncons(&self, func: &Self::Functor) -> Option<Vec<Self>>;

    fn interpret(builtin: Self::Builtin, tup: Tup<Self>) -> Option<Tup<Self>>;
}
