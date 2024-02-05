use super::lattice::{Bottom, LocalMinimum, Meet, Top};
use crate::util::tup::Tup;
use std::{backtrace, hash::Hash};

pub trait Uncons<F>: Sized {
    fn uncons(&self, func: &F) -> Option<Vec<Self>>;
}

pub trait Cons<C>: Sized {
    type Config;

    fn cons(config: &Self::Config, ctor: C, subterms: Vec<Self>) -> Option<Self>;
}
pub trait InterpretBuiltin<Builtin>: Sized {
    fn interpret(builtin: Builtin, tup: Tup<Self>) -> Option<Tup<Self>>;
}

pub trait ConstModel {
    type Constructor: Clone;
    type Functor: Clone;
    type Builtin: Clone;
}

macro_rules! assoc {
    (::$id:ident) => {
        <Self as TypeDomain>::$id
    };
    (::$rm:ident::$id:ident) => {
        <<Self as TypeDomain>::$rm as ConstModel>::$id
    };
}

pub trait TypeDomain:
    Cons<assoc!(::Model::Constructor), Config = assoc!(::Config)>
    + Cons<assoc!(::Model::Functor), Config = assoc!(::Config)>
    + Uncons<assoc!(::Model::Functor)>
    + InterpretBuiltin<assoc!(::Model::Builtin)>
    + LocalMinimum<assoc!(::Config)>
    + Top
    + Meet
    + Clone
{
    type Model: ConstModel;
    type Config;
}

pub trait Arity {
    fn arity(&self) -> usize;
}
