use std::collections::HashMap;

use crate::{
    traits::{
        lattice::{LocalMinimum, Meet, PreOrder, Top},
        structural::{Cons, InterpretBuiltin, TypeDomain, Uncons},
    },
    type_inference::Program,
    util::tup::Tup,
};

use super::const_model::{NemoBuiltin, NemoCtor, NemoFunctor, NemoModel};

#[derive(Debug, Clone)]
pub struct StructuredType {
    flat_type: (),
    backward_edge: Option<NonZeroU8>,
    and_nodes: HashMap<NemoFunctor, Vec<StructuredType>>,
}

pub type StructuredTypeConfig = ();

impl PreOrder for StructuredType {
    fn leq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Meet for StructuredType {
    fn meet_with(&mut self, other: &Self) {
        todo!()
    }
}

impl Top for StructuredType {
    fn top() -> Self {
        todo!()
    }
}

impl InterpretBuiltin<NemoBuiltin> for StructuredType {
    fn interpret(builtin: NemoBuiltin, tup: Tup<Self>) -> Option<Tup<Self>> {
        todo!()
    }
}

impl Uncons<NemoFunctor> for StructuredType {
    fn uncons(&self, func: &NemoFunctor) -> Option<Vec<Self>> {
        todo!()
    }
}

impl Cons<NemoFunctor> for StructuredType {
    type Config = StructuredTypeConfig;

    fn cons(config: &Self::Config, ctor: NemoFunctor, subterms: Vec<Self>) -> Option<Self> {
        todo!()
    }
}

impl Cons<NemoCtor> for StructuredType {
    type Config = StructuredTypeConfig;

    fn cons(config: &Self::Config, ctor: NemoCtor, subterms: Vec<Self>) -> Option<Self> {
        todo!()
    }
}

impl LocalMinimum<StructuredTypeConfig> for StructuredType {
    fn local_minimum(key: &StructuredTypeConfig) -> Self {
        todo!()
    }
}

impl TypeDomain for StructuredType {
    type Model = NemoModel;

    type Config = StructuredTypeConfig;

    fn configure<P>(program: &Program<P, NemoModel>) -> StructuredTypeConfig {
        todo!()
    }
}
