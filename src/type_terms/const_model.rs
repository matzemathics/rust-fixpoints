use std::{fmt::Debug, sync::Arc};

use crate::traits::structural::ConstModel;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum NemoFunctor {
    Double,
    Const(IdentConstant),
    Nested(NestedFunctor),
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum NestedFunctor {
    Map {
        tag: Option<TermTag>,
        keys: Arc<[MapKey]>,
    },
    List {
        tag: Option<TermTag>,
        length: usize,
    },
}

impl Debug for NestedFunctor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Map { tag, keys } => match tag {
                Some(tag) => f.write_fmt(format_args!("Map({tag:?}{{ {keys:?} }})")),
                None => f.write_fmt(format_args!("Map({{ {keys:?} }})")),
            },
            Self::List { tag, length } => match tag {
                Some(tag) => f.write_fmt(format_args!("List({tag:?}//{length})")),
                None => f.write_fmt(format_args!("List({length})")),
            },
        }
    }
}

impl NestedFunctor {
    pub fn arity(&self) -> usize {
        match self {
            NestedFunctor::Map { keys, .. } => keys.len(),
            NestedFunctor::List { length, .. } => *length,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentConstant {
    StrConst(StrConst),
    IntConst(IntConst),
    IriConst(IriConst),
}

#[derive(Debug, Clone)]
pub enum NemoCtor {
    Aggregate,
    Null(NullGenerator),
    Functor(NemoFunctor),
}

pub type IntConst = i64;
pub type StrConst = Arc<str>;
pub type IriConst = Arc<str>;
pub type NullGenerator = usize;
pub type MapKey = Arc<str>;
pub type TermTag = Arc<str>;

#[derive(Clone)]
pub enum NemoBuiltin {}

pub struct NemoModel;

impl ConstModel for NemoModel {
    type Constructor = NemoCtor;
    type Functor = NemoFunctor;
    type Builtin = NemoBuiltin;
}
