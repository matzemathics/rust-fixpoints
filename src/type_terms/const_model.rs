use std::{fmt::Debug, sync::Arc};

use crate::type_inference::model::{BodyTerm, HeadTerm};

use super::flat_type::FlatType;

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
    BoolConst(bool),
}

#[derive(Debug, Clone)]
pub enum NemoCtor {
    Aggregate,
    Null(NullGenerator),
    Functor(NemoFunctor),
}

impl From<NemoFunctor> for NemoCtor {
    fn from(value: NemoFunctor) -> Self {
        NemoCtor::Functor(value)
    }
}

pub type IntConst = i64;
pub type StrConst = Arc<str>;
pub type IriConst = Arc<str>;
pub type NullGenerator = usize;
pub type MapKey = Arc<str>;
pub type TermTag = Arc<str>;

#[derive(Debug, Clone)]
pub enum NemoBuiltin {
    Import(Vec<FlatType>),
}

pub trait TermLike: Sized {
    fn constant(f: IdentConstant) -> Self;
    fn variable(i: u16) -> Self;
    fn functor(s: Arc<str>, v: Vec<Self>) -> Self;
    fn wildcard() -> Option<Self>;
}

impl TermLike for HeadTerm<NemoCtor> {
    fn constant(c: IdentConstant) -> Self {
        HeadTerm::Ctor(NemoCtor::Functor(NemoFunctor::Const(c)), vec![])
    }

    fn variable(i: u16) -> Self {
        HeadTerm::Var(i)
    }

    fn functor(s: Arc<str>, v: Vec<Self>) -> Self {
        HeadTerm::Ctor(
            NemoCtor::Functor(NemoFunctor::Nested(NestedFunctor::List {
                tag: Some(s),
                length: v.len(),
            })),
            v,
        )
    }

    fn wildcard() -> Option<Self> {
        None
    }
}

impl TermLike for BodyTerm<NemoFunctor> {
    fn constant(c: IdentConstant) -> Self {
        BodyTerm::Functor {
            functor: NemoFunctor::Const(c),
            subterms: vec![],
        }
    }

    fn variable(i: u16) -> Self {
        BodyTerm::Var(i)
    }

    fn functor(s: Arc<str>, v: Vec<Self>) -> Self {
        BodyTerm::Functor {
            functor: NemoFunctor::Nested(NestedFunctor::List {
                tag: Some(s),
                length: v.len(),
            }),
            subterms: v,
        }
    }

    fn wildcard() -> Option<Self> {
        Some(BodyTerm::DontCare)
    }
}
