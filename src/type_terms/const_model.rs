use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum NemoFunctor {
    Double,
    StrConst(StrConst),
    IntConst(IntConst),
    RdfConst(RdfConst),
    Map {
        tag: Option<TermTag>,
        keys: Arc<[MapKey]>,
    },
    List {
        tag: Option<TermTag>,
        length: usize,
    },
}

#[derive(Debug, Clone)]
pub enum NemoCtor {
    Aggregate,
    Null(usize),
    Functor(NemoFunctor),
}

pub type IntConst = i64;
pub type StrConst = Arc<str>;
pub type RdfConst = Arc<str>;
pub type NullGenerator = usize;
pub type MapKey = Arc<str>;
pub type TermTag = Arc<str>;
