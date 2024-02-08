use std::{collections::HashSet, fmt::Debug, iter::repeat_with, sync::Arc};

use crate::{
    traits::{
        lattice::{Bottom, LocalMinimum, Meet, PreOrder, Top},
        structural::{Cons, InterpretBuiltin, TypeDomain, Uncons},
    },
    type_inference::Program,
};

use super::{
    const_model::{
        IdentConstant, MapKey, NemoBuiltin, NemoCtor, NemoFunctor, NemoModel, NestedFunctor,
        NullGenerator, TermTag,
    },
    hash_lattice::ToppedLattice,
};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum StdType {
    Empty = 0,      // term `()`,
    Pair = 1,       // terms of form `(x, y)`
    Triple = 2,     // terms of form `(x, y, z)`
    OtherList = 3,  // terms of form `(x_i, ...)` and none of the above
    TaggedList = 4, // terms of form `F(x_i, ...)`
    Map = 5,        // terms of form `{k_i = x_i, ...}` or `F{k_i = x_i, ...}`
    Null = 6,       // named nulls
    Double = 7,     // terms of type double
    String = 8,     // terms of type string
    OtherRdf = 9,   // other rdf terms
    Zero = 10,      // term `0`
    Pos = 11,       // term of type int > 0
    Neg = 12,       // term of type int < 0
    // Unused, mark the highest discriminant
    Max = 13,
}

impl StdType {
    fn discriminant(&self) -> u8 {
        unsafe { std::mem::transmute::<Self, u8>(*self) }
    }

    fn try_from_discriminant(d: u8) -> Result<Self, ()> {
        if d >= Self::Max.discriminant() {
            return Err(());
        }

        Ok(unsafe { std::mem::transmute::<u8, Self>(d) })
    }
}

#[derive(Debug)]
struct BitPositionVec(Vec<usize>);

macro_rules! impl_index_vec_from {
    ($ty:tt) => {
        impl From<$ty> for BitPositionVec {
            fn from(input: $ty) -> Self {
                let mut res = Vec::new();

                for i in 0..std::mem::size_of::<$ty>() * 8 {
                    if (input as usize & (1 << i)) != 0 {
                        res.push(i)
                    }
                }

                BitPositionVec(res)
            }
        }
    };
}

impl_index_vec_from!(u8);
impl_index_vec_from!(u16);

#[derive(Clone)]
struct StdTypeBitmap(u16);

impl Debug for StdTypeBitmap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_vec().fmt(f)
    }
}

impl StdTypeBitmap {
    fn set(&mut self, which: StdType) {
        self.0 |= 1u16 << (which.discriminant() as u16);
    }

    fn probe(&self, which: StdType) -> bool {
        (self.0 & (which.discriminant() as u16)) != 0
    }

    fn as_vec(&self) -> Vec<StdType> {
        BitPositionVec::from(self.0)
            .0
            .into_iter()
            .filter_map(|i| StdType::try_from_discriminant(i as u8).ok())
            .collect::<Vec<_>>()
    }
}

impl PreOrder for StdTypeBitmap {
    fn leq(&self, other: &Self) -> bool {
        (self.0 & other.0) == self.0
    }
}

impl Bottom for StdTypeBitmap {
    fn bot() -> Self {
        Self(0)
    }
}

impl Top for StdTypeBitmap {
    fn top() -> Self {
        Self((1 << StdType::Max.discriminant()) - 1)
    }
}

impl Meet for StdTypeBitmap {
    fn meet_with(&mut self, other: &Self) {
        self.0 &= other.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ListTagType {
    tag: TermTag,
    length: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MapTagType {
    tag: Option<TermTag>,
    keys: Arc<[MapKey]>,
}

#[derive(Clone, Debug)]
pub struct FlatType {
    std_types: StdTypeBitmap,
    constants: ToppedLattice<IdentConstant>,
    functors: ToppedLattice<NestedFunctor>,
    nulls: ToppedLattice<NullGenerator>,
}

impl LocalMinimum<FlatTypeConfig> for FlatType {
    fn local_minimum(config: &FlatTypeConfig) -> Self {
        // we cannot implement `Bottom`, because when the leq might
        // break, if empty-set is used, where no constants can appear
        // anyway (which might cause leq to return false, even though
        // the value should be greater)
        let mut result = Self {
            std_types: StdTypeBitmap::bot(),
            constants: ToppedLattice(None),
            functors: ToppedLattice(None),
            nulls: ToppedLattice(None),
        };

        if config.use_const_set {
            result.constants = ToppedLattice(Some(HashSet::new()))
        }
        if config.use_func_set {
            result.functors = ToppedLattice(Some(HashSet::new()))
        }
        if config.use_null_set {
            result.nulls = ToppedLattice(Some(HashSet::new()))
        }

        result
    }
}

impl Top for FlatType {
    fn top() -> Self {
        Self {
            std_types: StdTypeBitmap::top(),
            constants: ToppedLattice(None),
            functors: ToppedLattice(None),
            nulls: ToppedLattice(None),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FlatTypeConfig {
    use_const_set: bool,
    use_func_set: bool,
    use_null_set: bool,
}

impl PreOrder for FlatType {
    fn leq(&self, other: &Self) -> bool {
        self.std_types.leq(&other.std_types)
            && self.constants.leq(&other.constants)
            && self.functors.leq(&other.functors)
            && self.nulls.leq(&other.nulls)
    }
}

impl Cons<NemoFunctor> for FlatType {
    type Config = FlatTypeConfig;

    fn cons(config: &FlatTypeConfig, ctor: NemoFunctor, subterms: Vec<Self>) -> Option<Self> {
        let mut result = Self::local_minimum(config);

        match ctor {
            NemoFunctor::Double => {
                debug_assert!(subterms.len() == 0);
                result.std_types.set(StdType::Double);
            }
            NemoFunctor::Const(constant) => {
                debug_assert!(subterms.len() == 0);
                result
                    .constants
                    .0
                    .as_mut()
                    .expect("constant set not unused, if constants are present")
                    .insert(constant);
            }
            NemoFunctor::Nested(func) => {
                result
                    .functors
                    .0
                    .as_mut()
                    .expect("functor set not unused if functors are present")
                    .insert(func);
            }
        }

        Some(result)
    }
}

impl Cons<NemoCtor> for FlatType {
    type Config = FlatTypeConfig;

    fn cons(config: &Self::Config, ctor: NemoCtor, subterms: Vec<Self>) -> Option<Self> {
        match ctor {
            NemoCtor::Aggregate => todo!(),
            NemoCtor::Null(g) => {
                let mut result = Self::local_minimum(config);
                result
                    .nulls
                    .0
                    .as_mut()
                    .expect("nulls present, so null_set cannot be unused")
                    .insert(g);
                Some(result)
            }
            NemoCtor::Functor(f) => Self::cons(config, f, subterms),
        }
    }
}

impl Uncons<NemoFunctor> for FlatType {
    fn uncons(&self, functor: &NemoFunctor) -> Option<Vec<Self>> {
        match functor {
            NemoFunctor::Double => match self.std_types.probe(StdType::Double) {
                true => Some(Vec::new()),
                false => None,
            },
            NemoFunctor::Const(constant) => match self.constants.subsumes(constant) {
                true => Some(Vec::new()),
                false => None,
            },
            NemoFunctor::Nested(functor) => match self.functors.subsumes(functor) {
                true => Some(repeat_with(Self::top).take(functor.arity()).collect()),
                false => None,
            },
        }
    }
}

impl Meet for FlatType {
    fn meet_with(&mut self, other: &Self) {
        self.std_types.meet_with(&other.std_types);
        self.constants.meet_with(&other.constants);
        self.functors.meet_with(&other.functors);
        self.nulls.meet_with(&other.nulls);
    }
}

impl InterpretBuiltin<NemoBuiltin> for FlatType {
    fn interpret(
        _builtin: NemoBuiltin,
        _tup: crate::util::tup::Tup<Self>,
    ) -> Option<crate::util::tup::Tup<Self>> {
        todo!()
    }
}

impl TypeDomain for FlatType {
    type Model = NemoModel;
    type Config = FlatTypeConfig;

    fn configure<P>(program: &Program<P, Self::Model>) -> FlatTypeConfig {
        let mut config = FlatTypeConfig {
            use_const_set: false,
            use_func_set: false,
            use_null_set: false,
        };

        for clause in program.0.values().flatten() {
            for ctor in clause.head_ctors() {
                match ctor {
                    NemoCtor::Null(_) => config.use_null_set = true,
                    NemoCtor::Functor(NemoFunctor::Const(_)) => config.use_const_set = true,
                    NemoCtor::Functor(NemoFunctor::Nested(_)) => config.use_func_set = true,
                    _ => {}
                }
            }

            for functor in clause.body_functors() {
                match functor {
                    NemoFunctor::Nested(_) => config.use_func_set = true,
                    NemoFunctor::Const(_) => config.use_const_set = true,
                    _ => {}
                }
            }
        }

        config
    }
}
