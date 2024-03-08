use std::{cmp::Ordering, fmt::Debug, sync::Arc};

use crate::traits::lattice::{Bottom, Meet, ThreeWayCompare, Top, Union};

use super::{
    const_model::{IdentConstant, NemoFunctor},
    topped_lattice::ToppedLattice,
};

macro_rules! prod_lattice {
    {
        $vis:vis struct $ty_id:ident { $($key:ident: $key_ty:ty,)* }
    } => {
        #[derive(Clone, PartialEq, Eq)]
        $vis struct $ty_id {
            $($key: $key_ty,)*
        }

        impl Debug for $ty_id {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut debugger = f.debug_struct(stringify!($ty_id));

                $(
                    if self.$key != <$key_ty>::bot() {
                        debugger.field(stringify!($key), &self.$key);
                    }
                )*

                debugger.finish()
            }
        }

        impl PartialOrd for $ty_id {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(ThreeWayCompare::init()
                        $(.chain(&self.$key, &other.$key)?)*
                        .finish())
            }

            fn le(&self, other: &Self) -> bool {
                $(self.$key <= other.$key)&&*
            }

            fn ge(&self, other: &Self) -> bool {
                $(self.$key >= other.$key)&&*
            }

            fn lt(&self, other: &Self) -> bool {
                $(self.$key < other.$key)&&*
            }

            fn gt(&self, other: &Self) -> bool {
                $(self.$key > other.$key)&&*
            }
        }

        impl Meet for $ty_id {
            fn meet_with(&mut self, other: &Self) {
                $(self.$key.meet_with(&other.$key);)*
            }
        }

        impl Union for $ty_id {
            fn union_with(&mut self, other: &Self) {
                $(self.$key.union_with(&other.$key);)*
            }
        }

        impl Top for $ty_id {
            fn top() -> Self {
                Self {
                    $($key: <$key_ty>::top(),)*
                }
            }
        }

        impl Bottom for $ty_id {
            fn bot() -> Self {
                Self {
                    $($key: <$key_ty>::bot(),)*
                }
            }
        }
    };
}

prod_lattice! {
    pub struct IntType {
        negative63: bool,                 // -2^63 .. -2^31 - 1
        negative31: ToppedLattice<i64>,   // -2^31 ..       - 1
        positive31: ToppedLattice<i64>,   //  0    ..  2^31 - 1
        positive32: bool,                 //  2^31 ..  2^32 - 1
        positive63: bool,                 //  2^32 ..  2^63 - 1
        positive64: bool,                 //  2^63 ..  2^64 - 1
    }
}

impl IntType {
    pub fn contains(&self, val: i64) -> bool {
        match val {
            i if i >= 2i64.pow(32) => self.positive63,
            i if i >= 2i64.pow(31) => self.positive32,
            i if i >= 0 => self.positive31.subsumes(&i),
            i if i >= -(2i64.pow(31)) => self.negative31.subsumes(&i),
            _ => self.negative63,
        }
    }

    pub fn insert(&mut self, val: i64) {
        match val {
            i if i >= 2i64.pow(32) => self.positive63 = true,
            i if i >= 2i64.pow(31) => self.positive32 = true,
            i if i >= 0 => {
                self.positive31.insert(i);
            }
            i if i >= -(2i64.pow(31)) => {
                self.negative31.insert(i);
            }
            _ => self.negative63 = true,
        }
    }
}

prod_lattice! {
    pub struct FlatType {
        string: ToppedLattice<Arc<str>>,
        iri: ToppedLattice<Arc<str>>,
        language_tagged_string: bool,
        float: bool,
        double: bool,
        integer: IntType,
        boolean: bool,
        null: bool,
        other: bool,
    }
}

impl FlatType {
    pub(crate) fn int() -> Self {
        let mut res = Self::bot();
        res.integer = IntType::top();
        res
    }

    pub(crate) fn str() -> Self {
        let mut res = Self::bot();
        res.string = ToppedLattice::top();
        res
    }
}

pub trait TypeLike {
    fn contains(&self, val: &NemoFunctor) -> bool;
    fn from_constant(val: NemoFunctor) -> Self;
}

impl TypeLike for FlatType {
    fn contains(&self, val: &NemoFunctor) -> bool {
        match val {
            NemoFunctor::Double => self.double,
            NemoFunctor::Const(IdentConstant::IntConst(i)) => self.integer.contains(*i),
            NemoFunctor::Const(IdentConstant::StrConst(s)) => self.string.subsumes(s),
            NemoFunctor::Const(IdentConstant::IriConst(iri)) => self.iri.subsumes(iri),
            NemoFunctor::Nested(_) => unimplemented!(),
        }
    }

    fn from_constant(val: NemoFunctor) -> Self {
        let mut result = Self::bot();

        match val {
            NemoFunctor::Double => result.double = true,
            NemoFunctor::Const(IdentConstant::IntConst(i)) => result.integer.insert(i),
            NemoFunctor::Const(IdentConstant::StrConst(s)) => {
                result.string.insert(s);
            }
            NemoFunctor::Const(IdentConstant::IriConst(iri)) => {
                result.iri.insert(iri);
            }
            NemoFunctor::Nested(_) => unimplemented!(),
        }

        result
    }
}

prod_lattice! {
    pub struct WildcardType {
        wildcard: bool,
        flat_type: FlatType,
    }
}

impl From<FlatType> for WildcardType {
    fn from(value: FlatType) -> Self {
        Self {
            wildcard: false,
            flat_type: value,
        }
    }
}

impl TypeLike for WildcardType {
    fn contains(&self, val: &NemoFunctor) -> bool {
        self.flat_type.contains(val)
    }

    fn from_constant(val: NemoFunctor) -> Self {
        Self {
            wildcard: false,
            flat_type: FlatType::from_constant(val),
        }
    }
}

impl WildcardType {
    pub fn wildcard() -> Self {
        WildcardType {
            wildcard: true,
            flat_type: FlatType::bot(),
        }
    }
}
