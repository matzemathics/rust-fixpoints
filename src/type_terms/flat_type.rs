use std::sync::Arc;

use crate::traits::lattice::{Bottom, Meet, PreOrder, Top, Union};

use super::{
    const_model::{IdentConstant, NemoFunctor},
    topped_lattice::ToppedLattice,
};

macro_rules! prod_lattice {
    {
        $vis:vis struct $ty_id:ident { $($key:ident: $key_ty:ty,)* }
    } => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $ty_id {
            $($key: $key_ty,)*
        }

        impl PreOrder for $ty_id {
            fn leq(&self, other: &Self) -> bool {
                $(self.$key.leq(&other.$key))&&*
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
    pub fn contains(&self, val: &NemoFunctor) -> bool {
        match val {
            NemoFunctor::Double => self.double,
            NemoFunctor::Const(IdentConstant::IntConst(i)) => self.integer.contains(*i),
            NemoFunctor::Const(IdentConstant::StrConst(s)) => self.string.subsumes(s),
            NemoFunctor::Const(IdentConstant::IriConst(iri)) => self.iri.subsumes(iri),
            NemoFunctor::Nested(_) => unimplemented!(),
        }
    }
}
