use std::{fmt::Debug, sync::Arc};

use super::const_model::{MapKey, TermTag};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

struct StdTypeBitmap(u16);

impl Debug for StdTypeBitmap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_vec().fmt(f)
    }
}

impl StdTypeBitmap {
    fn set(mut self, which: StdType) -> Self {
        self.0 |= 1u16 << (which.discriminant() as u16);
        self
    }

    fn as_vec(&self) -> Vec<StdType> {
        BitPositionVec::from(self.0)
            .0
            .into_iter()
            .filter_map(|i| StdType::try_from_discriminant(i as u8).ok())
            .collect::<Vec<_>>()
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

pub struct FlatType {
    std_types: StdTypeBitmap,
}
