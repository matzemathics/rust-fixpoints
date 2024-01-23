use core::num;
use std::sync::Arc;

use crate::bitmap::Bitmap;
use crate::fixed_vec::FixedVec;
use crate::lattice::{Join, LocalMinimum, PreOrder};

use super::abstract_substitution::AbstractSubstitution;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExtType {
    Empty = 0,     // term `()`,
    Pair = 1,      // terms with form `(x, y)`
    Triple = 2,    // terms with form `(x, y, z)`
    OtherList = 3, // terms with form `(x_n, ...)` and none of the above
    Map = 4,       // terms with form `{k_n = x_n, ...}`
    Double = 5,    // terms of type double
    String = 6,    // terms of type string
    OtherRdf = 7,  // other rdf terms
    Zero = 8,      // term `0`
    Pos = 9,       // term of type int > 0
    Neg = 10,      // term of type int < 0
}

impl ExtType {
    fn discriminant(&self) -> u8 {
        unsafe { *<*const _>::from(&self).cast() }
    }
}

type IntConst = i64;
type StrConst = Arc<str>;
type RdfConst = Arc<str>;
type NullGenerator = usize;
type MapKey = Arc<str>;
type TermTag = Arc<str>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeDescriptor {
    int_constants: FixedVec<IntConst, 8>,
    str_constants: FixedVec<StrConst, 8>,
    rdf_constants: FixedVec<RdfConst, 8>,
    list_shapes: FixedVec<ListTagType, 7>,
    map_shapes: FixedVec<MapTagType, 7>,
    null_gens: FixedVec<NullGenerator, 7>,
}

#[repr(C, align(8))]
struct BitPartition {
    ext_types: u16,
    list_functors: u8,
    map_functors: u8,
    null_generators: u8,
    rdf_constants: u8,
    int_constants: u8,
    str_constants: u8,
}

impl BitPartition {
    fn zeroed() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

trait Flag<T> {
    fn set(self, which: T) -> Self;
}

impl Flag<ExtType> for BitPartition {
    fn set(mut self, which: ExtType) -> Self {
        self.ext_types |= 1 << which.discriminant();
        self
    }
}

impl From<BitPartition> for Bitmap {
    fn from(value: BitPartition) -> Self {
        Bitmap::from(unsafe { std::mem::transmute::<_, u64>(value) })
    }
}

macro_rules! impl_const_idx_flag {
    ($ty_name:ident, $fld_name:ident) => {
        struct $ty_name(u8);

        impl Flag<$ty_name> for BitPartition {
            fn set(mut self, which: $ty_name) -> Self {
                self.$fld_name |= 1 << which.0;
                self
            }
        }
    };

    ($ty_name:ident, $fld_name:ident, other) => {
        struct $ty_name(u8);

        impl $ty_name {
            fn other() -> Self {
                return Self(1 << 7);
            }
        }

        impl Flag<$ty_name> for BitPartition {
            fn set(mut self, which: $ty_name) -> Self {
                self.$fld_name |= 1 << which.0;
                self
            }
        }
    };
}

impl_const_idx_flag!(StrConstIdx, str_constants);
impl_const_idx_flag!(IntConstIdx, int_constants);
impl_const_idx_flag!(RdfConstIdx, rdf_constants);
impl_const_idx_flag!(NullIdx, null_generators, other);
impl_const_idx_flag!(ListTagIdx, list_functors, other);
impl_const_idx_flag!(MapTagIdx, map_functors, other);

trait FindIndex<T> {
    fn find_index(self, constant: &T) -> Option<usize>;
}

impl<T: Eq, I: Iterator<Item = T>> FindIndex<T> for I {
    fn find_index(self, constant: &T) -> Option<usize> {
        self.enumerate()
            .find_map(|(i, c)| (&c == constant).then_some(i))
    }
}

impl TypeDescriptor {
    fn abstraction(&self, f: &NemoFunctor) -> Bitmap {
        let partition = match f {
            NemoFunctor::Double => BitPartition::zeroed().set(ExtType::Double),
            NemoFunctor::StrConst(str_constant) => {
                match self.str_constants.iter().find_index(&str_constant) {
                    Some(i) => BitPartition::zeroed().set(StrConstIdx(i as u8)),
                    None => BitPartition::zeroed().set(ExtType::String),
                }
            }
            NemoFunctor::IntConst(int_const) => {
                match self.int_constants.iter().find_index(&int_const) {
                    Some(i) => BitPartition::zeroed().set(IntConstIdx(i as u8)),
                    None => match *int_const {
                        0 => BitPartition::zeroed().set(ExtType::Zero),
                        c if c > 0 => BitPartition::zeroed().set(ExtType::Pos),
                        _ => BitPartition::zeroed().set(ExtType::Neg),
                    },
                }
            }
            NemoFunctor::RdfConst(rdf_const) => {
                match self.rdf_constants.iter().find_index(&rdf_const) {
                    Some(i) => BitPartition::zeroed().set(RdfConstIdx(i as u8)),
                    None => BitPartition::zeroed().set(ExtType::OtherRdf),
                }
            }
            NemoFunctor::Null(gen) => match self.null_gens.iter().find_index(&gen) {
                Some(i) => BitPartition::zeroed().set(NullIdx(i as u8)),
                None => BitPartition::zeroed().set(NullIdx::other()),
            },
            NemoFunctor::Map { tag, keys } => {
                match self.map_shapes.iter().find_index(&&MapTagType {
                    tag: tag.clone(),
                    keys: keys.clone(),
                }) {
                    Some(i) => BitPartition::zeroed().set(MapTagIdx(i as u8)),
                    None => BitPartition::zeroed().set(MapTagIdx::other()),
                }
            }
            NemoFunctor::List {
                tag: Some(tag),
                length,
            } => {
                match self.list_shapes.iter().find_index(&&ListTagType {
                    tag: tag.clone(),
                    length: *length as u8,
                }) {
                    Some(i) => BitPartition::zeroed().set(ListTagIdx(i as u8)),
                    None => BitPartition::zeroed().set(ListTagIdx::other()),
                }
            }
            NemoFunctor::List { tag: None, length } => match *length {
                0 => BitPartition::zeroed().set(ExtType::Zero),
                2 => BitPartition::zeroed().set(ExtType::Pair),
                3 => BitPartition::zeroed().set(ExtType::Triple),
                _ => BitPartition::zeroed().set(ExtType::OtherList),
            },
        };

        Bitmap::from(partition)
    }

    fn abstract_input(&self) -> Bitmap {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ListTagType {
    tag: TermTag,
    length: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MapTagType {
    tag: Option<TermTag>,
    keys: Arc<[MapKey]>,
}

struct BitSubstitution {
    descriptor: Arc<TypeDescriptor>,
    variables: Vec<Bitmap>,
}

impl PreOrder for BitSubstitution {
    fn leq(&self, other: &Self) -> bool {
        debug_assert_eq!(self.descriptor, other.descriptor);

        if self.variables.len() != other.variables.len() {
            return false;
        }

        self.variables
            .iter()
            .zip(&other.variables)
            .all(|(left, right)| left.leq(right))
    }
}

impl Join for BitSubstitution {
    fn join(&self, other: &Self) -> Self {
        todo!()
    }
}

impl<T: AsRef<Self>> LocalMinimum<T> for BitSubstitution {
    fn local_minimum(t: &T) -> Self {
        todo!()
    }
}

enum NemoFunctor {
    Double,
    StrConst(StrConst),
    IntConst(IntConst),
    RdfConst(RdfConst),
    Null(NullGenerator),
    Map {
        tag: Option<TermTag>,
        keys: Arc<[MapKey]>,
    },
    List {
        tag: Option<TermTag>,
        length: usize,
    },
}

impl<P> AbstractSubstitution<NemoFunctor, P> for BitSubstitution {
    fn apply_eq(&mut self, lhs: super::Variable, rhs: super::Variable) {
        assert!((lhs as usize) < self.variables.len());
        assert!((rhs as usize) < self.variables.len());

        if lhs == rhs {
            return;
        }

        let lhs = &mut unsafe { *self.variables.as_mut_ptr().add(rhs as usize) };
        let rhs = &mut unsafe { *self.variables.as_mut_ptr().add(rhs as usize) };

        lhs.meet_with(rhs);
        *rhs = *lhs;
    }

    fn apply_func(&mut self, lhs: super::Variable, func: &NemoFunctor, _rhs: &[super::Variable]) {
        self.variables[lhs as usize].meet_with(&self.descriptor.abstraction(func))
    }

    fn extended(&self, num_vars: u16) -> Self {
        assert!((num_vars as usize) >= self.variables.len());

        let mut variables = Vec::with_capacity(num_vars as usize);
        variables.extend_from_slice(&self.variables);
        variables.resize_with(num_vars as usize, Bitmap::zeroed);

        Self {
            descriptor: self.descriptor.clone(),
            variables,
        }
    }

    fn restrict(&mut self, num_vars: u16) {
        assert!((num_vars as usize) <= self.variables.len());

        self.variables
            .resize_with(num_vars as usize, || unreachable!())
    }

    fn project(&self, vars: impl Iterator<Item = super::Variable>) -> Self {
        let variables = vars.map(|v| self.variables[v as usize]).collect();

        Self {
            variables,
            descriptor: self.descriptor.clone(),
        }
    }

    fn propagate(&mut self, vars: impl Iterator<Item = super::Variable>, update: Self) {
        for (index, var) in vars.enumerate() {
            self.variables[var as usize].meet_with(&update.variables[index]);
        }
    }

    fn len(&self) -> u16 {
        self.variables.len() as u16
    }
}
