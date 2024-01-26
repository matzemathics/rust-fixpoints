use core::num;
use std::env::var;
use std::fmt::Debug;
use std::iter::repeat_with;
use std::sync::Arc;

use crate::bitmap::Bitmap;
use crate::fixed_vec::FixedVec;
use crate::lattice::{Join, JoinSemiLattice, LocalMinimum, PreOrder};

use super::abstract_substitution::AbstractSubstitution;
use super::{IntConst, MapKey, NemoFunctor, NullGenerator, RdfConst, StrConst, TermTag};

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExtType {
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
}

impl ExtType {
    fn discriminant(&self) -> u8 {
        unsafe { *<*const _>::from(self).cast() }
    }

    fn try_from_discriminant(d: u8) -> Result<Self, ()> {
        match d {
            0 => Ok(ExtType::Empty),
            1 => Ok(ExtType::Pair),
            2 => Ok(ExtType::Triple),
            3 => Ok(ExtType::OtherList),
            4 => Ok(ExtType::TaggedList),
            5 => Ok(ExtType::Map),
            6 => Ok(ExtType::Null),
            7 => Ok(ExtType::Double),
            8 => Ok(ExtType::String),
            9 => Ok(ExtType::OtherRdf),
            10 => Ok(ExtType::Zero),
            11 => Ok(ExtType::Pos),
            12 => Ok(ExtType::Neg),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct TypeDescriptor {
    pub(crate) int_constants: FixedVec<IntConst, 8>,
    pub(crate) str_constants: FixedVec<StrConst, 8>,
    pub(crate) rdf_constants: FixedVec<RdfConst, 8>,
    pub(crate) list_shapes: FixedVec<ListTagType, 8>,
    pub(crate) map_shapes: FixedVec<MapTagType, 8>,
    pub(crate) null_gens: FixedVec<NullGenerator, 8>,
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

fn any() -> Bitmap {
    Bitmap::from(0xff_ff_ff_ff_ff_ff_ff_ff)
}

#[derive(Debug)]
struct IndexVec(Vec<usize>);

macro_rules! impl_index_vec_from {
    ($ty:tt) => {
        impl From<$ty> for IndexVec {
            fn from(input: $ty) -> Self {
                let mut res = Vec::new();

                for i in 0..std::mem::size_of::<$ty>() * 8 {
                    if (input as usize & (1 << i)) != 0 {
                        res.push(i)
                    }
                }

                IndexVec(res)
            }
        }
    };
}

impl_index_vec_from!(u8);
impl_index_vec_from!(u16);

impl Debug for BitPartition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ext_types = IndexVec::from(self.ext_types)
            .0
            .into_iter()
            .filter_map(|i| ExtType::try_from_discriminant(i as u8).ok())
            .collect::<Vec<_>>();

        f.debug_struct("BitPartition")
            .field("ext_types", &ext_types)
            .field("list_functors", &IndexVec::from(self.list_functors))
            .field("map_functors", &IndexVec::from(self.map_functors))
            .field("null_generators", &IndexVec::from(self.null_generators))
            .field("rdf_constants", &IndexVec::from(self.rdf_constants))
            .field("int_constants", &IndexVec::from(self.int_constants))
            .field("str_constants", &IndexVec::from(self.str_constants))
            .finish()
    }
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
        self.ext_types |= 1u16 << (which.discriminant() as u16);
        self
    }
}

impl From<BitPartition> for Bitmap {
    fn from(value: BitPartition) -> Self {
        Bitmap::from(unsafe { std::mem::transmute::<_, u64>(value) })
    }
}

impl From<Bitmap> for BitPartition {
    fn from(value: Bitmap) -> Self {
        unsafe { std::mem::transmute::<u64, BitPartition>(u64::from(value)) }
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
}

impl_const_idx_flag!(StrConstIdx, str_constants);
impl_const_idx_flag!(IntConstIdx, int_constants);
impl_const_idx_flag!(RdfConstIdx, rdf_constants);
impl_const_idx_flag!(NullIdx, null_generators);
impl_const_idx_flag!(ListTagIdx, list_functors);
impl_const_idx_flag!(MapTagIdx, map_functors);

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
                None => BitPartition::zeroed().set(ExtType::Null),
            },
            NemoFunctor::Map { tag, keys } => {
                match self.map_shapes.iter().find_index(&&MapTagType {
                    tag: tag.clone(),
                    keys: keys.clone(),
                }) {
                    Some(i) => BitPartition::zeroed().set(MapTagIdx(i as u8)),
                    None => BitPartition::zeroed().set(ExtType::Map),
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
                    None => BitPartition::zeroed().set(ExtType::TaggedList),
                }
            }
            NemoFunctor::List { tag: None, length } => match *length {
                0 => BitPartition::zeroed().set(ExtType::Empty),
                2 => BitPartition::zeroed().set(ExtType::Pair),
                3 => BitPartition::zeroed().set(ExtType::Triple),
                _ => BitPartition::zeroed().set(ExtType::OtherList),
            },
        };

        Bitmap::from(partition)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListTagType {
    tag: TermTag,
    length: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapTagType {
    tag: Option<TermTag>,
    keys: Arc<[MapKey]>,
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct BitSubstitution {
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
        assert_eq!(self.variables.len(), other.variables.len());
        assert_eq!(self.descriptor, other.descriptor);

        let variables = self
            .variables
            .iter()
            .zip(&other.variables)
            .map(|(l, r)| l.join(r))
            .collect();

        let descriptor = self.descriptor.clone();

        Self {
            descriptor,
            variables,
        }
    }
}

impl<T: AsRef<Self>> LocalMinimum<T> for BitSubstitution {
    fn local_minimum(t: &T) -> Self {
        let descriptor = t.as_ref().descriptor.clone();
        let mut variables = Vec::with_capacity(t.as_ref().variables.len());
        variables.resize_with(t.as_ref().variables.len(), Bitmap::zeroed);

        Self {
            descriptor,
            variables,
        }
    }
}

impl<P> AbstractSubstitution<NemoFunctor, P> for BitSubstitution {
    fn apply_eq(&mut self, lhs: super::Variable, rhs: super::Variable) {
        assert!((lhs as usize) < self.variables.len());
        assert!((rhs as usize) < self.variables.len());

        if lhs == rhs {
            return;
        }

        {
            // SAFETY: lhs and rhs don't overlap (checked above)
            // and are valid indices
            let lhs = &mut unsafe { *self.variables.as_mut_ptr().add(rhs as usize) };
            let rhs = &mut unsafe { *self.variables.as_mut_ptr().add(rhs as usize) };

            lhs.meet_with(rhs);
            *rhs = *lhs;
        }

        if self.variables[lhs as usize].is_zeroed() {
            for v in &mut self.variables {
                *v = Bitmap::zeroed()
            }
        }
    }

    fn apply_func(&mut self, lhs: super::Variable, func: &NemoFunctor, _rhs: &[super::Variable]) {
        self.variables[lhs as usize].meet_with(&self.descriptor.abstraction(func));

        if self.variables[lhs as usize].is_zeroed() {
            for v in &mut self.variables {
                *v = Bitmap::zeroed()
            }
        }
    }

    fn extended(&self, num_vars: u16) -> Self {
        assert!((num_vars as usize) >= self.variables.len());

        let mut variables = Vec::with_capacity(num_vars as usize);
        variables.extend_from_slice(&self.variables);
        variables.resize_with(num_vars as usize, any);

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

impl Debug for BitSubstitution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vars: Vec<_> = self
            .variables
            .iter()
            .map(|v| BitPartition::from(*v))
            .collect();

        f.debug_struct("BitSubstitution")
            // .field("descriptor", &self.descriptor)
            .field("variables", &vars)
            .finish()
    }
}

impl BitSubstitution {
    pub fn any(num_variables: u16, descriptor: Arc<TypeDescriptor>) -> Self {
        let variables = repeat_with(any).take(num_variables as usize).collect();
        Self {
            descriptor,
            variables,
        }
    }
}
