use std::sync::Arc;

use crate::lattice::{JoinSemiLattice, PreOrder};

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

type IntConst = i64;
type StrConst = String;
type RdfConst = String;
type NullGenerator = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeDescriptor {
    int_constants: [IntConst; 8],
    str_constants: [StrConst; 8],
    rdf_constants: [RdfConst; 8],
    list_shapes: Vec<ListTagType>,
    map_shapes: Vec<MapTagType>,
    null_gens: Vec<NullGenerator>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ListTagType {
    tag: Arc<str>,
    arity: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MapTagType {
    tag: Arc<str>,
    keys: Arc<[Arc<str>]>,
}

struct BitSubstitution {
    descriptor: Arc<TypeDescriptor>,
    variables: Vec<crate::bitmap::Bitmap>,
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

impl JoinSemiLattice for BitSubstitution {
    fn bot() -> Self {
        todo!()
    }

    fn join(&self, other: &Self) -> Self {
        todo!()
    }
}

enum NemoFunctor {
    StrConst(StrConst),
    IntConst(IntConst),
}

impl AbstractSubstitution<NemoFunctor> for BitSubstitution {
    fn apply_eq(&mut self, lhs: super::Variable, rhs: super::Variable) {
        todo!()
    }

    fn apply_func(&mut self, lhs: super::Variable, func: &F, rhs: &[super::Variable]) {
        todo!()
    }

    fn extended(&self, num_vars: u16) -> Self {
        todo!()
    }

    fn restrict(&mut self, num_vars: u16) {
        todo!()
    }

    fn project(&self, vars: &[super::Variable]) -> Self {
        todo!()
    }

    fn propagate(&mut self, vars: &[super::Variable], update: Self) {
        todo!()
    }

    fn len(&self) -> u16 {
        todo!()
    }
}
