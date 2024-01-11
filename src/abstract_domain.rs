use std::vec;

use crate::lattice::JoinSemiLattice;

pub trait Shape: Sized {
    /// Number of subterms ("positions") of this shape.
    fn arity(&self) -> u8;

    /// Returns true, iff all positions relevant to `self` are also specified in `other`.
    fn projection_of(&self, other: &Self) -> bool;

    /// `self[i]` refers to the same position as `other[map(i)]`
    ///
    /// returns `None`, if other does not contain a term for `self[i]`
    ///
    /// NOTE: must uphold `map(i) <= i` (i.e. no reordering of positions)
    fn map_index(&self, index: u8, other: &Self) -> Option<u8>;

    fn meet(&self, other: &Self) -> Option<Self>;
}

impl<T: PartialOrd, S: Shape> PartialOrd for Shaped<S, T> {
    fn le(&self, other: &Self) -> bool {
        if self.shape.arity() < other.arity() {
            return false;
        }

        if !other.shape.projection_of(&self.shape) {
            return false;
        }

        self.subterms
            .iter()
            .enumerate()
            .filter_map(|(i, t)| {
                self.shape
                    .map_index(i as u8, other)
                    .map(|i| (t, &other.subterms[i as usize]))
            })
            .all(|(t1, t2)| PartialOrd::le(t1, t2))
    }

    fn ge(&self, other: &Self) -> bool {
        other.le(self)
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        todo!("do we need this?")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Shaped<S, T> {
    shape: S,
    subterms: Vec<T>,
}

impl<S: Shape, T> Shaped<S, T> {
    fn project(self, shape: S) -> Result<Self, Self> {
        if !shape.projection_of(&self.shape) {
            return Err(self);
        }

        let subterms = Vec::with_capacity(shape.arity());

        for (index, term) in self.subterms.into_iter().enumerate() {
            if let Some(index) = self.shape.map_index(index, &shape) {
                debug_assert_eq!(index, subterms.len());
                subterms.push(term)
            }
        }

        Ok(Self { shape, subterms })
    }

    fn subterms(&self) -> &[T] {
        &self.subterms
    }

    fn subterms_mut(&self) -> &mut [T] {
        &mut self.subterms
    }

    fn shape(&self) -> &S {
        &self.shape
    }

    fn into_reordered(self, other: &S) -> Option<Self> {
        other.project_reorder(self)
    }

    fn unify(self, other: Self, unifier: impl FnMut(T, T) -> T) -> Option<Shaped<S, T>> {
        let shape = self.shape.meet(&other.shape)?;
        let mut subterms = Niterator::from_shaped(self, other, unifier).collect();
        Some(Shaped { shape, subterms })
    }
}

struct Niterator<T, S, U> {
    left: (vec::IntoIter<T>, S),
    right: (vec::IntoIter<T>, S),
    shape: S,
    unifier: U,
    index: usize,
}

impl<T, S, U> Iterator for Niterator<T, S, U>
where
    S: Shape,
    U: FnMut(T, T) -> T,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.shape.arity() {
            return None;
        }

        let result = match (
            self.shape.map_index(self.index, &self.left.1),
            self.shape.map_index(self.index, &self.right.1),
        ) {
            (None, None) => panic!("meet() does not introduce any new positions"),
            (None, Some(_)) => self
                .right
                .0
                .next()
                .expect("a subterm exists for all specified positions"),
            (Some(_), None) => self
                .left
                .0
                .next()
                .expect("a subterm exists for all specified positions"),
            (Some(_), Some(_)) => {
                let left = self
                    .left
                    .0
                    .next()
                    .expect("a subterm exists for all specified positions");
                let right = self
                    .right
                    .0
                    .next()
                    .expect("a subterm exists for all specified positions");
                self.unifier(left, right)
            }
        };

        self.index += 1;
        Some(result)
    }
}

impl<T, S, U> Niterator<T, S, U>
where
    S: Shape,
{
    fn from_shaped(left: Shaped<S, T>, right: Shaped<S, T>, unifier: U) -> Self {
        let shape = left.shape.meet(&right.shape).unwrap();
        let left = (left.subterms.into_iter(), left.shape);
        let right = (right.subterms.into_iter(), right.shape);
        Self {
            left,
            right,
            shape,
            unifier,
            index: 0,
        }
    }
}

pub trait AbstractDomain: JoinSemiLattice {
    type Shape: Clone + Shape;

    fn any() -> Self;

    fn shape(&self) -> &Self::Shape;
    fn subterm(&self, index: u8) -> Option<&Self>;

    fn unify_with(&mut self, other: Self);
    fn unify_nested(&mut self, functor: &Self::Shape, subterms: &[Option<&Self>]);

    fn unify_replace_both(&mut self, other: &mut Self) {
        let right = std::mem::replace(other, Self::bot());
        self.unify_with(right);
        *other = self.join(&Self::bot());
    }
}
