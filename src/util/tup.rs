use std::ops::{Deref, DerefMut};

use crate::traits::lattice::PreOrder;

#[derive(Debug, Clone)]
pub struct Tup<T>(Box<[T]>);

impl<T> Deref for Tup<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Tup<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: PreOrder> PreOrder for Tup<T> {
    fn leq(&self, other: &Self) -> bool {
        self.iter().zip(other.iter()).all(|(a, b)| a.leq(b))
    }
}

impl<T> FromIterator<T> for Tup<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Tup(iter.into_iter().collect())
    }
}

impl<Elem: Clone, T: AsRef<[Elem]>> From<T> for Tup<Elem> {
    fn from(value: T) -> Self {
        Tup(value.as_ref().into())
    }
}
