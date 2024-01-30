use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use crate::lattice::{Bottom, Meet, PreOrder};

use super::{BodyTerm, Uncons};

#[derive(Debug, Clone)]
pub(super) struct Tup<T>(Box<[T]>);

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

impl<T: Meet + Bottom + Clone> Tup<T> {
    pub(super) fn unify<F>(mut self, other: &Self, positions: &[BodyTerm<F>]) -> Option<Self>
    where
        T: Uncons<F>,
    {
        self.unify_with(other, positions).then_some(self)
    }

    pub(super) fn unify_with<F>(&mut self, other: &Self, positions: &[BodyTerm<F>]) -> bool
    where
        T: Uncons<F>,
    {
        debug_assert_eq!(other.len(), positions.len());
        let mut stack: Vec<_> = positions
            .iter()
            .zip(other.0.iter().map(Cow::Borrowed))
            .collect();

        while let Some((pos, t)) = stack.pop() {
            match pos {
                BodyTerm::Var(v) => self[*v as usize].meet_with(&t),
                BodyTerm::Functor { functor, subterms } => {
                    let Some(sub) = t.uncons(functor) else {
                        return false;
                    };

                    stack.extend(subterms.iter().zip(sub.into_iter().map(Cow::Owned)))
                }
                BodyTerm::DontCare => {}
            }
        }

        true
    }
}
