use std::borrow::Cow;

use crate::{traits::lattice::Meet, traits::structural::Uncons, util::tup::Tup};

use super::model::BodyTerm;

impl<T: Clone> Tup<T> {
    pub(super) fn unify<F>(mut self, other: &Self, positions: &[BodyTerm<F>]) -> Option<Self>
    where
        T: Meet + Uncons<F>,
    {
        self.unify_with(other, positions).then_some(self)
    }

    pub(super) fn unify_with<F>(&mut self, other: &Self, positions: &[BodyTerm<F>]) -> bool
    where
        T: Meet + Uncons<F>,
    {
        debug_assert_eq!(other.len(), positions.len());
        let mut stack: Vec<_> = positions
            .iter()
            .zip(other.iter().map(Cow::Borrowed))
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
