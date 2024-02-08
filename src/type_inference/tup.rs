use std::borrow::Cow;

use crate::{
    traits::{
        lattice::{Meet, Top},
        structural::{Cons, Uncons},
    },
    util::tup::Tup,
};

use super::model::{BodyTerm, HeadTerm};

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

impl<T: Clone> Tup<T> {
    fn interpret_head_term<C>(&self, config: &T::Config, t: &HeadTerm<C>) -> Option<T>
    where
        C: Clone,
        T: Cons<C>,
    {
        match t {
            HeadTerm::Var(v) => Some(self[*v as usize].clone()),
            HeadTerm::Ctor(c, subterms) => {
                let subterms = subterms
                    .iter()
                    .map(|term| self.interpret_head_term(config, term))
                    .collect::<Option<_>>()?;
                T::cons(config, c.clone(), subterms)
            }
        }
    }

    pub(super) fn interpret_head_atom<C: Clone>(
        config: &T::Config,
        original: &Self,
        shape: &[HeadTerm<C>],
    ) -> Option<Self>
    where
        T: Cons<C>,
    {
        shape
            .iter()
            .map(|term| original.interpret_head_term(config, term))
            .collect()
    }

    fn interpret_body_term<F>(&self, config: &T::Config, t: &BodyTerm<F>) -> Option<T>
    where
        F: Clone,
        T: Cons<F> + Top,
    {
        match t {
            BodyTerm::Var(v) => Some(self[*v as usize].clone()),
            BodyTerm::Functor { functor, subterms } => {
                let subterms = subterms
                    .iter()
                    .map(|t| self.interpret_body_term(config, t))
                    .collect::<Option<_>>()?;
                T::cons(config, functor.clone(), subterms)
            }
            BodyTerm::DontCare => Some(T::top()),
        }
    }

    pub(super) fn interpret_body_atom<F>(
        &self,
        config: &T::Config,
        shape: &[BodyTerm<F>],
    ) -> Option<Self>
    where
        F: Clone,
        T: Cons<F> + Top,
    {
        shape
            .iter()
            .map(|t| self.interpret_body_term(config, t))
            .collect()
    }
}
