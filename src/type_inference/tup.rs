use std::{borrow::Cow, cmp::Ordering};

use crate::{
    traits::{lattice::Meet, structural::TypeDomain},
    util::tup::Tup,
};

use super::model::{BodyTerm, HeadTerm};

pub(super) enum UnionResult {
    /// self was larger than other
    Larger,
    /// self == other
    Equal,
    /// self was smaller than other
    Smaller,
    /// self and other were incomparable
    Incomparable,
}

pub(crate) enum UnificationFailure<T: TypeDomain> {
    Incomparable {
        variable: u16,
        path: Vec<usize>,
        left: T,
        right: T,
    },
    Deconstruct {
        path: Vec<usize>,
        typ: T,
        pattern: T::Functor,
    },
}

impl<T: Clone> Tup<T> {
    pub(crate) fn unify(
        mut self,
        other: &Self,
        positions: &[BodyTerm<T::Functor>],
    ) -> Result<Self, UnificationFailure<T>>
    where
        T: TypeDomain,
    {
        self.unify_with(other, positions).map(|_| self)
    }

    pub(super) fn unify_with(
        &mut self,
        other: &Self,
        positions: &[BodyTerm<T::Functor>],
    ) -> Result<(), UnificationFailure<T>>
    where
        T: TypeDomain,
    {
        debug_assert_eq!(other.len(), positions.len());
        let mut stack: Vec<_> = positions
            .iter()
            .zip(other.iter().map(Cow::Borrowed))
            .enumerate()
            .map(|(p, pair)| (vec![p], pair))
            .collect();

        while let Some((path, (pos, t))) = stack.pop() {
            match pos {
                BodyTerm::Var(v) => {
                    let lhs = &mut self[*v as usize];
                    let before = lhs.clone();
                    lhs.meet_with(&t);

                    if lhs.is_empty() {
                        return Err(UnificationFailure::Incomparable {
                            variable: *v,
                            path,
                            left: before,
                            right: t.into_owned(),
                        });
                    }
                }
                BodyTerm::Functor { functor, subterms } => {
                    let Some(sub) = t.uncons(functor) else {
                        return Err(UnificationFailure::Deconstruct {
                            path,
                            typ: t.into_owned(),
                            pattern: functor.clone(),
                        });
                    };

                    for (position, (subterm, subtype)) in subterms.iter().zip(sub).enumerate() {
                        let mut new_path = path.clone();
                        new_path.push(position);
                        stack.push((new_path, (subterm, Cow::Owned(subtype))))
                    }
                }
                BodyTerm::DontCare => {}
            }
        }

        Ok(())
    }

    pub(super) fn generalized_unify<F>(
        &mut self,
        other: &Self,
        positions: &[HeadTerm<F>],
        uncons: impl Fn(&T, &F) -> Option<Vec<T>>,
    ) -> bool
    where
        T: Meet,
    {
        debug_assert_eq!(other.len(), positions.len());
        let mut stack: Vec<_> = positions
            .iter()
            .zip(other.iter().map(Cow::Borrowed))
            .collect();

        while let Some((pos, t)) = stack.pop() {
            match pos {
                HeadTerm::Var(v) => self[*v as usize].meet_with(&t),
                HeadTerm::Ctor(functor, subterms) => {
                    let Some(sub) = uncons(&t, functor) else {
                        return false;
                    };

                    stack.extend(subterms.iter().zip(sub.into_iter().map(Cow::Owned)))
                }
            }
        }

        true
    }

    pub(super) fn try_union_with(&mut self, other: &Self) -> Option<UnionResult>
    where
        T: PartialOrd,
    {
        let Some(cmp) = self
            .iter()
            .zip(other.iter())
            .map(|(l, r)| l.partial_cmp(r))
            .collect::<Option<Box<_>>>()
        else {
            return None;
        };

        let mut result = UnionResult::Equal;

        for (index, ordering) in cmp.into_iter().enumerate() {
            if matches!(ordering, Ordering::Less) {
                self[index] = other[index].clone();

                result = match result {
                    UnionResult::Incomparable => UnionResult::Incomparable,
                    UnionResult::Larger => UnionResult::Incomparable,
                    UnionResult::Equal => UnionResult::Smaller,
                    UnionResult::Smaller => UnionResult::Smaller,
                }
            } else if matches!(ordering, Ordering::Greater) {
                result = match result {
                    UnionResult::Larger => UnionResult::Larger,
                    UnionResult::Equal => UnionResult::Larger,
                    UnionResult::Smaller => UnionResult::Incomparable,
                    UnionResult::Incomparable => UnionResult::Incomparable,
                }
            }
        }

        Some(result)
    }
}

impl<T: TypeDomain> Tup<T> {
    fn interpret_head_term(&self, config: &T::Config, t: &HeadTerm<T::Constructor>) -> Option<T> {
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

    pub(super) fn interpret_head_atom(
        config: &T::Config,
        original: &Self,
        shape: &[HeadTerm<T::Constructor>],
    ) -> Option<Self> {
        shape
            .iter()
            .map(|term| original.interpret_head_term(config, term))
            .collect()
    }

    fn interpret_body_term(
        &self,
        config: &T::Config,
        t: &BodyTerm<T::Functor>,
        dont_care: T,
    ) -> Option<T> {
        match t {
            BodyTerm::Var(v) => Some(self[*v as usize].clone()),
            BodyTerm::Functor { functor, subterms } => {
                let subterms = subterms
                    .iter()
                    .map(|t| self.interpret_body_term(config, t, dont_care.clone()))
                    .collect::<Option<_>>()?;
                T::cons(config, functor.clone().into(), subterms)
            }
            BodyTerm::DontCare => Some(dont_care),
        }
    }

    pub(super) fn interpret_body_atom(
        &self,
        config: &T::Config,
        shape: &[BodyTerm<T::Functor>],
        dont_care: T,
    ) -> Option<Self> {
        shape
            .iter()
            .map(|t| self.interpret_body_term(config, t, dont_care.clone()))
            .collect()
    }
}
