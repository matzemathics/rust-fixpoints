use std::{
    borrow::Cow,
    iter::repeat_with,
    ops::{Deref, DerefMut},
};

use crate::{
    traits::{
        lattice::{Bottom, LocalMinimum, Meet, PreOrder, Top},
        structural::{Arity, Cons, InterpretBuiltin, Uncons},
    },
    util::tup::Tup,
};

use super::model::{BodyTerm, HeadTerm};

/// A table of incomparable tuples of types, i. e.
/// there are no rows r1, r2, such that for all
/// columns i, r1[i] <= r2[i].
///
/// Supported operation:
/// - meet (which is a database join) of two tables
///     according to some specification of what needs to match
///
/// - union of two tables with the same width
///
/// - new(w), which is the "top element" of all tables with width w
///
/// - empty, which is the bottom element, i. e. a table with no rows
#[derive(Debug, Clone)]
pub struct TypeTable<T> {
    rows: Vec<Tup<T>>,
}

impl<K, T> LocalMinimum<K> for TypeTable<T> {
    fn local_minimum(key: &K) -> Self {
        TypeTable { rows: Vec::new() }
    }
}

impl<T: PreOrder> PreOrder for TypeTable<T> {
    fn leq(&self, other: &Self) -> bool {
        self.rows
            .iter()
            .all(|row| other.rows.iter().any(|other_row| row.leq(other_row)))
    }
}

impl<T> TypeTable<T> {
    pub(super) fn new(width: u16) -> TypeTable<T>
    where
        T: Top,
    {
        Self {
            rows: vec![repeat_with(T::top).take(width as usize).collect()],
        }
    }
}

impl<T> TypeTable<T> {
    pub fn empty() -> Self {
        Self { rows: vec![] }
    }
}

impl<T: Clone> TypeTable<T> {
    fn add_row(&mut self, new_row: Tup<T>)
    where
        T: PreOrder,
    {
        for row in &mut self.rows {
            if new_row.leq(&row) {
                return;
            }

            if row.leq(&new_row) {
                *row = new_row;
                return;
            }
        }

        // new_row is incomparable to all current rows
        self.rows.push(new_row)
    }

    // updates self = self union other
    pub(super) fn union_with(&mut self, other: Self)
    where
        T: PreOrder,
    {
        if self.rows.len() == 0 {
            *self = other.clone();
            return;
        }

        for row in other.rows {
            self.add_row(row)
        }
    }
}

impl<T: Clone> TypeTable<T> {
    // computes the meet of self with other
    // other[i] is unified with positions[i](self)
    // this might increase the number of rows quadratically
    pub(super) fn meet<F>(&self, other: &Self, positions: &[BodyTerm<F>]) -> Self
    where
        T: Uncons<F> + Meet,
    {
        let mut result = Self {
            rows: Vec::default(),
        };

        for row in &self.rows {
            for other_row in &other.rows {
                if let Some(new_row) = row.clone().unify(other_row, positions) {
                    result.add_row(new_row)
                }
            }
        }

        result
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
            HeadTerm::NemoCtor(c, subterms) => {
                let subterms = subterms
                    .iter()
                    .map(|term| self.interpret_head_term(config, term))
                    .collect::<Option<_>>()?;
                T::cons(config, c.clone(), subterms)
            }
        }
    }

    fn interpret_head_atom<C: Clone>(
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

    fn interpret_body_atom<F>(&self, config: &T::Config, shape: &[BodyTerm<F>]) -> Option<Self>
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

impl<T> TypeTable<T> {
    pub(super) fn apply_ctor<Ctor>(
        self,
        config: &T::Config,
        shape: &[HeadTerm<Ctor>],
    ) -> TypeTable<T>
    where
        Ctor: Clone,
        T: Cons<Ctor> + Clone,
    {
        let rows = self
            .rows
            .into_iter()
            .filter_map(|row| Tup::interpret_head_atom(config, &row, shape))
            .collect();

        TypeTable { rows }
    }
}

impl<T> TypeTable<T> {
    pub(super) fn apply_builtin<F, Builtin>(
        &mut self,
        config: &T::Config,
        builtin: &Builtin,
        shape: &[BodyTerm<F>],
    ) where
        F: Clone,
        Builtin: Clone,
        T: Clone + Top + Cons<F> + Uncons<F> + InterpretBuiltin<Builtin> + Meet,
    {
        self.rows.retain_mut(|row| {
            row.interpret_body_atom(config, shape)
                .and_then(|tup| T::interpret(builtin.clone(), tup))
                .and_then(|assignment| row.unify_with(&assignment, shape).then_some(()))
                .is_some()
        })
    }
}
