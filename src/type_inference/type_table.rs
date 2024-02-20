use std::iter::repeat_with;

use crate::{
    traits::{
        lattice::{Bottom, Meet, PreOrder, Top},
        structural::{Cons, InterpretBuiltin, Uncons},
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

impl<T: PreOrder> Bottom for TypeTable<T> {
    fn bot() -> Self {
        Self { rows: Vec::new() }
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
    fn add_row(&mut self, mut new_row: Tup<T>)
    where
        T: PartialOrd,
    {
        loop {
            let mut changed = false;
            self.rows.retain(|row| {
                if new_row.try_union_with(row) {
                    changed = true;
                    false
                } else {
                    true
                }
            });

            if !changed {
                break;
            }
        }

        self.rows.push(new_row)
    }

    // updates self = self union other
    pub(super) fn union_with(&mut self, other: Self)
    where
        T: PartialOrd,
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
        T: Uncons<F> + Meet + PartialOrd,
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
        T: Clone + Top + Cons<F> + Uncons<F> + InterpretBuiltin<Builtin> + Meet + PartialOrd,
    {
        let rows = std::mem::take(&mut self.rows);
        for mut row in rows {
            let Some(assignment) = row
                .interpret_body_atom(config, shape)
                .and_then(|tup| T::interpret(builtin.clone(), tup))
            else {
                continue;
            };

            if row.unify_with(&assignment, shape) {
                self.add_row(row)
            }
        }
    }
}
