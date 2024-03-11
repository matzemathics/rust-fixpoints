use std::iter::repeat_with;

use crate::{
    traits::{
        lattice::{Bottom, PreOrder, Top},
        structural::TypeDomain,
    },
    util::tup::Tup,
};

use super::{
    model::{BodyTerm, HeadTerm},
    tup::UnionResult,
};

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
    pub(super) rows: Vec<Tup<T>>,
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

impl<T: Clone> TypeTable<T> {
    pub(super) fn new(width: u16) -> TypeTable<T>
    where
        T: Top,
    {
        Self {
            rows: vec![repeat_with(T::top).take(width as usize).collect()],
        }
    }

    pub(super) fn map<R>(self, f: impl Fn(T) -> R) -> TypeTable<R> {
        TypeTable {
            rows: self
                .rows
                .into_iter()
                .map(|tup| tup.into_iter().map(|t| f(t.clone())).collect())
                .collect(),
        }
    }
}

impl<T> TypeTable<T> {
    pub fn empty() -> Self {
        Self { rows: vec![] }
    }
}

impl<T: Clone> TypeTable<T> {
    pub(super) fn add_row(&mut self, mut new_row: Tup<T>) -> bool
    where
        T: PartialOrd,
    {
        let mut increased = false;

        loop {
            let mut new_row_changed = false;
            self.rows.retain(|row| match new_row.try_union_with(row) {
                Some(union_result) => {
                    if matches!(
                        union_result,
                        UnionResult::Larger | UnionResult::Incomparable
                    ) {
                        increased = true
                    }

                    if matches!(
                        union_result,
                        UnionResult::Incomparable | UnionResult::Smaller
                    ) {
                        new_row_changed = true
                    }

                    false
                }
                None => true,
            });

            if !new_row_changed {
                break;
            }
        }

        self.rows.push(new_row);
        increased
    }

    // updates self = self union other
    pub(super) fn union_with(&mut self, other: Self) -> bool
    where
        T: PartialOrd,
    {
        if self.rows.len() == 0 {
            *self = other.clone();
            return self.rows.len() > 0;
        }

        let mut changed = false;

        for row in other.rows {
            changed |= self.add_row(row)
        }

        changed
    }
}

impl<T: Clone> TypeTable<T> {
    // computes the meet of self with other
    // other[i] is unified with positions[i](self)
    // this might increase the number of rows quadratically
    pub(super) fn meet(&self, other: &Self, positions: &[BodyTerm<T::Functor>]) -> Self
    where
        T: TypeDomain,
    {
        let mut result = Self {
            rows: Vec::default(),
        };

        for row in &self.rows {
            for other_row in &other.rows {
                if let Some(new_row) = row.clone().unify(other_row, positions) {
                    result.add_row(new_row);
                }
            }
        }

        result
    }
}

impl<T: TypeDomain> TypeTable<T> {
    pub(super) fn apply_ctor(
        self,
        config: &T::Config,
        shape: &[HeadTerm<T::Constructor>],
    ) -> TypeTable<T> {
        let rows = self
            .rows
            .into_iter()
            .filter_map(|row| Tup::interpret_head_atom(config, &row, shape))
            .collect();

        TypeTable { rows }
    }

    pub(super) fn apply_atom(
        self,
        config: &T::Config,
        shape: &[BodyTerm<T::Functor>],
        dont_care: T,
    ) -> TypeTable<T> {
        let rows = self
            .rows
            .into_iter()
            .filter_map(|row| Tup::interpret_body_atom(&row, config, shape, dont_care.clone()))
            .collect();

        TypeTable { rows }
    }
}

impl<T: TypeDomain> TypeTable<T> {
    pub(super) fn apply_builtin(
        &mut self,
        config: &T::Config,
        builtin: &T::Builtin,
        shape: &[BodyTerm<T::Functor>],
    ) {
        let rows = std::mem::take(&mut self.rows);
        for mut row in rows {
            let Some(assignment) = row
                .interpret_body_atom(config, shape, T::top())
                .and_then(|tup| T::interpret(builtin.clone(), tup))
            else {
                continue;
            };

            if row.unify_with(&assignment, shape) {
                self.add_row(row);
            }
        }
    }
}
