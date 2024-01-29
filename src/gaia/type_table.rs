use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
};

use crate::lattice::{Bottom, Meet, PreOrder};

#[derive(Debug, Clone)]
struct Tup<T>(Box<[T]>);

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

impl<T: Meet + Bottom + Clone> Tup<T> {
    fn unify<F>(mut self, other: &Self, positions: &[Position<F>]) -> Option<Self>
    where
        T: Structural<F>,
    {
        debug_assert_eq!(other.len(), positions.len());
        let mut stack: Vec<_> = positions
            .iter()
            .zip(other.0.iter().map(Cow::Borrowed))
            .collect();

        while let Some((pos, t)) = stack.pop() {
            match pos {
                Position::Var(v) => self[*v as usize].meet_with(&t),
                Position::Func { functor, subterms } => {
                    let Some(sub) = t.uncons(functor) else {
                        return None;
                    };

                    stack.extend(subterms.iter().zip(sub.into_iter().map(Cow::Owned)))
                }
                Position::DontCare => {}
            }
        }

        Some(self)
    }
}

/// Stores types that can occur at any given variable
/// position
struct TypeTable<T> {
    width: u16,
    rows: Vec<Tup<T>>,
}

enum Position<F> {
    Var(u16),
    Func {
        functor: F,
        subterms: Vec<Position<F>>,
    },
    DontCare,
}

trait Structural<F>: Sized {
    fn cons(func: F, args: impl Iterator<Item = Self>) -> Self;
    fn uncons(&self, func: &F) -> Option<Vec<Self>>;
    fn is_principal(&self, func: &F) -> bool;
}

impl<T: Clone + PreOrder> TypeTable<T> {
    fn add_row(&mut self, new_row: Tup<T>) {
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

    fn add_row_borrowed(&mut self, new_row: &Tup<T>) {
        for row in &mut self.rows {
            if new_row.leq(&row) {
                return;
            }

            if row.leq(new_row) {
                *row = new_row.clone();
                return;
            }
        }

        // new_row is incomparable to all current rows
        self.rows.push(new_row.clone())
    }

    // updates self = self union other
    fn union_with(&mut self, other: &Self) {
        assert_eq!(self.width, other.width);
        for row in &other.rows {
            self.add_row_borrowed(row)
        }
    }
}

impl<T: Bottom + Clone + Meet> TypeTable<T> {
    // computes the meet of self with other
    // other[i] is unified with positions[i](self)
    // this might increase the number of rows quadratically
    fn meet<F>(&self, other: &Self, positions: &[Position<F>]) -> Self
    where
        T: Structural<F>,
    {
        debug_assert_eq!(other.width as usize, positions.len());
        let mut result = Self {
            width: self.width,
            rows: Vec::default(),
        };

        for row in &self.rows {
            for other_row in &other.rows {
                // TODO: eliminate comparable elements
                if let Some(new_row) = row.clone().unify(other_row, positions) {
                    result.add_row(new_row)
                }
            }
        }

        result
    }
}
