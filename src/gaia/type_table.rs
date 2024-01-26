use std::ops::{Deref, DerefMut};

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
    fn unify(&self, other: &Self, indices: &[u16]) -> Option<Self> {
        debug_assert_eq!(other.len(), indices.len());
        let mut result = Box::from_iter(self.iter().cloned());

        for (other_pos, &res_pos) in indices.iter().enumerate() {
            result[res_pos as usize].meet_with(&other[other_pos]);

            if result[res_pos as usize].is_bottom() {
                return None;
            }
        }

        Some(Tup(result))
    }
}

/// Stores types that can occur at any given variable
/// position
struct TypeTable<T> {
    width: u16,
    rows: Vec<Tup<T>>,
}

impl<T: Meet + Bottom + Clone> TypeTable<T> {
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

    // computes the meet of self with other
    // other[i] is unified with self[positions[i]]
    // this might increase the number of rows quadratically
    fn meet(&self, other: &Self, positions: &[u16]) -> Self {
        debug_assert_eq!(other.width as usize, positions.len());
        let mut result = Self {
            width: self.width,
            rows: Vec::default(),
        };

        for row in &self.rows {
            for other_row in &other.rows {
                // TODO: eliminate comparable elements
                if let Some(new_row) = row.unify(other_row, positions) {
                    result.add_row(new_row)
                }
            }
        }

        result
    }

    // updates self = self union other
    fn union_with(&mut self, other: &Self) {
        assert_eq!(self.width, other.width);
        for row in &other.rows {
            self.add_row_borrowed(row)
        }
    }
}
