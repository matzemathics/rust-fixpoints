use std::collections::HashSet;
use std::hash::Hash;

use crate::traits::lattice::{Bottom, Meet, PreOrder, Top};

#[derive(Debug, Clone)]
struct HashLattice<T>(Option<HashSet<T>>);

impl<T: Eq + Hash> PreOrder for HashLattice<T> {
    fn leq(&self, other: &Self) -> bool {
        match &other.0 {
            Some(other) => match &self.0 {
                Some(this) => other.difference(this).next().is_none(),
                None => false,
            },
            None => true,
        }
    }
}

impl<T: Eq + Hash> Bottom for HashLattice<T> {
    fn is_bottom(&self) -> bool {
        match &self.0 {
            Some(s) => s.is_empty(),
            None => false,
        }
    }

    fn bot() -> Self {
        HashLattice(Some(HashSet::new()))
    }
}

impl<T: Eq + Hash> Top for HashLattice<T> {
    fn is_top(&self) -> bool {
        self.0.is_none()
    }

    fn top() -> Self {
        HashLattice(None)
    }
}

impl<T: Hash + Eq + Clone> Meet for HashLattice<T> {
    fn meet_with(&mut self, other: &Self) {
        match &mut self.0 {
            Some(this) => match &other.0 {
                Some(other) => *this = this.intersection(other).cloned().collect(),
                None => {}
            },
            None => *self = other.clone(),
        }
    }
}
