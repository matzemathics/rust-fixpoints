use std::collections::HashSet;
use std::hash::Hash;

use crate::traits::lattice::{Meet, PreOrder, Top};

#[derive(Debug, Clone)]
pub struct ToppedLattice<T>(pub(super) Option<HashSet<T>>);

impl<T: Eq + Hash> PreOrder for ToppedLattice<T> {
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

// when defining bottom, only initialize sets, that are actually used!

impl<T: Eq + Hash> Top for ToppedLattice<T> {
    fn top() -> Self {
        ToppedLattice(None)
    }
}

impl<T: Hash + Eq + Clone> Meet for ToppedLattice<T> {
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

impl<T: Hash + Eq> ToppedLattice<T> {
    pub fn subsumes(&self, it: &T) -> bool {
        self.0.as_ref().map(|s| s.contains(it)).unwrap_or(true)
    }
}
