use std::collections::HashSet;
use std::hash::Hash;

use crate::traits::lattice::{Bottom, Meet, PreOrder, Top, Union};

#[derive(Debug, Clone)]
pub struct ToppedLattice<T>(pub(super) Option<HashSet<T>>);

impl<T: Eq + Hash> PartialEq for ToppedLattice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq + Hash> Eq for ToppedLattice<T> {}

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

impl<T: Eq + Hash> Bottom for ToppedLattice<T> {
    fn bot() -> Self {
        ToppedLattice(Some(HashSet::new()))
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

impl<T: Hash + Eq + Clone> Union for ToppedLattice<T> {
    fn union_with(&mut self, other: &Self) {
        let Some(other) = &other.0 else {
            self.0 = None;
            return;
        };

        let Some(inner) = &mut self.0 else { return };
        inner.extend(other.iter().cloned())
    }
}

impl<T: Hash + Eq> ToppedLattice<T> {
    pub fn subsumes(&self, it: &T) -> bool {
        self.0.as_ref().map(|s| s.contains(it)).unwrap_or(true)
    }
}
