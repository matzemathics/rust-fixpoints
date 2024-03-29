use std::fmt::Debug;
use std::hash::Hash;
use std::{cmp::Ordering, collections::HashSet};

use crate::traits::lattice::{Bottom, Meet, Top, Union};

#[derive(Clone)]
pub struct ToppedLattice<T>(pub(super) Option<HashSet<T>>);

impl<T: Debug> Debug for ToppedLattice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Some(set) => set.fmt(f),
            None => f.write_str("*"),
        }
    }
}

impl<T: Eq + Hash> PartialEq for ToppedLattice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq + Hash> Eq for ToppedLattice<T> {}

impl<T: Eq + Hash> PartialOrd for ToppedLattice<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (&self.0, &other.0) {
            (None, None) => Some(Ordering::Equal),
            (None, Some(_)) => Some(Ordering::Greater),
            (Some(_), None) => Some(Ordering::Less),
            (Some(left), Some(right)) => {
                // a subset b <=> a \\ b == {}
                let le = left.difference(&right).next().is_none();
                let ge = right.difference(&left).next().is_none();

                match (ge, le) {
                    (true, true) => Some(Ordering::Equal),
                    (true, false) => Some(Ordering::Greater),
                    (false, true) => Some(Ordering::Less),
                    (false, false) => None,
                }
            }
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

    pub fn insert(&mut self, it: T) -> bool {
        let Some(inner) = &mut self.0 else {
            return false;
        };

        inner.insert(it)
    }
}
