use std::cmp::Ordering;

pub trait PreOrder {
    fn leq(&self, other: &Self) -> bool;
}

pub trait Meet: PreOrder {
    fn meet_with(&mut self, other: &Self);
}

pub trait Union: PreOrder {
    fn union_with(&mut self, other: &Self);
}

pub trait Bottom: PreOrder + Sized {
    fn bot() -> Self;
}

pub trait Top: PreOrder + Sized {
    fn top() -> Self;
}

pub trait LocalMinimum<K> {
    fn local_minimum(key: &K) -> Self;
}

impl<T: Bottom, K> LocalMinimum<K> for T {
    fn local_minimum(_: &K) -> Self {
        T::bot()
    }
}

impl<T: PartialOrd> PreOrder for T {
    fn leq(&self, other: &Self) -> bool {
        self.le(other)
    }
}

impl Bottom for u64 {
    fn bot() -> Self {
        0
    }
}

impl Meet for bool {
    fn meet_with(&mut self, other: &Self) {
        *self &= *other;
    }
}

impl Union for bool {
    fn union_with(&mut self, other: &Self) {
        *self |= *other;
    }
}

impl Top for bool {
    fn top() -> Self {
        true
    }
}

impl Bottom for bool {
    fn bot() -> Self {
        false
    }
}

#[derive(Debug)]
pub struct ThreeWayCompare(Ordering);

impl ThreeWayCompare {
    pub fn init() -> Self {
        ThreeWayCompare(Ordering::Equal)
    }

    pub fn chain<T: PartialOrd>(self, left: &T, right: &T) -> Option<Self> {
        match self.0 {
            Ordering::Less => left.le(right).then_some(Self(Ordering::Less)),
            Ordering::Equal => left.partial_cmp(right).map(Self),
            Ordering::Greater => left.ge(right).then_some(Self(Ordering::Greater)),
        }
    }

    pub fn finish(self) -> Ordering {
        self.0
    }
}
