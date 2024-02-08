use std::ops::Deref;

pub trait PreOrder {
    fn leq(&self, other: &Self) -> bool;
}

pub trait Join: PreOrder {
    fn join(&self, other: &Self) -> Self;
}

pub trait Meet: PreOrder {
    fn meet_with(&mut self, other: &Self);
}

pub trait Bottom: PreOrder + Sized {
    fn bot() -> Self;
}

pub trait Top: PreOrder + Sized {
    fn top() -> Self;
}

pub(crate) trait JoinSemiLattice: Sized + Join + Bottom {
    fn join_opt(lhs: Option<&Self>, rhs: Option<&Self>) -> Self {
        match (lhs, rhs) {
            (None, None) => Self::bot(),
            (Some(x), None) => x.join(&Self::bot()),
            (None, Some(x)) => x.join(&Self::bot()),
            (Some(x), Some(y)) => x.join(y),
        }
    }
}

impl<T: Join + Bottom> JoinSemiLattice for T {}

pub trait LocalMinimum<K> {
    fn local_minimum(key: &K) -> Self;
}

impl<T: Bottom, K> LocalMinimum<K> for T {
    fn local_minimum(_: &K) -> Self {
        T::bot()
    }
}

pub trait BorrowedLubIterator<T> {
    fn borrowed_lub(self, min: T) -> T;
}

impl<T: Join, I> BorrowedLubIterator<T> for I
where
    I: Iterator,
    I::Item: Deref<Target = T>,
{
    fn borrowed_lub(self, min: T) -> T {
        self.fold(min, |a, b| a.join(&b))
    }
}

pub trait LubIterator<T> {
    fn lub(self, min: T) -> T;
}

impl<T: Join, I> LubIterator<T> for I
where
    I: Iterator<Item = T>,
{
    fn lub(self, min: T) -> T {
        self.fold(min, |a, b| a.join(&b))
    }
}

impl<T: PartialOrd> PreOrder for T {
    fn leq(&self, other: &Self) -> bool {
        self.le(other)
    }
}

impl Join for u64 {
    fn join(&self, other: &Self) -> Self {
        std::cmp::max(*self, *other)
    }
}

impl Bottom for u64 {
    fn bot() -> Self {
        0
    }
}

impl Join for u128 {
    fn join(&self, other: &Self) -> Self {
        std::cmp::max(*self, *other)
    }
}

impl Bottom for u128 {
    fn bot() -> Self {
        0
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_lub() {
        use super::BorrowedLubIterator;
        let nums = [1 as u64, 4, 2, 42, 1341, 3];
        assert_eq!(nums.iter().borrowed_lub(0), 1341);
    }
}
