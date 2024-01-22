use std::ops::Deref;

pub trait PreOrder {
    fn leq(&self, other: &Self) -> bool;
}

pub trait JoinSemiLattice: Sized + PreOrder {
    fn bot() -> Self;
    fn join(&self, other: &Self) -> Self;

    fn join_opt(lhs: Option<&Self>, rhs: Option<&Self>) -> Self {
        match (lhs, rhs) {
            (None, None) => Self::bot(),
            (Some(x), None) => x.join(&Self::bot()),
            (None, Some(x)) => x.join(&Self::bot()),
            (Some(x), Some(y)) => x.join(y),
        }
    }
}

pub trait BorrowedLubIterator<T> {
    fn borrowed_lub(self) -> T;
}

impl<T: JoinSemiLattice, I> BorrowedLubIterator<T> for I
where
    I: Iterator,
    I::Item: Deref<Target = T>,
{
    fn borrowed_lub(self) -> T {
        self.fold(T::bot(), |a, b| a.join(&b))
    }
}

pub trait LubIterator<T> {
    fn lub(self) -> T;
}

impl<T: JoinSemiLattice, I> LubIterator<T> for I
where
    I: Iterator<Item = T>,
{
    fn lub(self) -> T {
        self.fold(T::bot(), |a, b| a.join(&b))
    }
}

impl<T: PartialOrd> PreOrder for T {
    fn leq(&self, other: &Self) -> bool {
        self.le(other)
    }
}

impl JoinSemiLattice for u64 {
    fn bot() -> Self {
        0
    }

    fn join(&self, other: &Self) -> Self {
        std::cmp::max(*self, *other)
    }
}

impl JoinSemiLattice for u128 {
    fn bot() -> Self {
        0
    }

    fn join(&self, other: &Self) -> Self {
        std::cmp::max(*self, *other)
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_lub() {
        use super::BorrowedLubIterator;
        let nums = [1 as u64, 4, 2, 42, 1341, 3];
        assert_eq!(nums.iter().borrowed_lub(), 1341);
    }
}
