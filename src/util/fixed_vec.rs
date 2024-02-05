use std::{
    fmt::Debug,
    hash::Hash,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

pub struct FixedVec<T, const N: usize> {
    length: usize,
    elems: [MaybeUninit<T>; N],
}

impl<const N: usize, T> FixedVec<T, N> {
    pub fn new() -> Self {
        FixedVec {
            length: 0,
            elems: unsafe { MaybeUninit::uninit().assume_init() },
        }
    }

    pub fn push(&mut self, elem: T) {
        assert!(self.length < N);

        unsafe { self.elems[self.length].as_mut_ptr().write(elem) }
        self.length += 1;
    }
}

impl<T: Clone, const N: usize> Clone for FixedVec<T, N> {
    fn clone(&self) -> Self {
        let mut elems: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

        for elem in self.iter() {
            unsafe { elems[self.length].as_mut_ptr().write(elem.clone()) }
        }

        Self {
            length: self.length,
            elems,
        }
    }
}

impl<const N: usize, T> Deref for FixedVec<T, N> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.elems.as_ptr().cast(), self.length) }
    }
}

impl<const N: usize, T> DerefMut for FixedVec<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.elems.as_mut_ptr().cast(), self.length) }
    }
}

impl<T: PartialEq, const N: usize> PartialEq for FixedVec<T, N> {
    fn eq(&self, other: &Self) -> bool {
        let slice: &[_] = self;
        let other: &[_] = other;
        slice.eq(other)
    }
}

impl<T: Eq, const N: usize> Eq for FixedVec<T, N> {}

impl<T, const N: usize> Default for FixedVec<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Debug, const N: usize> Debug for FixedVec<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: Hash, const N: usize> Hash for FixedVec<T, N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let slice: &[_] = self;
        slice.hash(state)
    }
}
