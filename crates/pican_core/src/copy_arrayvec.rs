use std::{mem::MaybeUninit, ops::Deref};

use serde::Serialize;

#[derive(Clone, Copy)]
pub struct CopyArrayVec<T: Copy, const MAX: usize> {
    buf: [MaybeUninit<T>; MAX],
    len: usize,
}
impl<T: Copy + std::fmt::Debug, const MAX: usize> std::fmt::Debug for CopyArrayVec<T, MAX> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CopyArrayVec")
            .field("max", &MAX)
            .field("buf", &self.deref())
            .finish()
    }
}

impl<T: Copy, const MAX: usize> Default for CopyArrayVec<T, MAX> {
    fn default() -> Self {
        Self {
            buf: unsafe { MaybeUninit::uninit().assume_init() },
            len: 0,
        }
    }
}

impl<T: Copy, const MAX: usize> CopyArrayVec<T, MAX> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
    pub fn push(&mut self, el: T) {
        assert!(self.len() < MAX, "tried to push to full arrayvec");

        let next = self.len;
        self.buf[next].write(el);
        self.len += 1;
    }
}
impl<T: Copy, const MAX: usize> Deref for CopyArrayVec<T, MAX> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { core::mem::transmute::<_, &[T]>(&self.buf[0..self.len()]) }
    }
}

impl<T: Copy + Serialize, const MAX: usize> Serialize for CopyArrayVec<T, MAX> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.deref().serialize(serializer)
    }
}

impl<T: Copy + PartialEq, const MAX: usize> PartialEq for CopyArrayVec<T, MAX> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}
impl<T: Copy + Eq, const MAX: usize> Eq for CopyArrayVec<T, MAX> {}
impl<T: Copy + std::hash::Hash, const MAX: usize> std::hash::Hash for CopyArrayVec<T, MAX> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state)
    }
}

impl<T: Copy, const MAX: usize> FromIterator<T> for CopyArrayVec<T, MAX> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut me = Self::default();
        for item in iter {
            me.push(item);
        }
        me
    }
}
