use codespan::FileId;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Ident<'a>(&'a str);

impl<'a> Ident<'a> {
    pub fn new(val: &'a str) -> Self {
        Self(val)
    }
}

impl<'a> std::cmp::PartialEq<str> for Ident<'a> {
    fn eq(&self, other: &str) -> bool {
        self.0.eq(other)
    }
}

/// f32 wrapper that implements Eq and doesn't allow NaN or inf
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Float(f32);
impl Float {
    /// Try to create a new Float, will fail if not finite
    pub fn new(val: f32) -> Option<Float> {
        if val.is_finite() {
            Some(Self(val))
        } else {
            None
        }
    }
}
impl std::hash::Hash for Float {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&self.0.to_ne_bytes());
    }
}

impl std::cmp::Eq for Float {}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub struct Span {
    span: codespan::Span,
    file: codespan::FileId,
}

impl Span {
    pub fn new(start: u32, end: u32, file: FileId) -> Self {
        Self {
            span: codespan::Span::new(start, end),
            file,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub struct IrNode<T> {
    node: T,
    span: Span,
}

impl<T> IrNode<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
    pub fn map<To>(self, f: impl FnOnce(T) -> To) -> IrNode<To> {
        IrNode {
            node: f(self.node),
            span: self.span,
        }
    }
    pub fn lift<To: From<T>>(self) -> IrNode<To> {
        self.map(From::from)
    }
    pub fn as_ref(&self) -> IrNode<&T> {
        IrNode {
            node: &self.node,
            span: self.span,
        }
    }

    pub fn as_mut(&mut self) -> IrNode<&mut T> {
        IrNode {
            node: &mut self.node,
            span: self.span,
        }
    }
    pub fn get(&self) -> &T {
        &self.node
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.node
    }
    pub fn into_inner(self) -> T {
        self.node
    }
}

impl<T> AsRef<T> for IrNode<T> {
    fn as_ref(&self) -> &T {
        self.get()
    }
}
impl<T> AsMut<T> for IrNode<T> {
    fn as_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}
