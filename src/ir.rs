use codespan::FileId;
use serde::{Deserialize, Serialize};

/// f32 wrapper that implements Eq and doesn't allow NaN or inf
#[derive(PartialEq, PartialOrd, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Float(f32);

impl std::cmp::Eq for Float {}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug, Serialize, Deserialize)]
pub struct Span {
    span: codespan::Span,
    file: codespan::FileId,
}

impl Span {
    pub fn new(start: usize, end: usize, file: FileId) -> Self {
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
}
