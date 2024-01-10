use serde::Serialize;

use crate::ir::{Ident, IrNode};

use super::bindings::Bindings;

/// Single shader module
pub struct Module<'a> {
    pub entry_points: &'a [IrNode<EntryPoint<'a>>],
    pub bindings: Bindings<'a>,
}


pub struct Alias<'a> {
    pub kind: IrNode<AliasKind>,
    pub from: IrNode<Ident<'a>>,
    pub to:
}

pub enum AliasKind {
    Output,
    Input,
    Var,
}

pub struct EntryPoint<'a> {
    pub name: IrNode<Ident<'a>>,
    pub ops: IrNode<&'a [IrNode<Op<'a>>]>,
}
pub enum Op<'a> {

}

pub struct Op<'a> {

}

pub enum BinOpKind {
    Mov,
}

pub struct BinOp<'a> {
    pub kind: IrNode<BinOpKind>,
    pub args: [IrNode<Operand<'a>>; 2],
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct WideOperand {
    /// The relative address of the input, e.g. uniform[1]
    pub relative_address: usize,
}

pub enum DestOperation<'a> {

}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub enum SourceOperand<'a> {
    Wide(WideOperand),
    /// Only supports input and scratch registers
    Narrow,

}
