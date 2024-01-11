use arrayvec::ArrayVec;
use serde::Serialize;

use pican_core::{
    copy_arrayvec::CopyArrayVec,
    ir::{Ident, IrNode},
    ops::{OpCode, OperandKind},
    register::Register,
};

use super::bindings::Bindings;

/// Single shader module
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub struct Module<'a> {
    pub entry_points: &'a [IrNode<EntryPoint<'a>>],
    pub bindings: Bindings<'a>,
}

#[derive(Debug, Serialize, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EntryPoint<'a> {
    pub name: IrNode<Ident<'a>>,
    pub ops: IrNode<&'a [IrNode<Op<'a>>]>,
}

#[derive(Debug, Serialize, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Op<'a> {
    pub opcode: IrNode<OpCode>,
    pub operands: IrNode<CopyArrayVec<IrNode<Operand<'a>>, 4>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[typesum::sumtype]
pub enum Operand<'a> {
    Var(IrNode<Ident<'a>>),
    Register(IrNode<Register>),
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct WideOperand {
    /// The relative address of the input, e.g. uniform[1]
    pub relative_address: usize,
}
