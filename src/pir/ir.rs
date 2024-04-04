use copy_arrayvec::CopyArrayVec;
use serde::Serialize;

use crate::{
    ir::{Float, Ident, IrNode, SwizzleDims},
    ops::{CmpOp, OpCode},
    properties::OutputProperty,
    register::Register,
};

use super::ty::UniformTy;

use super::bindings::Bindings;

/// Single shader module
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub struct Module<'a> {
    pub bindings: Bindings<'a>,
    pub entry_points: &'a [IrNode<EntryPoint<'a>>],
    pub outputs: &'a [IrNode<&'a OutputBinding<'a>>],
    pub inputs: &'a [IrNode<&'a InputBinding<'a>>],
    /// Whether to not produce a DVLE output from this module
    pub no_dvle: bool,
}
#[derive(Debug, Serialize, PartialEq, Eq, Clone, Hash)]
pub struct InputBinding<'a> {
    pub name: IrNode<Ident<'a>>,
    pub index: usize,
    pub register: Option<IrNode<Register>>,
}

#[derive(Debug, Serialize, PartialEq, Eq, Clone, Hash)]
pub struct OutputBinding<'a> {
    pub register: Option<IrNode<Register>>,
    pub alias: Option<IrNode<Ident<'a>>>,
    pub property: IrNode<OutputProperty>,
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
#[derive(Debug, Serialize, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Operand<'a> {
    pub kind: IrNode<OperandKind<'a>>,
    pub relative_addr: Option<IrNode<u32>>,
    pub swizzle: Option<IrNode<SwizzleDims<'a>>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[typesum::sumtype]
#[serde(rename_all = "snake_case", tag = "ty", content = "value")]
pub enum OperandKind<'a> {
    Var(IrNode<Ident<'a>>),
    Register(IrNode<Register>),
    Cmp(IrNode<CmpOp>),
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct WideOperand {
    /// The relative address of the input, e.g. uniform[1]
    pub relative_address: usize,
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
pub struct Uniform {
    pub ty: IrNode<UniformTy>,
    /// Dimension of the uniform, for non-arrays this is None
    pub dimension: Option<IrNode<usize>>,
}

#[derive(Clone, Copy, Debug, Serialize, Hash, PartialEq, Eq)]
#[serde(rename_all = "snake_case", tag = "ty", content = "value")]
pub enum ConstantUniform<'a> {
    Integer(IrNode<[IrNode<i32>; 4]>),
    Float(IrNode<[IrNode<Float>; 4]>),
    FloatArray(IrNode<&'a [IrNode<[IrNode<Float>; 4]>]>),
}
