use std::rc::Rc;

use pican_core::register::Register;
use serde::{Deserialize, Serialize};

use pican_core::ir::{Float, IrNode};

pub use pican_core::ir::Ident;
pub use pican_core::ops::OpCode;

macro_rules! sum_node {
    ($vi:vis enum $name:ident {
        $($variant:ident ($inner:ident)),*
    }) => {
        #[typesum::sumtype]
        #[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
        $vi enum $name <'a> {
            $($variant ( IrNode<$inner <'a>> )),*
        }
    };
}
sum_node! {
pub enum Statement {
    EntryPoint(FunctionDecl),
    Comment(Comment),
    Op(Op),
    RegisterBind(RegisterBind),
    Uniform(UniformDecl),
    Constant(ConstantDecl)
}
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct ConstantDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    pub values: IrNode<&'a [IrNode<Float>]>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct UniformDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    /// Dimension of the uniform, None for vectors
    pub dimensions: Option<IrNode<u8>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct RegisterBind<'a> {
    pub reg: IrNode<Register>,
    pub name: IrNode<Ident<'a>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[typesum::sumtype]
pub enum Operand<'a> {
    Var(IrNode<Ident<'a>>),
    Register(IrNode<Register>),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Operands<'a>(pub &'a [IrNode<Operand<'a>>]);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Op<'a> {
    pub opcode: IrNode<OpCode>,
    pub operands: IrNode<Operands<'a>>,
}

pub type Stmt<'a> = IrNode<Statement<'a>>;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Comment<'a>(pub &'a str);

/// A section
///
/// e.g. .main
#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct FunctionDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    pub block: IrNode<Block<'a>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Block<'a> {
    pub statements: IrNode<&'a [Stmt<'a>]>,
}
