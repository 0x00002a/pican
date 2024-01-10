use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::ir::{Float, IrNode};

macro_rules! sum_node {
    ($vi:vis enum $name:ident {
        $($variant:ident ($inner:ident)),*
    }) => {
        #[typesum::sumtype]
        #[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
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

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct ConstantDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    pub values: IrNode<&'a [IrNode<Float>]>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct UniformDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    /// Dimension of the uniform, None for vectors
    pub dimensions: Option<IrNode<u8>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
struct RegisterBind<'a> {
    pub reg: IrNode<Ident<'a>>,
    pub name: IrNode<Ident<'a>>,
}

sum_node! {
    pub enum Operand {
        Var(Ident)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct Operands<'a>(pub &'a [IrNode<Operand<'a>>]);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub enum OpCode {
    Dp4,
    Mov,
    Mad,
    Min,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct Op<'a> {
    pub opcode: IrNode<OpCode>,
    pub operands: IrNode<Operands<'a>>,
}

pub use crate::ir::Ident;

pub type Stmt<'a> = IrNode<Statement<'a>>;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct Comment<'a> {
    pub content: IrNode<&'a str>,
}

/// A section
///
/// e.g. .main
#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct FunctionDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    pub block: IrNode<Block<'a>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize)]
pub struct Block<'a> {
    pub statements: IrNode<&'a [Stmt<'a>]>,
}
