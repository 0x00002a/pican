use pican_core::properties::OutputProperty;
use pican_core::register::Register;
use serde::Serialize;

use pican_core::ir::{Float, IrNode, SwizzleDims};

pub use pican_core::ir::Ident;
pub use pican_core::ops::OpCode;
use strum::EnumDiscriminants;
use typesum::sumtype;

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
    Constant(ConstantDecl),
    OutputBind(OutputBind),
    Directive(Directive),
    InputBind(InputBind)
}
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Directive<'a> {
    /// Directive telling us not to generate a DVLE for this module
    NoDvle,
    /// Manually defined entrypoint for this DVLE
    Entry(IrNode<Ident<'a>>),
    /// Geometry shader
    ///
    /// TODO: Actually implement parsing of this
    Gsh,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct InputBind<'a>(pub IrNode<Ident<'a>>);

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct OutputBind<'a> {
    pub alias: Option<IrNode<Ident<'a>>>,
    pub property: IrNode<OutputProperty>,
    pub register: Option<IrNode<Register>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug, EnumDiscriminants)]
pub enum Constant<'a> {
    Integer(IrNode<&'a [IrNode<u32>]>),
    Float(IrNode<&'a [IrNode<Float>]>),
    FloatArray {
        elements: IrNode<&'a [IrNode<&'a [IrNode<Float>]>]>,
        hint: Option<IrNode<u8>>,
    },
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct ConstantDecl<'a> {
    pub name: IrNode<Ident<'a>>,
    pub value: IrNode<Constant<'a>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct UniformDecl<'a> {
    /// The type of the uniforms declared
    pub ty: IrNode<UniformTy>,
    /// The uniforms bound in this decl e.g. .fvec m1[4], m2, m5[4]
    pub uniforms: IrNode<&'a [IrNode<Uniform<'a>>]>,
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub enum UniformTy {
    Bool,
    Integer,
    Float,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Uniform<'a> {
    pub name: IrNode<Ident<'a>>,
    /// Dimension of the uniform, None for vectors
    pub dimensions: Option<IrNode<u8>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct SwizzleExpr<'a, T> {
    pub target: IrNode<T>,
    pub swizzle: Option<IrNode<SwizzleDims<'a>>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct RegisterBind<'a> {
    pub name: IrNode<Ident<'a>>,
    pub reg: IrNode<SwizzleExpr<'a, RegisterBindTarget<'a>>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[sumtype]
pub enum RegisterBindTarget<'a> {
    Register(Register),
    Var(Ident<'a>),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[typesum::sumtype]
pub enum OperandKind<'a> {
    Var(IrNode<Ident<'a>>),
    Register(IrNode<Register>),
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
pub struct Operand<'a> {
    pub kind: IrNode<OperandKind<'a>>,
    pub relative_address: Option<IrNode<u32>>,
    pub swizzle: Option<IrNode<SwizzleDims<'a>>>,
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
