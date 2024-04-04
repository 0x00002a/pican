use crate::{
    ir::SwizzleDim,
    ops::{CmpOp, OpCode},
    register::{Register, RegisterKind},
};
use copy_arrayvec::CopyArrayVec;

use serde::{Deserialize, Serialize};
use typesum::sumtype;

/// Identifier for a free register which is awaiting allocation
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct FreeRegId(usize);

/// A register awaiting allocation
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct FreeRegister {
    pub kind: RegisterKind,
}

pub type SwizzleDims = CopyArrayVec<SwizzleDim, 4>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct RegisterId(usize);

impl RegisterId {
    pub fn first() -> Self {
        Self(0)
    }
    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

/// A register "hole", may be fixed or awaiting allocation
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub enum RegHoleKind {
    Fixed(Register),
    Free(FreeRegister),
}
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct RegHole {
    /// Identity of the register. It is stable across changes to
    /// `kind` allowing register allocation to not invalidate
    /// lookups based on the id of the hole
    pub id: RegisterId,
    pub kind: RegHoleKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct RegOperand {
    pub register: RegHole,
    pub swizzle: Option<CopyArrayVec<SwizzleDim, 4>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
#[sumtype]
pub enum Operand {
    Reg(RegOperand),
    Cmp(CmpOp),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize)]
pub struct Vec4<T> {
    pub x: T,
    pub y: T,
    pub z: T,
    pub w: T,
}

impl<T> Vec4<T> {
    pub fn new(x: T, y: T, z: T, w: T) -> Self {
        Self { x, y, z, w }
    }
}
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct ProcId(usize);

impl ProcId {
    pub fn first() -> Self {
        Self(0)
    }
    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub enum Directive {
    Proc { id: ProcId },
    End,
    NoDvle,
    Entry { name: ProcId },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
pub struct Instruction {
    pub opcode: OpCode,
    pub operands: CopyArrayVec<Operand, 4>,
}
