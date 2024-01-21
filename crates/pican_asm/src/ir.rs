use pican_core::{
    copy_arrayvec::CopyArrayVec,
    ir::{Float, Ident},
    ops::OpCode,
    properties::OutputProperty,
    register::{Register, RegisterKind},
};
use pican_pir::ty::UniformTy;
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
pub struct Operation {
    pub opcode: OpCode,
    pub operands: CopyArrayVec<RegHole, 4>,
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
    Proc {
        id: ProcId,
    },
    End,
    /// .fvec, .ivec, .bool
    /*In {
        name: Ident<'i>,
        reg: Option<Register>,
    },
    Out {
        name: Option<Ident<'i>>,
        property: OutputProperty,
    },*/
    NoDvle,
    Entry {
        name: ProcId,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Serialize)]
#[sumtype(only = from)]
pub enum Instruction {
    Op(Operation),
    Directive(Directive),
    //Label { name: Ident<'i> },
}
