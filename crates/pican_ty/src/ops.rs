use pican_core::ops::OpCode;

use crate::ty::Type;

pub enum OperandWidth {
    Wide,
    Narrow,
}
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum OperandTy {
    /// Destination register
    DstReg,
    /// Narrow input, must be input register
    SrcReg,
    /// Wide input, also allows uniforms with relative addressing
    WideSrc,
}

impl OperandTy {
    pub fn matches(self, ty: Type) -> bool {
        match self {
            OperandTy::DstReg => matches!(ty, Type::Register(_)),
            OperandTy::SrcReg => matches!(ty, Type::Register(_)),
            OperandTy::WideSrc => true,
        }
    }
}

pub struct OperandSlot {
    pub allowed_types: &'static [OperandTy],
}
pub struct OperandSlots {
    pub slots: &'static [OperandSlot],
}

impl OperandSlots {
    pub const fn new(slots: &'static [OperandSlot]) -> Self {
        Self { slots }
    }
}

/// rDest1, rSrc1, rSrc2
const DST_SRC_SRC: OperandSlots = OperandSlots::new(&[
    OperandSlot {
        allowed_types: &[OperandTy::DstReg],
    },
    OperandSlot {
        allowed_types: &[OperandTy::WideSrc],
    },
    OperandSlot {
        allowed_types: &[OperandTy::SrcReg],
    },
]);

/// rDest1, rSrc1
const DST_SRC: OperandSlots = OperandSlots::new(&[
    OperandSlot {
        allowed_types: &[OperandTy::DstReg],
    },
    OperandSlot {
        allowed_types: &[OperandTy::WideSrc],
    },
]);

/// Get the shape of the operand slots for an opcode
pub fn slots_for_opcode(op: OpCode) -> OperandSlots {
    match op {
        OpCode::Nop => todo!(),
        OpCode::End => todo!(),
        OpCode::Emit => todo!(),
        OpCode::SetEmit => todo!(),
        OpCode::Add
        | OpCode::Dp3
        | OpCode::Dp4
        | OpCode::Dph
        | OpCode::Dst
        | OpCode::Mul
        | OpCode::Sge
        | OpCode::Slt
        | OpCode::Max
        | OpCode::Min => DST_SRC_SRC,
        OpCode::Ex2
        | OpCode::Lg2
        | OpCode::LitP
        | OpCode::Flr
        | OpCode::Rcp
        | OpCode::Rsq
        | OpCode::Mov => DST_SRC,
        OpCode::Mova => todo!(),
        OpCode::Cmp => todo!(),
        OpCode::Call => todo!(),
        OpCode::For => todo!(),
        OpCode::Break => todo!(),
        OpCode::BreakC => todo!(),
        OpCode::CallC => todo!(),
        OpCode::IfC => todo!(),
        OpCode::JmpC => todo!(),
        OpCode::CallU => todo!(),
        OpCode::IfU => todo!(),
        OpCode::JmpU => todo!(),
        OpCode::Mad => todo!(),
    }
}
