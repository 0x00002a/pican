use crate::ops::OpCode;

use super::{PrimTy, RegisterTy, Type, UniformArrayTy, VecUniformTy};

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
    /// Comparison op type
    Cmp,
}

impl OperandTy {
    pub fn matches(self, ty: Type) -> bool {
        match self {
            OperandTy::DstReg => {
                let Type::Register(RegisterTy { kind, .. }) = ty else {
                    return false;
                };
                kind.is_type(crate::register::RegisterType::Output)
            }
            OperandTy::SrcReg => {
                let Type::Register(RegisterTy { kind, .. }) = ty else {
                    return false;
                };
                kind.is_type(crate::register::RegisterType::Input)
            }
            OperandTy::WideSrc => {
                OperandTy::SrcReg.matches(ty) || {
                    matches!(
                        ty,
                        Type::UniformArray(UniformArrayTy {
                            element_ty: PrimTy::Float,
                            ..
                        }) | Type::VecUniform(VecUniformTy {
                            prim_ty: PrimTy::Float,
                            ..
                        })
                    )
                }
            }
            OperandTy::Cmp => {
                matches!(ty, Type::CmpOp)
            }
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

const CMP_SLOTS: OperandSlots = OperandSlots::new(&[
    OperandSlot {
        allowed_types: &[OperandTy::SrcReg],
    },
    OperandSlot {
        allowed_types: &[OperandTy::Cmp],
    },
    OperandSlot {
        allowed_types: &[OperandTy::Cmp],
    },
    OperandSlot {
        allowed_types: &[OperandTy::SrcReg],
    },
]);

const NO_OPERANDS: OperandSlots = OperandSlots::new(&[]);

/// Get the shape of the operand slots for an opcode
pub fn slots_for_opcode(op: OpCode) -> OperandSlots {
    match op {
        OpCode::Break | OpCode::Nop | OpCode::End | OpCode::Emit => NO_OPERANDS,
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
        OpCode::MovA => todo!(),
        OpCode::Cmp => CMP_SLOTS,
        OpCode::Call => todo!(),
        OpCode::BreakC => todo!(),
        OpCode::CallC => todo!(),
        OpCode::IfC => todo!(),
        OpCode::JmpC => todo!(),
        OpCode::CallU => todo!(),
        OpCode::IfU => todo!(),
        OpCode::JmpU => todo!(),
        OpCode::Mad => OperandSlots::new(&[
            OperandSlot {
                allowed_types: &[OperandTy::DstReg],
            },
            OperandSlot {
                allowed_types: &[OperandTy::SrcReg],
            },
            OperandSlot {
                allowed_types: &[OperandTy::WideSrc],
            },
            OperandSlot {
                allowed_types: &[OperandTy::SrcReg],
            },
        ]),
        OpCode::DphI => todo!(),
        OpCode::DstI => todo!(),
        OpCode::SgeI => todo!(),
        OpCode::SltI => todo!(),
        OpCode::Loop => todo!(),
        OpCode::MadI => OperandSlots::new(&[
            OperandSlot {
                allowed_types: &[OperandTy::DstReg],
            },
            OperandSlot {
                allowed_types: &[OperandTy::SrcReg],
            },
            OperandSlot {
                allowed_types: &[OperandTy::SrcReg],
            },
            OperandSlot {
                allowed_types: &[OperandTy::WideSrc],
            },
        ]),
        OpCode::Unknown => todo!(),
    }
}
