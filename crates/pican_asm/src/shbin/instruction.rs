use std::io::Write;

use binrw::binrw;
use modular_bitfield::prelude::*;
use pican_core::{
    ops::OpCode,
    register::{Register, RegisterKind},
};
use serde::de::IntoDeserializer;
use strum::EnumDiscriminants;
use typesum::sumtype;

use crate::ir::RegisterId;

use super::RegisterIndex;

#[bitfield(filled = false)]
#[derive(Debug, BitfieldSpecifier)]
pub struct ComponentMask {
    w: bool,
    z: bool,
    y: bool,
    x: bool,
}

impl std::fmt::Display for ComponentMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.x() || self.y() || self.z() || self.w() {
            write!(
                f,
                ".{}{}{}{}",
                if self.x() { "x" } else { "" },
                if self.y() { "y" } else { "" },
                if self.z() { "z" } else { "" },
                if self.w() { "w" } else { "" },
            )
        } else {
            write!(f, "")
        }
    }
}

#[derive(Debug, BitfieldSpecifier, PartialEq, Eq)]
#[bits = 2]
pub enum Component {
    X,
    Y,
    Z,
    W,
}

impl std::fmt::Display for Component {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::X => "x",
                Self::Y => "y",
                Self::Z => "z",
                Self::W => "w",
            }
        )
    }
}

#[bitfield]
#[derive(Debug, BitfieldSpecifier)]
pub struct ComponentSelector {
    w: Component,
    z: Component,
    y: Component,
    x: Component,
}
impl std::fmt::Display for ComponentSelector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.x() != Component::X
            || self.y() != Component::Y
            || self.z() != Component::Z
            || self.w() != Component::W
        {
            write!(f, ".{}{}{}{}", self.x(), self.y(), self.z(), self.w())
        } else {
            write!(f, "")
        }
    }
}

#[bitfield(filled = false)]
#[derive(Debug, BitfieldSpecifier)]
pub struct OperandSource {
    negate: bool,
    selector: ComponentSelector,
}

#[bitfield(bytes = 4)]
#[binrw]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[br(map(Self::from_bytes))]
#[bw(map(|x: &Self| x.into_bytes()))]
pub struct OperandDescriptor {
    destination_mask: ComponentMask,
    s1: OperandSource,
    s2: OperandSource,
    s3: OperandSource,
    #[skip]
    __: B1,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat1 {
    desc: B7,
    src2: B5,
    src1: B7,
    idx1: B2,
    dst: B5,
    opc: B6,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat1U {
    desc: B7,
    #[skip]
    __: B5,
    src1: B7,
    idx1: B2,
    dst: B5,
    opc: B6,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat1I {
    desc: B7,
    src2: B7,
    src1: B5,
    idx2: B2,
    dst: B5,
    opc: B6,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat1C {
    desc: B7,
    src2: B5,
    src1: B7,
    idx1: B2,
    cmpy: B3,
    cmpx: B3,
    opc: B5,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat2 {
    num: B8,
    #[skip]
    __: B2,
    dst: B12,
    condop: B2,
    refy: B1,
    refx: B1,
    opc: B6,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat3 {
    num: B8,
    #[skip]
    __: B2,
    dst: B12,
    const_id: B4,
    opc: B6,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat4 {
    num: B8,
    #[skip]
    __: B2,
    dst: B12,
    winding: B1,
    primemit: B1,
    vtxid: B2,
    opc: B6,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat5 {
    desc: B5,
    src3: B5,
    src2: B7,
    src1: B5,
    idx2: B2,
    dst: B5,
    opc: B3,
}

#[bitfield]
#[derive(Debug)]
pub struct InstructionFormat5I {
    desc: B5,
    src3: B7,
    src2: B5,
    src1: B5,
    idx3: B2,
    dst: B5,
    opc: B3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: OpCode,
    pub operands: Operands,
}

impl Instruction {
    pub fn dst_to_register(index: u8) -> Register {
        match index {
            0x00..=0x0F => Register {
                kind: RegisterKind::Output,
                index: index.into(),
            },
            0x10..=0x1F => Register {
                kind: RegisterKind::Scratch,
                index: (index - 0x10).into(),
            },
            _ => panic!("dst register index invalid {index}"),
        }
    }

    pub fn src_to_register(index: u8) -> Register {
        match index {
            0x00..=0x0F => Register::new(RegisterKind::Input, index.into()),
            0x10..=0x1F => Register::new(RegisterKind::Scratch, (index - 0x10).into()),
            0x20..=0x7F => Register::new(RegisterKind::FloatingVecUniform, (index - 0x20).into()),
            _ => panic!("src register index invalid {index}"),
        }
    }

    pub fn to_asm(&self, operands: &[OperandDescriptor]) -> String {
        let operands = match self.operands {
            Operands::TwoArguments {
                dst,
                src1,
                src2,
                desc,
                ..
            } => {
                let pattern = &operands[desc as usize];
                println!("desc: {desc:02X}, {pattern:?}");
                format!(
                    "{}{}, {}{}{}, {}{}{}",
                    dst,
                    pattern.destination_mask(),
                    if pattern.s1().negate() { "-" } else { "" },
                    src1,
                    pattern.s1().selector(),
                    if pattern.s2().negate() { "-" } else { "" },
                    src2,
                    pattern.s2().selector(),
                )
            }
            Operands::OneArgument { dst, src1, desc } => {
                let pattern = &operands[desc as usize];
                format!(
                    "{dst}{}, {}{src1}{}",
                    pattern.destination_mask(),
                    if pattern.s1().negate() { "-" } else { "" },
                    pattern.s1().selector(),
                )
            }

            Operands::Mad {
                dst,
                src1,
                src2,
                src3,
                desc: Some(desc),
            } => {
                let pattern = &operands[desc as usize];
                format!(
                    "{dst}{}, {}{src1}{}, {}{src2}{}, {}{src3}{}",
                    pattern.destination_mask(),
                    if pattern.s1().negate() { "-" } else { "" },
                    pattern.s1().selector(),
                    if pattern.s2().negate() { "-" } else { "" },
                    pattern.s2().selector(),
                    if pattern.s3().negate() { "-" } else { "" },
                    pattern.s3().selector(),
                )
            }
            Operands::Mad { desc: None, .. } => todo!(),
            Operands::Cmp { .. } => todo!(),
            Operands::SetEmit { .. } => todo!(),
            Operands::ControlFlow { .. } => todo!(),
            Operands::Zero => "".to_owned(),
            Operands::Unknown => todo!(),
        };
        format!("{} {}", self.opcode, operands)
    }
}

pub const FORMAT_TABLE: &[(InstructionFormatKind, OpCode)] = &[
    (InstructionFormatKind::One, OpCode::Add),
    (InstructionFormatKind::One, OpCode::Dp3),
    (InstructionFormatKind::One, OpCode::Dp4),
    (InstructionFormatKind::One, OpCode::Dph),
    (InstructionFormatKind::One, OpCode::Dst),
    (InstructionFormatKind::OneU, OpCode::Ex2),
    (InstructionFormatKind::OneU, OpCode::Lg2),
    (InstructionFormatKind::OneU, OpCode::LitP),
    (InstructionFormatKind::One, OpCode::Mul),
    (InstructionFormatKind::One, OpCode::Sge),
    (InstructionFormatKind::One, OpCode::Slt),
    (InstructionFormatKind::OneU, OpCode::Flr),
    (InstructionFormatKind::One, OpCode::Max),
    (InstructionFormatKind::One, OpCode::Min),
    (InstructionFormatKind::OneU, OpCode::Rcp),
    (InstructionFormatKind::OneU, OpCode::Rsq),
    (InstructionFormatKind::OneU, OpCode::MovA),
    (InstructionFormatKind::OneU, OpCode::Mov),
    (InstructionFormatKind::OneI, OpCode::DphI),
    (InstructionFormatKind::OneI, OpCode::DstI),
    (InstructionFormatKind::OneI, OpCode::SgeI),
    (InstructionFormatKind::OneI, OpCode::SltI),
    (InstructionFormatKind::Zero, OpCode::Break),
    (InstructionFormatKind::Zero, OpCode::Nop),
    (InstructionFormatKind::Zero, OpCode::End),
    (InstructionFormatKind::Two, OpCode::BreakC),
    (InstructionFormatKind::Two, OpCode::Call),
    (InstructionFormatKind::Two, OpCode::CallC),
    (InstructionFormatKind::Three, OpCode::CallU),
    (InstructionFormatKind::Three, OpCode::IfU),
    (InstructionFormatKind::Two, OpCode::IfC),
    (InstructionFormatKind::Three, OpCode::Loop),
    (InstructionFormatKind::Zero, OpCode::Emit),
    (InstructionFormatKind::Four, OpCode::SetEmit),
    (InstructionFormatKind::Two, OpCode::JmpC),
    (InstructionFormatKind::Three, OpCode::JmpU),
    (InstructionFormatKind::OneC, OpCode::Cmp),
    (InstructionFormatKind::FiveI, OpCode::MadI),
    (InstructionFormatKind::Five, OpCode::Mad),
    (InstructionFormatKind::Unknown, OpCode::Unknown),
];

#[derive(EnumDiscriminants)]
#[strum_discriminants(name(InstructionFormatKind))]
#[sumtype(only = from, is)]
pub enum InstructionFormat {
    One(InstructionFormat1),
    OneI(InstructionFormat1I),
    OneU(InstructionFormat1U),
    OneC(InstructionFormat1C),
    Two(InstructionFormat2),
    Three(InstructionFormat3),
    Four(InstructionFormat4),
    Five(InstructionFormat5),
    FiveI(InstructionFormat5I),
    #[sumtype(only = is)]
    Zero,
    #[sumtype(only = is)]
    Unknown,
}

impl InstructionFormat {
    pub fn from_bytes(kind: InstructionFormatKind, bytes: [u8; 4]) -> Self {
        match kind {
            InstructionFormatKind::One => InstructionFormat1::from_bytes(bytes).into(),
            InstructionFormatKind::OneI => InstructionFormat1I::from_bytes(bytes).into(),
            InstructionFormatKind::OneU => InstructionFormat1U::from_bytes(bytes).into(),
            InstructionFormatKind::OneC => InstructionFormat1C::from_bytes(bytes).into(),
            InstructionFormatKind::Two => InstructionFormat2::from_bytes(bytes).into(),
            InstructionFormatKind::Three => InstructionFormat3::from_bytes(bytes).into(),
            InstructionFormatKind::Four => InstructionFormat4::from_bytes(bytes).into(),
            InstructionFormatKind::Five => InstructionFormat5::from_bytes(bytes).into(),
            InstructionFormatKind::FiveI => InstructionFormat5I::from_bytes(bytes).into(),
            InstructionFormatKind::Zero => InstructionFormat::Zero,
            InstructionFormatKind::Unknown => InstructionFormat::Unknown,
        }
    }
    pub fn to_operands(self) -> Operands {
        fn dsr(idx: u8) -> Register {
            Instruction::dst_to_register(idx)
        }
        fn sr(idx: u8) -> Register {
            Instruction::src_to_register(idx)
        }
        match self {
            InstructionFormat::One(o) => Operands::TwoArguments {
                dst: dsr(o.dst()),
                src1: sr(o.src1()),
                src2: sr(o.src2()),
                desc: o.desc(),
                inverse: false,
            },
            InstructionFormat::OneI(o) => Operands::TwoArguments {
                dst: dsr(o.dst()),
                src1: sr(o.src1()),
                src2: sr(o.src2()),
                desc: o.desc(),
                inverse: true,
            },
            InstructionFormat::OneU(o) => Operands::OneArgument {
                dst: dsr(o.dst()),
                src1: sr(o.src1()),
                desc: o.desc(),
            },
            InstructionFormat::OneC(o) => Operands::Cmp {
                src1: sr(o.src1()),
                src2: sr(o.src2()),
            },
            InstructionFormat::Two(o) => Operands::ControlFlow {
                cond: Some(o.condop()),
                dst_offset: o.dst(),
                num: o.num(),
            },
            InstructionFormat::Three(o) => Operands::ControlFlow {
                cond: None,
                dst_offset: o.dst(),
                num: o.num(),
            },
            InstructionFormat::Four(o) => Operands::SetEmit {
                vtxid: o.vtxid(),
                winding: o.winding(),
                primemit: o.primemit(),
            },
            InstructionFormat::Five(o) => Operands::Mad {
                dst: sr(o.dst()),
                src1: sr(o.src1()),
                src2: sr(o.src2()),
                src3: sr(o.src3()),
                desc: Some(o.desc()),
            },
            InstructionFormat::FiveI(o) => Operands::Mad {
                dst: sr(o.dst()),
                src1: sr(o.src1()),
                src2: sr(o.src2()),
                src3: sr(o.src3()),
                desc: None,
            },
            InstructionFormat::Zero => Operands::Zero,
            InstructionFormat::Unknown => Operands::Unknown,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operands {
    /// (dst, src1, src2, desc)
    TwoArguments {
        dst: Register,
        src1: Register,
        src2: Register,
        desc: u8,
        /// Whether it's the special form that has narrow src2 (uses 1I encoding instead)
        inverse: bool,
    },

    /// (dst, src1, desc)
    OneArgument {
        dst: Register,
        src1: Register,
        desc: u8,
    },
    Cmp {
        src1: Register,
        src2: Register,
    },
    SetEmit {
        vtxid: u8,
        winding: u8,
        primemit: u8,
    },
    ControlFlow {
        cond: Option<u8>,
        dst_offset: u16,
        num: u8,
    },
    Mad {
        dst: Register,
        src1: Register,
        src2: Register,
        src3: Register,
        desc: Option<u8>,
    },
    Zero,
    Unknown,
}

pub fn disassemble_blob(blob: &[u32]) -> Vec<Instruction> {
    let mut rv = vec![];

    for &instr in blob {
        let bytes = instr.to_le_bytes();
        let opcode_b = (instr >> 0x1A) as u8;
        assert!(
            opcode_b <= OpCode::maximum_value(),
            "opcode is invalid {opcode_b}"
        );
        let opcode = OpCode::binary_to_op(opcode_b);
        let format = FORMAT_TABLE.iter().find(|(k, o)| *o == opcode).unwrap().0;
        let operands = InstructionFormat::from_bytes(format, bytes).to_operands();
        rv.push(Instruction { opcode, operands });
    }

    rv
}
