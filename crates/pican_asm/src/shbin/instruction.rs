use std::io::{Cursor, Seek, Write};

use binrw::{binrw, BinRead, BinReaderExt, BinWrite, BinWriterExt};
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
    #[bits = 1]
    negate: bool,
    #[bits = 8]
    selector: ComponentSelector,
}

#[bitfield(bytes = 8)]
#[binrw]
#[brw(little)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OperandDescriptor {
    #[bits = 4]
    destination_mask: ComponentMask,
    #[bits = 9]
    s1: OperandSource,
    #[bits = 9]
    s2: OperandSource,
    #[bits = 9]
    s3: OperandSource,
    #[skip]
    _unknown: B33,
}

#[bitfield]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat1 {
    desc: B7,
    src2: B5,
    src1: B7,
    idx1: B2,
    dst: B5,
    opc: B6,
}

#[bitfield]
#[derive(Debug, BinWrite, BinRead)]
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
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat1I {
    desc: B7,
    src2: B7,
    src1: B5,
    idx2: B2,
    dst: B5,
    opc: B6,
}

#[bitfield]
#[derive(Debug, BinWrite, BinRead)]
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
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat2 {
    num: B8,
    #[skip]
    __: B2,
    dst_offset: B12,
    condop: B2,
    refy: B1,
    refx: B1,
    opc: B6,
}

#[bitfield]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat3 {
    num: B8,
    #[skip]
    __: B2,
    dst_offset: B12,
    const_id: B4,
    opc: B6,
}

#[bitfield(bytes = 4)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat4 {
    #[skip]
    _padding: B22,
    winding: B1,
    primemit: B1,
    vtxid: B2,
    opc: B6,
}

#[bitfield]
#[derive(Debug, BinWrite, BinRead)]
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
#[derive(Debug, BinWrite, BinRead)]
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
            0x00..=0x0F => Register::new(RegisterKind::Output, index.into()),
            0x10..=0x1F => Register::new(RegisterKind::Scratch, (index - 0x10).into()),
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
        let lookup_pat = |desc| &operands[(desc) as usize];
        let operands = match self.operands {
            Operands::TwoArguments {
                dst,
                src1,
                src2,
                desc,
                ..
            } => {
                let pattern = lookup_pat(desc);
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
                let pattern = lookup_pat(desc);
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
                let pattern = lookup_pat(desc);
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
            Operands::ControlFlowConstant { .. } => todo!(),
        };
        format!("{} {}", self.opcode, operands)
    }
}

macro_rules! instruct_rec {
    ($v:ident, dst: $d:ident) => {
        $v.set_dst(dst_idx($d))
    };
    ($v:ident, src1: $s:ident) => {
        $v.set_src1(src_idx($s))
    };
    ($v:ident, src2: $s:ident) => {
        $v.set_src2(src_idx($s))
    };
    ($v:ident, src3: $s:ident) => {
        $v.set_src3(src_idx($s))
    };
    ($v:ident, $t:ident: $d:ident) => {
        paste::paste! { $v.[<set_ $t>]($d) }
    };
}
macro_rules! operand_rec {
    ($v:ident, dst: $d:ident) => {
        v.dst()
    };
    ($v:ident, src1: $s:ident) => {
        $v.set_src1(src_idx($s))
    };
    ($v:ident, src2: $s:ident) => {
        $v.set_src2(src_idx($s))
    };
    ($v:ident, desc: $d:ident) => {
        $v.set_desc($d)
    };
}

fn src_idx(r: Register) -> u8 {
    let off = match r.kind {
        RegisterKind::Input => 0x0,
        RegisterKind::Scratch => 0x10,
        RegisterKind::FloatingVecUniform => 0x20,
        _ => unreachable!(),
    };
    off + r.index as u8
}
fn dst_idx(r: Register) -> u8 {
    let off = match r.kind {
        RegisterKind::Output => 0x0,
        RegisterKind::Scratch => 0x10,
        _ => unreachable!(),
    };
    off + r.index as u8
}

macro_rules! map_vtn {
    ($vr:expr, dst) => {
        Instruction::dst_to_register($vr)
    };
    ($vr:expr, src1) => {
        Instruction::src_to_register($vr)
    };
    ($vr:expr, src2) => {
        Instruction::src_to_register($vr)
    };
    ($vr:expr, src3) => {
        Instruction::src_to_register($vr)
    };
    ($vr:expr, desc) => {
        $vr.into()
    };
    ($vr:expr, $o:ident) => {
        $vr
    };
}

macro_rules! instruct {
    ($f:ident { $($n:ident: $v:ident),+ }) => {
        {
            let mut v = $f::new();
            $(instruct_rec!(v, $n: $v);)+
            v.into()
        }
    }

}

#[binrw]
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

macro_rules! instructs {
    ($from:ident, $to:ident, $set_opcode:ident, $($f:ident { $($v:ident: $c:expr),* } = $arm:ident $armty:ident { $($vtn:ident => $vtt:ident),* },)*) => {
        impl From<$to> for $from {
            fn from(t: $to) -> Self {
                match t {
                    $( $to::$arm (o) => {
                        $(let $vtt = map_vtn!(o.$vtn(), $vtn);)*
                        $from :: $f { $($v: $c,)* $($vtt,)* }
                    },)*
                    $to::Five(o) => Operands::Mad {
                        dst:  Instruction::dst_to_register(o.dst()),
                        src1: Instruction::src_to_register(o.src1()),
                        src2: Instruction::src_to_register(o.src2()),
                        src3: Instruction::src_to_register(o.src3()),
                        desc: Some(o.desc()),
                    },
                    $to::FiveI(o) => Operands::Mad {
                        dst:  Instruction::dst_to_register(o.dst()),
                        src1: Instruction::src_to_register(o.src1()),
                        src2: Instruction::src_to_register(o.src2()),
                        src3: Instruction::src_to_register(o.src3()),
                        desc: None,
                    },
                    $to::Zero => $from::Zero,
                    $to::Unknown => $from::Unknown,
                }
            }
        }

        impl From<$from> for $to {
            fn from(t: $from) -> Self {
                match t {
                    $( $from::$f { $($v: $c,)* $($vtt,)* } => instruct! ( $armty { $($vtn: $vtt),* } ),)*
                    $from::Mad {
                        dst,
                        src1,
                        src2,
                        src3,
                        desc: Some(desc),
                    } => {
                        instruct!(InstructionFormat5 { dst: dst, src1: src1, src2: src2, src3: src3, desc: desc })
                    },
                    $from::Mad {
                        dst,
                        src1,
                        src2,
                        src3,
                        desc: None,
                    } => {
                        instruct!(InstructionFormat5I { dst: dst, src1: src1, src2: src2, src3: src3 })
                    },
                    $from::Zero => $to::Zero,
                    $from::Unknown => $to::Unknown,
                }
            }
        }
        impl $to {
            fn $set_opcode(&mut self, op: OpCode) {
                let op = op.bin_id();
                match self {
                    $( $to::$arm (o) => {
                        o.set_opc(op)
                    },)*
                    $to::Five(o) => o.set_opc(op >> 3),
                    $to::FiveI(o) => o.set_opc(op >> 3),
                    _ => {},
                }
            }
        }
    }
}

instructs! {
    Operands,
    InstructionFormat,
    set_opcode,
    TwoArguments { inverse: false } = One InstructionFormat1 { dst => dst, src1 => src1, src2 => src2, desc => desc, idx1 => relative_offset },
    TwoArguments { inverse: true } = OneI InstructionFormat1I { dst => dst, src1 => src1, src2 => src2, desc => desc, idx2 => relative_offset },
    OneArgument { } = OneU InstructionFormat1U { dst => dst, src1 => src1, desc => desc },
    Cmp { } = OneC InstructionFormat1C { src1 => src1, src2 => src2 },
    ControlFlow { } = Two InstructionFormat2 { condop => cond, dst_offset => dst_offset, num => num, refx => refx, refy => refy },
    ControlFlowConstant {} = Three InstructionFormat3 { dst_offset => dst_offset, num => num, const_id => constant_id },
    SetEmit {} = Four InstructionFormat4 { winding => winding, vtxid => vtxid, primemit => primemit },
}

impl BinWrite for Instruction {
    type Args<'a> = ();

    fn write_options<W: Write + std::io::prelude::Seek>(
        &self,
        w: &mut W,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> binrw::prelude::BinResult<()> {
        assert!(
            !matches!(self.operands, Operands::Unknown),
            "cannot write unknown operands"
        );
        let start = w.stream_position()?;
        if matches!(self.operands, Operands::Zero) {
            w.write_type(&((self.opcode.bin_id() as u32) << 26), endian)?;
        }
        let mut format: InstructionFormat = self.operands.into();
        format.set_opcode(self.opcode);
        w.write_type(&format, endian)?;
        let end = w.stream_position()?;
        assert_eq!(
            end - start,
            4,
            "wrote the wrong number of bytes for instruction: {self:?}"
        );
        Ok(())
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operands {
    /// (dst, src1, src2, desc)
    TwoArguments {
        dst: Register,
        src1: Register,
        src2: Register,
        relative_offset: u8,
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
        cond: u8,
        refx: u8,
        refy: u8,
        dst_offset: u16,
        num: u8,
    },
    ControlFlowConstant {
        constant_id: u8,
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
pub fn assemble_program(
    program: &[Instruction],
    to: &mut (impl Write + Seek),
) -> binrw::BinResult<()> {
    for instr in program {
        to.write_type(&instr, binrw::Endian::Little)?;
    }
    Ok(())
}
impl BinRead for Instruction {
    type Args<'a> = ();

    fn read_options<R: std::io::prelude::Read + Seek>(
        reader: &mut R,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> binrw::prelude::BinResult<Self> {
        let instr: u32 = reader.read_type(endian)?;

        let bytes = instr.to_le_bytes();
        let opcode_b = (instr >> 0x1A) as u8;
        assert!(
            opcode_b <= OpCode::maximum_value(),
            "opcode is invalid {opcode_b}"
        );
        let opcode = OpCode::binary_to_op(opcode_b);
        let format = FORMAT_TABLE.iter().find(|(k, o)| *o == opcode).unwrap().0;
        let operands = InstructionFormat::from_bytes(format, bytes).into();
        Ok(Instruction { opcode, operands })
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use binrw::{BinRead, BinWriterExt};
    use pican_core::ops::OpCode;

    use super::{Instruction, Operands};

    #[test]
    fn instr_roundtrip() {
        let ops = [Instruction {
            opcode: OpCode::End,
            operands: Operands::Zero,
        }];
        let mut c = Cursor::new(Vec::new());
        c.write_type(&ops[0], binrw::Endian::Little).unwrap();
        let bin = c.into_inner();
        let rt = Instruction::read_le(&mut Cursor::new(&bin)).unwrap();
        assert_eq!(&ops[0], &rt);
    }
}
