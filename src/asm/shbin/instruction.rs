use std::io::{Seek, Write};

use crate::{
    ir::SwizzleDim,
    ops::{CmpOp, OpCode},
    register::{Register, RegisterKind},
};
use binrw::{binrw, BinRead, BinReaderExt, BinWrite, BinWriterExt};
use modular_bitfield::prelude::*;
use strum::EnumDiscriminants;
use typesum::sumtype;

use crate::asm::ir::SwizzleDims;

const OPCODE_OFFSET: u32 = 0x1a;

#[bitfield(filled = false)]
#[derive(Debug, BitfieldSpecifier, Clone, Copy, PartialEq, Eq, BinRead, BinWrite)]
#[br(try_map = Self::from_bytes)]
#[bw(map = |&x| Self::into_bytes(x))]
pub struct ComponentMask {
    #[bits = 1]
    pub w: bool,
    #[bits = 1]
    pub z: bool,
    #[bits = 1]
    pub y: bool,
    #[bits = 1]
    pub x: bool,
}
impl From<SwizzleDims> for ComponentMask {
    fn from(value: SwizzleDims) -> Self {
        value.iter().fold(
            Self::new()
                .with_x(false)
                .with_y(false)
                .with_z(false)
                .with_w(false),
            |s, v| match v {
                crate::ir::SwizzleDim::X => s.with_x(true),
                crate::ir::SwizzleDim::Y => s.with_y(true),
                crate::ir::SwizzleDim::Z => s.with_z(true),
                crate::ir::SwizzleDim::W => s.with_w(true),
            },
        )
    }
}
impl Default for ComponentMask {
    fn default() -> Self {
        Self::new()
            .with_x(true)
            .with_y(true)
            .with_z(true)
            .with_w(true)
    }
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

impl From<SwizzleDim> for Component {
    fn from(value: SwizzleDim) -> Self {
        match value {
            SwizzleDim::X => Self::X,
            SwizzleDim::Y => Self::Y,
            SwizzleDim::Z => Self::Z,
            SwizzleDim::W => Self::W,
        }
    }
}

#[bitfield(bits = 8)]
#[derive(Debug, BitfieldSpecifier, PartialEq, Eq, Clone, Copy)]
pub struct ComponentSelector {
    pub w: Component,
    pub z: Component,
    pub y: Component,
    pub x: Component,
}

impl ComponentSelector {
    pub fn as_u8(self) -> u8 {
        self.bytes[0]
    }
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
impl From<SwizzleDims> for ComponentSelector {
    fn from(value: SwizzleDims) -> Self {
        match value.len() {
            0 => Self::default(),
            1 => Self::new()
                .with_x(value[0].into())
                .with_y(value[0].into())
                .with_z(value[0].into())
                .with_w(value[0].into()),
            2 => Self::new()
                .with_x(value[0].into())
                .with_y(value[1].into())
                .with_z(value[1].into())
                .with_w(value[1].into()),
            3 => Self::new()
                .with_x(value[0].into())
                .with_y(value[1].into())
                .with_z(value[2].into())
                .with_w(value[2].into()),
            4 => Self::new()
                .with_x(value[0].into())
                .with_y(value[1].into())
                .with_z(value[2].into())
                .with_w(value[3].into()),
            _ => unreachable!(),
        }
    }
}
impl Default for ComponentSelector {
    fn default() -> Self {
        Self::new()
            .with_x(Component::X)
            .with_y(Component::Y)
            .with_z(Component::Z)
            .with_w(Component::W)
    }
}

#[bitfield(filled = false)]
#[derive(Debug, BitfieldSpecifier)]
pub struct OperandSource {
    #[bits = 1]
    pub negate: bool,
    #[bits = 8]
    pub selector: ComponentSelector,
}
impl Default for OperandSource {
    fn default() -> Self {
        Self::new()
            .with_negate(false)
            .with_selector(Default::default())
    }
}
impl From<SwizzleDims> for OperandSource {
    fn from(value: SwizzleDims) -> Self {
        Self::new().with_negate(false).with_selector(value.into())
    }
}

#[bitfield(bits = 64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, BinRead, BinWrite)]
#[brw(little)]
pub struct OperandDescriptor {
    #[bits = 4]
    pub destination_mask: ComponentMask,
    #[bits = 9]
    pub s1: OperandSource,
    #[bits = 9]
    pub s2: OperandSource,
    #[bits = 9]
    pub s3: OperandSource,
    #[skip]
    _unknown: B33,
}
impl OperandDescriptor {
    pub fn as_u64(self) -> u64 {
        u64::from_le_bytes(self.into_bytes())
    }
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat1 {
    desc: B7,
    src2: B5,
    src1: B7,
    idx1: B2,
    dst: B5,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B6,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat1U {
    desc: B7,
    #[skip]
    __: B5,
    src1: B7,
    idx1: B2,
    dst: B5,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B6,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat1I {
    desc: B7,
    src2: B7,
    src1: B5,
    idx2: B2,
    dst: B5,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B6,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat1C {
    desc: B7,
    src2: B5,
    src1: B7,
    idx1: B2,
    cmpy: B3,
    cmpx: B3,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B5,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat2 {
    num: B8,
    #[skip]
    __: B2,
    dst_offset: B12,
    condop: B2,
    refy: B1,
    refx: B1,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B6,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat3 {
    num: B8,
    #[skip]
    __: B2,
    dst_offset: B12,
    const_id: B4,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B6,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat4 {
    #[skip]
    _padding: B22,
    winding: B1,
    primemit: B1,
    vtxid: B2,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B6,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat5 {
    desc: B5,
    src3: B5,
    src2: B7,
    src1: B5,
    idx2: B2,
    dst: B5,
    #[allow(unused)] // clippy can't work out that we write this for some reason
    opc: B3,
}

#[bitfield(bits = 32)]
#[derive(Debug, BinWrite, BinRead)]
pub struct InstructionFormat5I {
    desc: B5,
    src3: B7,
    src2: B5,
    src1: B5,
    idx3: B2,
    dst: B5,
    #[allow(unused)] // clippy can't work out that we write this for some reason
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
            Operands::OneArgument {
                dst, src1, desc, ..
            } => {
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
                desc,
                ..
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
            Operands::Cmp {
                src1,
                src2,
                cmpx,
                cmpy,
                desc,
                ..
            } => {
                let pattern = lookup_pat(desc);
                format!(
                    "{}{}{}, {}, {}, {}{}{}",
                    if pattern.s1().negate() { "-" } else { "" },
                    src1,
                    pattern.s1().selector(),
                    cmpx,
                    cmpy,
                    if pattern.s2().negate() { "-" } else { "" },
                    src2,
                    pattern.s2().selector(),
                )
            }
            Operands::SetEmit { .. } => todo!(),
            Operands::ControlFlow {
                cond, refx, refy, ..
            } => match cond {
                0x0 => format!("cmp.x == {refx} || cmp.y == {refy}"),
                0x1 => format!("cmp.x == {refx} && cmp.y == {refy}"),
                0x2 => format!("cmp.x == {refx}"),
                0x3 => format!("cmp.y == {refy}"),
                _ => unreachable!(),
            },
            Operands::Zero => "".to_owned(),
            Operands::Unknown => todo!(),
            Operands::ControlFlowConstant { .. } => todo!(),
        };
        format!("{} {}", self.opcode, operands)
    }
}

pub fn src_idx(r: Register) -> u8 {
    let off = match r.kind {
        RegisterKind::Input => 0x0,
        RegisterKind::Scratch => 0x10,
        RegisterKind::FloatingVecUniform => 0x20,
        _ => panic!("register is invalid kind for src {:?}", r.kind),
    };
    off + r.index as u8
}
pub fn dst_idx(r: Register) -> u8 {
    let off = match r.kind {
        RegisterKind::Output => 0x0,
        RegisterKind::Scratch => 0x10,
        _ => panic!("register is invalid kind for dst {:?}", r.kind),
    };
    off + r.index as u8
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

impl BinWrite for Instruction {
    type Args<'a> = ();

    fn write_options<W: Write + std::io::prelude::Seek>(
        &self,
        w: &mut W,
        endian: binrw::Endian,
        _: Self::Args<'_>,
    ) -> binrw::prelude::BinResult<()> {
        assert!(
            !matches!(self.operands, Operands::Unknown),
            "cannot write unknown operands"
        );
        let start = w.stream_position()?;
        if matches!(self.operands, Operands::Zero) {
            w.write_type(&((self.opcode.bin_id() as u32) << OPCODE_OFFSET), endian)?;
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
pub trait OpCodeInstructionFormat {
    fn instruction_format(self) -> InstructionFormatKind;
}
impl OpCodeInstructionFormat for OpCode {
    fn instruction_format(self) -> InstructionFormatKind {
        let r = FORMAT_TABLE[self as u8 as usize].0;
        debug_assert_eq!(FORMAT_TABLE.iter().find(|(_, o)| *o == self).unwrap().0, r);
        r
    }
}

//pub const INSTR_FMT_TO_OPERANDS_KIND = &[];

#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(name(OperandsKind))]
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
        relative_offset: u8,
        desc: u8,
    },
    Cmp {
        src1: Register,
        src2: Register,
        desc: u8,
        /// address register index for src1
        adx1: u8,
        cmpy: CmpOp,
        cmpx: CmpOp,
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
        adx: u8,
        desc: u8,
        inverse: bool,
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
        _: Self::Args<'_>,
    ) -> binrw::prelude::BinResult<Self> {
        let instr: u32 = reader.read_type(endian)?;

        let bytes = instr.to_le_bytes();
        let opcode_b = (instr >> OPCODE_OFFSET) as u8;
        assert!(
            opcode_b <= OpCode::maximum_value(),
            "opcode is invalid {opcode_b}"
        );
        let opcode = OpCode::binary_to_op(opcode_b);
        let format = FORMAT_TABLE.iter().find(|(_, o)| *o == opcode).unwrap().0;
        let operands = InstructionFormat::from_bytes(format, bytes).into();
        Ok(Instruction { opcode, operands })
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
    ($v:ident, cmpx: $s:ident) => {
        $v.set_cmpx($s as u8)
    };
    ($v:ident, cmpy: $s:ident) => {
        $v.set_cmpy($s as u8)
    };
    ($v:ident, $t:ident: $d:ident) => {
        paste::paste! { $v.[<set_ $t>]($d) }
    };
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

    ($vr:expr, cmpx) => {
        CmpOp::from_u8($vr)
    };

    ($vr:expr, cmpy) => {
        CmpOp::from_u8($vr)
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

macro_rules! instructs {
    ($from:ident, $to:ident, $kind_map:ident, $set_opcode:ident, $($f:ident { $($v:ident: $c:expr),* } = $arm:ident $armty:ident { $($vtn:ident => $vtt:ident),* },)*) => {
        impl From<$to> for $from {
            fn from(t: $to) -> Self {
                match t {
                    $( $to::$arm (o) => {
                        $(let $vtt = map_vtn!(o.$vtn(), $vtn);)*
                        $from :: $f { $($v: $c,)* $($vtt,)* }
                    },)*
                    $to::Zero => $from::Zero,
                    $to::Unknown => $from::Unknown,
                }
            }
        }

        impl From<$from> for $to {
            fn from(t: $from) -> Self {
                match t {
                    $( $from::$f { $($v: $c,)* $($vtt,)* } => instruct! ( $armty { $($vtn: $vtt),* } ),)*
                    $from::Zero => $to::Zero,
                    $from::Unknown => $to::Unknown,
                }
            }
        }
        impl $to {
            fn $set_opcode(&mut self, op: OpCode) {
                let op = op.bin_id();
                #[allow(unreachable_patterns)]
                match self {
                    $to::Five(o) => o.set_opc(op >> 3),
                    $to::FiveI(o) => o.set_opc(op >> 3),
                    $to::OneC(o) => o.set_opc(op >> 1),
                    $( $to::$arm (o) => {
                        o.set_opc(op)
                    },)*
                    _ => {},
                }
            }
        }

        paste::paste! {
            pub const $kind_map: &[([<$to Kind>], [<$from Kind>])] = &[$(([<$to Kind>]::$arm, [<$from Kind>]::$f),)* ([<$to Kind>]::Unknown, [<$from Kind>]::Unknown), ([<$to Kind>]::Zero, [<$from Kind>]::Zero)];
        }
    }
}

instructs! {
    Operands,
    InstructionFormat,
    INSTR_FMT_TO_OPERANDS_KIND,
    set_opcode,
    TwoArguments { inverse: false } = One InstructionFormat1 { dst => dst, src1 => src1, src2 => src2, desc => desc, idx1 => relative_offset },
    TwoArguments { inverse: true } = OneI InstructionFormat1I { dst => dst, src1 => src1, src2 => src2, desc => desc, idx2 => relative_offset },
    OneArgument { } = OneU InstructionFormat1U { dst => dst, src1 => src1, desc => desc, idx1 => relative_offset },
    Cmp { } = OneC InstructionFormat1C { src1 => src1, src2 => src2, idx1 => adx1, cmpx => cmpx, cmpy => cmpy, desc => desc },
    ControlFlow { } = Two InstructionFormat2 { condop => cond, dst_offset => dst_offset, num => num, refx => refx, refy => refy },
    ControlFlowConstant {} = Three InstructionFormat3 { dst_offset => dst_offset, num => num, const_id => constant_id },
    SetEmit {} = Four InstructionFormat4 { winding => winding, vtxid => vtxid, primemit => primemit },
    Mad { inverse: false } = Five InstructionFormat5 { src1 => src1, src2 => src2, src3 => src3, idx2 => adx, dst => dst, desc => desc },
    Mad { inverse: true } = FiveI InstructionFormat5I { src1 => src1, src2 => src2, src3 => src3, idx3 => adx, dst => dst, desc => desc },
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::{ir::SwizzleDim, ops::OpCode};
    use binrw::{BinRead, BinWriterExt};
    use copy_arrayvec::CopyArrayVec;

    use crate::asm::shbin::instruction::{Component, ComponentSelector};

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
    #[test]
    fn comp_selector_from_swizzle_dims() {
        assert_eq!(
            ComponentSelector::from(CopyArrayVec::from_iter([
                SwizzleDim::X,
                SwizzleDim::X,
                SwizzleDim::Y
            ])),
            ComponentSelector::new()
                .with_x(Component::X)
                .with_y(Component::X)
                .with_z(Component::Y)
                .with_w(Component::Y)
        )
    }
}
