use binrw::binrw;
use modular_bitfield::prelude::*;

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

#[bitfield]
#[binrw]
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub enum Instruction {
    /// (dst, src1, src2, desc)
    Add(u8, u8, u8, u8),
    Dp3(u8, u8, u8, u8),
    Dp4(u8, u8, u8, u8),
    Dph(u8, u8, u8, u8),
    Dst(u8, u8, u8, u8),

    /// (dst, src1, desc)
    Ex2(u8, u8, u8),
    Lg2(u8, u8, u8),
    Litp(u8, u8, u8),

    /// (dst, src1, src2, desc)
    Mul(u8, u8, u8, u8),
    Sge(u8, u8, u8, u8),
    Slt(u8, u8, u8, u8),

    /// (dst, src1, desc)
    Flr(u8, u8, u8),

    /// (dst, src1, src2, desc)
    Max(u8, u8, u8, u8),
    Min(u8, u8, u8, u8),

    /// (dst, src1, desc)
    Rcp(u8, u8, u8),
    Rsq(u8, u8, u8),

    Mova(u8, u8, u8),
    Mov(u8, u8, u8),

    /// (dst, src1, src2, desc)
    Dphi(u8, u8, u8, u8),
    Dsti(u8, u8, u8, u8),
    Sgei(u8, u8, u8, u8),
    Slti(u8, u8, u8, u8),

    /// ()
    Break,
    Nop,
    End,

    /// (cond, dst, num)
    Breakc(u8, u16, u8),
    Call(u8, u16, u8),
    Callc(u8, u16, u8),

    /// (dst, num)
    Callu(u16, u8),
    Ifu(u16, u8),

    /// (cond, dst, num)
    Ifc(u8, u16, u8),

    /// (dst, num)
    Loop(u16, u8),

    /// ()
    Emit,

    /// (vtxid, winding, primemit)
    Setemit(u8, u8, u8),

    /// (cond, dst, num)
    Jmpc(u8, u16, u8),

    /// (dst, num)
    Jmpu(u16, u8),

    /// (src1, src2)
    Cmp(u8, u8),

    /// (dst, src1, src2, src3)
    Madi(u8, u8, u8, u8),

    /// (dst, src1, src2, src3, desc)
    Mad(u8, u8, u8, u8, u8),

    /// (opc)
    Unknown(u8),
}

impl Instruction {
    pub fn dst_to_register(index: u8) -> String {
        match index {
            0x00..=0x0F => format!("o{}", index),
            0x10..=0x1F => format!("r{}", index - 0x10),
            _ => unreachable!(),
        }
    }

    pub fn src_to_register(index: u8) -> String {
        match index {
            0x00..=0x0F => format!("v{}", index),
            0x10..=0x1F => format!("r{}", index - 0x10),
            0x20..=0x7F => format!("c{}", index - 0x20),
            _ => unreachable!(),
        }
    }

    pub fn to_asm(&self, operands: &[OperandDescriptor]) -> String {
        match self {
            Self::Add(dst, src1, src2, desc)
            | Self::Dp3(dst, src1, src2, desc)
            | Self::Dp4(dst, src1, src2, desc)
            | Self::Dph(dst, src1, src2, desc)
            | Self::Dst(dst, src1, src2, desc)
            | Self::Mul(dst, src1, src2, desc)
            | Self::Sge(dst, src1, src2, desc)
            | Self::Slt(dst, src1, src2, desc)
            | Self::Max(dst, src1, src2, desc)
            | Self::Min(dst, src1, src2, desc) => {
                let pattern = &operands[*desc as usize];
                println!("desc: {desc:02X}, {pattern:?}");
                format!(
                    "{} {}{}, {}{}{}, {}{}{}",
                    match self {
                        Self::Add(_, _, _, _) => "add",
                        Self::Dp3(_, _, _, _) => "dp3",
                        Self::Dp4(_, _, _, _) => "dp4",
                        Self::Dph(_, _, _, _) => "dph",
                        Self::Dst(_, _, _, _) => "dst",
                        Self::Mul(_, _, _, _) => "mul",
                        Self::Sge(_, _, _, _) => "sge",
                        Self::Slt(_, _, _, _) => "slt",
                        Self::Max(_, _, _, _) => "max",
                        Self::Min(_, _, _, _) => "min",
                        _ => unreachable!(),
                    },
                    Self::dst_to_register(*dst),
                    pattern.destination_mask(),
                    if pattern.s1().negate() { "-" } else { "" },
                    Self::src_to_register(*src1),
                    pattern.s1().selector(),
                    if pattern.s2().negate() { "-" } else { "" },
                    Self::src_to_register(*src2),
                    pattern.s2().selector(),
                )
            }

            Self::Ex2(dst, src1, desc)
            | Self::Lg2(dst, src1, desc)
            | Self::Litp(dst, src1, desc)
            | Self::Flr(dst, src1, desc)
            | Self::Rcp(dst, src1, desc)
            | Self::Rsq(dst, src1, desc)
            | Self::Mova(dst, src1, desc)
            | Self::Mov(dst, src1, desc) => {
                let pattern = &operands[*desc as usize];
                println!("desc: {desc:02X}, {pattern:?}");
                format!(
                    "{} {}{}, {}{}{}",
                    match self {
                        Self::Ex2(_, _, _) => "ex2",
                        Self::Lg2(_, _, _) => "lg2",
                        Self::Litp(_, _, _) => "litp",
                        Self::Flr(_, _, _) => "flr",
                        Self::Rcp(_, _, _) => "rcp",
                        Self::Rsq(_, _, _) => "rsq",
                        Self::Mova(_, _, _) => "mova",
                        Self::Mov(_, _, _) => "mov",
                        _ => unreachable!(),
                    },
                    Self::dst_to_register(*dst),
                    pattern.destination_mask(),
                    if pattern.s1().negate() { "-" } else { "" },
                    Self::src_to_register(*src1),
                    pattern.s1().selector(),
                )
            }

            Self::Dphi(_, _, _, _) => todo!(),
            Self::Dsti(_, _, _, _) => todo!(),
            Self::Sgei(_, _, _, _) => todo!(),
            Self::Slti(_, _, _, _) => todo!(),

            Self::Break => "break".into(),
            Self::Nop => "nop".into(),
            Self::End => "end".into(),

            Self::Breakc(_, _, _) => todo!(),
            Self::Call(_, _, _) => todo!(),
            Self::Callc(_, _, _) => todo!(),

            Self::Callu(_, _) => todo!(),
            Self::Ifu(_, _) => todo!(),

            Self::Ifc(_, _, _) => todo!(),

            Self::Loop(_, _) => todo!(),

            Self::Emit => "emit".into(),

            Self::Setemit(_, _, _) => todo!(),

            Self::Jmpc(_, _, _) => todo!(),

            Self::Jmpu(_, _) => todo!(),

            Self::Cmp(_, _) => todo!(),

            Self::Madi(_, _, _, _) => todo!(),

            Self::Mad(dst, src1, src2, src3, desc) => {
                let pattern = &operands[*desc as usize];
                println!("desc: {desc:02X}, {pattern:?}");
                format!(
                    "mad {}{}, {}{}{}, {}{}{}, {}{}{}",
                    Self::dst_to_register(*dst),
                    pattern.destination_mask(),
                    if pattern.s1().negate() { "-" } else { "" },
                    Self::src_to_register(*src1),
                    pattern.s1().selector(),
                    if pattern.s2().negate() { "-" } else { "" },
                    Self::src_to_register(*src2),
                    pattern.s2().selector(),
                    if pattern.s3().negate() { "-" } else { "" },
                    Self::src_to_register(*src3),
                    pattern.s3().selector(),
                )
            }

            Self::Unknown(_) => todo!(),
        }
    }
}

pub fn disassemble_blob(blob: &[u32]) -> Vec<Instruction> {
    let mut rv = vec![];

    for &instr in blob {
        let bytes = instr.to_le_bytes();
        let opcode = (instr >> 0x1A) as u8;
        rv.push(match opcode {
            0x00..=0x04 | 0x08..=0x0A | 0x0C | 0x0D => {
                let instr = InstructionFormat1::from_bytes(bytes);
                println!("{:?}", instr);
                (match instr.opc() {
                    0x00 => Instruction::Add,
                    0x01 => Instruction::Dp3,
                    0x02 => Instruction::Dp4,
                    0x03 => Instruction::Dph,
                    0x04 => Instruction::Dst,

                    0x08 => Instruction::Mul,
                    0x09 => Instruction::Sge,
                    0x0A => Instruction::Slt,

                    0x0C => Instruction::Max,
                    0x0D => Instruction::Min,

                    _ => unreachable!(),
                })(instr.dst(), instr.src1(), instr.src2(), instr.desc())
            }
            0x05..=0x07 | 0x0B | 0x0E | 0x0F | 0x12 | 0x13 => {
                let instr = InstructionFormat1U::from_bytes(bytes);
                println!("{:?}", instr);
                (match instr.opc() {
                    0x05 => Instruction::Ex2,
                    0x06 => Instruction::Lg2,
                    0x07 => Instruction::Litp,

                    0x0B => Instruction::Flr,

                    0x0E => Instruction::Rcp,
                    0x0F => Instruction::Rsq,

                    0x12 => Instruction::Mova,
                    0x13 => Instruction::Mov,

                    _ => unreachable!(),
                })(instr.dst(), instr.src1(), instr.desc())
            }
            0x18..=0x1B => {
                let instr = InstructionFormat1I::from_bytes(bytes);
                println!("{:?}", instr);
                (match instr.opc() {
                    0x18 => Instruction::Dphi,
                    0x19 => Instruction::Dsti,
                    0x1A => Instruction::Sgei,
                    0x1B => Instruction::Slti,

                    _ => unreachable!(),
                })(instr.dst(), instr.src1(), instr.src2(), instr.desc())
            }
            0x20 => Instruction::Break,
            0x21 => Instruction::Nop,
            0x22 => Instruction::End,
            0x23..=0x25 | 0x28 | 0x2C => {
                let instr = InstructionFormat2::from_bytes(bytes);
                println!("{:?}", instr);
                (match instr.opc() {
                    0x23 => Instruction::Breakc,
                    0x24 => Instruction::Call,
                    0x25 => Instruction::Callc,

                    0x28 => Instruction::Ifc,

                    0x2C => Instruction::Jmpc,

                    _ => unreachable!(),
                })(instr.condop(), instr.dst(), instr.num())
            }
            0x26 | 0x27 | 0x29 | 0x2D => {
                let instr = InstructionFormat3::from_bytes(bytes);
                println!("{:?}", instr);
                (match instr.opc() {
                    0x26 => Instruction::Callu,
                    0x27 => Instruction::Ifu,
                    0x29 => Instruction::Loop,

                    0x2D => Instruction::Jmpu,

                    _ => unreachable!(),
                })(instr.dst(), instr.num())
            }
            0x2A => Instruction::Emit,
            0x2B => {
                let instr = InstructionFormat4::from_bytes(bytes);
                println!("{:?}", instr);
                Instruction::Setemit(instr.vtxid(), instr.winding(), instr.primemit())
            }
            0x2E | 0x2F => {
                let instr = InstructionFormat1C::from_bytes(bytes);
                println!("{:?}", instr);
                Instruction::Cmp(instr.src1(), instr.src2())
            }
            0x38..=0x3F => {
                let instr = InstructionFormat5::from_bytes(bytes);
                println!("{:?}", instr);
                Instruction::Mad(
                    instr.dst(),
                    instr.src1(),
                    instr.src2(),
                    instr.src3(),
                    instr.desc(),
                )
            }
            0x30..=0x37 => {
                let instr = InstructionFormat5I::from_bytes(bytes);
                println!("{:?}", instr);
                Instruction::Madi(instr.dst(), instr.src1(), instr.src2(), instr.src3())
            }
            0x10 | 0x11 | 0x14..=0x17 | 0x1C..=0x1F => Instruction::Unknown(opcode),
            _ => unreachable!(),
        })
    }

    rv
}
