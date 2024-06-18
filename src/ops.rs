use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, EnumString, IntoEnumIterator, VariantNames};

#[derive(
    EnumString,
    EnumIter,
    VariantNames,
    Clone,
    Copy,
    Hash,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    Debug,
    Display,
    PartialOrd,
    Ord,
)]
#[strum(serialize_all = "lowercase")]
#[serde(rename = "lowercase")]
pub enum OpCode {
    /// Adds two vectors component by component; DST[i] = SRC1[i]+SRC2[i] for all i
    Add,
    /// Computes dot product on 3-component vectors; DST = SRC1.SRC2
    Dp3,
    /// Computes dot product on 4-component vectors; DST = SRC1.SRC2
    Dp4,
    /// Computes dot product on a 3-component vector with 1.0 appended to it and a 4-component vector; DST = SRC1.SRC2 (with SRC1 homogenous)
    Dph,
    /// Equivalent to Microsoft's dst instruction: DST = {1, SRC1[1]*SRC2[1], SRC1[2], SRC2[3]}
    Dst,
    /// Computes SRC1's first component exponent with base 2; DST[i] = EXP2(SRC1[0]) for all i
    Ex2,
    /// Computes SRC1's first component logarithm with base 2; DST[i] = LOG2(SRC1[0]) for all i
    Lg2,
    /// Partial lighting computation, may be used in conjunction with EX2, LG2, etc to compute the vertex lighting coefficients. See the Microsoft and ARB docs for more information on how to implement the full lit function; DST = {max(src.x, 0), max(min(src.y, 127.9961), -127.9961), 0, max(src.w, 0)} and it sets the cmp.x and cmp.y flags based on if the respective src.x and src.w components are >= 0.
    LitP,
    /// Multiplies two vectors component by component; DST[i] = SRC1[i].SRC2[i] for all i
    Mul,
    /// Sets output if SRC1 is greater than or equal to SRC2; DST[i] = (SRC1[i] >= SRC2[i]) ? 1.0 : 0.0 for all i
    Sge,
    /// Sets output if SRC1 is strictly less than SRC2; DST[i] = (SRC1[i] < SRC2[i]) ? 1.0 : 0.0 for all i
    Slt,
    /// Computes SRC1's floor component by component; DST[i] = FLOOR(SRC1[i]) for all i
    Flr,
    /// Takes the max of two vectors, component by component; DST[i] = MAX(SRC1[i], SRC2[i]) for all i
    Max,
    /// Takes the min of two vectors, component by component; DST[i] = MIN(SRC1[i], SRC2[i]) for all i
    Min,
    /// Computes the reciprocal of the vector's first component; DST[i] = 1/SRC1[0] for all i
    Rcp,
    /// Computes the reciprocal of the square root of the vector's first component; DST[i] = 1/sqrt(SRC1[0]) for all i
    Rsq,
    /// Move to address register; Casts the float value given by SRC1 to an integer (truncating the fractional part) and assigns the result to (a0.x, a0.y, _, _), respecting the destination component mask.
    MovA,
    /// Moves value from one register to another; DST = SRC1.
    Mov,
    /// Computes dot product on a 3-component vector with 1.0 appended to it and a 4-component vector; DST = SRC1.SRC2 (with SRC1 homogenous)
    DphI,
    /// DST with sources swapped.
    DstI,
    /// Sets output if SRC1 is greater than or equal to SRC2; DST[i] = (SRC1[i] >= SRC2[i]) ? 1.0 : 0.0 for all i
    SgeI,
    /// Sets output if SRC1 is strictly less than SRC2; DST[i] = (SRC1[i] < SRC2[i]) ? 1.0 : 0.0 for all i
    SltI,
    /// Breaks out of LOOP block; do not use while in nested IF/CALL block inside LOOP block.
    Break,
    /// Does literally nothing.
    Nop,
    /// Signals the shader unit that processing for this vertex/primitive is done.
    End,
    /// If condition (see below for details) is true, then breaks out of LOOP block.
    BreakC,
    /// Jumps to DST and executes instructions until it reaches DST+NUM instructions
    Call,
    /// If condition (see below for details) is true, then jumps to DST and executes instructions until it reaches DST+NUM instructions, else does nothing.
    CallC,
    /// Jumps to DST and executes instructions until it reaches DST+NUM instructions if BOOL is true
    CallU,
    /// If condition BOOL is true, then executes instructions until DST, then jumps to DST+NUM; else, jumps to DST.
    IfU,
    /// If condition (see below for details) is true, then executes instructions until DST, then jumps to DST+NUM; else, jumps to DST
    IfC,
    /// Loops over the code between itself and DST (inclusive), performing INT.x+1 iterations in total. First, aL is initialized to INT.y. After each iteration, aL is incremented by INT.z.
    Loop,
    ///    (geometry shader only) Emits a vertex (and primitive if FLAG_PRIMEMIT was set in the corresponding SETEMIT). SETEMIT must be called before this.
    Emit,
    ///     (geometry shader only) Sets VTXID, FLAG_WINDING and FLAG_PRIMEMIT for the next EMIT instruction. VTXID is the ID of the vertex about to be emitted within the primitive, while FLAG_PRIMEMIT is zero if we are just emitting a single vertex and non-zero if are emitting a vertex and primitive simultaneously. FLAG_WINDING controls the output primitive's winding. Note that the output vertex buffer (which holds 4 vertices) is not cleared when the primitive is emitted, meaning that vertices from the previous primitive can be reused for the current one. (this is still a working hypothesis and unconfirmed)
    SetEmit,
    /// If condition (see below for details) is true, then jumps to DST, else does nothing.
    JmpC,
    /// If condition BOOL is true, then jumps to DST, else does nothing. Having bit 0 of NUM = 1 will invert the test, jumping if BOOL is false instead.
    JmpU,
    ///     Sets booleans cmp.x and cmp.y based on the operand's x and y components and the CMPX and CMPY comparison operators respectively. See below for details about operators. It's unknown whether CMP respects the destination component mask or not.
    Cmp,
    ///I    Multiplies two vectors and adds a third one component by component; DST[i] = SRC3[i] + SRC2[i].SRC1[i] for all i; this is not an FMA, the intermediate result is rounded
    MadI,
    ///
    Mad,
    /// An unknown instruction, this is not a valid error reporting path it indicates an instruction we don't know about
    Unknown,
}

impl OpCode {
    pub fn maximum_value() -> u8 {
        63
    }
    pub fn variants_lookup() -> impl Iterator<Item = (&'static str, Self)> {
        Self::VARIANTS.iter().copied().zip(Self::iter())
    }
    pub fn is_no_argument(self) -> bool {
        matches!(self, Self::End | Self::Break | Self::Emit | Self::Nop)
    }
    /// Convert binary identifier to opcode
    ///
    /// # Panics
    /// if b >= 64
    pub const fn binary_to_op(b: u8) -> OpCode {
        assert!(b < 64, "binary value out of range");
        Self::binary_lookup()[b as usize].1
    }

    /// Convert an opcode to binary
    pub fn bin_id(self) -> u8 {
        for (id, v) in Self::binary_lookup() {
            if self == v {
                return id;
            }
        }
        unreachable!()
    }
    pub const fn binary_lookup() -> [(u8, OpCode); 64] {
        use OpCode::*;
        [
            (0x00, Add),
            (0x01, Dp3),
            (0x02, Dp4),
            (0x03, Dph),
            (0x04, Dst),
            (0x05, Ex2),
            (0x06, Lg2),
            (0x07, LitP),
            (0x08, Mul),
            (0x09, Sge),
            (0x0A, Slt),
            (0x0B, Flr),
            (0x0C, Max),
            (0x0D, Min),
            (0x0E, Rcp),
            (0x0F, Rsq),
            (0x10, Unknown),
            (0x11, Unknown),
            (0x12, MovA),
            (0x13, Mov),
            (0x14, Unknown),
            (0x15, Unknown),
            (0x16, Unknown),
            (0x17, Unknown),
            (0x18, DphI),
            (0x19, DstI),
            (0x1A, SgeI),
            (0x1B, SltI),
            (0x1C, Unknown),
            (0x1D, Unknown),
            (0x1E, Unknown),
            (0x1F, Unknown),
            (0x20, Break),
            (0x21, Nop),
            (0x22, End),
            (0x23, BreakC),
            (0x24, Call),
            (0x25, CallC),
            (0x26, CallU),
            (0x27, IfU),
            (0x28, IfC),
            (0x29, Loop),
            (0x2A, Emit),
            (0x2B, SetEmit),
            (0x2C, JmpC),
            (0x2D, JmpU),
            (0x2E, Cmp),
            (0x2F, Cmp),
            (0x30, MadI),
            (0x31, MadI),
            (0x32, MadI),
            (0x33, MadI),
            (0x34, MadI),
            (0x35, MadI),
            (0x36, MadI),
            (0x37, MadI),
            (0x38, Mad),
            (0x39, Mad),
            (0x3A, Mad),
            (0x3B, Mad),
            (0x3C, Mad),
            (0x3D, Mad),
            (0x3E, Mad),
            (0x3F, Mad),
        ]
    }
}

pub enum OperandWidth {
    Wide,
    Narrow,
}

#[derive(
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Clone,
    Copy,
    Debug,
    EnumIter,
    EnumString,
    Display,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
#[strum(serialize_all = "snake_case")]
pub enum CmpOp {
    Eq = 0,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Unknown1,
    Unknown2,
}

impl CmpOp {
    pub fn from_u8(i: u8) -> CmpOp {
        Self::iter().nth(i as usize).unwrap()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Serialize, Debug)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum CondOp {
    Or,
    And,
    X,
    Y,
}
