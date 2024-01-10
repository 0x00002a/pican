use serde::{Deserialize, Serialize};
use strum::{
    Display, EnumIter, EnumString, EnumVariantNames, IntoEnumIterator, VariantIterator,
    VariantNames,
};

#[derive(
    EnumString,
    EnumIter,
    EnumVariantNames,
    Clone,
    Copy,
    Hash,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    Debug,
    Display,
)]
#[strum(serialize_all = "lowercase")]
#[serde(rename = "lowercase")]
pub enum OpCode {
    Nop,
    End,
    Emit,
    SetEmit,
    Add,
    Dp3,
    Dp4,
    Dph,
    Dst,
    Mul,
    Sge,
    Slt,
    Max,
    Min,
    Ex2,
    Lg2,
    LitP,
    Flr,
    Rcp,
    Rsq,
    Mov,
    Mova,
    Cmp,
    Call,
    For,
    Break,
    BreakC,
    CallC,
    IfC,
    JmpC,
    CallU,
    IfU,
    JmpU,
    Mad,
}

impl OpCode {
    pub fn variants_lookup() -> impl Iterator<Item = (&'static str, Self)> {
        Self::VARIANTS.iter().copied().zip(Self::iter())
    }
}

pub enum OperandWidth {
    Wide,
    Narrow,
}
pub enum OperandKind {
    Input(),
}
