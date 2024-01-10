use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter, IntoEnumIterator};

/// Every kind of register
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EnumIter)]
pub enum RegisterKind {
    Input,
    Output,
    /// Both source and dest
    Scratch,
    FloatingVecUniform,
    IntegerVecUniform,
    BoolUniform,
}

struct RegisterKindInfo {
    num: u8,
    kind: RegisterKind,
    prefix: char,
}

const REGISTER_INFOS: &[RegisterKindInfo] = &[
    RegisterKindInfo {
        num: 15,
        kind: RegisterKind::Input,
        prefix: 'v',
    },
    RegisterKindInfo {
        num: 15,
        kind: RegisterKind::Output,
        prefix: 'o',
    },
    RegisterKindInfo {
        num: 15,
        kind: RegisterKind::Scratch,
        prefix: 'r',
    },
    RegisterKindInfo {
        num: 95,
        kind: RegisterKind::FloatingVecUniform,
        prefix: 'c',
    },
    RegisterKindInfo {
        num: 3,
        kind: RegisterKind::IntegerVecUniform,
        prefix: 'i',
    },
    RegisterKindInfo {
        num: 15,
        kind: RegisterKind::BoolUniform,
        prefix: 'b',
    },
];

impl RegisterKind {
    /// Check if register can be used as a given type
    ///
    /// ```
    /// assert!(RegisterKind::Input.is_type(RegisterType::Input));
    /// assert!(!RegisterKind::Output.is_type(RegisterType::Input));
    /// assert!(RegisterKind::Scratch.is_type(RegisterType::Input));
    /// assert!(RegisterKind::Scratch.is_type(RegisterType::Output));
    /// ```
    pub fn is_type(self, ty: RegisterType) -> bool {
        match ty {
            RegisterType::Special => matches!(
                self,
                Self::FloatingVecUniform | Self::IntegerVecUniform | Self::BoolUniform
            ),
            RegisterType::Input => matches!(self, Self::Input | Self::Scratch),
            RegisterType::Output => matches!(self, Self::Output | Self::Scratch),
        }
    }
    pub fn prefix(self) -> char {
        REGISTER_INFOS[self as usize].prefix
    }

    pub fn max_index(self) -> usize {
        REGISTER_INFOS[self as usize].num as usize
    }
    pub fn all() -> impl Iterator<Item = Self> {
        Self::iter()
    }
}

/// Types of register
pub enum RegisterType {
    /// Requires special operations to use
    Special,
    /// Valid as source in operand (i.e. can be read from)
    Input,
    /// Valid as destination in operand (i.e. can be written to)
    Output,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct Register {
    pub kind: RegisterKind,
    pub index: usize,
}

pub enum ParseRegisterError {}

impl Register {
    /// Create a new registers
    ///
    /// # Panics
    /// If `index` is too large for the kind
    pub fn new(kind: RegisterKind, index: usize) -> Self {
        assert!(
            index <= kind.max_index(),
            "index {index} is too large for register of kind {kind:?}"
        );
        Self { kind, index }
    }
}
