use std::str::FromStr;

use serde::{Deserialize, Serialize};
use strum::{EnumIter, IntoEnumIterator};

/// Every kind of register
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, EnumIter)]
#[serde(rename_all = "snake_case")]
pub enum RegisterKind {
    Input,
    Output,
    /// Both source and dest
    Scratch,
    FloatingVecUniform,
    IntegerVecUniform,
    BoolUniform,
}

#[derive(Clone, Copy)]
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
    /// use pican::register::{RegisterType, RegisterKind};
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
impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}{}", self.kind.prefix(), self.index))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseRegisterKindError {
    #[error("input is the empty string")]
    EmptyInput,
    #[error("unrecognised prefix '{0}'")]
    UnrecognisedPrefix(char),
}
#[derive(thiserror::Error, Debug)]
pub enum ParseRegisterError {
    #[error("input is too short")]
    TooShort,
    #[error(transparent)]
    Kind(#[from] ParseRegisterKindError),
    #[error("index is too large for this register kind")]
    IndexOutOfBounds,
    #[error(transparent)]
    Index(<usize as FromStr>::Err),
}
impl FromStr for RegisterKind {
    type Err = ParseRegisterKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(ParseRegisterKindError::EmptyInput);
        }
        let prefix = s.chars().next().unwrap();
        REGISTER_INFOS
            .iter()
            .find(|i| i.prefix == prefix)
            .map(|i| i.kind)
            .ok_or(ParseRegisterKindError::UnrecognisedPrefix(prefix))
    }
}

impl FromStr for Register {
    type Err = ParseRegisterError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 2 {
            return Err(ParseRegisterError::TooShort);
        }
        let kind = s.parse()?;
        let index = s[1..].parse().map_err(ParseRegisterError::Index)?;
        Ok(Register { kind, index })
    }
}

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
