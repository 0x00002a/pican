use serde::{Deserialize, Serialize};

/// Every kind of register
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RegisterKind {
    Input,
    Output,
    /// Both source and dest
    Scratch,
    FloatingVecUniform,
    IntegerVecUniform,
    BoolUniform,
}

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
