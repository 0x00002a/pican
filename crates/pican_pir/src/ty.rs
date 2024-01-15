use serde::{Deserialize, Serialize};

use pican_core::register::RegisterKind;

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Debug, Deserialize, Hash)]
pub enum Ty {
    Register(RegisterKind),
    Uniform(UniformTy),
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Debug, Deserialize, Hash)]
pub enum UniformTy {
    Bool,
    Integer,
    Float,
}
