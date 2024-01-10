use serde::{Deserialize, Serialize};

use crate::register::RegisterKind;

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Debug, Deserialize, Hash)]
pub enum Ty {
    Register(RegisterKind),
    Uniform(UniformTy),
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Debug, Deserialize, Hash)]
pub struct UniformTy {
    pub size: usize,
}
