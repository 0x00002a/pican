use serde::{Deserialize, Serialize};

use pican_core::register::RegisterKind;

#[derive(Clone, Copy, PartialEq, Eq, Serialize, Debug, Deserialize, Hash)]
#[serde(rename_all = "snake_case")]
pub enum UniformTy {
    Bool,
    Integer,
    Float,
}
