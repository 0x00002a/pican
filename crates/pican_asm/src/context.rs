use std::collections::HashMap;

use pican_core::ir::Float;

use crate::ir::{ProcId, RegisterId, Vec4};

#[derive(Debug)]
pub enum ConstantUniform {
    IVec(Vec4<i32>),
    FVec(Vec4<Float>),
}

#[derive(Debug)]
pub enum ProcInfo {}

#[derive(Debug, Default)]
pub struct AsmContext {
    /// Register -> Constant lookup
    constants: HashMap<RegisterId, ConstantUniform>,
    #[allow(unused)]
    procs: HashMap<ProcId, ProcInfo>,
}

impl AsmContext {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn define_constant(&mut self, reg: RegisterId, value: ConstantUniform) {
        self.constants.insert(reg, value);
    }
}
