use std::{collections::HashMap, ops::Range};

use pican_core::{
    ir::{Float, Ident},
    properties::OutputProperty,
    register::Register,
};
use string_interner::StringInterner;

use crate::{
    float24::Float24,
    instrs::InstructionOffset,
    ir::{ProcId, RegisterId, Vec4},
    shbin::{instruction::ComponentMask, IoRegisterBitMask},
};

#[derive(Debug)]
pub enum ConstantUniform {
    IVec(Vec4<i8>),
    FVec(Vec4<Float24>),
    Bool(bool),
}

#[derive(Debug, Clone, Copy)]
pub struct ProcInfo {
    pub instr_start: InstructionOffset,
    pub instr_end: InstructionOffset,
}

pub type SymbolId = string_interner::DefaultSymbol;

#[derive(Debug, Default, Clone)]
pub struct Symbols {
    syms: StringInterner,
}
impl Symbols {
    pub fn iter(&self) -> impl Iterator<Item = (SymbolId, &str)> {
        self.syms.into_iter()
    }
    pub fn lookup_id(&self, id: SymbolId) -> Option<&str> {
        self.syms.resolve(id)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OutputInfo {
    pub property: OutputProperty,
    pub register: RegisterId,
    pub mask: ComponentMask,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BoundUniform {
    pub name: SymbolId,
    pub start_register: Register,
    pub end_register: Register,
}

#[derive(Debug, Default)]
pub struct AsmContext {
    /// Register -> Constant lookup
    pub constants: HashMap<RegisterId, ConstantUniform>,
    pub allocated_registers: HashMap<RegisterId, Register>,
    pub outputs: Vec<OutputInfo>,
    pub uniforms: Vec<BoundUniform>,
    procs: HashMap<ProcId, ProcInfo>,
    pub symbols: Symbols,
    main: Option<ProcId>,
    pub used_input_registers: IoRegisterBitMask,
    pub used_output_registers: IoRegisterBitMask,
}

impl AsmContext {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn define_symbol(&mut self, name: Ident) -> SymbolId {
        self.symbols.syms.get_or_intern(name.as_str())
    }
    pub fn define_constant(&mut self, reg: RegisterId, value: ConstantUniform) {
        self.constants.insert(reg, value);
    }
    pub fn define_proc(&mut self, id: ProcId, info: ProcInfo) {
        self.procs.insert(id, info);
    }
    pub fn main_proc(&self) -> Option<ProcInfo> {
        let k = self.main.or_else(|| {
            if self.procs.len() == 1 {
                Some(self.procs.keys().last().copied().unwrap())
            } else {
                None
            }
        });
        k.and_then(|k| self.procs.get(&k).copied())
    }
}
