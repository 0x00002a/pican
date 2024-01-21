use crate::ir::Instruction;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InstructionOffset(usize);

pub struct InstructionPack {
    instrs: Vec<Instruction>,
}

impl InstructionPack {
    pub fn push(&mut self, instr: impl Into<Instruction>) -> InstructionOffset {
        let offset = self.instrs.len();
        self.instrs.push(instr.into());
        InstructionOffset(offset)
    }
}
