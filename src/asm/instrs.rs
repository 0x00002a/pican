use std::ops::Range;

use copy_arrayvec::CopyArrayVec;
use serde::{Deserialize, Serialize};

use super::{ir::Instruction, MAX_SHBIN_INSTRUCTIONS};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct InstructionOffset(usize);

impl InstructionOffset {
    pub fn to_words(self) -> u32 {
        self.0 as u32
    }
}

impl std::ops::Add<usize> for InstructionOffset {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl std::ops::Sub<usize> for InstructionOffset {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        Self(self.0 - rhs)
    }
}

#[derive(Debug, Default)]
pub struct InstructionPack {
    instrs: CopyArrayVec<Instruction, MAX_SHBIN_INSTRUCTIONS>,
}

impl InstructionPack {
    pub fn next_offset(&self) -> InstructionOffset {
        InstructionOffset(self.instrs.len())
    }
    pub fn last_offset(&self) -> Option<InstructionOffset> {
        if self.instrs.is_empty() {
            None
        } else {
            Some(InstructionOffset(self.instrs.len() - 1))
        }
    }
    pub fn push(&mut self, instr: impl Into<Instruction>) -> InstructionOffset {
        let offset = self.instrs.len();
        self.instrs.push(instr.into());
        InstructionOffset(offset)
    }
    pub fn select(&self, range: Range<InstructionOffset>) -> &[Instruction] {
        assert!(!self.instrs.is_empty());
        assert!(range.end.0 < self.instrs.len());

        &self.instrs[range.start.0..range.end.0]
    }
    pub fn iter(&self) -> impl Iterator<Item = Instruction> + '_ {
        self.instrs.iter().copied()
    }
}
