use bumpalo::Bump;

use crate::diagnostics::Diagnostics;

pub struct PicanContext {
    pub diag: Diagnostics,
}

#[derive(Debug, Default)]
pub struct IrContext {
    arena: Bump,
}

impl IrContext {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn arena(&self) -> &Bump {
        &self.arena
    }
}
