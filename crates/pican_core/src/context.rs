use std::ffi::OsString;

use bumpalo::Bump;
use codespan::{FileId, Files};

use crate::diagnostics::Diagnostics;

#[derive(Debug)]
pub struct PicanContext<S: AsRef<str>> {
    pub diag: Diagnostics,
    pub files: Files<S>,
}
impl<S: AsRef<str>> Default for PicanContext<S> {
    fn default() -> Self {
        Self {
            diag: Default::default(),
            files: Default::default(),
        }
    }
}

impl<S: AsRef<str>> PicanContext<S> {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn add_file(&mut self, name: impl Into<OsString>, contents: S) -> FileId {
        self.files.add(name, contents)
    }
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
