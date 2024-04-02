use context::TyContext;
use crate::context::PicanContext;
use crate::pir::ir::Module;

pub mod check;
pub mod context;
pub mod ops;
pub mod ty;

pub trait PicanTyCheck {
    fn types_for_module<'a, 'b, 'c: 'b>(&'c self, m: &'b Module<'a>) -> TyContext<'a, 'b>;
}

impl<S: AsRef<str>> PicanTyCheck for PicanContext<S> {
    fn types_for_module<'a, 'b, 'c: 'b>(&'c self, m: &'b Module<'a>) -> TyContext<'a, 'b> {
        TyContext::new(m, &self.diag)
    }
}
