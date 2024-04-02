use pican_core::{
    diagnostics::{DiagnosticBuilder, Diagnostics, FatalErrorEmitted},
    ir::{Ident, IrNode},
    ops::OpCode,
};
use pican_pir::{
    bindings::BindingValue,
    ir::{Module, Operand},
};

use crate::{
    check,
    ops::OperandSlot,
    ty::{ContextuallyTyped, Type},
};

pub struct TyContext<'a, 'b> {
    module: &'b Module<'a>,
    diag: &'b Diagnostics,
}

impl<'a, 'b> TyContext<'a, 'b> {
    pub(super) fn new(module: &'b Module<'a>, diag: &'b Diagnostics) -> Self {
        Self { module, diag }
    }

    pub fn type_of(&self, expr: &impl ContextuallyTyped<'a>) -> Result<Type, FatalErrorEmitted> {
        expr.ty_with_ctx(self)
    }
    pub fn lookup(
        &self,
        i: IrNode<Ident<'a>>,
    ) -> Result<IrNode<BindingValue<'a>>, FatalErrorEmitted> {
        self.module.bindings.lookup_with_diag(i, self.diag)
    }

    pub fn check(&self) -> Result<(), FatalErrorEmitted> {
        let mut failed = None;
        for op in self
            .module
            .entry_points
            .iter()
            .flat_map(|ent| ent.get().ops.get().iter())
            .map(|o| o.get())
        {
            if let Err(e) = check::check_operation(op, self) {
                failed.replace(e);
            }
        }
        if let Some(e) = failed {
            Err(e)
        } else {
            Ok(())
        }
    }
    pub(crate) fn emit_slot_mismatch(
        &self,
        op: IrNode<OpCode>,
        operand: &IrNode<Operand<'a>>,
        slot: &OperandSlot,
    ) -> Result<(), FatalErrorEmitted> {
        self.diag.fatal(
            DiagnosticBuilder::error()
                .at(operand)
                .primary("type mismatch")
                .note(&op, format!("for opcode {}", op.get()))
                .note(
                    &operand.get().kind,
                    format!(
                        "expected one of {:?} but got {}",
                        slot.allowed_types,
                        self.type_of(operand.get())?,
                    ),
                )
                .build(),
        )
    }
}
