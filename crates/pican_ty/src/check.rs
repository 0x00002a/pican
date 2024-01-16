use pican_core::diagnostics::FatalErrorEmitted;
use pican_pir as pir;
use pir::ir::Op;

use crate::{context::TyContext, ops::slots_for_opcode};

pub(crate) fn check_operation<'a, 'b>(
    op: &Op<'a>,
    ctx: &TyContext<'a, 'b>,
) -> Result<(), FatalErrorEmitted> {
    let slots = slots_for_opcode(*op.opcode.get());
    for (slot, operand) in slots.slots.iter().zip(op.operands.get().iter()) {
        let ty = ctx.type_of(operand.get())?;
        let ok = slot.allowed_types.iter().any(|t| t.matches(ty));
        if !ok {
            return ctx.emit_slot_mismatch(op.opcode, operand);
        }
    }
    Ok(())
}
