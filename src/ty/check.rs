use crate::diagnostics::FatalErrorEmitted;
use crate::pir;
use pir::ir::Op;

use super::context::TyContext;
use super::ops::slots_for_opcode;

pub(crate) fn check_operation<'a>(
    op: &Op<'a>,
    ctx: &TyContext<'a, '_>,
) -> Result<(), FatalErrorEmitted> {
    let slots = slots_for_opcode(*op.opcode.get());
    for (slot, operand) in slots.slots.iter().zip(op.operands.get().iter()) {
        let ty = ctx.type_of(operand.get())?;
        let ok = slot.allowed_types.iter().any(|t| t.matches(ty));
        if !ok {
            return ctx.emit_slot_mismatch(op.opcode, operand, slot);
        }
    }
    Ok(())
}
