use crate::diagnostics::FatalErrorEmitted;
use crate::pir;
use pir::ir::Op;

use super::context::TyContext;
use super::ops::slots_for_opcode;

pub(crate) fn check_operation<'a>(
    op: &Op<'a>,
    ctx: &TyContext<'a, '_>,
) -> Result<(), FatalErrorEmitted> {
    match op {
        Op::Regular { opcode, operands } => {
            let slots = slots_for_opcode(*opcode.get());
            for (slot, operand) in slots.slots.iter().zip(operands.get().iter()) {
                let ty = ctx.type_of(operand.get())?;
                let ok = slot.allowed_types.iter().any(|t| t.matches(ty));
                if !ok {
                    return ctx.emit_slot_mismatch(*opcode, operand, slot);
                }
            }
        }
        Op::Cond(_) => todo!(),
    }
    Ok(())
}
