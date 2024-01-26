use std::collections::HashMap;

use pican_core::{
    context::PicanContext,
    diagnostics::{DiagnosticBuilder, Diagnostics, FatalErrorEmitted},
    ir::{Float, Ident, IrNode, SwizzleDims},
    register::{Register, RegisterKind},
};
use pican_pir::{
    bindings::BindingValue,
    ir::{EntryPoint, Module, Operand},
};

use crate::{
    context::{AsmContext, ProcInfo},
    float24::Float24,
    instrs::InstructionPack,
    ir::{self, FreeRegister, Instruction, ProcId, RegHole, RegHoleKind, RegisterId, Vec4},
};

#[derive(Debug)]
struct ProcCache<'i> {
    lookup: HashMap<Ident<'i>, ProcId>,
    next_id: ProcId,
}
impl<'i> Default for ProcCache<'i> {
    fn default() -> Self {
        Self {
            lookup: Default::default(),
            next_id: ProcId::first(),
        }
    }
}

impl<'i> ProcCache<'i> {
    fn id_for(&mut self, name: Ident<'i>) -> ProcId {
        if let Some(p) = self.lookup.get(&name) {
            *p
        } else {
            let id = self.next_id;
            self.next_id = self.next_id.next();
            self.lookup.insert(name, id);
            id
        }
    }
}
struct RegCache {
    lookup: HashMap<Register, RegisterId>,
    next_id: RegisterId,
}
impl Default for RegCache {
    fn default() -> Self {
        Self {
            lookup: Default::default(),
            next_id: RegisterId::first(),
        }
    }
}

impl RegCache {
    fn id_for_reg(&mut self, r: Register) -> RegisterId {
        if let Some(id) = self.lookup.get(&r) {
            *id
        } else {
            let id = self.next_id;
            self.next_id = self.next_id.next();
            self.lookup.insert(r, id);
            id
        }
    }
    fn next_reg_id(&mut self) -> RegisterId {
        let id = self.next_id;
        self.next_id = self.next_id.next();
        id
    }
}

struct LowerCtx<'a, 'm, 'c> {
    asm_ctx: AsmContext,
    asm: InstructionPack,
    procs: ProcCache<'a>,
    pir: &'m Module<'a>,
    #[allow(unused)]
    diag: &'c Diagnostics,
    regs: RegCache,
    ident_to_reg: HashMap<Ident<'a>, RegHole>,
}
impl<'a, 'm, 'c> LowerCtx<'a, 'm, 'c> {
    fn lower_register(&mut self, r: Register) -> RegHole {
        let id = self.id_for_fixed(r);
        RegHole {
            id,
            kind: RegHoleKind::Fixed(r),
        }
    }
    fn resolve_operand_ident(&mut self, ident: &Ident<'a>) -> (RegHole, Option<SwizzleDims<'a>>) {
        match self
            .pir
            .bindings
            .lookup(ident)
            .expect("missing identifier, should've been caught earlier")
            .into_inner()
        {
            BindingValue::SwizzleRegister(v) => (
                *self.ident_to_reg.get(ident).unwrap(),
                Some(v.swizzle.into_inner()),
            ),
            BindingValue::SwizzleVar(v) => {
                // todo: should swizzles compose?
                (
                    self.resolve_operand_ident(v.target.get()).0,
                    Some(v.swizzle.into_inner()),
                )
            }
            BindingValue::Alias(i) => self.resolve_operand_ident(&i),
            _ => (*self.ident_to_reg.get(ident).unwrap(), None),
        }
    }
    fn lower_operand(&mut self, operand: &Operand<'a>) -> ir::Operand {
        let (register, swizzle) = match operand.kind.get() {
            pican_pir::ir::OperandKind::Var(v) => self.resolve_operand_ident(v.get()),
            pican_pir::ir::OperandKind::Register(r) => (self.lower_register(*r.get()), None),
        };
        let swizzle = operand
            .swizzle
            .map(|s| s.into_inner())
            .or(swizzle)
            .map(|s| s.0.into_inner())
            .map(|s| s.iter().copied().collect());
        ir::Operand { register, swizzle }
    }
    fn lower_entry_point(&mut self, ent: &EntryPoint<'a>) -> Result<(), FatalErrorEmitted> {
        let id = self.procs.id_for(*ent.name.get());
        assert!(!ent.ops.get().is_empty(), "no instructions for proc");
        let start = self.asm.next_offset();

        for op in ent.ops.get().iter().map(|p| p.get()) {
            let operands = op
                .operands
                .get()
                .iter()
                .map(|operand| self.lower_operand(operand.get()))
                .collect();
            self.asm.push(Instruction {
                opcode: op.opcode.into_inner(),
                operands,
            });
        }
        let end = self.asm.next_offset();
        self.asm_ctx.define_proc(
            id,
            ProcInfo {
                instr_start: start,
                instr_end: end,
            },
        );
        Ok(())
    }
    fn id_for_fixed(&mut self, r: Register) -> RegisterId {
        let id = self.regs.id_for_reg(r);
        self.asm_ctx.allocated_registers.insert(id, r);
        id
    }
    fn lower(mut self) -> Result<(AsmContext, InstructionPack), FatalErrorEmitted> {
        for (name, value) in self.pir.bindings.entries() {
            let name = *name.get();
            match value.get() {
                pican_pir::bindings::BindingValue::Register(r) => {
                    let r = *r;
                    match r.kind {
                        RegisterKind::Input => self.asm_ctx.used_input_registers.mark_used(r),
                        RegisterKind::Output => self.asm_ctx.used_input_registers.mark_used(r),
                        _ => {}
                    }
                    let reg = self.lower_register(r);
                    self.ident_to_reg.insert(name, reg);
                }
                pican_pir::bindings::BindingValue::Uniform(u) => {
                    let id = self.regs.next_reg_id();
                    self.ident_to_reg.insert(
                        name,
                        RegHole {
                            id,
                            kind: RegHoleKind::Free(FreeRegister {
                                kind: match u.ty.get() {
                                    pican_pir::ty::UniformTy::Bool => RegisterKind::BoolUniform,
                                    pican_pir::ty::UniformTy::Integer => {
                                        RegisterKind::IntegerVecUniform
                                    }
                                    pican_pir::ty::UniformTy::Float => {
                                        RegisterKind::FloatingVecUniform
                                    }
                                },
                            }),
                        },
                    );
                }
                pican_pir::bindings::BindingValue::Constant(c) => match c {
                    pican_pir::ir::ConstantUniform::Integer(i) => {
                        let i = i.get();
                        let id = self.regs.next_reg_id();
                        let conv_i =
                            |i: IrNode<i32>| -> Result<i8, FatalErrorEmitted> {
                                i.into_inner().try_into().map_err(|_| {
                                    self.diag.fatal::<()>(DiagnosticBuilder::error().at(&i).primary(
                                        "integer too large, it must fit in an 8-bit signed",
                                    ).build()).unwrap_err()
                                })
                            };
                        self.asm_ctx.define_constant(
                            id,
                            crate::context::ConstantUniform::IVec(Vec4::new(
                                conv_i(i[0])?,
                                conv_i(i[1])?,
                                conv_i(i[2])?,
                                conv_i(i[3])?,
                            )),
                        );
                        self.ident_to_reg.insert(
                            name,
                            RegHole {
                                id,
                                kind: RegHoleKind::Free(FreeRegister {
                                    kind: RegisterKind::IntegerVecUniform,
                                }),
                            },
                        );
                    }
                    pican_pir::ir::ConstantUniform::Float(i) => {
                        let i = i.get();
                        let id = self.regs.next_reg_id();
                        let conv_f = |i: IrNode<Float>| -> Result<Float24, FatalErrorEmitted> {
                            i.into_inner().try_into().map_err(|e| {
                                self.diag
                                    .fatal::<()>(
                                        DiagnosticBuilder::error()
                                            .at(&i)
                                            .primary(format!("float doesn't fit in gpu format {e}"))
                                            .build(),
                                    )
                                    .unwrap_err()
                            })
                        };
                        self.asm_ctx.define_constant(
                            id,
                            crate::context::ConstantUniform::FVec(Vec4::new(
                                conv_f(i[0])?,
                                conv_f(i[1])?,
                                conv_f(i[2])?,
                                conv_f(i[3])?,
                            )),
                        );
                        self.ident_to_reg.insert(
                            name,
                            RegHole {
                                id,
                                kind: RegHoleKind::Free(FreeRegister {
                                    kind: RegisterKind::IntegerVecUniform,
                                }),
                            },
                        );
                    }
                    pican_pir::ir::ConstantUniform::FloatArray(_) => todo!(),
                },
                pican_pir::bindings::BindingValue::OutputProperty(o) => {
                    assert!(o.alias.is_some());

                    let reg = if let Some(r) = o.register {
                        let r = r.into_inner();
                        self.asm_ctx.used_output_registers.mark_used(r);
                        self.lower_register(r)
                    } else {
                        let id = self.regs.next_reg_id();
                        RegHole {
                            id,
                            kind: RegHoleKind::Free(FreeRegister {
                                kind: RegisterKind::Output,
                            }),
                        }
                    };
                    self.ident_to_reg.insert(name, reg);
                }
                pican_pir::bindings::BindingValue::Input(_) => todo!(),

                pican_pir::bindings::BindingValue::SwizzleRegister(_)
                | pican_pir::bindings::BindingValue::SwizzleVar(_)
                | pican_pir::bindings::BindingValue::Alias(_) => {}
            }
        }
        for ent in self.pir.entry_points {
            self.lower_entry_point(ent.get())?;
        }
        Ok((self.asm_ctx, self.asm))
    }
}

pub fn from_pir<S: AsRef<str>>(
    pir: &Module<'_>,
    ctx: &PicanContext<S>,
) -> Result<(AsmContext, InstructionPack), FatalErrorEmitted> {
    let asm_ctx = AsmContext::new();
    let procs = ProcCache::default();

    LowerCtx {
        asm_ctx,
        procs,
        asm: InstructionPack::default(),
        pir,
        diag: &ctx.diag,
        regs: RegCache::default(),
        ident_to_reg: Default::default(),
    }
    .lower()
}
