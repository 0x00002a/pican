use std::collections::HashMap;

use pican_core::{
    context::PicanContext,
    diagnostics::{Diagnostics, FatalErrorEmitted},
    ir::{Ident, SwizzleDims},
    register::{Register, RegisterKind},
};
use pican_pir::{
    bindings::BindingValue,
    ir::{EntryPoint, Module, Operand},
};

use crate::{
    context::AsmContext,
    instrs::InstructionPack,
    ir::{
        self, FreeRegister, Instruction, Operation, ProcId, RegHole, RegHoleKind, RegisterId, Vec4,
    },
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
        let id = self.regs.id_for_reg(r);
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
        self.asm
            .push(Instruction::Directive(crate::ir::Directive::Proc { id }));

        for op in ent.ops.get().iter().map(|p| p.get()) {
            let operands = op
                .operands
                .get()
                .iter()
                .map(|operand| self.lower_operand(operand.get()))
                .collect();
            self.asm.push(Operation {
                opcode: op.opcode.into_inner(),
                operands,
            });
        }

        self.asm
            .push(Instruction::Directive(crate::ir::Directive::End));
        Ok(())
    }
    fn lower(mut self) -> Result<(AsmContext, InstructionPack), FatalErrorEmitted> {
        for (name, value) in self.pir.bindings.entries() {
            let name = *name.get();
            match value.get() {
                pican_pir::bindings::BindingValue::Register(r) => {
                    let reg = self.lower_register(*r);
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
                        self.asm_ctx.define_constant(
                            id,
                            crate::context::ConstantUniform::IVec(Vec4::new(
                                *i[0].get(),
                                *i[1].get(),
                                *i[2].get(),
                                *i[3].get(),
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
                        self.asm_ctx.define_constant(
                            id,
                            crate::context::ConstantUniform::FVec(Vec4::new(
                                *i[0].get(),
                                *i[1].get(),
                                *i[2].get(),
                                *i[3].get(),
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
                        let id = self.regs.id_for_reg(*r.get());
                        RegHole {
                            id,
                            kind: RegHoleKind::Fixed(r.into_inner()),
                        }
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
