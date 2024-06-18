use std::collections::HashMap;

use crate::{
    context::PicanContext,
    diagnostics::{DiagnosticBuilder, Diagnostics, FatalErrorEmitted},
    frontend::ast::OpCode,
    ir::{Float, HasSpan, Ident, IrNode, SwizzleDims},
    pir::ir::CondExpr,
    register::{Register, RegisterKind},
};
use crate::{
    context::PicanOptions,
    pir::{
        bindings::BindingValue,
        ir::{EntryPoint, InputBinding, Module, Operand},
    },
};

use super::{
    context::{AsmContext, BoundUniform, ConstantUniform, OutputInfo, ProcInfo},
    float24::Float24,
    instrs::InstructionPack,
    ir::{self, Instruction, ProcId, RegHole, RegHoleKind, RegOperand, RegisterId, Vec4},
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

#[derive(Default)]
struct FreeRegTracker {
    next_bool: usize,
    next_int: usize,
    next_float: usize,
    next_input: usize,
    next_output: usize,
    // these count up but resolve to max - value
    next_c_int: usize,
    next_c_float: usize,
    next_c_bool: usize,
}
impl FreeRegTracker {
    fn allocate_impl(&mut self, kind: RegisterKind, constant: bool) -> Option<Register> {
        let next = match kind {
            RegisterKind::FloatingVecUniform => &mut self.next_float,
            RegisterKind::IntegerVecUniform => &mut self.next_int,
            RegisterKind::BoolUniform => &mut self.next_bool,
            RegisterKind::Input => &mut self.next_input,
            RegisterKind::Output => &mut self.next_output,
            RegisterKind::Scratch => unreachable!(),
        };
        let constant_accessor = match kind {
            RegisterKind::FloatingVecUniform => Some(&mut self.next_c_float),
            RegisterKind::IntegerVecUniform => Some(&mut self.next_c_int),
            RegisterKind::BoolUniform => Some(&mut self.next_c_bool),
            _ => None,
        };
        if *next > kind.max_index()
            || Some(*next) == constant_accessor.as_ref().map(|c| kind.max_index() - **c)
        {
            None
        } else {
            let (next, v) = if constant {
                let c = constant_accessor.expect("tried to allocate non-uniform constant");
                let v = kind.max_index() - *c;
                (c, v)
            } else {
                let v = *next;
                (next, v)
            };
            let r = Register::new(kind, v);
            *next += 1;
            Some(r)
        }
    }

    fn allocate_diag(
        &mut self,
        kind: RegisterKind,
        diag: &Diagnostics,
        span: &impl HasSpan,
    ) -> Result<Register, FatalErrorEmitted> {
        self.allocate_diag_impl(kind, diag, span, false)
    }

    fn allocate_diag_constant(
        &mut self,
        kind: RegisterKind,
        diag: &Diagnostics,
        span: &impl HasSpan,
    ) -> Result<Register, FatalErrorEmitted> {
        self.allocate_diag_impl(kind, diag, span, true)
    }

    fn allocate_diag_impl(
        &mut self,
        kind: RegisterKind,
        diag: &Diagnostics,
        span: &impl HasSpan,
        c: bool,
    ) -> Result<Register, FatalErrorEmitted> {
        self.allocate_impl(kind, c).ok_or_else(|| {
            diag.fatal::<()>(
                DiagnosticBuilder::error()
                    .at(span)
                    .primary(
                        "sorry but I've run out of available registers for this type of uniform",
                    )
                    .build(),
            )
            .unwrap_err()
        })
    }
}

#[derive(Hash, Clone, Copy, PartialEq, Eq, Debug)]
struct IdentKey<'a> {
    ident: Ident<'a>,
    index: Option<u32>,
}

impl<'a> IdentKey<'a> {
    fn new(ident: Ident<'a>, index: Option<u32>) -> Self {
        Self { ident, index }
    }

    fn with_ident(self, i: Ident<'a>) -> Self {
        Self {
            ident: i,
            index: self.index,
        }
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
    ident_to_reg: HashMap<IdentKey<'a>, RegHole>,
    unif_regs: FreeRegTracker,
    opts: PicanOptions,
}
impl<'a, 'm, 'c> LowerCtx<'a, 'm, 'c> {
    fn lower_register(&mut self, r: Register) -> RegHole {
        let id = self.id_for_fixed(r);
        RegHole {
            id,
            kind: RegHoleKind::Fixed(r),
        }
    }
    fn resolve_operand_ident(
        &mut self,
        ident: &IdentKey<'a>,
    ) -> (RegHole, Option<SwizzleDims<'a>>) {
        match self
            .pir
            .bindings
            .lookup(&ident.ident)
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
                    self.resolve_operand_ident(&ident.with_ident(v.target.into_inner()))
                        .0,
                    Some(v.swizzle.into_inner()),
                )
            }
            BindingValue::Alias(i) => self.resolve_operand_ident(&ident.with_ident(i)),
            _ => (
                *self
                    .ident_to_reg
                    .get(ident)
                    .unwrap_or_else(|| panic!("ident to reg failed with {ident:?}")),
                None,
            ),
        }
    }
    fn lower_operand(
        &mut self,
        Operand {
            negate,
            kind,
            relative_addr,
            swizzle,
        }: &Operand<'a>,
    ) -> ir::Operand {
        let (register, swiz) = match kind.get() {
            crate::pir::ir::OperandKind::Var(v) => self.resolve_operand_ident(&IdentKey::new(
                v.into_inner(),
                relative_addr.map(|a| a.into_inner()),
            )),
            crate::pir::ir::OperandKind::Register(r) => (self.lower_register(*r.get()), None),
            crate::pir::ir::OperandKind::Cmp(c) => return ir::Operand::Cmp(c.into_inner()),
        };
        let swizzle = swizzle
            .map(|s| s.into_inner())
            .or(swiz)
            .map(|s| s.0.into_inner())
            .map(|s| s.iter().copied().collect());
        ir::Operand::Reg(RegOperand {
            register,
            swizzle,
            negate: *negate,
        })
    }
    fn lower_entry_point(&mut self, ent: &EntryPoint<'a>) -> Result<(), FatalErrorEmitted> {
        let id = self.procs.id_for(*ent.name.get());
        assert!(!ent.ops.get().is_empty(), "no instructions for proc");
        let start = self.asm.next_offset();

        for op in ent.ops.get().iter().map(|p| p.get()) {
            match op {
                crate::pir::ir::Op::Regular { opcode, operands } => {
                    let operands = operands
                        .get()
                        .iter()
                        .map(|operand| self.lower_operand(operand.get()))
                        .collect();
                    self.asm.push(Instruction {
                        opcode: opcode.into_inner(),
                        operands,
                    });
                }
                crate::pir::ir::Op::Cond(CondExpr {
                    cond,
                    num_instrs,
                    dest_offset,
                }) => {
                    self.asm.push(Instruction {
                        opcode: OpCode::IfC,
                        operands: [
                            ir::Operand::Word(
                                num_instrs
                                    .into_inner()
                                    .try_into()
                                    .expect("too many instructions"),
                            ),
                            ir::Operand::Word(dest_offset.into_inner().try_into().unwrap()),
                            ir::Operand::Cond(cond.into_inner()),
                        ]
                        .into_iter()
                        .collect(),
                    });
                }
            }
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
                crate::pir::bindings::BindingValue::Register(r) => {
                    let r = *r;
                    match r.kind {
                        RegisterKind::Input if !self.opts.picasso_compat_bug_for_bug => {
                            self.asm_ctx.used_input_registers.mark_used(r)
                        }
                        RegisterKind::Output => self.asm_ctx.used_output_registers.mark_used(r),
                        _ => {}
                    }
                    let reg = self.lower_register(r);
                    self.ident_to_reg.insert(IdentKey::new(name, None), reg);
                }
                crate::pir::bindings::BindingValue::Uniform(u) => {
                    let kind = match u.ty.get() {
                        crate::pir::ty::UniformTy::Bool => RegisterKind::BoolUniform,
                        crate::pir::ty::UniformTy::Integer => RegisterKind::IntegerVecUniform,
                        crate::pir::ty::UniformTy::Float => RegisterKind::FloatingVecUniform,
                    };

                    let (start, end) = if let Some(dim) = u.dimension {
                        let mut start = None;
                        let mut end = None;
                        for d in 0..dim.into_inner() {
                            let reg = self.unif_regs.allocate_diag(kind, self.diag, &value)?;
                            let r = self.lower_register(reg);
                            self.ident_to_reg
                                .insert(IdentKey::new(name, Some(d as u32)), r);
                            if start.is_none() {
                                start.replace(reg);
                            }
                            if d == dim.into_inner() - 1 {
                                end.replace(reg);
                            }
                        }
                        (start.unwrap(), end.unwrap())
                    } else {
                        let reg = self.unif_regs.allocate_diag(kind, self.diag, &value)?;
                        let r = self.lower_register(reg);
                        self.ident_to_reg.insert(IdentKey::new(name, None), r);
                        (reg, reg)
                    };

                    let name = self.asm_ctx.define_symbol(name);
                    self.asm_ctx.uniforms.push(BoundUniform {
                        name,
                        start_register: start,
                        end_register: end,
                    })
                }
                crate::pir::bindings::BindingValue::Constant(c) => {
                    let (kind, v) = match c {
                        crate::pir::ir::ConstantUniform::Integer(i) => {
                            let i = i.get();
                            let conv_i = |i: IrNode<i32>| -> Result<i8, FatalErrorEmitted> {
                                i.into_inner().try_into().map_err(|_| {
                                    self.diag.fatal::<()>(DiagnosticBuilder::error().at(&i).primary(
                                        "integer too large, it must fit in an 8-bit signed",
                                    ).build()).unwrap_err()
                                })
                            };
                            (
                                RegisterKind::IntegerVecUniform,
                                ConstantUniform::IVec(Vec4::new(
                                    conv_i(i[0])?,
                                    conv_i(i[1])?,
                                    conv_i(i[2])?,
                                    conv_i(i[3])?,
                                )),
                            )
                        }
                        crate::pir::ir::ConstantUniform::Float(i) => {
                            let i = i.get();
                            let conv_f = |i: IrNode<Float>| -> Result<Float24, FatalErrorEmitted> {
                                i.into_inner().try_into().map_err(|e| {
                                    self.diag
                                        .fatal::<()>(
                                            DiagnosticBuilder::error()
                                                .at(&i)
                                                .primary(format!(
                                                    "float doesn't fit in gpu format {e}"
                                                ))
                                                .build(),
                                        )
                                        .unwrap_err()
                                })
                            };
                            (
                                RegisterKind::FloatingVecUniform,
                                ConstantUniform::FVec(Vec4::new(
                                    conv_f(i[0])?,
                                    conv_f(i[1])?,
                                    conv_f(i[2])?,
                                    conv_f(i[3])?,
                                )),
                            )
                        }
                        crate::pir::ir::ConstantUniform::FloatArray(_) => todo!(),
                    };

                    let id = self.regs.next_reg_id();
                    self.asm_ctx.define_constant(id, v);

                    let reg = self
                        .unif_regs
                        .allocate_diag_constant(kind, self.diag, &value)?;
                    self.asm_ctx.allocated_registers.insert(id, reg);
                    let r = self.lower_register(reg);
                    self.ident_to_reg.insert(IdentKey::new(name, None), r);
                }
                crate::pir::bindings::BindingValue::OutputProperty(o) => {
                    assert!(o.alias.is_some());

                    let r = if let Some(r) = o.register {
                        r.into_inner()
                    } else {
                        self.unif_regs
                            .allocate_diag(RegisterKind::Output, self.diag, &value)?
                    };
                    self.asm_ctx.used_output_registers.mark_used(r);
                    let reg = self.lower_register(r);
                    self.asm_ctx.outputs.push(OutputInfo {
                        property: o.property.into_inner(),
                        register: reg.id,
                        mask: Default::default(),
                    });
                    self.ident_to_reg.insert(IdentKey::new(name, None), reg);
                }
                crate::pir::bindings::BindingValue::Input(
                    i @ InputBinding {
                        name,
                        index,
                        register,
                    },
                ) => {
                    let reg = register
                        .map(|r| r.into_inner())
                        .unwrap_or(Register::new(RegisterKind::Input, *index));
                    self.asm_ctx.used_input_registers.mark_used(reg);
                    let r = self.lower_register(reg);
                    self.ident_to_reg
                        .insert(IdentKey::new(name.into_inner(), None), r);
                    let name = self.asm_ctx.define_symbol(i.name.into_inner());
                    self.asm_ctx.uniforms.push(BoundUniform {
                        name,
                        start_register: reg,
                        end_register: reg,
                    })
                }

                crate::pir::bindings::BindingValue::SwizzleRegister(_)
                | crate::pir::bindings::BindingValue::SwizzleVar(_)
                | crate::pir::bindings::BindingValue::Alias(_) => {}
            }
        }
        for ent in self.pir.entry_points {
            self.lower_entry_point(ent.get())?;
        }
        self.asm_ctx
            .uniforms
            .sort_by_key(|u| u.start_register.index);
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
        unif_regs: Default::default(),
        opts: ctx.opts,
    }
    .lower()
}
