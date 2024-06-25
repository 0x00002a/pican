use std::collections::HashMap;

use super::{
    context::{AsmContext, SymbolId},
    instrs::InstructionPack,
    ir::{self, RegOperand, RegisterId, SwizzleDims},
    shbin::{
        self,
        instruction::{OpCodeInstructionFormat, OperandDescriptor, OperandSource},
        ConstantTableEntry, ExecutableSection, ExecutableSectionHeader, Shbin, UniformTableEntry,
    },
};

use crate::{
    asm::{context::ConstantUniform, shbin::instruction::ComponentSelector},
    frontend::ast::OpCode,
    register::{RegisterKind, RegisterType},
};
use binrw::NullString;
use copy_arrayvec::CopyArrayVec;
use shbin::instruction as shi;

pub fn lower_to_shbin(ctx: &AsmContext, instrs: &InstructionPack) -> shbin::Shbin {
    LowerCtx::default().lower_module(ctx, instrs)
}

pub const MAX_SHBIN_DESCRIPTORS: usize = 128;

#[derive(Clone, Copy)]
struct OpdescMask(u64);
impl OpdescMask {
    const fn new(component: bool, s1: bool, s2: bool, s3: bool) -> Self {
        let c = if component { 0xF } else { 0 };
        let s1 = if s1 { 0x1FF } else { 0 };
        let s2 = if s2 { 0x1FF } else { 0 };
        let s3 = if s3 { 0x1FF } else { 0 };
        // this is the integer for of an operand descriptor
        Self(c | (s1 << 4) | (s2 << 13) | (s3 << 22))
    }
    const fn as_u64(self) -> u64 {
        self.0
    }

    /// Turn the mask into the minimal form for compat
    ///
    /// This allows merging of operand descriptors later on
    fn optimise(self, opcode: OpCode, desc: OperandDescriptor) -> Self {
        // this will be used to mask out the unneeded swizzles on the src's
        let mut src_swiz_mask = [ComponentSelector::new(); 3];
        // build a component of the swizzle mask as a u8, we could use ComponentSelector but it'd be painful
        let mk_swizzle_comp_mask = |n: u8| -> u8 { 3 << (6 - n * 2) };
        match opcode {
            OpCode::Mul
            | OpCode::Sge
            | OpCode::Slt
            | OpCode::Flr
            | OpCode::Max
            | OpCode::Min
            | OpCode::Mov
            | OpCode::Mad
            | OpCode::Add => {
                // for all of these only use maximally the destination swizzle, so we need to mask
                // those bits on all the sources
                let comp = desc.destination_mask();
                let mut swiz_mask = 0u8;
                if !comp.w() {
                    swiz_mask |= mk_swizzle_comp_mask(3);
                }
                if !comp.z() {
                    swiz_mask |= mk_swizzle_comp_mask(2);
                }
                if !comp.y() {
                    swiz_mask |= mk_swizzle_comp_mask(1);
                }
                if !comp.x() {
                    swiz_mask |= mk_swizzle_comp_mask(0);
                }
                let swiz = ComponentSelector::from_bytes(swiz_mask.to_le_bytes());
                src_swiz_mask[0] = swiz;
                src_swiz_mask[1] = swiz;
                src_swiz_mask[2] = swiz;
            }
            OpCode::Dp3 => {
                let swiz = ComponentSelector::from_bytes(mk_swizzle_comp_mask(3).to_le_bytes());
                src_swiz_mask[0] = swiz;
                src_swiz_mask[1] = swiz;
            }
            OpCode::Dph => {
                src_swiz_mask[0] =
                    ComponentSelector::from_bytes(mk_swizzle_comp_mask(3).to_le_bytes());
            }
            OpCode::Ex2 | OpCode::Lg2 | OpCode::Rcp | OpCode::Rsq => {
                src_swiz_mask[0] = ComponentSelector::from_bytes(
                    (mk_swizzle_comp_mask(1) | mk_swizzle_comp_mask(2) | mk_swizzle_comp_mask(3))
                        .to_le_bytes(),
                );
            }
            OpCode::Cmp => {
                src_swiz_mask[0] = ComponentSelector::from_bytes(
                    (mk_swizzle_comp_mask(2) | mk_swizzle_comp_mask(3)).to_le_bytes(),
                );
            }
            _ => {}
        }

        let mask = self.as_u64()
            & !(OperandDescriptor::new()
                .with_s1(OperandSource::new().with_selector(src_swiz_mask[0]))
                .with_s2(OperandSource::new().with_selector(src_swiz_mask[1]))
                .with_s3(OperandSource::new().with_selector(src_swiz_mask[2]))
                .as_u64());
        Self(mask)
    }
}
impl std::ops::BitAnd for OpdescMask {
    type Output = OpdescMask;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl std::ops::BitOr for OpdescMask {
    type Output = OpdescMask;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

const OPDESC_MASK_DST_SRC: OpdescMask = OpdescMask::new(true, true, false, false);

const OPDESC_MASK_DST_SRC_SRC: OpdescMask = OpdescMask::new(true, true, true, false);

const OPDESC_MASK_DST_SRC_SRC_SRC: OpdescMask = OpdescMask::new(true, true, true, true);

impl OperandDescriptor {
    fn update(self, other: OperandDescriptor, mask: OpdescMask) -> Self {
        let me = self.as_u64();
        let other = other.as_u64();
        let mask = mask.as_u64();
        let res = (me & !mask) | (other & mask);
        Self::from_bytes(res.to_le_bytes())
    }
    fn compatible_with(self, other: OperandDescriptor, mask: OpdescMask) -> bool {
        let mask = mask.as_u64();
        other.as_u64() & mask == self.as_u64() & mask
    }
}

#[derive(Default)]
struct LowerCtx {
    descriptors: CopyArrayVec<shi::OperandDescriptor, MAX_SHBIN_DESCRIPTORS>,
    masks: CopyArrayVec<OpdescMask, MAX_SHBIN_DESCRIPTORS>,
}

impl LowerCtx {
    fn add_desc(&mut self, (desc, mask): (OperandDescriptor, OpdescMask), opcode: OpCode) -> u8 {
        // so as far as I can work out from picasso's code opdescs can be "compressed" by setting them to the max
        // number of src's while keeping the destination mask the same (depending on opcode)
        let mask = mask.optimise(opcode, desc);

        for i in 0..self.descriptors.len() {
            let c_desc = self.descriptors[i];
            let min_mask = self.masks[i] & mask;
            if c_desc.compatible_with(desc, min_mask) {
                self.descriptors[i] = c_desc.update(desc, mask);
                self.masks[i] = self.masks[i] | mask;
                return i as u8;
            }
        }
        let offset = self.descriptors.len() as u8;
        self.descriptors.push(desc);
        self.masks.push(mask);
        offset
    }
    fn convert_operands(
        &mut self,
        operands: &[ir::Operand],
        ty: shi::InstructionFormatKind,
        opcode: OpCode,
    ) -> shi::Operands {
        let reg_operands = || {
            operands
                .iter()
                .map(|o| match o {
                    ir::Operand::Reg(r) => r,
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>()
        };
        let two_arg_desc = || {
            let operands = reg_operands();
            (
                shi::OperandDescriptor::new()
                    .with_destination_mask(operands[0].swizzle.into())
                    .with_s1(operands[1].into())
                    .with_s2(operands[2].into()),
                OPDESC_MASK_DST_SRC_SRC,
            )
        };
        let mad_desc = || {
            let operands = reg_operands();
            (
                shi::OperandDescriptor::new()
                    .with_destination_mask(operands[0].swizzle.into())
                    .with_s1(operands[1].into())
                    .with_s2(operands[2].into())
                    .with_s3(operands[3].into()),
                OPDESC_MASK_DST_SRC_SRC_SRC,
            )
        };
        let resolve_reg = |idx: usize| {
            match operands[idx].as_reg().unwrap().register.kind {
                ir::RegHoleKind::Fixed(f) => f,
                ir::RegHoleKind::Free(_) => panic!("found free register at operand index {idx}, did the register allocation pass not run?"),
            }
        };
        let resolve_dst = || {
            let r = resolve_reg(0);
            assert!(
                r.kind.is_type(RegisterType::Output),
                "register is invalid for dst, operand 0, ty: {:?}",
                r.kind
            );
            r
        };
        let resolve_src = |idx: usize| {
            let r = resolve_reg(idx);
            assert!(
                r.kind.is_type(RegisterType::Input) | r.kind.is_type(RegisterType::Special),
                "register is invalid for src, operand {idx} ty {:?}",
                r.kind,
            );
            r
        };
        match ty {
            shi::InstructionFormatKind::One => {
                let desc = self.add_desc(two_arg_desc(), opcode);
                shi::Operands::TwoArguments {
                    dst: resolve_dst(),
                    src1: resolve_src(1),
                    src2: resolve_src(2),
                    relative_offset: 0,
                    desc,
                    inverse: false,
                }
            }
            shi::InstructionFormatKind::OneI => {
                let desc = self.add_desc(two_arg_desc(), opcode);
                shi::Operands::TwoArguments {
                    dst: resolve_dst(),
                    src1: resolve_src(1),
                    src2: resolve_src(2),
                    relative_offset: 0,
                    desc,
                    inverse: true,
                }
            }
            shi::InstructionFormatKind::OneU => {
                let operands = reg_operands();
                let (opdesc, mask) = {
                    let opdesc =
                        OperandDescriptor::new().with_destination_mask(operands[0].swizzle.into());
                    if matches!(opcode, OpCode::Rsq) {
                        (opdesc, OPDESC_MASK_DST_SRC)
                    } else {
                        let opdesc = opdesc.with_s1(operands[1].into());
                        (opdesc, OPDESC_MASK_DST_SRC)
                    }
                };
                let desc = self.add_desc((opdesc, mask), opcode);
                shi::Operands::OneArgument {
                    dst: resolve_dst(),
                    src1: resolve_src(1),
                    relative_offset: 0,
                    desc,
                }
            }
            shi::InstructionFormatKind::OneC => shi::Operands::Cmp {
                src1: resolve_src(0),
                src2: resolve_src(3),
                desc: self.add_desc(
                    (
                        OperandDescriptor::new()
                            .with_s1(operands[0].as_reg().unwrap().into())
                            .with_s2(operands[3].as_reg().unwrap().into()),
                        OpdescMask::new(false, true, true, false),
                    ),
                    opcode,
                ),
                adx1: 0,
                cmpx: operands[1].into_cmp().unwrap(),
                cmpy: operands[2].into_cmp().unwrap(),
            },
            shi::InstructionFormatKind::Two => shi::Operands::ControlFlow {
                cond: operands[2].as_cond().unwrap().as_u8(),
                refx: 1,
                refy: 1,
                dst_offset: operands[1]
                    .into_word()
                    .unwrap()
                    .try_into()
                    .expect("dst offset too large to fit in binary"),
                num: operands[0]
                    .into_word()
                    .unwrap()
                    .try_into()
                    .expect("num offset too large to fit in binary"),
            },
            shi::InstructionFormatKind::Three => todo!(),
            shi::InstructionFormatKind::Four => todo!(),
            shi::InstructionFormatKind::Five => shi::Operands::Mad {
                dst: resolve_dst(),
                src1: resolve_src(1),
                src2: resolve_src(2),
                src3: resolve_src(3),
                adx: 0,
                desc: self.add_desc(mad_desc(), opcode),
                inverse: false,
            },
            shi::InstructionFormatKind::FiveI => shi::Operands::Mad {
                dst: resolve_dst(),
                src1: resolve_src(1),
                src2: resolve_src(2),
                src3: resolve_src(3),
                adx: 0,
                desc: self.add_desc(mad_desc(), opcode),
                inverse: true,
            },
            shi::InstructionFormatKind::Zero => {
                assert_eq!(operands.len(), 0);
                shi::Operands::Zero
            }
            shi::InstructionFormatKind::Unknown => todo!(),
        }
    }

    fn lower_instr(&mut self, i: ir::Instruction) -> shi::Instruction {
        let operands = self.convert_operands(&i.operands, i.opcode.instruction_format(), i.opcode);
        shi::Instruction {
            opcode: i.opcode,
            operands,
        }
    }
    fn lower_module(mut self, mctx: &AsmContext, code: &InstructionPack) -> shbin::Shbin {
        let mut out = Shbin::default();
        for instr in code.iter() {
            let i = self.lower_instr(instr);
            out.dvlp.compiled_blob.push(i);
        }
        if let Some(main) = mctx.main_proc() {
            let find_alloc_reg = |id: &RegisterId| {
                mctx.allocated_registers
                    .get(id)
                    .expect("found unallocated constant, did register allocation not happen?")
            };
            let (sym_to_offset, sym_tbl) = build_symbol_table(mctx);

            out.dvles.push(ExecutableSection {
                header: ExecutableSectionHeader {
                    shader_ty: shbin::ShaderType::Vertex,
                    merge_vertex_geo: 0,
                    main_offset_words: main.instr_start.to_words(),
                    endmain_offset_words: main.instr_end.to_words(),
                    used_input_registers: mctx.used_input_registers,
                    used_output_registers: mctx.used_output_registers,
                    geo_shader_type: 0,
                    start_float_register_idx: 0,
                    fully_defined_verts_variable: 0,
                    fully_defined_verts_fixed: 0,
                },
                constant_table: mctx
                    .constants
                    .iter()
                    .map(|(r, c)| {
                        let r = find_alloc_reg(r);
                        let register_id = r.index.try_into().unwrap();
                        #[track_caller]
                        fn check_ty(r: RegisterKind, e: RegisterKind) {
                            assert_eq!(r, e, "wrong register type allocated for constant");
                        }
                        match c {
                            ConstantUniform::IVec(i) => {
                                check_ty(r.kind, RegisterKind::IntegerVecUniform);
                                ConstantTableEntry::IVec4 {
                                    register_id,
                                    x: i.x,
                                    y: i.y,
                                    z: i.z,
                                    w: i.w,
                                }
                            }
                            ConstantUniform::FVec(f) => {
                                check_ty(r.kind, RegisterKind::FloatingVecUniform);
                                ConstantTableEntry::Vec4 {
                                    register_id,
                                    x: f.x,
                                    y: f.y,
                                    z: f.z,
                                    w: f.w,
                                }
                            }
                            ConstantUniform::Bool(_) => todo!(),
                        }
                    })
                    .collect::<Vec<_>>()
                    .into(),
                label_table: Default::default(),
                output_register_table: mctx
                    .outputs
                    .iter()
                    .map(|o| {
                        let register_id = find_alloc_reg(&o.register).index.try_into().unwrap();
                        shbin::OutputRegisterEntry {
                            ty: o.property,
                            register_id,
                            // todo
                            output_mask: Default::default(),
                        }
                    })
                    .collect::<Vec<_>>()
                    .into(),
                uniform_table: mctx
                    .uniforms
                    .iter()
                    .map(|unif| UniformTableEntry {
                        symbol_offset: *sym_to_offset.get(&unif.name).unwrap(),
                        start_register: unif.start_register.into(),
                        end_register: unif.end_register.into(),
                    })
                    .collect::<Vec<_>>()
                    .into(),
                symbol_table: sym_tbl.into(),
            });
        } else {
            // todo: add logging and warn here instead
            eprintln!("no main found");
        }
        out.dvlp.operand_desc_table = self.descriptors.iter().copied().collect::<Vec<_>>().into();
        out
    }
}
impl From<Option<SwizzleDims>> for shi::ComponentMask {
    fn from(value: Option<SwizzleDims>) -> Self {
        value.map(Into::into).unwrap_or_default()
    }
}

impl From<Option<SwizzleDims>> for shi::OperandSource {
    fn from(value: Option<SwizzleDims>) -> Self {
        value.map(Into::into).unwrap_or_default()
    }
}

impl From<&RegOperand> for shi::OperandSource {
    fn from(value: &RegOperand) -> Self {
        shi::OperandSource::from(value.swizzle).with_negate(value.negate)
    }
}

fn build_symbol_table(ctx: &AsmContext) -> (HashMap<SymbolId, u32>, Vec<NullString>) {
    let mut sym_to_offset = HashMap::new();
    let mut tbl = Vec::new();
    let mut offset = 0;
    for id in ctx.uniforms.iter().map(|uni| uni.name) {
        let sym = ctx.symbols.lookup_id(id).unwrap();
        sym_to_offset.insert(id, offset as u32);
        offset += sym.len() + 1;
        tbl.push(sym.into());
    }
    (sym_to_offset, tbl)
}

#[cfg(test)]
mod tests {
    use crate::asm::{
        lower::OpdescMask,
        shbin::instruction::{
            Component, ComponentMask, ComponentSelector, OperandDescriptor, OperandSource,
        },
    };

    #[test]
    fn matches_mask_for_differing_swizzles() {
        let full_comp = ComponentSelector::new()
            .with_x(Component::X)
            .with_y(Component::Y)
            .with_z(Component::Z)
            .with_w(Component::W);
        let fst = OperandDescriptor::new()
            .with_destination_mask(
                ComponentMask::new()
                    .with_x(false)
                    .with_y(true)
                    .with_z(true)
                    .with_w(true),
            )
            .with_s1(OperandSource::new().with_selector(full_comp));

        let snd = OperandDescriptor::new()
            .with_destination_mask(
                ComponentMask::new()
                    .with_x(true)
                    .with_y(true)
                    .with_z(true)
                    .with_w(true),
            )
            .with_s1(OperandSource::new().with_selector(full_comp))
            .with_s2(OperandSource::new().with_selector(full_comp));
        assert!(!fst.compatible_with(snd, OpdescMask::new(true, true, true, false)));

        assert!(fst.compatible_with(snd, OpdescMask::new(false, true, false, false)));
    }
}
