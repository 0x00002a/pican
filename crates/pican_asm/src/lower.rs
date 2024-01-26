use std::ops::Range;

use crate::{
    context::AsmContext,
    float24::Float24,
    instrs::{InstructionOffset, InstructionPack},
    ir::{self, RegisterId, SwizzleDims},
    shbin::{
        self,
        instruction::{OpCodeInstructionFormat, OperandDescriptor},
        ConstantTableEntry, ExecutableSection, ExecutableSectionHeader, Shbin,
    },
};

use pican_core::{
    copy_arrayvec::CopyArrayVec,
    ir::SwizzleDim,
    register::{Register, RegisterKind, RegisterType},
};
use shbin::instruction as shi;

pub fn lower_to_shbin(ctx: &AsmContext, instrs: &InstructionPack) -> shbin::Shbin {
    LowerCtx::default().lower_module(ctx, instrs)
}

pub const MAX_SHBIN_DESCRIPTORS: usize = 128;

#[derive(Default)]
struct LowerCtx {
    descriptors: CopyArrayVec<shi::OperandDescriptor, MAX_SHBIN_DESCRIPTORS>,
}
impl LowerCtx {
    fn add_desc(&mut self, desc: OperandDescriptor) -> u8 {
        let offset = self.descriptors.len() as u8;
        self.descriptors.push(desc);
        offset
    }
    //fn conv_operand(&mut self, dst: Option<ir::Operand>, src1: Option<ir::Operand>, src2: Option<ir::Operand>, src3: Option<ir::Operand>, )
    fn convert_operands(
        &mut self,
        operands: &[ir::Operand],
        ty: shi::InstructionFormatKind,
    ) -> shi::Operands {
        let two_arg_desc = || {
            shi::OperandDescriptor::new()
                .with_destination_mask(operands[0].swizzle.into())
                .with_s1(operands[1].swizzle.into())
                .with_s2(operands[2].swizzle.into())
        };
        let resolve_reg = |idx| {
            match operands[0].register.kind {
                ir::RegHoleKind::Fixed(f) => f,
                ir::RegHoleKind::Free(_) => panic!("found free register at operand index {idx}, did the register allocation pass not run?"),
            }
        };
        match ty {
            shi::InstructionFormatKind::One => {
                let desc = self.add_desc(two_arg_desc());
                shi::Operands::TwoArguments {
                    dst: resolve_reg(0),
                    src1: resolve_reg(1),
                    src2: resolve_reg(2),
                    relative_offset: 0,
                    desc,
                    inverse: false,
                }
            }
            shi::InstructionFormatKind::OneI => {
                let desc = self.add_desc(two_arg_desc());
                shi::Operands::TwoArguments {
                    dst: resolve_reg(0),
                    src1: resolve_reg(1),
                    src2: resolve_reg(2),
                    relative_offset: 0,
                    desc,
                    inverse: true,
                }
            }
            shi::InstructionFormatKind::OneU => {
                let desc = self.add_desc(
                    OperandDescriptor::new()
                        .with_destination_mask(operands[0].swizzle.into())
                        .with_s1(operands[1].swizzle.into()),
                );
                shi::Operands::OneArgument {
                    dst: resolve_reg(0),
                    src1: resolve_reg(1),
                    relative_offset: 0,
                    desc,
                }
            }
            shi::InstructionFormatKind::OneC => shi::Operands::Cmp {
                src1: resolve_reg(0),
                src2: resolve_reg(1),
                desc: self.add_desc(two_arg_desc()),
                adx1: 0,
                cmpy: todo!(),
                cmpx: todo!(),
            },
            shi::InstructionFormatKind::Two => todo!(),
            shi::InstructionFormatKind::Three => todo!(),
            shi::InstructionFormatKind::Four => todo!(),
            shi::InstructionFormatKind::Five => todo!(),
            shi::InstructionFormatKind::FiveI => todo!(),
            shi::InstructionFormatKind::Zero => todo!(),
            shi::InstructionFormatKind::Unknown => todo!(),
        }
    }

    fn lower_instr(&mut self, i: ir::Instruction) -> shi::Instruction {
        let operands = self.convert_operands(&i.operands, i.opcode.instruction_format());
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
                            crate::context::ConstantUniform::IVec(i) => {
                                check_ty(r.kind, RegisterKind::IntegerVecUniform);
                                ConstantTableEntry::IVec4 {
                                    register_id,
                                    x: i.x,
                                    y: i.y,
                                    z: i.z,
                                    w: i.w,
                                }
                            }
                            crate::context::ConstantUniform::FVec(f) => {
                                check_ty(r.kind, RegisterKind::FloatingVecUniform);
                                ConstantTableEntry::Vec4 {
                                    register_id,
                                    x: f.x,
                                    y: f.y,
                                    z: f.z,
                                    w: f.w,
                                }
                            }
                            crate::context::ConstantUniform::Bool(_) => todo!(),
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
                uniform_table: Default::default(),
                symbol_table: Default::default(),
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
