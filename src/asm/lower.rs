use std::{collections::HashMap, ops::Range};

use crate::{
    context::{AsmContext, SymbolId},
    float24::Float24,
    instrs::{InstructionOffset, InstructionPack},
    ir::{self, RegisterId, SwizzleDims},
    shbin::{
        self,
        instruction::{OpCodeInstructionFormat, OperandDescriptor},
        ConstantTableEntry, ExecutableSection, ExecutableSectionHeader, MaxSize, Shbin,
        SymbolTable, UniformTableEntry,
    },
};

use binrw::NullString;
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
        if let Some(offset) = self.descriptors.iter().position(|d| d == &desc) {
            offset as u8
        } else {
            let offset = self.descriptors.len() as u8;
            self.descriptors.push(desc);
            offset
        }
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
        let mad_desc = || {
            shi::OperandDescriptor::new()
                .with_destination_mask(operands[0].swizzle.into())
                .with_s1(operands[1].swizzle.into())
                .with_s2(operands[2].swizzle.into())
                .with_s3(operands[3].swizzle.into())
        };
        let resolve_reg = |idx: usize| {
            match operands[idx].register.kind {
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
                let desc = self.add_desc(two_arg_desc());
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
                let desc = self.add_desc(two_arg_desc());
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
                let desc = self.add_desc(
                    OperandDescriptor::new()
                        .with_destination_mask(operands[0].swizzle.into())
                        .with_s1(operands[1].swizzle.into()),
                );
                shi::Operands::OneArgument {
                    dst: resolve_dst(),
                    src1: resolve_src(1),
                    relative_offset: 0,
                    desc,
                }
            }
            shi::InstructionFormatKind::OneC => shi::Operands::Cmp {
                src1: resolve_src(0),
                src2: resolve_src(1),
                desc: self.add_desc(two_arg_desc()),
                adx1: 0,
                cmpy: todo!(),
                cmpx: todo!(),
            },
            shi::InstructionFormatKind::Two => todo!(),
            shi::InstructionFormatKind::Three => todo!(),
            shi::InstructionFormatKind::Four => todo!(),
            shi::InstructionFormatKind::Five => shi::Operands::Mad {
                dst: resolve_dst(),
                src1: resolve_src(1),
                src2: resolve_src(2),
                src3: resolve_src(3),
                adx: 0,
                desc: self.add_desc(mad_desc()),
                inverse: false,
            },
            shi::InstructionFormatKind::FiveI => shi::Operands::Mad {
                dst: resolve_dst(),
                src1: resolve_src(1),
                src2: resolve_src(2),
                src3: resolve_src(3),
                adx: 0,
                desc: self.add_desc(mad_desc()),
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
