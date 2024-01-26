use std::{
    io::{Cursor, Seek, SeekFrom},
    mem::size_of,
    ops::{Deref, DerefMut},
};

use binrw::{
    binread, binrw, file_ptr::IntoSeekFrom, BinRead, BinReaderExt, BinResult, BinWrite,
    BinWriterExt, NamedArgs, NullString, VecArgs,
};
use pican_core::{
    ops::OpCode,
    properties::OutputProperty,
    register::{Register, RegisterKind},
};

use self::instruction::{ComponentMask, Instruction, OperandDescriptor};

use super::float24::Float24;

pub mod instruction;

const DVLP_HEADER_SZ: u64 = 0x28;

#[binread]
#[doc(alias = "DVLB")]
#[br(magic = b"DVLB", stream = s)]
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Shbin {
    #[br(temp)]
    pub dvle_count: u32,
    #[br(count = dvle_count, temp)]
    pub dvle_offsets: Vec<u32>,
    #[br(args(s.stream_position().unwrap()))]
    pub dvlp: Dvlp,
    #[br(parse_with = binrw::file_ptr::parse_from_iter(dvle_offsets.iter().copied()), seek_before(SeekFrom::Start(0)))]
    pub dvles: Vec<ExecutableSection>,
}
impl Shbin {
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut c = Cursor::new(Vec::new());
        c.write_type(self, binrw::Endian::Little).unwrap();
        c.into_inner()
    }
}

impl BinWrite for Shbin {
    type Args<'a> = ();

    fn write_options<W: std::io::prelude::Write + Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        _: Self::Args<'_>,
    ) -> BinResult<()> {
        let p = writer.stream_position()?;

        let mut dvlp_buf = Cursor::new(Vec::new());
        dvlp_buf.write_type_args(&self.dvlp, endian, (0u64,))?;
        assert_eq!(
            dvlp_buf.stream_position()?,
            self.dvlp.expected_sz(),
            "dvlp didn't match expected size"
        );

        writer.write_type(b"DVLB", endian)?;

        let dvle_count = self.dvles.len() as u32;
        writer.write_type(&dvle_count, endian)?;
        let mut dvle_offsets = Vec::new();
        let mut off = (writer.stream_position()? - p)
            + dvle_count as u64 * size_of::<u32>() as u64
            + dvlp_buf.get_ref().len() as u64;

        for dvle in &self.dvles {
            let before = dvlp_buf.stream_position()?;
            dvlp_buf.write_type(&dvle, endian)?;
            dvle_offsets.push(off as u32);
            let pos = dvlp_buf.stream_position()? - before;
            off += pos;
        }
        let dvlp_buf = dvlp_buf.into_inner();
        writer.write_type(&dvle_offsets, endian)?;
        writer.write_all(&dvlp_buf)?;
        let pos = writer.stream_position()?;
        for _ in pos..pos.next_multiple_of(4) {
            writer.write_type(&0u8, endian)?;
        }

        Ok(())
    }
}

#[binrw]
#[brw(magic = b"DVLP", stream = s, import(start: u64))]
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Dvlp {
    _mversion: u32,
    #[br(args { header_start: start, inner: () })]
    #[bw(args { offset: DVLP_HEADER_SZ })]
    pub compiled_blob: ShaderBlob,
    #[br(args { header_start: start, inner: () })]
    #[bw(args { offset: (compiled_blob.bin_size()) as u64 + DVLP_HEADER_SZ })]
    pub operand_desc_table: OffsetTable<OperandDescriptor>,
    #[bw(assert(s.stream_position().unwrap() == 24), calc = DVLP_HEADER_SZ as u32 + compiled_blob.bin_size() as u32 + operand_desc_table.data.iter().map(|b| b.bin_size()).sum::<usize>() as u32)]
    #[br(temp)]
    _unk1_sym_offset: u32,
    pub rest: [u32; 3],
    #[br(ignore)]
    #[bw(calc = compiled_blob.deref().clone(), assert(s.stream_position().unwrap() == DVLP_HEADER_SZ))]
    compiled_blob_data: Vec<Instruction>,
    #[br(ignore)]
    #[bw(calc = operand_desc_table.data.clone(), assert(s.stream_position().unwrap() == DVLP_HEADER_SZ + compiled_blob.bin_size() as u64))]
    operand_desc_data: Vec<OperandDescriptor>,
}

impl Dvlp {
    fn expected_sz(&self) -> u64 {
        DVLP_HEADER_SZ
            + (self.compiled_blob.len() * size_of::<u32>()
                + self.operand_desc_table.data.len() * size_of::<OperandDescriptor>())
                as u64
    }
}

impl BinSize for u32 {
    fn bin_size(&self) -> usize {
        4
    }
}

impl BinSize for u8 {
    fn bin_size(&self) -> usize {
        1
    }
}

impl BinSize for OperandDescriptor {
    fn bin_size(&self) -> usize {
        8
    }
}

pub struct Operation {
    pub opcode: OpCode,
}

#[binrw]
#[doc(alias = "DVLE")]
#[derive(Clone, Debug, PartialEq, Eq)]
#[brw(magic = b"DVLE")]
pub struct ExecutableSectionHeader {
    #[br(temp)]
    #[bw(calc = 4098)]
    pub _mversion: u16,
    pub shader_ty: ShaderType,
    pub merge_vertex_geo: u8,
    pub main_offset_words: u32,
    pub endmain_offset_words: u32,
    pub used_input_registers: IoRegisterBitMask,
    pub used_output_registers: IoRegisterBitMask,
    pub geo_shader_type: u8,
    pub start_float_register_idx: u8,
    pub fully_defined_verts_variable: u8,
    pub fully_defined_verts_fixed: u8,
}

#[binread]
#[br(stream = s)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExecutableSection {
    #[br(try_calc = s.stream_position(), temp)]
    header_start: u64,
    #[br(assert(s.stream_position().unwrap() - header_start == 0x18))]
    pub header: ExecutableSectionHeader,
    #[br(args { header_start, inner: () })]
    pub constant_table: OffsetTable<ConstantTableEntry>,
    #[br(args { header_start, inner: () })]
    pub label_table: OffsetTable<LabelTableEntry>,
    #[br(args { header_start, inner: () })]
    pub output_register_table: OffsetTable<OutputRegisterEntry>,
    #[br(args { header_start, inner: () })]
    pub uniform_table: OffsetTable<UniformTableEntry>,
    #[br(args { header_start, inner: () })]
    pub symbol_table: SizedTable<NullString>,
}
impl BinWrite for ExecutableSection {
    type Args<'a> = ();

    fn write_options<W: std::io::prelude::Write + std::io::prelude::Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        _: Self::Args<'_>,
    ) -> BinResult<()> {
        writer.write_type(&self.header, endian)?;
        struct TablesWriter<'w, W> {
            writer: &'w mut W,
            endian: binrw::Endian,
            offset: u64,
        }
        impl<'w, W: std::io::prelude::Write + std::io::prelude::Seek> TablesWriter<'w, W> {
            fn write<T: BinSize + BinWrite>(&mut self, tbl: &OffsetTable<T>) -> BinResult<()> {
                self.writer.write_type_args(
                    tbl,
                    self.endian,
                    OffsetTableWriteArgs {
                        offset: self.offset,
                    },
                )?;
                self.offset += tbl.add_offset() as u64;
                Ok(())
            }
        }
        let mut w = TablesWriter {
            writer,
            endian,
            offset: 64,
        };
        w.write(&self.constant_table)?;
        w.write(&self.label_table)?;
        w.write(&self.output_register_table)?;
        w.write(&self.uniform_table)?;
        let off = w.offset;
        writer.write_type_args(
            &self.symbol_table,
            endian,
            OffsetTableWriteArgs { offset: off },
        )?;

        writer.write_type(&self.constant_table.data, endian)?;
        writer.write_type(&self.label_table.data, endian)?;
        writer.write_type(&self.output_register_table.data, endian)?;
        writer.write_type(&self.uniform_table.data, endian)?;
        writer.write_type(&self.symbol_table.data.0, endian)?;

        let pos = writer.stream_position()?;
        for _ in pos..pos.next_multiple_of(4) {
            writer.write_type(&0u8, endian)?;
        }

        Ok(())
    }
}
pub trait BinSize {
    fn bin_size(&self) -> usize;
}
impl BinSize for NullString {
    fn bin_size(&self) -> usize {
        self.0.len() + 1
    }
}

impl<T: BinSize> OffsetTable<T> {
    fn add_offset(&self) -> usize {
        self.data.iter().map(|d| d.bin_size()).sum()
    }
}
impl<T> From<Vec<T>> for OffsetTable<T> {
    fn from(value: Vec<T>) -> Self {
        Self { data: value }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, BinRead, BinWrite, Default)]
#[br(import_raw(args: OffsetTableArgs<()>))]
#[bw(import_raw(args: OffsetTableWriteArgs))]
pub struct ShaderBlob(
    #[br(args { header_start: args.header_start, inner: () })]
    #[bw(args { offset: args.offset })]
    pub OffsetTable<Instruction>,
);
impl BinSize for ShaderBlob {
    fn bin_size(&self) -> usize {
        self.0.data.len() * 4
    }
}
impl Deref for ShaderBlob {
    type Target = Vec<Instruction>;

    fn deref(&self) -> &Self::Target {
        &self.0.data
    }
}
impl DerefMut for ShaderBlob {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0.data
    }
}

#[binrw]
#[derive(Clone, Debug, PartialEq, Eq, Default, Copy)]
pub struct IoRegisterBitMask(u16);

impl IoRegisterBitMask {
    pub fn mark_used(&mut self, r: Register) {
        self.0 |= 1 << r.index;
    }
}

#[binrw]
#[derive(Clone, Debug, PartialEq, Eq, Default)]
#[bw(import_raw(args: OffsetTableWriteArgs))]
#[br(import_raw(args: OffsetTableArgs<<T as BinRead>::Args<'_>>))]
pub struct SizedTable<T>
where
    T: BinRead + BinWrite + BinSize,
    for<'a> <T as BinRead>::Args<'a>: Clone,
{
    #[bw(calc = args.offset as u32)]
    #[br(temp)]
    offset: u32,
    #[br(temp)]
    #[bw(calc = data.bin_size() as u32)]
    size: u32,
    #[bw(ignore)]
    #[br(args(size, args.inner), seek_before = SeekFrom::Start(args.header_start + offset as u64), restore_position)]
    pub data: MaxSize<T>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MaxSize<T>(pub Vec<T>);

impl<T> Default for MaxSize<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: BinRead> BinRead for MaxSize<T>
where
    for<'a> T::Args<'a>: Clone,
{
    type Args<'a> = (u32, T::Args<'a>);

    fn read_options<R: std::io::prelude::Read + Seek>(
        reader: &mut R,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let to = reader.stream_position()?;
        let end = to + args.0 as u64;
        let mut syms = Vec::new();
        while reader.stream_position()? < end {
            let string = T::read_options(reader, endian, args.1.clone())?;
            syms.push(string);
        }
        assert_eq!(reader.stream_position()?, end);
        Ok(Self(syms))
    }
}

impl<T: BinSize> BinSize for MaxSize<T> {
    fn bin_size(&self) -> usize {
        self.0.iter().map(|d| d.bin_size()).sum()
    }
}

#[derive(NamedArgs)]
pub struct OffsetTableArgs<Inner> {
    header_start: u64,
    inner: Inner,
}

#[derive(NamedArgs)]
pub struct OffsetTableWriteArgs {
    offset: u64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OffsetTable<T> {
    pub data: Vec<T>,
}
impl<T> Default for OffsetTable<T> {
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<T: BinRead + 'static> BinRead for OffsetTable<T>
where
    for<'a> T::Args<'a>: Clone,
{
    type Args<'a> = OffsetTableArgs<T::Args<'a>>;

    fn read_options<R: std::io::prelude::Read + std::io::prelude::Seek>(
        reader: &mut R,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let offset: u32 = reader.read_type(endian)?;
        let count: u32 = reader.read_type(endian)?;
        let pos = reader.stream_position()?;
        reader.seek(SeekFrom::Start(offset as u64 + args.header_start))?;
        let data = reader.read_type_args(
            endian,
            VecArgs {
                count: count as usize,
                inner: args.inner,
            },
        )?;
        // reset the read head
        reader.seek(SeekFrom::Start(pos))?;
        Ok(Self { data })
    }
}

impl<T: BinWrite> BinWrite for OffsetTable<T> {
    type Args<'a> = OffsetTableWriteArgs;

    fn write_options<W: std::io::prelude::Write + std::io::prelude::Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> BinResult<()> {
        writer.write_type(&(args.offset as u32), endian)?;
        writer.write_type(&(self.data.len() as u32), endian)?;
        //writer.write_type_args(&self.data, endian, args.inner)?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct ExeSectionOffset(u32);
impl IntoSeekFrom for ExeSectionOffset {
    fn into_seek_from(self) -> std::io::SeekFrom {
        std::io::SeekFrom::End((self.0 as i64) - 64)
    }
}
#[binrw]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LabelTableEntry {
    #[brw(pad_after = 2)]
    pub id: u16,
    pub location_offset: u32,
    pub location_size: u32,
    pub symbol_offset: u32,
}
impl BinSize for LabelTableEntry {
    fn bin_size(&self) -> usize {
        0x10
    }
}

#[binrw]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstantTableEntry {
    #[brw(magic = 0u16)]
    Bool {
        #[brw(pad_before = 1)]
        register_id: u8,
        #[brw(pad_after = 3)]
        value: u8,
    },
    #[brw(magic = 1u16)]
    IVec4 {
        #[brw(pad_before = 1)]
        register_id: u8,
        x: i8,
        y: i8,
        z: i8,
        #[brw(pad_after = 3)]
        w: i8,
    },
    #[brw(magic = 2u16)]
    Vec4 {
        #[brw(pad_after = 1)]
        register_id: u8,
        #[brw(pad_before = 1)]
        x: Float24,
        #[brw(pad_before = 1)]
        y: Float24,
        #[brw(pad_before = 1)]
        z: Float24,
        #[brw(pad_before = 1)]
        w: Float24,
    },
}
impl BinSize for ConstantTableEntry {
    fn bin_size(&self) -> usize {
        0x14
    }
}

#[binrw]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OutputRegisterEntry {
    #[brw(pad_after = 1)]
    pub ty: OutputProperty,
    pub register_id: u16,
    #[brw(pad_after = 3)]
    pub output_mask: ComponentMask,
}

impl BinSize for OutputRegisterEntry {
    fn bin_size(&self) -> usize {
        0x8
    }
}

#[binrw]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RegisterIndex(
    #[bw(map = Self::register_to_index)]
    #[br(try_map = Self::index_to_register)]
    Register,
);

const REG_TY_IDX_OFFSET: &[(RegisterKind, u16)] = &[
    (RegisterKind::Input, 0),
    (RegisterKind::FloatingVecUniform, 0x10),
    (RegisterKind::IntegerVecUniform, 0x70),
    (RegisterKind::BoolUniform, 0x78),
];

impl RegisterIndex {
    fn register_to_index(r: &Register) -> u16 {
        let offset = REG_TY_IDX_OFFSET
            .iter()
            .find(|(k, _)| k == &r.kind)
            .unwrap()
            .1;
        offset + r.index as u16
    }
    fn index_to_register(idx: u16) -> Result<Register, &'static str> {
        let (k, off) = REG_TY_IDX_OFFSET
            .iter()
            .take_while(|(_, o)| o <= &idx)
            .last()
            .ok_or("out of bounds")?;
        let index = idx - off;
        Ok(Register::new(*k, index as usize))
    }
}

#[binrw]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniformTableEntry {
    pub symbol_offset: u32,
    pub start_register: RegisterIndex,
    pub end_register: RegisterIndex,
}
impl BinSize for UniformTableEntry {
    fn bin_size(&self) -> usize {
        0x8
    }
}

#[binrw]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstantType {
    #[brw(magic = 0u8)]
    Bool,
    #[brw(magic = 1u8)]
    IVec4,
    #[brw(magic = 2u8)]
    Vec4,
}

#[binrw]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShaderType {
    #[brw(magic = 0u8)]
    Vertex,
    #[brw(magic = 1u8)]
    Geometry,
}
