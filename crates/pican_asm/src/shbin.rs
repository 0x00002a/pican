use std::{
    io::{Seek, SeekFrom},
    mem::size_of,
};

use binrw::{
    binread, binrw, binwrite,
    file_ptr::{FilePtr, IntoSeekFrom},
    BinRead, BinReaderExt, BinResult, BinWrite, BinWriterExt, NamedArgs, NullString, PosValue,
    VecArgs,
};
use pican_core::{
    properties::OutputProperty,
    register::{Register, RegisterKind, RegisterType},
};

use super::float24::Float24;

#[binrw]
#[doc(alias = "DVLB")]
#[brw(magic = b"DVLB")]
#[derive(Clone, Debug)]
pub struct Shbin {
    pub dvle_count: u32,
    #[br(count = dvle_count)]
    pub dvle_offsets: Vec<u32>,
    pub dvlp: Dvlp,
    #[br(parse_with = binrw::file_ptr::parse_from_iter(dvle_offsets.iter().copied()), seek_before(SeekFrom::Start(0)))]
    pub dvles: Vec<ExecutableSection>,
}

#[binrw]
#[brw(magic = b"DVLP")]
#[derive(Clone, Debug)]
pub struct Dvlp {
    #[br(temp)]
    #[bw(calc = 4098)]
    mversion: u32,
    pub data: [u32; 9],
}

#[binrw]
#[doc(alias = "DVLE")]
#[derive(Clone, Debug)]
#[brw(magic = b"DVLE")]
pub struct ExecutableSectionHeader {
    pub mversion: u16,
    pub shader_ty: ShaderType,
    pub merge_vertex_geo: u8,
    pub main_offset_words: u32,
    pub endmain_offset_words: u32,
    pub used_input_registers: u16,
    pub used_output_registers: u16,
    pub geo_shader_type: u8,
    pub start_float_register_idx: u8,
    pub fully_defined_verts_variable: u8,
    pub fully_defined_verts_fixed: u8,
    /*pub constant_table_offset: u32,
    pub constant_table_count: u32,
    pub label_table_offset: u32,
    pub label_table_count: u32,
    pub output_register_table_offset: u32,
    pub output_register_table_count: u32,
    pub uniform_table_offset: u32,
    pub uniform_table_count: u32,
    pub symbol_table_offset: u32,
    pub symbol_size_bytes: u32,*/
}

#[binread]
#[br(stream = s)]
#[derive(Clone, Debug)]
pub struct ExecutableSection {
    #[br(try_calc = s.stream_position(), dbg)]
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
        args: Self::Args<'_>,
    ) -> BinResult<()> {
        let start = writer.stream_position()?;
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
        writer.write_type(&self.symbol_table.0, endian)?;

        Ok(())
    }
}
pub trait BinSize {
    fn bin_size(&self) -> usize;
}
impl BinSize for NullString {
    fn bin_size(&self) -> usize {
        self.0.len()
    }
}

impl<T: BinSize> OffsetTable<T> {
    fn add_offset(&self) -> usize {
        self.data.iter().map(|d| d.bin_size()).sum()
    }
}

#[derive(Clone, Debug)]
pub struct SizedTable<T>(pub Vec<T>);

impl<T: BinRead + 'static> BinRead for SizedTable<T>
where
    for<'a> T::Args<'a>: Clone,
{
    type Args<'a> = OffsetTableArgs<T::Args<'a>>;

    fn read_options<R: std::io::prelude::Read + Seek>(
        reader: &mut R,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> BinResult<Self> {
        let offset: u32 = reader.read_type(endian)?;
        let size: u32 = reader.read_type(endian)?;
        let pos = reader.stream_position()?;
        let to = args.header_start + offset as u64;
        reader.seek(SeekFrom::Start(to))?;
        let mut syms = Vec::new();
        let end = to + size as u64;
        while reader.stream_position()? < end {
            let string = T::read_options(reader, endian, args.inner.clone())?;
            syms.push(string);
        }
        assert_eq!(reader.stream_position()?, end);

        reader.seek(SeekFrom::Start(pos))?;
        Ok(Self(syms))
    }
}
impl<T: BinWrite + BinSize> BinWrite for SizedTable<T> {
    type Args<'a> = OffsetTableWriteArgs;

    fn write_options<W: std::io::prelude::Write + Seek>(
        &self,
        writer: &mut W,
        endian: binrw::Endian,
        args: Self::Args<'_>,
    ) -> BinResult<()> {
        writer.write_type(&args.offset, endian)?;
        let size = self.0.iter().map(|s| s.bin_size()).sum::<usize>() as u32;
        writer.write_type(&size, endian)?;
        Ok(())
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

#[derive(Clone, Debug)]
pub struct OffsetTable<T> {
    pub data: Vec<T>,
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
        println!("header start: {}", args.header_start);
        let offset: u32 = reader.read_type(endian)?;
        let count: u32 = reader.read_type(endian)?;
        println!("off: {offset} {count}");
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
        writer.write_type(&args.offset, endian)?;
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
pub enum ConstantTableEntry {
    #[brw(magic = 0u8)]
    Bool { register_id: u8, value: u8 },
    #[brw(magic = 1u8)]
    IVec4 {
        register_id: u8,
        x: u8,
        y: u8,
        z: u8,
        w: u8,
    },
    #[brw(magic = 2u8)]
    Vec4 {
        register_id: u8,
        x: Float24,
        y: Float24,
        z: Float24,
        w: Float24,
    },
}
impl BinSize for ConstantTableEntry {
    fn bin_size(&self) -> usize {
        0x14
    }
}

#[binrw]
#[derive(Clone, Copy, Debug)]
pub struct OutputRegisterEntry {
    #[brw(pad_after = 1)]
    pub ty: OutputProperty,
    pub register_id: u16,
    #[brw(pad_after = 2)]
    pub output_mask: SwizzleMask,
}

impl BinSize for OutputRegisterEntry {
    fn bin_size(&self) -> usize {
        0x8
    }
}

#[binrw]
#[derive(Clone, Copy, Debug)]
pub struct SwizzleMask(u16);

#[binrw]
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
pub enum ShaderType {
    #[brw(magic = 0u8)]
    Vertex,
    #[brw(magic = 1u8)]
    Geometry,
}

#[cfg(test)]
mod tests {
    use std::io::{Cursor, SeekFrom};

    use binrw::{binread, BinReaderExt, BinWrite};

    use super::Shbin;

    #[test]
    fn try_example() {
        let input = include_bytes!("./example.shbin");
        let shbin: Shbin = Cursor::new(input).read_le().unwrap();
        let mut w = Cursor::new(Vec::new());
        shbin.write_le(&mut w).unwrap();
        println!("{shbin:#?}");
        assert_eq!(w.into_inner(), input);
    }
}
