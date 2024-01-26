use binrw::BinRead;
use std::io::{BufReader, Cursor};

use pican_asm::shbin::Shbin;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let input = &args[1];
    let input = std::fs::read(input).expect("couldn't open input file");
    let bin = Shbin::read_le(&mut Cursor::new(&input)).expect("failed to parse shbin");
    for instr in bin.dvlp.compiled_blob.iter() {
        println!("{}", instr.to_asm(&bin.dvlp.operand_desc_table.data));
    }
}
