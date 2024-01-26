pub mod context;
mod emit;
pub mod float24;
pub mod from_pir;
pub mod instrs;
pub mod ir;
pub mod lower;
pub mod shbin;

pub const MAX_SHBIN_INSTRUCTIONS: usize = 512;
