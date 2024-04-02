use binrw::{BinReaderExt, BinWrite};
use pican::asm::shbin::Shbin;
use pretty_assertions::assert_eq;
use std::io::Cursor;

include!(concat!(env!("OUT_DIR"), "/picasso_match_tests.rs"));
