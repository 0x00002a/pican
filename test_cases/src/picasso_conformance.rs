use binrw::BinReaderExt;
use std::path::Path;

use pican_asm as pasm;
use pican_core::context::{IrContext, PicanContext};

fn run_pican(input: &Path) -> Vec<u8> {
    assert!(input.exists(), "cannot find pican input: {input:?}");
    let mut ctx = PicanContext::new();
    ctx.opts.picasso_compat_bug_for_bug = true;
    let input_id = ctx.add_file(
        input.as_os_str(),
        std::fs::read_to_string(input).expect("failed to read input"),
    );

    let pir_ctx = IrContext::new();
    let pir = pican_frontend::parse_and_lower(input_id, &ctx, &pir_ctx).unwrap();

    let Ok((actx, instrs)) = pasm::from_pir::from_pir(&pir, &ctx) else {
        super::print_diagnostics(&ctx.diag.as_codespan(), &ctx.files);
        panic!("failed to lower PIR to ASM");
    };
    let bin = pasm::lower::lower_to_shbin(&actx, &instrs);
    bin.to_bytes()
}

use pretty_assertions::assert_eq;

include!(concat!(env!("OUT_DIR"), "/picasso_conformance_tests.rs"));
