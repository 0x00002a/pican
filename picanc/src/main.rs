use anyhow::{anyhow, Context};
use args::CliArgs;
use binrw::BinRead;
use clap::Parser;
use codespan_reporting::{
    files::Files,
    term::{self, termcolor::StandardStream, Config},
};
use pican::{
    asm::{context::AsmContext, instrs::InstructionPack, lower::lower_to_shbin, shbin::Shbin},
    context::{IrContext, PicanContext},
    frontend::parse_and_lower,
    span::FileId,
    ty::PicanTyCheck,
};
use std::{
    io::{Cursor, Write},
    path::Path,
};

mod args;

fn print_diagnostics<'a>(
    diags: &[codespan_reporting::diagnostic::Diagnostic<FileId>],
    files: &'a impl Files<'a, FileId = FileId>,
) {
    let writer = StandardStream::stdout(codespan_reporting::term::termcolor::ColorChoice::Auto);
    let config = Config::default();
    for diag in diags {
        term::emit(&mut writer.lock(), &config, files, diag).expect("failed to write diagnostic");
    }
}

fn lower_to_asm(
    input_file: &Path,
    ctx: &mut PicanContext<String>,
) -> anyhow::Result<(AsmContext, InstructionPack)> {
    let pir_ctx = IrContext::new();

    let input_id = ctx.add_file(
        input_file,
        std::fs::read_to_string(input_file)
            .with_context(|| anyhow!("failed to open input file for reading"))?,
        //.with_context(|| "failed to open input file for reading".to_owned())?,
    );
    let pir = parse_and_lower(input_id, ctx, &pir_ctx)
        .ok_or_else(|| anyhow!("failed to lower AST to PIR"))?;
    let tycheck = ctx.types_for_module(&pir);
    tycheck
        .check()
        .map_err(|_| anyhow!("typechecking failed"))?;

    pican::asm::from_pir::from_pir(&pir, ctx).map_err(|_| anyhow!("failed to lower PIR to asm"))
}

fn run(args: &CliArgs, ctx: &mut PicanContext<String>) -> anyhow::Result<()> {
    match &args.op {
        args::Operation::Assemble {
            output_file,
            input_file,
        } => {
            if let Some(output_file) = output_file {
                std::fs::create_dir_all(output_file.parent().expect("output file is invalid"))?;
            }

            let (asm_ctx, asm) = lower_to_asm(input_file, ctx)?;

            let bin = lower_to_shbin(&asm_ctx, &asm);

            let blob = bin.to_bytes();

            if let Some(output_file) = output_file {
                std::fs::write(output_file, blob)?;
            } else {
                std::io::stdout().write_all(&blob)?;
            }
        }
        args::Operation::Disassemble { input_file } => {
            let input = std::fs::read(input_file).expect("couldn't open input file");
            let bin = Shbin::read_le(&mut Cursor::new(&input)).expect("failed to parse shbin");
            for instr in bin.dvlp.compiled_blob.iter() {
                println!("{}", instr.to_asm(&bin.dvlp.operand_desc_table.data));
            }
        }
        args::Operation::Check { input_file } => {
            lower_to_asm(input_file, ctx)?;
        }
    }

    Ok(())
}

fn main() {
    let args = CliArgs::parse();

    let mut ctx = PicanContext::new();
    let _ = run(&args, &mut ctx);
    print_diagnostics(&ctx.diag.as_codespan(), &ctx.files);
}
