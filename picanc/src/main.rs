use anyhow::{anyhow, Context};
use args::CliArgs;
use clap::Parser;
use codespan_reporting::{
    files::Files,
    term::{self, termcolor::StandardStream, Config},
};
use pican::{
    context::{IrContext, PicanContext},
    frontend::parse_and_lower,
    span::FileId,
    ty::PicanTyCheck,
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

fn run(args: &CliArgs, ctx: &mut PicanContext<String>) -> anyhow::Result<()> {
    match &args.op {
        args::Operation::Assemble { output_file, .. } => todo!(),
        args::Operation::Disassemble { .. } => todo!(),
        args::Operation::Check { input_file } => {
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

            pican::asm::from_pir::from_pir(&pir, ctx)
                .map_err(|_| anyhow!("failed to lower PIR to asm"))?;
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
