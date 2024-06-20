use args::Args;
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

fn run<S: AsRef<str>>(args: &Args, input_id: FileId, ctx: &mut PicanContext<S>) -> Option<()> {
    let pir_ctx = IrContext::new();
    let pir = parse_and_lower(input_id, ctx, &pir_ctx)?;
    let tycheck = ctx.types_for_module(&pir);
    tycheck.check().ok()?;

    if args.dump_pir {
        let json = serde_json::to_string_pretty(&pir).unwrap();
        println!("{json}");
    }
    let Ok(_) = pican::asm::from_pir::from_pir(&pir, ctx) else {
        panic!("failed to lower PIR to ASM");
    };

    Some(())
}

fn main() {
    let args = args::Args::parse();

    let mut ctx = PicanContext::new();
    let input_id = ctx.add_file(
        &args.input,
        std::fs::read_to_string(&args.input).expect("failed to read input"),
    );
    let _ = run(&args, input_id, &mut ctx);
    print_diagnostics(&ctx.diag.as_codespan(), &ctx.files);
}
