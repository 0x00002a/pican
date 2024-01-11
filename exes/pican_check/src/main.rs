use clap::Parser;
use codespan_reporting::{
    files::Files,
    term::{self, termcolor::StandardStream, Config},
};
use pican_core::{
    context::{IrContext, PicanContext},
    span::FileId,
};
use pican_frontend::parse_and_lower;

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

fn main() {
    let args = args::Args::parse();

    let mut ctx = PicanContext::new();
    let input_id = ctx.add_file(
        &args.input,
        std::fs::read_to_string(&args.input).expect("failed to read input"),
    );

    let pir_ctx = IrContext::new();
    let Some(pir) = parse_and_lower(input_id, &ctx, &pir_ctx) else {
        print_diagnostics(&ctx.diag.as_codespan(), &ctx.files);
        return;
    };

    let json = serde_json::to_string_pretty(&pir).unwrap();
    println!("{json}");
}
