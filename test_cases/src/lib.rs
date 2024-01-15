#![allow(unused)]
use codespan_reporting::{
    files::Files,
    term::{self, termcolor::StandardStream, Config},
};
use pican_core::span::FileId;

mod all_programs_compile;

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
