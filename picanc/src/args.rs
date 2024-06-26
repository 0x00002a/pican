use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
pub struct CliArgs {
    #[command(subcommand)]
    pub op: Operation,
}

#[derive(Subcommand)]
pub enum Operation {
    #[command(visible_alias = "asm", about = "Assemble file")]
    Assemble {
        #[arg(help = "File to output shbin", short = 'o', long = "output")]
        output_file: Option<PathBuf>,

        #[arg(help = "Input file to process")]
        input_file: PathBuf,
    },
    #[command(about = "Disassemble shbin")]
    Disassemble {
        #[arg(help = "Input file to process")]
        input_file: PathBuf,
    },
    #[command(about = "Run lints and diagnostics on file without assembling it")]
    Check {
        #[arg(help = "Input file to process")]
        input_file: PathBuf,
    },
}
