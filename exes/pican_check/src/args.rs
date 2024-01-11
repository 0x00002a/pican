use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Args {
    #[arg(name = "INPUT")]
    pub input: PathBuf,
}
