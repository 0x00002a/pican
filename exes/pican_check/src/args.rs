use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Args {
    #[arg(name = "INPUT")]
    pub input: PathBuf,
    #[arg(help = "Dump PIR as json", long = "dump-pir", default_value_t = false)]
    pub dump_pir: bool,
}
