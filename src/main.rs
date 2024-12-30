use std::process;
use rugit::{Cli, run};
use clap::Parser;

fn main() {
    let cli = Cli::parse();
    run(&cli).unwrap_or_else(|err| {
        eprintln!("rugit error: {err}");
        process::exit(1);
    });
}
