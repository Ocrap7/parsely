use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Compile the source
    Build {
        /// Source input files
        sources: Vec<PathBuf>,
        #[arg(short, long)]
        context: Option<PathBuf>,
        /// Output directory
        #[arg(short, long, default_value = "dist")]
        output: PathBuf,
    },
}
