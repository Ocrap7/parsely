use std::{
    fs::File,
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use parsely_gen_ts::{
    diagnostics::{DiagnosticFmt, DiagnosticModuleFmt},
    module::{Config, Module},
};
use parsely_lexer::{Lexer, Span};
use parsely_parser::program::Program;

mod args;

const EXTENSION: &str = "par";

fn main() {
    let args = args::Args::parse();

    match args.command {
        args::Commands::Build {
            sources,
            output,
            context,
        } => {
            if let Err(e) = std::fs::create_dir_all(&output) {
                print_error(format!(
                    "Unable to open or create output directory `{}`: {e}",
                    output.display()
                ));
            }

            let context_path = context.unwrap_or_else(|| std::env::current_dir().unwrap());
            let config = Config {
                context_path: context_path.into_boxed_path(),
            };

            compile_files(&sources, &output, &output, &config);
        }
    }
}

fn print_error(str: String) {
    let diags = &[parsely_gen_ts::diagnostics::Diagnostic::Message(
        str,
        Span::EMPTY,
        parsely_gen_ts::diagnostics::DiagnosticLevel::Error,
    )];
    let fmt = DiagnosticFmt(diags);
    println!("{fmt}");
}

fn compile_files(sources: &[PathBuf], base: &Path, output: impl AsRef<Path>, config: &Config) {
    for file in sources {
        if file.is_dir() {
            let files = match std::fs::read_dir(file) {
                Ok(files) => files,
                Err(e) => {
                    print_error(format!(
                        "Error reading directory listing `{}`: {e}",
                        file.display()
                    ));
                    continue;
                }
            };

            let sources = files
                .filter_map(|source_file| {
                    let source = match source_file {
                        Ok(file) => file,
                        Err(e) => {
                            print_error(format!(
                                "error reading directory listing `{}`: {e}",
                                file.display()
                            ));
                            return None;
                        }
                    };

                    Some(source.path())
                })
                .collect::<Vec<_>>();

            let output_file_dir = base.join(file);
            if let Err(e) = std::fs::create_dir_all(&output_file_dir) {
                print_error(format!(
                    "Unable to open or create output directory `{}`: {e}",
                    output.as_ref().display()
                ));
            }

            compile_files(&sources, base, output_file_dir, config);
            continue;
        } else if !file.is_file() {
            print_error(format!("Input path `{}` is not a file", file.display()));
            continue;
        }

        if file
            .extension()
            .map(|file| file != EXTENSION)
            .unwrap_or(false)
        {
            continue;
        }

        let Some(filename) = file.file_name() else {
            print_error(format!("Input path `{}` is not a file", file.display()));
            continue;
        };

        let output = output.as_ref().join(filename).with_extension("ts");
        match compile_file(&file, output, config) {
            Ok((module, program)) => {
                let fmt = DiagnosticModuleFmt(module.diagnostics(), &module, &program);
                print!("{fmt}");
            }
            Err(e) => {
                match e.kind() {
                    io::ErrorKind::NotFound => {
                        print_error(format!("File `{}` not found", file.display()))
                    }
                    _ => print_error(format!("IO error when compiling file: {e}")),
                };
            }
        }
    }
}

fn compile_file(
    input: impl AsRef<Path>,
    output: impl AsRef<Path>,
    config: &Config,
) -> io::Result<(Module, Program)> {
    let mut file = File::open(&input)?;

    let mut buffer = String::with_capacity(256);
    file.read_to_string(&mut buffer)?;

    let tokens = Lexer::run(buffer.as_bytes());
    let program = Program::new(&input, buffer, tokens).parse().unwrap();

    let module = Module::run_new(
        input
            .as_ref()
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .as_ref(),
        &program,
        config,
    )
    .unwrap();

    let mut output_file = File::create(&output)?;
    output_file.write_all(module.buffer.as_bytes())?;

    Ok((module, program))
}
