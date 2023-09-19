use std::{
    fs::File,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser;
use parsely_diagnostics::{Diagnostic, DiagnosticFmt, DiagnosticLevel, DiagnosticModuleFmt};
use parsely_gen::{
    llvm_codegen::{new_context, LlvmCodegen},
    visitor::Visitor,
};
// use parsely_gen_asm::{module::Module, pack::Pack};
use parsely_lexer::{Lexer, Span};
use parsely_parser::program::Program;

mod args;

const EXTENSION: &str = "par";

fn main() {
    let args = args::Args::parse();

    match args.command {
        args::Commands::Build { sources, output } => {
            if let Err(e) = std::fs::create_dir_all(&output) {
                print_error(format!(
                    "Unable to open or create output directory `{}`: {e}",
                    output.display()
                ));
            }

            compile_files(&sources, &output, &output);
        }
    }
}

fn print_error(str: String) {
    let diags = &[Diagnostic::Message(
        str,
        Span::EMPTY,
        DiagnosticLevel::Error,
    )];
    let fmt = DiagnosticFmt(diags);
    println!("{fmt}");
}

fn compile_files(sources: &[PathBuf], base: &Path, output: impl AsRef<Path>) {
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

            compile_files(&sources, base, output_file_dir);
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

        let output = output.as_ref().join(filename).with_extension("rs");
        match compile_file(&file, output) {
            Ok((module, program)) => {
                // let fmt = DiagnosticModuleFmt(module.diagnostics(), &program);
                // print!("{fmt}");
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

fn compile_file(input: impl AsRef<Path>, output: impl AsRef<Path>) -> io::Result<((), Program)> {
    let mut file = File::open(&input)?;

    let mut buffer = String::with_capacity(256);
    file.read_to_string(&mut buffer)?;

    let tokens = Lexer::run(buffer.as_bytes());
    println!("{tokens:#?}");
    let (program, diagnostics) = Program::new(&input, buffer, tokens).parse().unwrap();
    println!("{:3?}", program.items);

    let dmd = DiagnosticModuleFmt(&diagnostics, &program);
    println!("{dmd}");

    let mut output_file = File::create(&output)?;
    write!(output_file, "{:#?}", program.items)?;

    let mut p = parsely_gen::resolve::DeclarationResolver::new("mymod");
    p.visit_program(&program);

    let mut ires = p.to_interface_resolve();
    ires.visit_program(&program);

    let mut ty_res = ires.to_type_resolver();
    ty_res.visit_program(&program);

    println!("{:#?}", ty_res);

    let ctx = new_context();
    let codegen = LlvmCodegen::new(ty_res, &ctx);
    codegen.gen_program(&program);
    let module = codegen.finish(&output);

    // println!("{:#?}", pack);

    // let module = Module::run_new(
    //     input
    //         .as_ref()
    //         .file_stem()
    //         .unwrap()
    //         .to_string_lossy()
    //         .as_ref(),
    //     &program,
    //     diagnostics,
    // )
    // .unwrap();
    let mut output_file = File::create(output.as_ref().with_extension("ll"))?;
    write!(output_file, "{}", module)?;

    // output_file.wri;

    Ok(((), program))
}
