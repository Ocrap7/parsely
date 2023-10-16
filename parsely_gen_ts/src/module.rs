use std::{fmt::Write as FWrite, fs::File, io::Write, path::Path};

use parsely_parser::program::Program;

use crate::{
    diagnostics::{Diagnostic, DiagnosticModuleFmt},
    Result,
};

pub struct Module {
    pub buffer: String,

    pub(crate) name: String,
    errors: Vec<Diagnostic>,
    pub(crate) dirty: bool,
}

pub struct Config {
    pub context_path: Box<Path>,
}

impl Module {
    const BUFFER_SIZE_INIT: usize = 1024;

    pub fn run_new(name: impl ToString, program: &Program, config: &Config) -> Result<Module> {
        let mut module = Module {
            buffer: String::new(),
            name: name.to_string(),
            errors: Vec::new(),
            dirty: false,
        };

        match module.run(program, config) {
            Ok(_) | Err(Diagnostic::Caught(_)) => Ok(module),
            Err(e) => return Err(e),
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.errors
    }

    pub(crate) fn push_error(&mut self, error: Diagnostic) {
        self.errors.push(error);
    }

    pub fn run(&mut self, program: &Program, config: &Config) -> Result<()> {
        let mut buffer = String::with_capacity(Self::BUFFER_SIZE_INIT);
        let mut input_type = String::new();
        let mut params = String::new();

        let input_name = "Input";

        for item in &program.items {
            if let Some(inputs) = self.gen_item(&mut buffer, item)? {
                writeln!(input_type, "export type {} = {{", input_name)?;
                write!(params, ", {{ ")?;

                for input in inputs.inputs.value.iter() {
                    write!(input_type, "{}", input.ident)?;
                    write!(params, "{}, ", input.ident)?;
                    if input.opt.is_some() {
                        write!(input_type, "?")?;
                    }

                    write!(input_type, ": ")?;
                    writeln!(input_type, "{}", input.template.value.trim())?;
                }

                writeln!(input_type, "}}")?;
                write!(params, "}}: {}", input_name)?;
            }
        }

        // let rel_context_path = Path::rel

        let full_view_path = program.path.canonicalize().unwrap();
        let full_context_path = config.context_path.canonicalize().unwrap().join("context.ts");
        println!("{} {}", full_view_path.display(), config.context_path.display());
        let rel_context_path = pathdiff::diff_paths(&full_context_path, &full_view_path).unwrap();
        let rel_context_path = rel_context_path.display();
        let rel_context_path = rel_context_path.to_string().replace('\\', "/");
        // rel_context_path.

        self.buffer = format!("{}import Context from '{}'\nexport default function render<C extends Context>(ctx: C{}) {{return `{}`}}", input_type, rel_context_path, params, buffer);

        self.dirty = !self.errors.is_empty();
        Ok(())
    }
}
