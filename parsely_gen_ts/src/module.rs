use std::{fs::File, io::Write};

use parsely_parser::program::Program;

use crate::{
    diagnostics::{Diagnostic, DiagnosticFmt},
    Result,
};

pub struct Module {
    pub(crate) buffer: String,

    pub(crate) name: String,
    errors: Vec<Diagnostic>,
    pub(crate) dirty: bool,
}

impl Module {
    const BUFFER_SIZE_INIT: usize = 1024;

    pub fn run_new(name: impl ToString, program: &Program) -> Result<()> {
        let mut module = Module {
            buffer: String::new(),
            name: name.to_string(),
            errors: Vec::new(),
            dirty: false,
        };

        match module.run(program) {
            Ok(_) | Err(Diagnostic::Caught(_)) => {
                let fmtr = DiagnosticFmt(&module.errors, &module, program);
                println!("{}", fmtr);

                let mut file = File::create("out.ts").unwrap();
                write!(file, "{}", module.buffer).unwrap();
                file.flush().unwrap();
            }
            Err(e) => return Err(e),
        }

        Ok(())
    }

    pub(crate) fn push_error(&mut self, error: Diagnostic) {
        self.errors.push(error);
    }

    pub fn run(&mut self, program: &Program) -> Result<()> {
        let mut buffer = String::with_capacity(Self::BUFFER_SIZE_INIT);

        for item in &program.items {
            self.gen_item(&mut buffer, item)?;
        }

        self.buffer = buffer;

        self.dirty = !self.errors.is_empty();
        Ok(())
    }
}
