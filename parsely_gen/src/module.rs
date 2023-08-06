use std::{fs::File, io::Write};

use parsely_parser::item::Program;

use crate::{symbols::SymbolTable, Diagnostic, DiagnosticFmt, Result};

pub(crate) const EMPTY_NAME: &str = "";

pub struct Module<'ctx> {
    pub(crate) context: &'ctx inkwell::context::Context,
    pub(crate) module: inkwell::module::Module<'ctx>,
    pub(crate) name: String,
    pub(crate) symbol_table: SymbolTable<'ctx>,
    errors: Vec<Diagnostic>,

    pub(crate) builder: inkwell::builder::Builder<'ctx>,
    pub(crate) alloc_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) basic_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) dirty: bool,
}

impl<'ctx> Module<'ctx> {
    pub fn new(name: impl ToString, context: &'ctx inkwell::context::Context) -> Module {
        Module {
            module: context.create_module(&name.to_string()),
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),

            builder: context.create_builder(),
            alloc_block: None,
            basic_block: None,
            context,
            dirty: false,
        }
    }

    pub fn run_new(name: impl ToString, program: &Program) -> Result<()> {
        let ctx = inkwell::context::Context::create();

        let mut module = Module {
            module: ctx.create_module(&name.to_string()),
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),

            builder: ctx.create_builder(),
            alloc_block: None,
            basic_block: None,
            context: &ctx,
            dirty: false,
        };

        module.symbol_table.push_scope();

        match module.run(program) {
            Ok(_) | Err(Diagnostic::Caught(_)) => {
                println!("{:#?}", module.symbol_table);
                let output = module.module.to_string();

                // println!("{:#?}", program.tokens);
                let fmtr = DiagnosticFmt(&module.errors, &module, program);
                println!("{}", fmtr);

                let mut file = File::create("out.ir").unwrap();
                write!(file, "{}", output).unwrap();
            }
            Err(e) => return Err(e),
        }

        Ok(())
    }

    pub(crate) fn push_error(&mut self, error: Diagnostic) {
        println!("PUshed error");
        self.errors.push(error);
    }

    pub(crate) fn bb(&self) -> &inkwell::basic_block::BasicBlock<'ctx> {
        self.basic_block.as_ref().expect("Expected basic block!")
    }

    pub fn run(&mut self, program: &Program) -> Result<()> {
        for item in &program.items {
            self.gen_item(item)?;
        }

        self.dirty = !self.errors.is_empty();
        Ok(())
    }
}
