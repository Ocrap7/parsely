use std::{fs::File, io::Write};

use parsely_parser::ast::Program;

use crate::{symbols::SymbolTable, Result};

pub(crate) const EMPTY_NAME: &str = "";

pub struct Module<'ctx> {
    pub(crate) context: &'ctx inkwell::context::Context,
    pub(crate) module: inkwell::module::Module<'ctx>,
    pub(crate) name: String,
    pub(crate) symbol_table: SymbolTable<'ctx>,

    pub(crate) builder: inkwell::builder::Builder<'ctx>,
    pub(crate) alloc_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) basic_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> Module<'ctx> {
    pub fn new(name: impl ToString, context: &'ctx inkwell::context::Context) -> Module {
        Module {
            module: context.create_module(&name.to_string()),
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
            builder: context.create_builder(),
            alloc_block: None,
            basic_block: None,
            context,
        }
    }

    pub fn run_new(name: impl ToString, program: &Program) -> Result<()> {
        let ctx = inkwell::context::Context::create();

        let mut module = Module {
            module: ctx.create_module(&name.to_string()),
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
            builder: ctx.create_builder(),
            alloc_block: None,
            basic_block: None,
            context: &ctx,
        };

        module.symbol_table.push_scope();

        module.run(program).map(|module| {
            println!("{:#?}", module.symbol_table);
            let output = module.module.to_string();

            let mut file = File::create("out.ir").unwrap();
            write!(file, "{}", output).unwrap();
        })
    }

    pub(crate) fn bb(&self) -> &inkwell::basic_block::BasicBlock<'ctx> {
        self.basic_block.as_ref().expect("Expected basic block!")
    }

    pub fn run(mut self, program: &Program) -> Result<Module<'ctx>> {
        for item in &program.items {
            self.gen_item(item)?;
        }
        Ok(self)
    }
}
