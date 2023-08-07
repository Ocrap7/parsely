use std::{fs::File, io::Write};

use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
};
use parsely_parser::item::Program;

use crate::{symbols::SymbolTable, Diagnostic, DiagnosticFmt, Result};

pub(crate) const EMPTY_NAME: &str = "";

pub struct Module<'ctx> {
    pub(crate) context: &'ctx inkwell::context::Context,
    pub(crate) module: inkwell::module::Module<'ctx>,
    pub(crate) name: String,
    pub(crate) symbol_table: SymbolTable<'ctx>,
    errors: Vec<Diagnostic>,
    pub(crate) dirty: bool,

    pub(crate) builder: inkwell::builder::Builder<'ctx>,
    pub(crate) alloc_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) basic_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) return_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    pub(crate) return_alloc: Option<inkwell::values::PointerValue<'ctx>>,

    pub(crate) target: Target,
    pub(crate) target_machine: TargetMachine,
    pub(crate) target_data: TargetData,
}

impl<'ctx> Module<'ctx> {
    pub fn new(name: impl ToString, context: &'ctx inkwell::context::Context) -> Module {
        let (target, target_machine, target_data) = Self::default_target();

        Module {
            module: context.create_module(&name.to_string()),
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            dirty: false,

            builder: context.create_builder(),
            alloc_block: None,
            basic_block: None,
            return_block: None,
            return_alloc: None,

            context,
            target,
            target_machine,
            target_data,
        }
    }

    pub fn run_new(name: impl ToString, program: &Program) -> Result<()> {
        let ctx = inkwell::context::Context::create();
        let (target, target_machine, target_data) = Self::default_target();

        let mut module = Module {
            module: ctx.create_module(&name.to_string()),
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            dirty: false,

            builder: ctx.create_builder(),
            alloc_block: None,
            basic_block: None,
            return_block: None,
            return_alloc: None,

            context: &ctx,
            target,
            target_machine,
            target_data,
        };

        module.symbol_table.push_scope();

        match module.run(program) {
            Ok(_) | Err(Diagnostic::Caught(_)) => {
                println!("{:#?}", module.symbol_table);
                let output = module.module.to_string();

                // println!("{:#?}", program.tokens);
                let fmtr = DiagnosticFmt(&module.errors, &module, program);
                println!("{}", fmtr);

                let mut file = File::create("out.ll").unwrap();
                write!(file, "{}", output).unwrap();
                file.flush().unwrap();
                println!("Here");
            }
            Err(e) => return Err(e),
        }

        Ok(())
    }

    pub(crate) fn push_error(&mut self, error: Diagnostic) {
        self.errors.push(error);
    }

    pub(crate) fn bb(&self) -> &inkwell::basic_block::BasicBlock<'ctx> {
        self.basic_block.as_ref().expect("Expected basic block!")
    }

    pub fn run(&mut self, program: &Program) -> Result<()> {
        for item in &program.items {
            self.gen_item(item)?;
        }

        self.dirty = self.errors.len() != 0;
        Ok(())
    }

    fn default_target() -> (Target, TargetMachine, TargetData) {
        Target::initialize_all(&InitializationConfig::default());
        let target = Target::from_triple(&TargetMachine::get_default_triple())
            .expect("Unable to get target");

        let target_machine = target
            .create_target_machine(
                &TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_name().to_str().unwrap(),
                TargetMachine::get_host_cpu_features().to_str().unwrap(),
                inkwell::OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("Unable to create target machine");

        let target_data = target_machine.get_target_data();
        (target, target_machine, target_data)
    }
}
