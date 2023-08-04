use inkwell::{types::BasicTypeEnum, values::PointerValue};
use parsely_parser::{
    item::Noun,
    item::TopLevelItem,
    statement::{Statement, VariableInit},
};

use crate::{
    llvm_value::{Function, Type, TypeFlags, Value, Variable},
    module::{Module, EMPTY_NAME},
    Result,
};

impl<'ctx> Module<'ctx> {
    pub(crate) fn gen_item(&mut self, item: &TopLevelItem) -> Result<()> {
        match item {
            TopLevelItem::Definition(def) => match def.what.noun {
                Noun::Function(_) => {
                    let ty = self.context.void_type().fn_type(&[], false);

                    let func = self.module.add_function(&def.ident.value, ty, None);

                    self.alloc_block = Some(self.context.append_basic_block(func, "locals"));
                    let entry = self.context.append_basic_block(func, "entry");
                    self.basic_block = Some(entry);
                    self.builder.position_at_end(entry);

                    let func = Function {
                        fn_type: ty,
                        fn_val: func,
                        param_types: Vec::new(),
                        return_type: None,
                    };

                    self.symbol_table.insert_function(&def.ident.value, func);
                    self.symbol_table.push_scope();

                    for stmt in def.init.as_fn().statements.iter() {
                        self.gen_statement(stmt)?;
                    }

                    self.builder.position_at_end(self.alloc_block.unwrap());
                    self.builder.build_unconditional_branch(entry);

                    let scope = self.symbol_table.pop_scope();
                    println!("{:#?}", scope);

                    self.basic_block = None;
                }
                _ => unimplemented!(),
            },
        }

        Ok(())
    }

    /// Inserts an alloca instruction at the beginning of the function (the alloc_block)
    ///
    /// I'm not sure if this is necessary but other compilers seem to do this
    fn insert_alloca(&mut self, ty: &Type<'ctx>, name: &str) -> Result<PointerValue<'ctx>> {
        let alloc_block = self.alloc_block.expect("Alloc block not set!");
        let old_block = self.builder.get_insert_block().expect("Block not set");
        self.builder.position_at_end(alloc_block);

        let alloc = self.builder.build_alloca::<BasicTypeEnum>(
            ty.llvm
                .try_into()
                .expect("Unable to get basic type from any type"),
            name,
        );
        self.builder.position_at_end(old_block);

        Ok(alloc)
    }

    fn gen_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Definition(def) => match def.what.noun {
                Noun::Variable(_) => {
                    let init = self.gen_variable_init(def.init.as_var())?;
                    let alloc = self.insert_alloca(&init.ty, &def.ident.value)?;

                    self.builder.build_store(alloc, init.llvm);

                    let var = Variable {
                        alloc,
                        flags: TypeFlags::empty(),
                        ty: init.ty.clone(),
                    };

                    self.symbol_table.insert_variable(&def.ident.value, var);
                }
                Noun::Constant(_) => {
                    let init = self.gen_variable_init(def.init.as_var())?;
                    let alloc = self.insert_alloca(&init.ty, &def.ident.value)?;

                    self.builder.build_store(alloc, init.llvm);

                    let var = Variable {
                        alloc,
                        flags: TypeFlags::empty(),
                        ty: init.ty.clone(),
                    };

                    self.symbol_table.insert_variable(&def.ident.value, var);
                }
                _ => unimplemented!(),
            },
            Statement::Execute(exe) => match &exe.what.noun {
                Noun::Function(_) => {
                    let func = self.symbol_table.find_function(&exe.ident.value)?;

                    self.builder.build_call(func.fn_val, &[], EMPTY_NAME);
                }
                t => panic!("{:?} is not callable", t),
            },
        }

        Ok(())
    }

    fn gen_variable_init(&mut self, init: &VariableInit) -> Result<Value<'ctx>> {
        match init {
            VariableInit::Const(c) => self.gen_expression(&c.value),
            VariableInit::Mutable(c) => self.gen_expression(&c.value),
        }
    }
}
