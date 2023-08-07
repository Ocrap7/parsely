use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{AnyValue, BasicValueEnum, PointerValue},
};
use parsely_parser::{
    item::Noun,
    item::TopLevelItem,
    statement::{Statement, VariableInit},
};

use crate::{
    attempt,
    llvm_value::{Function, Type, TypeBuilder, TypeFlags, Value, Variable},
    module::{Module, EMPTY_NAME},
    raise, ErrorHelper, Result,
};

impl<'ctx> Module<'ctx> {
    pub(crate) fn gen_item(&mut self, item: &TopLevelItem) -> Result<()> {
        match item {
            TopLevelItem::Definition(def) => match def.what.noun {
                Noun::Function(_) => {
                    let args = if let Some(args) = &def.args {
                        args.args
                            .iter()
                            .filter_map(|arg| {
                                attempt!(self, self.gen_type(&arg.ty.ty))
                                    .ok()
                                    .map(|a| (arg.ident.value.as_str(), a))
                            })
                            .collect::<Vec<_>>()
                    } else {
                        Vec::new()
                    };

                    let ret_ty = if let Some(out) = &def.out {
                        attempt!(self, self.gen_type(&out.of_type.ty)).ok()
                    } else {
                        None
                    };

                    let llvm_args = args
                        .iter()
                        .map(|arg| arg.1.llvm.try_into().unwrap())
                        .collect::<Vec<_>>();

                    let void_ty = self.context.void_type().to_type();
                    let ty =
                        BasicTypeEnum::try_from(ret_ty.as_ref().unwrap_or_else(|| &void_ty).llvm)
                            .expect("Unable to get basic type enum")
                            .fn_type(&llvm_args, false);

                    let llvm_func = self.module.add_function(&def.ident.value, ty, None);

                    self.alloc_block = Some(self.context.append_basic_block(llvm_func, "locals"));
                    let entry = self.context.append_basic_block(llvm_func, "entry");
                    self.basic_block = Some(entry);
                    self.return_block = Some(self.context.append_basic_block(llvm_func, "end"));

                    // Add parameter allocas
                    self.builder.position_at_end(self.alloc_block.unwrap());
                    let var_allocs = args
                        .iter()
                        .map(|arg| {
                            self.builder.build_alloca::<BasicTypeEnum>(
                                arg.1
                                    .llvm
                                    .try_into()
                                    .expect("Unable to get basic type from any type"),
                                EMPTY_NAME,
                            )
                        })
                        .collect::<Vec<_>>();

                    if let Some(ret) = &ret_ty {
                        self.return_alloc = Some(
                            self.builder.build_alloca::<BasicTypeEnum>(
                                ret.llvm
                                    .try_into()
                                    .expect("Unable to get basic type from any type"),
                                "ret",
                            ),
                        );
                    }

                    self.builder.position_at_end(entry);

                    let func = Function {
                        fn_type: ty,
                        fn_val: llvm_func,
                        param_types: args.iter().map(|f| f.1.clone()).collect(),
                        return_type: ret_ty.clone(),
                    };

                    self.symbol_table.insert_function(&def.ident.value, func);
                    self.symbol_table.push_scope();

                    for ((arg, param), alloc) in
                        args.iter().zip(llvm_func.get_param_iter()).zip(var_allocs)
                    {
                        self.builder.build_store(alloc, param);

                        self.symbol_table.insert_variable(
                            arg.0,
                            Variable {
                                ty: arg.1.clone(),
                                flags: TypeFlags::empty(),
                                alloc,
                            },
                        );
                    }

                    for (i, stmt) in def.init.as_fn().statements.iter().enumerate() {
                        let Ok(val) = attempt!(self, self.gen_statement(stmt)) else {
                            continue;  
                        };

                        // Set return value to the last statement in the function body (if it has a value)
                        if let Some(val) = val {
                            if i == def.init.as_fn().statements.len() - 1 && ret_ty.is_some() {
                                self.builder.build_store(self.return_alloc.unwrap(), val.llvm);
                            }
                        }
                    }

                    self.builder
                        .build_unconditional_branch(self.return_block.unwrap());
                    self.builder.position_at_end(self.alloc_block.unwrap());
                    self.builder.build_unconditional_branch(entry);

                    self.builder.position_at_end(self.return_block.unwrap());

                    if let Some(ret) = &ret_ty {
                        let value = self.builder.build_load(
                            BasicTypeEnum::try_from(ret.llvm)
                                .expect("Unable to convert to basic type"),
                            self.return_alloc.unwrap(),
                            EMPTY_NAME,
                        );
                        self.builder.build_return(Some(&value));
                    } else {
                        self.builder.build_return(None);
                    }

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
    fn insert_alloca(&self, ty: &Type<'ctx>, name: &str) -> PointerValue<'ctx> {
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

        alloc
    }

    fn gen_statement(&mut self, statement: &Statement) -> Result<Option<Value<'ctx>>> {
        match statement {
            Statement::Definition(def) => match def.what.noun {
                Noun::Variable(_) => {
                    let init = self.gen_variable_init(def.init.as_var())?;
                    let alloc = self.insert_alloca(&init.ty, &def.ident.value);

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
                    let alloc = self.insert_alloca(&init.ty, &def.ident.value);

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
                    let Some(func) = self.symbol_table.find_function(&exe.ident.value) else {
                        return Err(raise!(@not_found => self, exe.ident.clone())).caught();
                    };

                    let val = self.builder.build_call(func.fn_val, &[], EMPTY_NAME);
                    let val = BasicValueEnum::try_from(val.as_any_value_enum());

                    if let (Some(ret), Ok(val)) = (&func.return_type, val) {
                        return Ok(Some(Value {
                            llvm: val,
                            ty: ret.clone(),
                        }));
                    }
                }
                t => panic!("{:?} is not callable", t),
            },
            Statement::Evaluate(eval) => {
                return self.gen_expression(&eval.expr).map(Some);
            }
        }

        Ok(None)
    }

    fn gen_variable_init(&mut self, init: &VariableInit) -> Result<Value<'ctx>> {
        match init {
            VariableInit::Const(c) => self.gen_expression(&c.value),
            VariableInit::Mutable(c) => self.gen_expression(&c.value),
        }
    }
}
