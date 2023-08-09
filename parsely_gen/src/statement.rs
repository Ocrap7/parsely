use std::fmt::Write;

use parsely_parser::{
    expr::Expression,
    statement::Statement,
    types::{Type, TypeArray},
};

use crate::{
    module::Module,
    types::{array_type, GenType},
    Result,
};

impl Module {
    pub(crate) fn gen_statement(
        &mut self,
        buffer: &mut impl Write,
        stmt: &Statement,
    ) -> Result<()> {
        match stmt {
            Statement::Expression(expr) => {
                self.gen_expression(buffer, &expr.expression)?;
                writeln!(buffer, ";")?;
            }
            Statement::VariableDeclaration(var) => {
                let init = var.init.as_ref().map(|init| {
                    let mut buffer = String::new();
                    self.gen_expression(&mut buffer, &init.expression)
                        .map(|i| (i, buffer))
                });

                match init {
                    Some(Ok((ty @ GenType::Array(_, _), init))) => {
                        assert!(var.arrays.len() > 0);

                        self.gen_type(buffer, &var.ty)?;
                        write!(buffer, " {}", var.ident.value)?;

                        let dimensions = ty.dimensions();
                        self.symbol_table.insert(&var.ident.value, ty);

                        let decl_ty: Vec<_> = var
                            .arrays
                            .iter()
                            .map(|dim| dim.dimension.value.map(|v| v.value as usize))
                            .collect();

                        if decl_ty.len() != dimensions.len() {
                            for dim in decl_ty {
                                write!(buffer, "[")?;
                                if let Some(size) = dim {
                                    write!(buffer, "{}", size)?;
                                }
                                write!(buffer, "]")?;
                            }
                        } else {
                            for dim in dimensions {
                                write!(buffer, "[")?;
                                if let Some(size) = dim {
                                    write!(buffer, "{}", size)?;
                                }
                                write!(buffer, "]")?;
                            }
                        }

                        write!(buffer, " = {}", init)?;

                        writeln!(buffer, ";")?;
                    }
                    Some(Err(e)) => return Err(e),
                    init => {
                        if var.arrays.len() > 0 {
                            write!(buffer, "struct slice")?;

                            let ty = Type::Slice(TypeArray {
                                arrays: var.arrays.clone(),
                                element: var.ty.clone(),
                            });
                            let gt = GenType::from(&ty);

                            if let Some(Ok((ty, _))) = &init {
                                self.symbol_table.insert(&var.ident.value, ty.clone());
                            } else {
                                self.symbol_table.insert(&var.ident.value, gt);
                            }
                        } else {
                            self.gen_type(buffer, &var.ty)?;

                            if let Some(Ok((ty, _))) = &init {
                                self.symbol_table.insert(&var.ident.value, ty.clone());
                            } else {
                                self.symbol_table
                                    .insert(&var.ident.value, var.ty.as_ref().into());
                            }
                        }

                        write!(buffer, " {}", var.ident.value)?;

                        if let Some(Ok((ty, init))) = init {
                            println!("ty: {ty:?}");
                            for dim in ty.array_dimensions() {
                                write!(buffer, "[{dim}]")?;
                            }

                            write!(buffer, " = {}", init)?;
                        }

                        writeln!(buffer, ";")?;
                    }
                }

                // match (&var.ty, &var.init.as_ref().map(|vi| vi.expression.as_ref())) {
                //     (_, Some(Expression::ArrayInit(a))) => {
                //         assert!(var.arrays.len() > 0);

                //         self.gen_type(buffer, &var.ty)?;
                //         write!(buffer, " {}", var.ident.value)?;

                //         let ty = Type::Array(TypeArray {
                //             arrays: var.arrays.clone(),
                //             element: var.ty.clone(),
                //         });

                //         self.symbol_table.insert(&var.ident.value, (&ty).into());

                //         for (dim, size) in var.arrays.iter().zip(a.dimensions()) {
                //             write!(buffer, "[")?;
                //             if let Some(size) = dim.dimension.value.as_ref() {
                //                 write!(buffer, "{}", size.value)?;
                //             } else {
                //                 write!(buffer, "{}", size)?;
                //             }
                //             write!(buffer, "]")?;
                //         }

                //         if let Some(init) = &var.init {
                //             write!(buffer, " = ")?;
                //             self.gen_expression(buffer, &init.expression)?;
                //         }

                //         writeln!(buffer, ";")?;
                //     }
                //     _ => {
                //         if var.arrays.len() > 0 {
                //             write!(buffer, "struct slice")?;

                //             let ty = Type::Slice(TypeArray {
                //                 arrays: var.arrays.clone(),
                //                 element: var.ty.clone(),
                //             });
                //             let gt = GenType::from(&ty);
                //             println!("{:?}", gt);

                //             self.symbol_table.insert(&var.ident.value, gt);
                //         } else {
                //             self.gen_type(buffer, &var.ty)?;

                //             self.symbol_table
                //                 .insert(&var.ident.value, var.ty.as_ref().into());
                //         }
                //         write!(buffer, " {}", var.ident.value)?;
                //         // for dim in &var.arrays {
                //         //     write!(buffer, "[")?;
                //         //     if let Some(size) = dim.dimension.value.as_ref() {
                //         //         write!(buffer, "{}", size.value)?;
                //         //     }
                //         //     write!(buffer, "]")?;
                //         // }

                //         if let Some(init) = &var.init {
                //             write!(buffer, " = ")?;
                //             self.gen_expression(buffer, &init.expression)?;
                //         }

                //         writeln!(buffer, ";")?;
                //     }
                // }
            }
            Statement::IfStatement(stmt) => {
                write!(buffer, "if (")?;
                self.gen_expression(buffer, &stmt.condition)?;

                writeln!(buffer, ") {{")?;
                for stmt in stmt.body.value.iter() {
                    self.gen_statement(buffer, stmt)?;
                }
                writeln!(buffer, "}}")?;
            }
            Statement::WhileLoop(stmt) => {
                write!(buffer, "while (")?;
                self.gen_expression(buffer, &stmt.condition)?;

                writeln!(buffer, ") {{")?;
                for stmt in stmt.body.value.iter() {
                    self.gen_statement(buffer, stmt)?;
                }
                writeln!(buffer, "}}")?;
            }
        }
        Ok(())
    }
}
