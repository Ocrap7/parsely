use std::fmt::Write;

use parsely_parser::{expr::Expression, statement::Statement, types::Type};

use crate::{module::Module, Result};

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
                match (&var.ty, &var.init.as_ref().map(|vi| vi.expression.as_ref())) {
                    (_, Some(Expression::ArrayInit(_))) => {
                        self.gen_type(buffer, &var.ty)?;
                        write!(buffer, " {}", var.ident.value)?;

                        for dim in &var.arrays {
                            write!(buffer, "[")?;
                            if let Some(size) = dim.dimension.value.as_ref() {
                                write!(buffer, "{}", size.value)?;
                            }
                            write!(buffer, "]")?;
                        }

                        if let Some(init) = &var.init {
                            write!(buffer, " = ")?;
                            self.gen_expression(buffer, &init.expression)?;
                        }

                        writeln!(buffer, ";")?;
                    }
                    _ => {
                        if var.arrays.len() > 0 {
                            write!(buffer, "struct slice")?;
                        } else {
                            self.gen_type(buffer, &var.ty)?;
                        }
                        write!(buffer, " {}", var.ident.value)?;
                        // for dim in &var.arrays {
                        //     write!(buffer, "[")?;
                        //     if let Some(size) = dim.dimension.value.as_ref() {
                        //         write!(buffer, "{}", size.value)?;
                        //     }
                        //     write!(buffer, "]")?;
                        // }

                        if let Some(init) = &var.init {
                            write!(buffer, " = ")?;
                            self.gen_expression(buffer, &init.expression)?;
                        }

                        writeln!(buffer, ";")?;
                    }
                }
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
