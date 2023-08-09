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
                let init = var
                    .init
                    .as_ref()
                    .map(|init| {
                        let mut buffer = String::new();
                        self.gen_expression(&mut buffer, &init.expression)
                            .map(|i| (i, buffer))
                    })
                    .and_then(|init| init.ok());


                self.adjust_type_decl_init(
                    buffer,
                    &var.ty,
                    &var.ident.value,
                    &var.arrays,
                    init.as_ref().map(|init| (&init.0 .0, init.0 .1)),
                )?;

                if let Some(((_, temp), init)) = &init {
                    write!(buffer, " = {}", init)?;
                }

                writeln!(buffer, ";")?;
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
            Statement::ReturnStatement(ret) => {
                write!(buffer, "return ")?;
                self.gen_expression(buffer, &ret.expr)?;
                writeln!(buffer, ";")?;
            }
        }
        Ok(())
    }
}
