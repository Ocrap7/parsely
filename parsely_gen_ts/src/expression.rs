use std::fmt::Write;

use parsely_parser::expression::{Expression, Literal};

use crate::{module::Module, Result};

impl Module {
    pub fn gen_expression(&self, buffer: &mut impl Write, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Literal(lit) => self.gen_literal(buffer, lit)?,
            Expression::Ident(i) => write!(buffer, "{}", i.value)?,
            Expression::ArrayInit(array) => {
                for value in array.elements.value.iter() {
                    self.gen_expression(buffer, value)?;
                    write!(buffer, " ")?;
                }
            },
            Expression::Template(t) => write!(buffer, "${{{}}}", t.value.trim())?,
        }
        Ok(())
    }

    pub fn gen_literal(&self, buffer: &mut impl Write, literal: &Literal) -> Result<()> {
        match literal {
            Literal::Int(i) => write!(buffer, "{}", i.value.value)?,
            Literal::Float(f) => write!(buffer, "{}", f.value.value)?,
            Literal::String(s) => write!(buffer, "{}", s.value.value)?,
        }

        Ok(())
    }
}
