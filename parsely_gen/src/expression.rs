use std::fmt::Write;

use parsely_parser::expr::{Expression, Literal};

use crate::{module::Module, Result};

impl Module {
    pub(crate) fn gen_expression(
        &mut self,
        buffer: &mut impl Write,
        expr: &Expression,
    ) -> Result<()> {
        match expr {
            Expression::Literal(lit) => self.gen_literal(buffer, lit)?,
            Expression::BinOp(binop) => {
                self.gen_expression(buffer, &binop.left)?;
                write!(buffer, " {} ", binop.op.as_str())?;
                self.gen_expression(buffer, &binop.right)?;
            }
            Expression::Parens(parens) => {
                write!(buffer, "(")?;
                self.gen_expression(buffer, &parens.value)?;
                write!(buffer, ")")?;
            }
            Expression::ArrayInit(array) => {
                write!(buffer, "{{")?;

                let mut iter = array.elements.value.iter();

                if let Some(e) = iter.next() {
                    self.gen_expression(buffer, e)?;
                }

                for e in iter {
                    write!(buffer, ", ")?;
                    self.gen_expression(buffer, e)?;
                }

                write!(buffer, "}}")?;
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn gen_literal(&mut self, buffer: &mut impl Write, lit: &Literal) -> Result<()> {
        match lit {
            Literal::Int(i) => write!(buffer, "{}", i.value)?,
            Literal::Float(f) => write!(buffer, "{}", f.value)?,
            // Literal::String(f) => write!(buffer, "\"{}\"", f.value)?,
            Literal::String(f) => write!(
                buffer,
                "{{ .len = {}, .value = \"{}\" }}",
                f.value.value.len(),
                f.value.value,
            )?,
        }
        Ok(())
    }
}
