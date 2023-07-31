use std::{fmt::Write, num::NonZeroUsize};

use parsely_parser::{
    expr::{Expression, Literal},
    types::Type,
};

use crate::{module::Module, types::GenType, Result};

const EMPTY: Type = Type::Empty;

impl Module {
    pub(crate) fn gen_expression(
        &mut self,
        buffer: &mut impl Write,
        expr: &Expression,
    ) -> Result<GenType> {
        match expr {
            Expression::Literal(lit) => self.gen_literal(buffer, lit),
            Expression::Ident(ident) => {
                write!(buffer, "{}", ident.value)?;

                if let Some(ty) = self.symbol_table.find_symbol(&ident.value) {
                    Ok(ty.clone())
                } else {
                    Ok(GenType::Void)
                }
            }
            Expression::BinOp(binop) => {
                let left = self.gen_expression(buffer, &binop.left)?;
                write!(buffer, " {} ", binop.op.as_str())?;
                let right = self.gen_expression(buffer, &binop.right)?;
                Ok(left)
            }
            Expression::Parens(parens) => {
                write!(buffer, "(")?;
                let expr = self.gen_expression(buffer, &parens.value)?;
                write!(buffer, ")")?;

                Ok(expr)
            }
            Expression::ArrayInit(array) => {
                write!(buffer, "{{")?;

                let mut iter = array.elements.value.iter();

                let ty = if let Some(e) = iter.next() {
                    self.gen_expression(buffer, e)?
                } else {
                    return Ok(GenType::Array(Box::new(GenType::Void), None));
                };

                for e in iter {
                    write!(buffer, ", ")?;
                    self.gen_expression(buffer, e)?;
                }

                write!(buffer, "}}")?;

                Ok(GenType::Array(
                    Box::new(ty),
                    NonZeroUsize::new(array.elements.value.len()),
                ))
            }
            Expression::Index(index) => {
                let mut expr = String::new();
                let ty = self.gen_expression(&mut expr, &index.expr)?;

                match &ty {
                    GenType::Array(a, size) => {
                        write!(buffer, "{}[", expr)?;
                        self.gen_expression(buffer, &index.index.value)?;
                        write!(buffer, "]")?;

                        Ok(a.as_ref().clone())
                    }
                    GenType::Slice(a) => {
                        write!(
                            buffer,
                            "(({} (*){}) {}.ptr)[",
                            a.base_type(),
                            a.array_dimensions_string(),
                            expr
                        )?;
                        self.gen_expression(buffer, &index.index.value)?;
                        write!(buffer, "]")?;

                        Ok(a.as_ref().clone())
                    }
                    _ => panic!("Array type is not indexable!"),
                }
            }
            Expression::Slice(slice) => {
                write!(buffer, "{{ .ptr = ")?;
                if slice.range.value.left.is_some() {
                    write!(buffer, "&")?;
                }
                let ty = self.gen_expression(buffer, &slice.expr)?;

                let ty = match &ty {
                    GenType::Array(a, _) => {
                        if let Some(left) = &slice.range.value.left {
                            write!(buffer, "[",)?;
                            self.gen_expression(buffer, left)?;
                            write!(buffer, "]",)?;
                        }

                        write!(buffer, ", .len = sizeof(",)?;

                        self.gen_expression(buffer, &slice.expr)?;
                        write!(buffer, ")/sizeof(",)?;
                        self.gen_expression(buffer, &slice.expr)?;
                        write!(buffer, "[0])",)?;

                        if let Some(right) = &slice.range.value.right {
                            write!(buffer, " - ")?;
                            self.gen_expression(buffer, right)?;
                        }

                        if let Some(left) = &slice.range.value.left {
                            write!(buffer, " - ")?;
                            self.gen_expression(buffer, left)?;
                        }

                        a.as_ref().clone()
                    }
                    GenType::Slice(a) => {
                        write!(buffer, ".ptr",)?;
                        if let Some(left) = &slice.range.value.left {
                            write!(buffer, "[")?;
                            self.gen_expression(buffer, left)?;
                            write!(buffer, "]")?;
                        }
                        write!(buffer, ", .len = ")?;

                        self.gen_expression(buffer, &slice.expr)?;

                        write!(buffer, ".len",)?;
                        if let Some(right) = &slice.range.value.right {
                            write!(buffer, " - ")?;
                            self.gen_expression(buffer, right)?;
                        }

                        if let Some(left) = &slice.range.value.left {
                            write!(buffer, " - ")?;
                            self.gen_expression(buffer, left)?;
                        }

                        a.as_ref().clone()
                    }
                    _ => ty,
                };

                write!(buffer, "}}")?;

                Ok(GenType::Slice(Box::new(ty)))
            }
            _ => unimplemented!(),
        }

        // Ok(&Type::Empty)
    }

    fn gen_literal(&mut self, buffer: &mut impl Write, lit: &Literal) -> Result<GenType> {
        match lit {
            Literal::Int(i) => {
                write!(buffer, "{}", i.value)?;
                Ok(GenType::Int(64))
            }
            Literal::Float(f) => {
                write!(buffer, "{}", f.value)?;
                Ok(GenType::Float)
            }
            // Literal::String(f) => write!(buffer, "\"{}\"", f.value)?,
            Literal::String(f) => {
                write!(
                    buffer,
                    "{{ .len = {}, .value = \"{}\" }}",
                    f.value.value.len(),
                    f.value.value,
                )?;

                Ok(GenType::String)
            }
        }
    }
}
