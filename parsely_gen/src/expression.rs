use std::{fmt::Write, num::NonZeroUsize};

use parsely_parser::{
    expr::{Expression, Literal},
    statement::ArrayDimension,
    types::{Type, TypeArray},
};

use crate::{module::Module, types::GenType, Result};

impl Module {
    /// Returns Ok((_, true)) when result should be stored in a temporary variable.
    pub(crate) fn gen_expression(
        &mut self,
        buffer: &mut impl Write,
        expr: &Expression,
    ) -> Result<(GenType, bool)> {
        match expr {
            Expression::Literal(lit) => self.gen_literal(buffer, lit),
            Expression::Ident(ident) => {
                write!(buffer, "{}", ident.value)?;

                if let Some(ty) = self.symbol_table.find_symbol(&ident.value) {
                    Ok((ty.clone(), false))
                } else {
                    Ok((GenType::Void, false))
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

                let (ty, temp) = if let Some(e) = iter.next() {
                    self.gen_expression(buffer, e)?
                } else {
                    return Ok((GenType::Array(Box::new(GenType::Void), None), false));
                };

                for e in iter {
                    write!(buffer, ", ")?;
                    self.gen_expression(buffer, e)?;
                }

                write!(buffer, "}}")?;

                Ok((
                    GenType::Array(Box::new(ty), NonZeroUsize::new(array.elements.value.len())),
                    true,
                ))
            }
            Expression::Index(index) => {
                let mut expr = String::new();
                let (ty, temp) = self.gen_expression(&mut expr, &index.expr)?;

                match &ty {
                    GenType::String => {
                        write!(buffer, "({}.ptr)[", expr)?;
                        self.gen_expression(buffer, &index.index.value)?;
                        write!(buffer, "]")?;

                        Ok((GenType::Char, false))
                    }
                    GenType::Array(a, _) => {
                        write!(buffer, "{}[", expr)?;
                        self.gen_expression(buffer, &index.index.value)?;
                        write!(buffer, "]")?;

                        Ok((a.as_ref().clone(), false))
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

                        Ok((a.as_ref().clone(), false))
                    }
                    _ => panic!("Array type is not indexable!"),
                }
            }
            Expression::Slice(slice) => {
                write!(buffer, "{{ .ptr = ")?;
                if slice.range.value.left.is_some() {
                    write!(buffer, "&")?;
                }
                let (ty, temp) = self.gen_expression(buffer, &slice.expr)?;

                let ty = match &ty {
                    GenType::String => {
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

                        write!(buffer, "}}")?;

                        return Ok((GenType::String, true));
                    }
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

                Ok((GenType::Slice(Box::new(ty)), true))
            }
            Expression::Call(call) => {
                let mut args = call.args.value.iter();

                let mut args: Vec<_> = args
                    .map(|arg| -> Result<String> {
                        let mut value = String::new();

                        if let Ok((ty, true)) = self.gen_expression(&mut value, arg) {
                            let var = self.symbol_table.get_temp("tmp").unwrap();
                            self.adjust_type_decl_init(buffer, &ty, &var, &[], Some((&ty, false)))?;
                            writeln!(buffer, "= {};", value)?;
                            Ok(var)
                        } else {
                            Ok(value)
                        }
                    })
                    .collect();

                let (ty, temp) = self.gen_expression(buffer, &call.expr)?;
                write!(buffer, "(")?;

                let mut args = args.into_iter();

                if let Some(arg) = args.next() {
                    write!(buffer, "{}", arg?)?;
                }

                for arg in args {
                    write!(buffer, ", ")?;
                    write!(buffer, "{}", arg?)?;
                }
                write!(buffer, ")")?;

                match ty {
                    GenType::Function(ret) => Ok((ret.as_ref().clone(), false)),
                    _ => Ok((GenType::Void, false)),
                }
            }
            e => unimplemented!("{:?}", e),
        }

        // Ok(&Type::Empty)
    }

    /// Returns Ok((_, true)) when result should be stored in temporary variable
    fn gen_literal(&mut self, buffer: &mut impl Write, lit: &Literal) -> Result<(GenType, bool)> {
        match lit {
            Literal::Int(i) => {
                write!(buffer, "{}", i.value)?;
                Ok((GenType::Int(64), false))
            }
            Literal::Float(f) => {
                write!(buffer, "{}", f.value)?;
                Ok((GenType::Float, false))
            }
            // Literal::String(f) => write!(buffer, "\"{}\"", f.value)?,
            Literal::String(f) => {
                write!(
                    buffer,
                    "{{ .len = {}, .ptr = \"{}\" }}",
                    f.value.value.len(),
                    f.value.value,
                )?;

                Ok((GenType::String, true))
            }
        }
    }
}
