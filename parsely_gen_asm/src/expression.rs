use std::fmt::Write;

use parsely_lexer::{AsSpan, Tok};
use parsely_parser::expression::{Expression, Literal};

use crate::{module::Module, value::SymbolValue};
use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};

impl Module {
    pub fn gen_expression(
        &self,
        buffer: &mut impl Write,
        expr: &Expression,
    ) -> Result<SymbolValue> {
        match expr {
            Expression::Literal(lit) => self.gen_literal(buffer, lit),
            Expression::Ident(ident) => self.find_symbol(ident).map(|sym| sym.value.clone()),
            // Expression::ArrayInit(array) => {
            //     for value in array.elements.value.iter() {
            //         self.gen_expression(buffer, value)?;
            //         write!(buffer, " ")?;
            //     }
            // }
            Expression::BinOp(op) => {
                let lval = self.gen_expression(buffer, &op.left)?;
                write!(buffer, "{}", op.op.as_str())?;
                let rval = self.gen_expression(buffer, &op.right)?;

                let result = match (&lval, &rval, &op.op) {
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum +]) => {
                        SymbolValue::Int(left + right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum -]) => {
                        SymbolValue::Int(left - right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum *]) => {
                        SymbolValue::Int(left * right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum /]) => {
                        SymbolValue::Int(left / right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum %]) => {
                        SymbolValue::Int(left % right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum &]) => {
                        SymbolValue::Int(left & right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum |]) => {
                        SymbolValue::Int(left | right)
                    }
                    (SymbolValue::Int(left), SymbolValue::Int(right), Tok![enum ^]) => {
                        SymbolValue::Int(left ^ right)
                    }

                    (SymbolValue::Float(left), SymbolValue::Float(right), Tok![enum +]) => {
                        SymbolValue::Float(left + right)
                    }
                    (SymbolValue::Float(left), SymbolValue::Float(right), Tok![enum -]) => {
                        SymbolValue::Float(left - right)
                    }
                    (SymbolValue::Float(left), SymbolValue::Float(right), Tok![enum *]) => {
                        SymbolValue::Float(left * right)
                    }
                    (SymbolValue::Float(left), SymbolValue::Float(right), Tok![enum /]) => {
                        SymbolValue::Float(left / right)
                    }
                    (SymbolValue::Float(left), SymbolValue::Float(right), Tok![enum %]) => {
                        SymbolValue::Float(left % right)
                    }

                    _ => {
                        return Err(Diagnostic::Message(
                            format!(
                                "Cannot apply operator `{}` to types of `{}` and `{}`",
                                op.op.as_str(),
                                lval.type_str(),
                                rval.type_str(),
                            ),
                            op.as_span(),
                            DiagnosticLevel::Error,
                        ))
                    }
                };

                Ok(result)
            }
            Expression::Parens(parens) => {
                write!(buffer, "(")?;
                let result = self.gen_expression(buffer, &parens.value)?;
                write!(buffer, ")")?;

                Ok(result)
            }
            Expression::Poison => {
                write!(buffer, "**poison**")?;
                Ok(SymbolValue::Poison)
            }
        }
    }

    pub fn gen_literal(&self, buffer: &mut impl Write, literal: &Literal) -> Result<SymbolValue> {
        match literal {
            Literal::Int(i) => {
                write!(buffer, "{}", i.value.value)?;
                Ok(SymbolValue::Int(i.value.value))
            }
            Literal::Float(f) => {
                write!(buffer, "{}", f.value.value)?;
                Ok(SymbolValue::Float(f.value.value))
            }
            Literal::String(s) => {
                write!(buffer, "{}", s.value.value)?;
                Ok(SymbolValue::String(s.value.value.clone()))
            }
        }
    }
}
