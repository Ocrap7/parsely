use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use parsely_lexer::{AsSpan, Span};
use parsely_parser::expression::{
    BinOperator, BinOrUnary, ByOrImpl, Expression, Literal, UnaryOperator,
};

use crate::{
    attempt,
    llvm_value::{AsValue, TypeBuilder, Value},
    module::{Module, EMPTY_NAME},
    raise, ErrorHelper, Result,
};

pub enum ValOrExpr<'ctx, 'a> {
    Val(Result<Value<'ctx>>, Span),
    Expr(&'a Expression),
}

impl<'ctx, 'a> ValOrExpr<'ctx, 'a> {
    pub fn val_or(
        self,
        mut f: impl FnMut(&Expression) -> Result<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        match self {
            ValOrExpr::Val(val, _) => val,
            ValOrExpr::Expr(e) => f(e),
        }
    }
}

impl<'ctx, 'a> AsSpan for ValOrExpr<'ctx, 'a> {
    fn as_span(&self) -> Span {
        match self {
            ValOrExpr::Val(_, span) => *span,
            ValOrExpr::Expr(expr) => expr.as_span(),
        }
    }
}

pub enum Op<'ctx, 'a> {
    Bin {
        left: ValOrExpr<'ctx, 'a>,
        op: BinOperator,
        right: &'a Expression,
    },
    Uni {
        expr: ValOrExpr<'ctx, 'a>,
        op: UnaryOperator,
    },
}

impl<'ctx, 'a> From<&'a BinOrUnary> for Op<'ctx, 'a> {
    fn from(value: &'a BinOrUnary) -> Self {
        match value {
            BinOrUnary::BinOp(b) => Op::Bin {
                left: ValOrExpr::Expr(&b.left),
                right: &b.right,
                op: b.op.clone(),
            },
            BinOrUnary::UnaryOp(u) => Op::Uni {
                expr: ValOrExpr::Expr(&u.expr),
                op: u.op.clone(),
            },
        }
    }
}

impl<'ctx, 'a> From<(Result<Value<'ctx>>, Span, &'a ByOrImpl)> for Op<'ctx, 'a> {
    fn from(value: (Result<Value<'ctx>>, Span, &'a ByOrImpl)) -> Self {
        match &value.2 {
            ByOrImpl::OpBy(b) => Op::Bin {
                left: ValOrExpr::Val(value.0, value.1),
                right: &b.expr,
                op: b.op.clone(),
            },
            ByOrImpl::UnaryImpl(u) => Op::Uni {
                expr: ValOrExpr::Val(value.0, value.1),
                op: u.op.clone(),
            },
        }
    }
}

impl<'ctx> Module<'ctx> {
    pub(crate) fn gen_expression(&mut self, expr: &Expression) -> Result<Value<'ctx>> {
        match expr {
            Expression::Value(lit) => self.gen_literal(&lit.lit),
            Expression::ValueOf(v) => {
                let Some(var) = self.symbol_table.find_variable(&v.ident.value) else {
                    return Err(raise!(@not_found => self, v.ident.clone())).caught();
                };

                let value = self.builder.build_load::<BasicTypeEnum>(
                    var.ty.llvm.try_into().expect("Unable to get as basic type"),
                    var.alloc,
                    EMPTY_NAME,
                );

                Ok(Value {
                    llvm: value,
                    ty: var.ty.clone(),
                })
            }
            Expression::Result(res) => {
                let Some(first) = res.op.first.as_ref() else {
                    raise!(@log Error => self, "Found empty expression", res.the_tok.as_span().join(res.of.as_span()));
                    return Err(crate::Diagnostic::Caught(res.op.as_span()));
                };
                let mut acc = self.gen_bin_or_uni(first.into());
                let mut span = first.as_span();

                for expr in res.op.rest.iter() {
                    acc = attempt!(self, self.gen_bin_or_uni((acc, span, expr).into()));
                    span = span.join(expr.as_span());
                }

                acc.caught_span(span)
            }
        }
    }

    fn gen_bin_or_uni(&mut self, op: Op<'ctx, '_>) -> Result<Value<'ctx>> {
        match op {
            Op::Bin {
                left: left_raw,
                right: right_raw,
                op,
            } => {
                let left_span = left_raw.as_span();
                let left = left_raw.val_or(|left| self.gen_expression(left));
                let right = self.gen_expression(right_raw);

                let (left, right) = match (left, right) {
                    (Ok(left), Ok(right)) => (left, right),
                    (Err(e), _) => return Err(e),
                    (_, Err(e)) => return Err(e),
                };

                match (op, &left.ty.llvm, &right.ty.llvm) {
                    // Adding ints
                    (BinOperator::Add(_), AnyTypeEnum::IntType(l), AnyTypeEnum::IntType(r))
                        if l.size_of() == r.size_of() =>
                    {
                        // TODO: maybe we should match on llvm types instead of saying into
                        let result = self.builder.build_int_add(
                            left.llvm.into_int_value(),
                            right.llvm.into_int_value(),
                            EMPTY_NAME,
                        );

                        Ok(result.as_value(left.ty.clone()))
                    }

                    // Incompatible types for operator
                    _ => Err(raise!(@mismatch => self, left_span, right_raw.as_span())).caught(),
                }
            }
            Op::Uni { expr: expr_raw, op } => {
                let expr_span = expr_raw.as_span();
                let expr = expr_raw.val_or(|expr| self.gen_expression(expr))?;

                match op {
                    UnaryOperator::Neg(_) => match &expr.ty.llvm {
                        AnyTypeEnum::IntType(_) => {
                            let result = self
                                .builder
                                .build_int_neg(expr.llvm.into_int_value(), EMPTY_NAME);
                            Ok(result.as_value(expr.ty.clone()))
                        }
                        _ => Err(raise!(@mismatch => self, expr_span)).caught(),
                    },
                }
            }
        }
    }

    fn gen_literal(&mut self, lit: &Literal) -> Result<Value<'ctx>> {
        match lit {
            Literal::Int(i) => {
                let ty = self.context.i64_type().to_type();
                Ok(ty.into_int_type().const_int(i.value, false).as_value(ty))
            }
            Literal::Float(f) => {
                let ty = self.context.f64_type().to_type();
                Ok(ty.into_float_type().const_float(f.value).as_value(ty))
            }
            Literal::Bool(b) => {
                let ty = self.context.bool_type().to_type();
                Ok(ty
                    .into_int_type()
                    .const_int(if b.value { 1 } else { 0 }, false)
                    .as_value(ty))
            }
            Literal::String(s) => {
                let llvm_bt = self.context.i8_type();
                let base_type = llvm_bt.to_type();

                let array_vals: Vec<_> = s
                    .value
                    .chars()
                    .map(|c| llvm_bt.const_int(c as _, false))
                    .collect();

                let array_vals = llvm_bt.const_array(&array_vals);

                let val = Ok(base_type
                    .into_int_type()
                    .array_type(s.value.len() as _)
                    .const_array(&[array_vals])
                    .as_value(base_type.array_type(s.value.len() as _)));
                val
            }
        }
    }
}
