use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use parsely_parser::ast::{
    BinOp, BinOperator, BinOrUnary, ByOrImpl, Expression, Literal, Operation, UnaryOp,
    UnaryOperator,
};

use crate::{
    llvm_value::{AsValue, TypeBuilder, Value},
    module::{Module, EMPTY_NAME},
    Result,
};

pub enum ValOrExpr<'ctx, 'a> {
    Val(Result<Value<'ctx>>),
    Expr(&'a Box<Expression>),
}

impl<'ctx, 'a> ValOrExpr<'ctx, 'a> {
    pub fn val_or(
        self,
        mut f: impl FnMut(&Expression) -> Result<Value<'ctx>>,
    ) -> Result<Value<'ctx>> {
        match self {
            ValOrExpr::Val(val) => val,
            ValOrExpr::Expr(e) => f(e),
        }
    }
}

pub enum Op<'ctx, 'a> {
    Bin {
        left: ValOrExpr<'ctx, 'a>,
        op: BinOperator,
        right: &'a Box<Expression>,
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

impl<'ctx, 'a> From<(Result<Value<'ctx>>, &'a ByOrImpl)> for Op<'ctx, 'a> {
    fn from(value: (Result<Value<'ctx>>, &'a ByOrImpl)) -> Self {
        match &value.1 {
            ByOrImpl::OpBy(b) => Op::Bin {
                left: ValOrExpr::Val(value.0),
                right: &b.expr,
                op: b.op.clone(),
            },
            ByOrImpl::UnaryImpl(u) => Op::Uni {
                expr: ValOrExpr::Val(value.0),
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
                let var = self.symbol_table.find_variable(&v.ident.value)?;

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
                let first = res.op.first.as_ref().expect("Void expression");
                let mut acc = self.gen_bin_or_uni(first.into());

                for expr in res.op.rest.iter() {
                    acc = self.gen_bin_or_uni((acc, expr).into());
                }

                acc
            }
        }
    }

    fn gen_bin_or_uni(&mut self, op: Op<'ctx, '_>) -> Result<Value<'ctx>> {
        match op {
            Op::Bin { left, right, op } => {
                let left = left.val_or(|left| self.gen_expression(left));
                let right = self.gen_expression(&right);

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
                    _ => Err(crate::GenError::IncompatibleTypes(
                        left.ty.to_string(),
                        right.ty.to_string(),
                    )),
                }
            }
            Op::Uni { expr, op } => {
                let expr = expr.val_or(|expr| self.gen_expression(expr))?;

                match op {
                    UnaryOperator::Neg(_) => match &expr.ty.llvm {
                        AnyTypeEnum::IntType(_) => {
                            let result = self
                                .builder
                                .build_int_neg(expr.llvm.into_int_value(), EMPTY_NAME);
                            Ok(result.as_value(expr.ty.clone()))
                        }
                        _ => Err(crate::GenError::IncompatibleType(expr.ty.to_string())),
                    },
                }
            }
        }
    }

    fn gen_literal(&mut self, lit: &Literal) -> Result<Value<'ctx>> {
        match lit {
            Literal::Int(i) => {
                let ty = self.context.i64_type().as_type();
                Ok(ty.into_int_type().const_int(i.value, false).as_value(ty))
            }
            Literal::Float(f) => {
                let ty = self.context.f64_type().as_type();
                Ok(ty.into_float_type().const_float(f.value).as_value(ty))
            }
            Literal::Bool(b) => {
                let ty = self.context.bool_type().as_type();
                Ok(ty
                    .into_int_type()
                    .const_int(if b.value { 1 } else { 0 }, false)
                    .as_value(ty))
            }
            Literal::String(s) => {
                let llvm_bt = self.context.i8_type();
                let base_type = llvm_bt.as_type();

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
