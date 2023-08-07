use parsely_parser::types;

use crate::{
    llvm_value::{Type, TypeBuilder, TypeFlags},
    module::Module,
    raise, ErrorHelper, Result,
};

impl<'ctx> Module<'ctx> {
    pub fn slice_type(&self, base: Type<'ctx>) -> Type<'ctx> {
        let struct_ty = self.context.struct_type(
            &[
                self.context
                    .ptr_sized_int_type(&self.target_data, None)
                    .into(),
                base.llvm.try_into().expect("Unable to get basic type"),
            ],
            false,
        );

        Type {
            llvm: struct_ty.into(),
            flags: TypeFlags::SLICE,
            base_type: Some(Box::new(base)),
        }
    }

    pub fn gen_type(&mut self, ty: &types::Type) -> Result<Type<'ctx>> {
        match ty {
            types::Type::Int(_) => Ok(self.context.i64_type().to_type()),
            types::Type::Float(_) => Ok(self.context.f64_type().to_type()),
            types::Type::Bool(_) => Ok(self.context.bool_type().to_type()),
            types::Type::Named(i) => {
                let Some(ty) = self.symbol_table.find_type(&i.value) else {
                    return Err(raise!(@not_found => self, i.clone()).caught());
                };

                return Ok(ty.clone());
            }
            types::Type::Array(a) => {
                let ty = self.gen_type(&a.base_type)?;
                Ok(self.slice_type(ty))
            }
        }
    }
}
