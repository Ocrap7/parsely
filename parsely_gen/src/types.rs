use std::fmt::Write;

use parsely_parser::types::{Type, TypeInt};

use crate::{
    module::{Buffers, Module},
    Result,
};

impl Module {
    pub(crate) fn gen_type<B: Write>(&mut self, buffer: &mut B, ty: &Type) -> Result<()> {
        match ty {
            Type::Int(TypeInt { size, .. }) => write!(buffer, "int{}", size)?,
            Type::Str(_) => write!(buffer, "struct str")?,
            Type::Named(ident) => write!(buffer, "{}", ident)?,
            Type::Void(_) => write!(buffer, "void")?,
        }
        Ok(())
    }
}
