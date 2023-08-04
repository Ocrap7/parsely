use std::{
    fmt::{Display, Write},
    num::NonZeroUsize,
};

use parsely_parser::{
    typess::{Type, TypeInt},
};

use crate::{
    module::{Buffers, Module},
    Result,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenType {
    Int,
    Float,
    Slice(Box<GenType>),
    Named(String),
}

impl GenType {
    pub fn base_type(&self) -> &GenType {
        match self {
            GenType::Slice(a) => a.base_type(),
            _ => self,
        }
    }

    pub fn dimensions(&self) -> Vec<Option<usize>> {
        match self {
            GenType::Slice(a) => a
                .dimensions()
                .into_iter()
                .chain([None].into_iter())
                .collect(),
            _ => Vec::new(),
        }
    }
}

impl Display for GenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenType::Float => write!(f, "float"),
            GenType::Int => write!(f, "int"),
            GenType::Slice(a) => {
                write!(f, "{}[]", a)?;

                Ok(())
            }
            GenType::Named(name) => write!(f, "{}", name),
        }
    }
}

pub fn slice_type(ty: &Box<Type>, n: usize) -> GenType {
    match n {
        0 => ty.as_ref().into(),
        _ => GenType::Slice(Box::new(slice_type(ty, n - 1))),
    }
}

impl From<&Type> for GenType {
    fn from(value: &Type) -> Self {
        match value {
            Type::Int(n) => GenType::Int,
            Type::Slice(a) => slice_type(&a.element, a.arrays.len()),
            Type::Named(i) => GenType::Named(i.value.clone()),
            t => unimplemented!("{:?}", t),
        }
    }
}

impl Module {
    pub(crate) fn gen_type<B: Write>(&mut self, buffer: &mut B, ty: &Type) -> Result<GenType> {
        match ty {
            Type::Int(TypeInt { size, .. }) => {
                write!(buffer, "int{}", size)?;
                Ok(GenType::Int(*size))
            }
            Type::Str(_) => {
                write!(buffer, "struct str")?;
                Ok(GenType::String)
            }
            Type::Named(ident) => {
                write!(buffer, "{}", ident)?;
                Ok(GenType::Named(ident.value.clone()))
            }
            Type::Void(_) => {
                write!(buffer, "void")?;
                Ok(GenType::Void)
            }
            _ => unimplemented!(),
        }
    }
}
