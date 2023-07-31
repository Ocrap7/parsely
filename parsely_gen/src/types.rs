use std::{
    fmt::{Display, Write},
    num::NonZeroUsize,
};

use parsely_parser::{
    statement::ArrayDimension,
    types::{Type, TypeInt},
};

use crate::{
    module::{Buffers, Module},
    Result,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenType {
    Int(usize),
    Float,
    String,
    Array(Box<GenType>, Option<NonZeroUsize>),
    Slice(Box<GenType>),
    Named(String),
    Void,
}

impl GenType {
    pub fn base_type(&self) -> &GenType {
        match self {
            GenType::Array(a, _) => a.base_type(),
            GenType::Slice(a) => a.base_type(),
            _ => self,
        }
    }

    pub fn dimensions(&self) -> Vec<Option<usize>> {
        match self {
            GenType::Array(a, size) => a
                .dimensions()
                .into_iter()
                .chain([size.map(|s| s.get())].into_iter())
                .collect(),
            GenType::Slice(a) => a
                .dimensions()
                .into_iter()
                .chain([None].into_iter())
                .collect(),
            _ => Vec::new(),
        }
    }

    pub fn array_dimensions(&self) -> Vec<usize> {
        match self {
            GenType::Array(a, size) => a
                .array_dimensions()
                .into_iter()
                .chain([size.unwrap().get()].into_iter())
                .collect(),
            _ => Vec::new(),
        }
    }

    pub fn array_dimensions_string(&self) -> String {
        match self {
            GenType::Array(a, size) => a
                .array_dimensions_string()
                .chars()
                .chain(format!("[{}]", size.unwrap().get()).chars())
                .collect(),
            _ => String::new(),
        }
    }


}

impl Display for GenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenType::Void => write!(f, "void"),
            GenType::Float => write!(f, "float"),
            GenType::Int(n) => write!(f, "int{}", n),
            GenType::String => write!(f, "struct str"),
            GenType::Array(a, size) => {
                write!(f, "{}", a)?;

                write!(f, "[")?;
                if let Some(size) = size {
                    write!(f, "{}", size)?;
                }
                write!(f, "]")?;

                Ok(())
            }
            GenType::Slice(a) => {
                write!(f, "{}[]", a)?;

                Ok(())
            }
            GenType::Named(name) => write!(f, "{}", name),
        }
    }
}

pub fn array_type(ty: &Box<Type>, sizes: &Vec<ArrayDimension>, n: usize) -> GenType {
    match n {
        0 => ty.as_ref().into(),
        _ => GenType::Array(
            Box::new(array_type(ty, sizes, n - 1)),
            sizes[sizes.len() - n]
                .dimension
                .value
                .map(|v| NonZeroUsize::new(v.value as usize).unwrap()),
        ),
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
            Type::Void(_) => Self::Void,
            Type::Int(n) => GenType::Int(n.size),
            // Type::Array(a) => GenType::Array(
            //     Box::new(a.element.as_ref().into()),
            //     a.arrays
            //         .iter()
            //         .map(|a| a.dimension.value.map(|i| i.value as usize))
            //         .collect(),
            // ),
            Type::Array(a) => array_type(&a.element, &a.arrays, a.arrays.len()),
            Type::Slice(a) => slice_type(&a.element, a.arrays.len()),
            Type::Named(i) => GenType::Named(i.value.clone()),
            Type::Str(_) => GenType::String,
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
