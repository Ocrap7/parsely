use std::{
    fmt::{Display, Write},
    num::NonZeroUsize,
};

use parsely_parser::{
    statement::ArrayDimension,
    types::{Type, TypeArray, TypeInt},
};

use crate::{
    module::{Buffers, Module},
    Result,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenType {
    Int(usize),
    Char,
    Float,
    String,
    Array(Box<GenType>, Option<NonZeroUsize>),
    Slice(Box<GenType>),
    Named(String),
    Function(Box<GenType>),
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
            GenType::String => vec![None],
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
            GenType::Char => write!(f, "char"),
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
            GenType::Function(ret) => write!(f, "func: {ret}"),
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

    /// This function writes a type/name declaration to `buffer` depending on the given parameters.
    ///
    /// `decl_ty` is the type the user wrote for the declared variable
    /// `decl_name` is the name of the declared variable
    /// `decl_arrays` are the array dimensions after the variable name (int a[][2])
    ///
    /// `init_ty` this is the type of the variable initializer (if there is one)
    ///
    /// This function will convert arrays to slice types and char arrays to strings.
    pub(crate) fn adjust_type_decl_init(
        &mut self,
        buffer: &mut impl Write,
        decl_ty: &Type,
        decl_name: &str,
        decl_arrays: &[ArrayDimension],

        init_ty: Option<(&GenType, bool)>,
    ) -> Result<()> {
        match init_ty {
            Some((ty @ GenType::Array(_, _), temp)) => {
                assert!(decl_arrays.len() > 0);

                let mut skip = 0;
                if let Type::Named(i) = decl_ty {
                    if i.value == "char" {
                        write!(buffer, "struct str")?;
                        skip = 1;
                    } else {
                        self.gen_type(buffer, decl_ty)?;
                    }
                } else {
                    self.gen_type(buffer, decl_ty)?;
                }

                write!(buffer, " {}", decl_name)?;

                let dimensions = &ty.dimensions()[skip..];
                self.symbol_table.insert(decl_name, ty.clone());

                let decl_ty: Vec<_> = decl_arrays
                    .iter()
                    .skip(skip)
                    .map(|dim| dim.dimension.value.map(|v| v.value as usize))
                    .collect();

                if decl_ty.len() != dimensions.len() {
                    for dim in decl_ty {
                        write!(buffer, "[")?;
                        if let Some(size) = dim {
                            write!(buffer, "{}", size)?;
                        }
                        write!(buffer, "]")?;
                    }
                } else {
                    for dim in dimensions {
                        write!(buffer, "[")?;
                        if let Some(size) = dim {
                            write!(buffer, "{}", size)?;
                        }
                        write!(buffer, "]")?;
                    }
                }
            }
            _ => {
                'outer: {
                    if decl_arrays.len() > 0 {
                        if let Type::Named(i) = decl_ty {
                            if i.value == "char" {
                                write!(buffer, "struct str")?;

                                if let Some((ty, temp)) = init_ty {
                                    self.symbol_table.insert(decl_name, ty.clone());
                                } else {
                                    self.symbol_table.insert(decl_name, GenType::String);
                                }

                                break 'outer;
                            }
                        }

                        write!(buffer, "struct slice")?;

                        let ty = Type::Slice(TypeArray {
                            arrays: decl_arrays.to_vec(),
                            element: Box::new(decl_ty.clone()),
                        });
                        let gt = GenType::from(&ty);

                        if let Some((ty, temp)) = init_ty {
                            self.symbol_table.insert(decl_name, ty.clone());
                        } else {
                            self.symbol_table.insert(decl_name, gt);
                        }
                    } else {
                        self.gen_type(buffer, decl_ty)?;

                        if let Some((ty, temp)) = init_ty {
                            self.symbol_table.insert(&decl_name, ty.clone());
                        } else {
                            self.symbol_table.insert(decl_name, decl_ty.into());
                        }
                    }
                }

                write!(buffer, " {}", decl_name)?;

                if let Some((ty, temp)) = init_ty {
                    for dim in ty.array_dimensions() {
                        write!(buffer, "[{dim}]")?;
                    }
                }
            }
        }

        Ok(())
    }

    // pub(crate) fn adjust_type_decl(
    //     buffer: &mut impl Write,
    //     decl_ty: &Type,
    //     decl_name: &str,
    //     decl_arrays: &[ArrayDimension],
    // ) -> Result<()> {
    //     Ok(())
    // }
}
