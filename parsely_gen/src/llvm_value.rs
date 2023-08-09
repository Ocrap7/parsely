bitflags::bitflags! {
    #[derive(Debug, Clone)]
    pub struct TypeFlags: u16 {
        const SIGNED = 0b1;
        const MUTABLE = 0b10;
        const SLICE = 0b100;
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub fn_type: inkwell::types::FunctionType<'a>,
    pub fn_val: inkwell::values::FunctionValue<'a>,
    pub param_types: Vec<Type<'a>>,
    pub return_type: Option<Type<'a>>,
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub ty: Type<'a>,
    pub flags: TypeFlags,
    pub alloc: inkwell::values::PointerValue<'a>,
}

#[derive(Debug, Clone)]
pub struct Type<'a> {
    pub llvm: inkwell::types::AnyTypeEnum<'a>,
    pub flags: TypeFlags,
    pub base_type: Option<Box<Type<'a>>>,
}

impl<'ctx> Type<'ctx> {
    pub fn array_type(&self, size: u32) -> Type<'ctx> {
        use inkwell::types::{AnyType, BasicType};
        Type {
            llvm: inkwell::types::BasicTypeEnum::try_from(self.llvm)
                .expect("Unable to convert to basic type!")
                .array_type(size)
                .as_any_type_enum(),
            flags: TypeFlags::empty(),
            base_type: Some(Box::new(self.clone())),
        }
    }
}

pub trait TypeBuilder<'ctx> {
    fn signed(self, signed: bool) -> Type<'ctx>;
    fn mutable(self, mutable: bool) -> Type<'ctx>;
    fn base(self, base: Type<'ctx>) -> Type<'ctx>;
    fn to_type(self) -> Type<'ctx>;
}

impl<'ctx, T: inkwell::types::AnyType<'ctx>> TypeBuilder<'ctx> for T {
    fn signed(self, signed: bool) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            flags: if signed {
                TypeFlags::SIGNED
            } else {
                TypeFlags::empty()
            },
            base_type: None,
        }
    }

    fn mutable(self, mutable: bool) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            flags: if mutable {
                TypeFlags::MUTABLE
            } else {
                TypeFlags::empty()
            },
            base_type: None,
        }
    }

    fn base(self, base: Type<'ctx>) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            flags: TypeFlags::empty(),
            base_type: Some(Box::new(base)),
        }
    }

    fn to_type(self) -> Type<'ctx> {
        Type {
            llvm: self.as_any_type_enum(),
            flags: TypeFlags::empty(),
            base_type: None,
        }
    }
}

impl<'ctx> TypeBuilder<'ctx> for Type<'ctx> {
    fn signed(mut self, signed: bool) -> Type<'ctx> {
        self.flags.set(TypeFlags::SIGNED, signed);
        self
    }

    fn mutable(mut self, mutable: bool) -> Type<'ctx> {
        self.flags.set(TypeFlags::MUTABLE, mutable);
        self
    }

    fn base(mut self, base: Type<'ctx>) -> Type<'ctx> {
        self.base_type = Some(Box::new(base));
        self
    }

    fn to_type(self) -> Type<'ctx> {
        self
    }
}

impl<'ctx> std::ops::Deref for Type<'ctx> {
    type Target = inkwell::types::AnyTypeEnum<'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.llvm
    }
}

#[derive(Debug)]
pub struct Value<'a> {
    pub llvm: inkwell::values::BasicValueEnum<'a>,
    pub ty: Type<'a>,
}

pub trait AsValue<'ctx> {
    fn as_value(&self, ty: Type<'ctx>) -> Value<'ctx>;
}

impl<'ctx, T: inkwell::values::BasicValue<'ctx>> AsValue<'ctx> for T {
    fn as_value(&self, ty: Type<'ctx>) -> Value<'ctx> {
        Value {
            ty,
            llvm: self.as_basic_value_enum(),
        }
    }
}

// impl<'a> ValueProvider<'a> for inkwell::context::Context {
//     fn int(&self, n: u64) -> Value<'a> {
//         Value {
//             llvm: inkwell::values::BasicValueEnum::IntValue(self.i64_type().const_int(n, false)),
//         }
//     }

//     fn float(&self, f: f64) -> Value<'a> {
//         Value {
//             llvm: inkwell::values::BasicValueEnum::FloatValue(self.f64_type().const_float(n)),
//         }
//     }
// }
