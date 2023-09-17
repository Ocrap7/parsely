use std::{collections::HashMap, num::NonZeroU32, sync::Arc};

use parsely_parser::NodeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
}

impl Type {
    // pub fn matches(&self, other: &Type) -> bool {
    //     match (&self.kind, &other.kind) {
    //         (TypeKind::Bool, TypeKind::Bool) => true,
    //         (TypeKind::Char, TypeKind::Char) => true,
    //         (TypeKind::Float { .. }, TypeKind::Float { .. }) => true,
    //         (TypeKind::Integer { .. }, TypeKind::Integer { .. }) => true,
    //         _ => false
    //     }
    // }
}

impl Type {
    pub fn new_unit() -> Type {
        Type {
            kind: TypeKind::Unit,
        }
    }

    pub fn new_bool() -> Type {
        Type {
            kind: TypeKind::Bool,
        }
    }

    pub fn new_int(size: Option<u32>) -> Type {
        Type {
            kind: TypeKind::Integer {
                signed: false,
                size: size.and_then(NonZeroU32::new),
            },
        }
    }

    pub fn new_int_nz(size: Option<NonZeroU32>) -> Type {
        Type {
            kind: TypeKind::Integer { signed: true, size },
        }
    }

    pub fn new_uint(size: Option<u32>) -> Type {
        Type {
            kind: TypeKind::Integer {
                signed: false,
                size: size.and_then(NonZeroU32::new),
            },
        }
    }

    pub fn new_uint_nz(size: Option<NonZeroU32>) -> Type {
        Type {
            kind: TypeKind::Integer { signed: true, size },
        }
    }

    pub fn new_float(size: Option<u32>) -> Type {
        Type {
            kind: TypeKind::Float {
                size: size.and_then(NonZeroU32::new),
            },
        }
    }

    pub fn new_char() -> Type {
        Type {
            kind: TypeKind::Char,
        }
    }

    pub fn new_str() -> Type {
        Type {
            kind: TypeKind::Str,
        }
    }

    pub fn new_slice(base: impl Into<Type>) -> Type {
        Type {
            kind: TypeKind::Slice {
                base: Box::new(base.into()),
            },
        }
    }

    pub fn new_index_ref(base: impl Into<Type>, mutable: bool) -> Type {
        Type {
            kind: TypeKind::IndexRef {
                base: Box::new(base.into()),
                mutable,
            },
        }
    }

    pub fn new_ref(base: impl Into<Type>, mutable: bool) -> Type {
        Type {
            kind: TypeKind::Ref {
                base: Box::new(base.into()),
                mutable,
            },
        }
    }

    pub fn new_optional(base: impl Into<Type>) -> Type {
        Type {
            kind: TypeKind::Optional {
                base: Box::new(base.into()),
            },
        }
    }

    pub fn deref_base(&self) -> Type {
        match &self.kind {
            TypeKind::Str => Type::new_char(),
            TypeKind::Slice { base } => base.as_ref().clone(),
            TypeKind::Ref { base, .. } => base.as_ref().clone(),
            _ => Type::new_unit(),
        }
    }

    pub fn index_result(&self) -> Type {
        match &self.kind {
            TypeKind::Alias { base, .. } => base.index_result(),
            TypeKind::Str => Type::new_char(),
            TypeKind::Slice { base } => base.as_ref().clone(),
            TypeKind::Array { base, .. } => base.as_ref().clone(),
            TypeKind::IndexRef { base, .. } => base.as_ref().clone(),
            _ => Type::new_unit(),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self {
            kind: TypeKind::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Unit,
    Bool,
    Char,
    Integer {
        signed: bool,
        size: Option<NonZeroU32>,
    },
    Float {
        size: Option<NonZeroU32>,
    },
    Slice {
        base: Box<Type>,
    },
    Array {
        base: Box<Type>,
        size: usize,
    },
    Tuple {
        fields: Vec<Type>,
    },
    Str,
    Struct {
        id: NodeId,
        fields: HashMap<Arc<str>, Type>,
    },
    Union {
        id: NodeId,
        tag: Option<Box<Type>>,
        fields: HashMap<Arc<str>, Type>,
    },
    Alias {
        id: NodeId,
        base: Box<Type>,
    },
    Ref {
        base: Box<Type>,
        mutable: bool,
    },
    IndexRef {
        base: Box<Type>,
        mutable: bool,
    },
    Optional {
        base: Box<Type>,
    },
    Tbd {
        id: NodeId,
    },
    Function {
        id: NodeId,
        params: HashMap<Arc<str>, Type>,
        return_ty: Box<Type>,
    },
}

#[derive(Debug, Clone)]
pub struct Field {
    id: NodeId,
}
