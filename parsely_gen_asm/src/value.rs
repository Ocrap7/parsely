use std::fmt::Display;

#[derive(Debug)]
pub enum Value {
    Register(String),
    Constant(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Register(str) => f.write_str(str),
            Value::Constant(str) => f.write_str(str),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolValue {
    Poison,
    Int(u64),
    Float(f64),
    String(String),
    Pointer(usize),
    Function(),
}

impl SymbolValue {
    pub fn type_str(&self) -> &str {
        match self {
            Self::Poison => "<poison>",
            Self::Int(_) => "int",
            Self::Float(_) => "float",
            Self::String(_) => "string",
            Self::Pointer(_) => "pointer",
            Self::Function() => "function",
        }
    }
}

impl Display for SymbolValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolValue::Poison => write!(f, "<poison>"),
            SymbolValue::Int(i) => write!(f, "{i}"),
            SymbolValue::Float(i) => write!(f, "{i}"),
            SymbolValue::String(i) => write!(f, "\"{i}\""),
            SymbolValue::Pointer(i) => write!(f, "0x{i:x}"),
            SymbolValue::Function() => write!(f, "<function>"),
        }
    }
}
