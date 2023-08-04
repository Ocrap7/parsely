use thiserror::Error;

pub mod module;

mod item;
mod expression;
mod llvm_value;
mod symbols;

#[derive(Error, Debug)]
pub enum GenError {
    #[error("error writing to output file buffers")]
    Format(#[from] std::fmt::Error),
    #[error("symbol {0} not found")]
    SymbolNotFound(String),
    #[error("incompatible type {0}")]
    IncompatibleType(String),
    #[error("incompatible type {0} with {1}")]
    IncompatibleTypes(String, String),
}

type Result<T> = std::result::Result<T, GenError>;

#[cfg(test)]
mod tests {}
