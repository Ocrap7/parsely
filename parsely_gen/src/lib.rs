use thiserror::Error;

pub mod module;
mod item;
mod types;
mod statement;
mod expression;

#[derive(Error, Debug)]
pub enum GenError {
    #[error("error writing to output file buffers")]
    Format(#[from] std::fmt::Error),
}

type Result<T> = std::result::Result<T, GenError>;

#[cfg(test)]
mod tests {
    use super::*;
}
