#![feature(box_patterns)]
#![feature(result_option_inspect)]

pub mod llvm_codegen;
pub mod path;
pub mod resolve;
pub mod ty;
pub mod visitor;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
