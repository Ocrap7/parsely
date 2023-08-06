use std::{fs::File, io::Read, path::PathBuf, str::FromStr};

use parsely_gen::module::Module;
use parsely_lexer::Lexer;
use parsely_parser::item::Program;

fn main() {
    let path = PathBuf::from_str("examples/test.par").unwrap();

    let mut file = File::open(&path).unwrap();

    let mut buffer = String::with_capacity(256);
    file.read_to_string(&mut buffer).unwrap();

    let tokens = Lexer::run(buffer.as_bytes());
    // println!("{:#?}", tokens);

    // filename without extension
    let module_name = path
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .as_ref()
        .to_string();

    let program = Program::new(path, buffer, tokens).parse().unwrap();
    // println!("{:#?}", program);

    Module::run_new(module_name, &program).unwrap();
}
