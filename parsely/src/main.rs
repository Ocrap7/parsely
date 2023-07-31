use std::{
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    str::FromStr,
};

use parsely_gen::module::Module;
use parsely_lexer::Lexer;
use parsely_parser::ParseStream;

fn main() {
    let path = PathBuf::from_str("examples/test.par").unwrap();

    let mut file = File::open(&path).unwrap();

    let mut buffer = String::with_capacity(256);
    file.read_to_string(&mut buffer).unwrap();

    let tokens = Lexer::run(buffer.as_bytes());
    println!("{:#?}", tokens);
    let stream = ParseStream::from(&tokens);

    let program = stream.parse().unwrap();

    let module = Module::new(path.file_stem().unwrap().to_string_lossy().as_ref());
    let (header, code) = module.run(&program).unwrap();

    let header_path = path.with_extension("h");
    let code_path = path.with_extension("c");

    let mut header_file = File::create(header_path).unwrap();
    let mut code_file = File::create(code_path).unwrap();

    header_file.write(header.as_bytes()).unwrap();
    code_file.write(code.as_bytes()).unwrap();
}
