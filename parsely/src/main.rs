use std::{fs::File, io::Read, path::PathBuf, str::FromStr};

use parsely_gen::module::Module;
use parsely_lexer::Lexer;
use parsely_parser::{item::Program, ParseStream};

fn main() {
    let path = PathBuf::from_str("examples/test.par").unwrap();

    let mut file = File::open(&path).unwrap();

    let mut buffer = String::with_capacity(256);
    file.read_to_string(&mut buffer).unwrap();

    let tokens = Lexer::run(buffer.as_bytes());
    println!("{:#?}", tokens);
    let stream = ParseStream::from(&tokens);

    let program: Program = stream.parse().unwrap();
    println!("{:#?}", program);

    Module::run_new(
        path.file_stem().unwrap().to_string_lossy().as_ref(),
        &program,
    )
    .unwrap();
}
