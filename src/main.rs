mod parser;
mod tokenizer;

use crate::parser::get_ast;
use crate::tokenizer::get_tokens;
use std::io::{stdin, Read};

fn main() {
    let mut buffer = String::new();
    let _: Result<usize, _> = stdin().read_to_string(&mut buffer);
    println!("{:#?}", get_ast(&get_tokens(&buffer)));
}
