mod parser;
mod tokenizer;

use crate::parser::get_ast;
use crate::tokenizer::get_tokens;
use std::{io, io::Read};

fn main() {
    let mut buffer = String::new();
    let _: usize = io::stdin().read_to_string(&mut buffer).unwrap();
    println!("{:#?}", get_ast(&get_tokens(&buffer)));
}
