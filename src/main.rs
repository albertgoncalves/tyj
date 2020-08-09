mod parser;
mod tokenizer;
mod typer;

use crate::parser::get_ast;
use crate::tokenizer::get_tokens;
use crate::typer::get_types;
use std::io::{stdin, Read};

fn main() {
    let mut buffer = String::new();
    let _: Result<usize, _> = stdin().read_to_string(&mut buffer);
    println!("{:#?}", get_types(&get_ast(&get_tokens(&buffer))));
}
