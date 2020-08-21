mod parser;
mod tokenizer;
// mod typer;

use crate::parser::{get_ast, Error};
use crate::tokenizer::{get_tokens, Count};
// use crate::typer::get_types;
use std::env::args;
use std::fs::read_to_string;
use std::process::exit;

macro_rules! PARSE_ERROR {
    () => {
        "\x1b[1m{}\x1b[0m:\
         \x1b[1;4m{}\x1b[0m:\
         \x1b[1;31mParse Error\x1b[0m"
    };
}

macro_rules! EXIT {
    () => {
        exit(1)
    };
}

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        EXIT!();
    }
    let filename: &str = &args[1];
    let source: String = match read_to_string(filename) {
        Ok(source) => source,
        _ => EXIT!(),
    };
    match get_ast(&get_tokens(&source)) {
        Ok(ast) => println!("{:#?}", ast),
        Err(error) => {
            let line: Count = match error {
                Error::EOF => {
                    let mut line: Count = 0;
                    for x in source.chars() {
                        if x == '\n' {
                            line += 1;
                        }
                    }
                    line
                }
                Error::Token(token) => token.line + 1,
            };
            eprintln!(PARSE_ERROR!(), filename, line);
            EXIT!()
        }
    }
}
