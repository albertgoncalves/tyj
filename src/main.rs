mod parser;
mod tokenizer;
mod typer;

use crate::parser::{get_ast, Error as ParseError, Syntax};
use crate::tokenizer::{get_tokens, Count};
use crate::typer::{get_types, Error as TypeError};
use std::env::args;
use std::fs::read_to_string;
use std::process::exit;

macro_rules! ERROR {
    () => {
        "{}:\x1b[4m{}\x1b[0m:\x1b[1m{}\x1b[0m"
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
    let ast: Vec<Syntax> = match get_ast(&get_tokens(&source)) {
        Ok(ast) => ast,
        Err(error) => {
            let line: Count = match error {
                ParseError::EOF => {
                    let mut line: Count = 0;
                    for x in source.chars() {
                        if x == '\n' {
                            line += 1;
                        }
                    }
                    line
                }
                ParseError::Token(token) => token.line + 1,
            };
            eprintln!(ERROR!(), filename, line, "parse error");
            EXIT!()
        }
    };
    match get_types(&ast) {
        Ok(table) => println!("{:#?}", table),
        Err(TypeError { syntax, message }) => {
            eprintln!(ERROR!(), filename, syntax.line + 1, message);
            EXIT!()
        }
    }
}
