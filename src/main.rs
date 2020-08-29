mod parser;
mod tokenizer;
mod typer;

use crate::parser::{get_ast, Error as ParseError, Syntax};
use crate::tokenizer::{get_tokens, Count};
use crate::typer::{get_types, Error as TypeError, Message};
use std::env::args;
use std::fs::read_to_string;
use std::process::exit;

macro_rules! ERROR {
    () => {
        "{}:{}:\x1b[1m{}\x1b[0m"
    };
}

macro_rules! EXIT {
    () => {
        exit(1)
    };
}

fn get_message(message: &Message) -> &'static str {
    match message {
        Message::ArrayMultiType => "array contains multiple types",
        Message::IdentShadow => "shadowed identifier",
        Message::IdentUninit => "uninitialized identifier",
        Message::IdentUnknown => "unknown identifier",
        Message::IncompatibleTypes => "incompatible types",
        Message::ObjDuplicateKeys => "object contains duplicate keys",
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        EXIT!();
    }
    let filename: &str = &args[1];
    let source: String = match read_to_string(filename) {
        Ok(source) => source,
        Err(_) => EXIT!(),
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
            eprintln!(
                ERROR!(),
                filename,
                syntax.line + 1,
                get_message(&message)
            );
            EXIT!()
        }
    }
}
