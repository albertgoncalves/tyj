mod parser;
mod tokenizer;
mod typer;

use crate::parser::{get_ast, Error as ParseError, Syntax};
use crate::tokenizer::{get_tokens, Count};
use crate::typer::{get_types, Error as TypeError};
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

macro_rules! TYPE_ERROR {
    () => {
        "\x1b[1m{}\x1b[0m:\
         \x1b[1;4m{}\x1b[0m:\
         \x1b[1;31mType Error\x1b[0m:{}"
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
            eprintln!(PARSE_ERROR!(), filename, line);
            EXIT!()
        }
    };

    macro_rules! exit_type_error {
        ($syntax:expr, $error:expr $(,)?) => {{
            eprintln!(TYPE_ERROR!(), filename, $syntax.line + 1, $error);
            EXIT!()
        }};
    }

    match get_types(&ast) {
        Ok((types, indices)) => println!("{:?}\n{:?}", types, indices),
        Err(TypeError::ShadowIdent(syntax)) => {
            exit_type_error!(syntax, "Shadowed Identifier")
        }
        Err(TypeError::UnknownIdent(syntax)) => {
            exit_type_error!(syntax, "Unknown Identifier")
        }
        Err(TypeError::UnhandledExpr(syntax)) => {
            exit_type_error!(syntax, "Unhandled Expression")
        }
        Err(TypeError::UnhandledSyntax(syntax)) => {
            exit_type_error!(syntax, "Unhandled Syntax")
        }
    }
}
