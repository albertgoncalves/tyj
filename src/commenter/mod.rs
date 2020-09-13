#[cfg(test)]
mod test;

use crate::tokenizer::{Count, DECIMAL};
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
enum Tkn<'a> {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    Comma,
    Arrow,
    Obj,
    Fn,
    Ident(&'a str),
    Num,
    Str,
    Bool,
    Null,
    Undef,
}

#[derive(Debug, PartialEq)]
struct Lex<'a> {
    token: Tkn<'a>,
    line: Count,
}

fn get_tokens<'a>(comment: &'a str) -> Vec<Lex<'a>> {
    let mut chars: Peekable<CharIndices> = comment.char_indices().peekable();
    let mut tokens: Vec<Lex> = Vec::with_capacity(comment.len());
    let mut line: Count = 0;

    macro_rules! eat {
        () => {{
            let _: Option<(usize, char)> = chars.next();
        }};
    }

    macro_rules! push {
        ($token:expr $(,)?) => {{
            tokens.push(Lex { token: $token, line })
        }};
    }

    while let Some((i, x)) = chars.next() {
        match x {
            '\n' => line += 1,
            '{' => push!(Tkn::LBrace),
            '}' => push!(Tkn::RBrace),
            '(' => push!(Tkn::LParen),
            ')' => push!(Tkn::RParen),
            ':' => push!(Tkn::Colon),
            ',' => push!(Tkn::Comma),
            '-' if chars.peek() == Some(&(i + 1, '>')) => push!(Tkn::Arrow),
            _ if x.is_alphabetic() || x == '_' => {
                let mut k: usize = i;
                loop {
                    if let Some((j, c)) = chars.peek() {
                        k = *j;
                        if c.is_alphabetic()
                            || c.is_digit(DECIMAL)
                            || *c == '_'
                        {
                            eat!()
                        } else {
                            break;
                        }
                    } else {
                        k += 1;
                        break;
                    }
                }
                let token: Tkn = match &comment[i..k] {
                    "obj" => Tkn::Obj,
                    "fn" => Tkn::Fn,
                    "number" => Tkn::Num,
                    "bool" => Tkn::Bool,
                    "string" => Tkn::Str,
                    "null" => Tkn::Null,
                    "undefined" => Tkn::Undef,
                    ident => Tkn::Ident(ident),
                };
                push!(token);
            }
            _ => (),
        }
    }
    tokens
}
