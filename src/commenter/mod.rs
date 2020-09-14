#[cfg(test)]
mod test;

use crate::tokenizer::{Count, DECIMAL};
use std::iter::Peekable;
use std::slice::Iter;
use std::str::CharIndices;

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
struct Lex<'a> {
    token: Tkn<'a>,
    line: Count,
}

#[derive(Debug, PartialEq)]
struct Prop<'a> {
    pub(crate) key: &'a str,
    pub(crate) value: Type<'a>,
}

#[derive(Debug, PartialEq)]
enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Ident(&'a str),
    Props(Vec<Prop<'a>>),
    Fn { args: Vec<Type<'a>>, return_: Box<Type<'a>> },
}

#[derive(Debug, PartialEq)]
enum Stmt<'a> {
    Obj { ident: &'a str, props: Vec<Prop<'a>> },
    Fn { ident: &'a str, args: Vec<Type<'a>>, return_: Type<'a> },
}

#[derive(Debug, PartialEq)]
struct Sig<'a> {
    statement: Stmt<'a>,
    line: Count,
}

#[derive(Debug, PartialEq)]
enum Error<'a> {
    Token(Lex<'a>),
    Type(Type<'a>),
    EOF,
}

fn get_tokens<'a>(comment: &'a str) -> Vec<Lex<'a>> {
    let mut chars: Peekable<CharIndices> = comment.char_indices().peekable();
    let mut tokens: Vec<Lex> = Vec::with_capacity(comment.len());
    let mut line: Count = 0;

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
                            let _: Option<(usize, char)> = chars.next();
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
                    x => Tkn::Ident(x),
                };
                push!(token);
            }
            _ => (),
        }
    }
    tokens
}

macro_rules! eat {
    ($tokens:expr $(,)?) => {{
        if $tokens.next().is_some() {
            ()
        } else {
            return Err(Error::EOF);
        }
    }};
}

fn get_ident<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<&'a str, Error<'a>> {
    match tokens.next() {
        Some(Lex { token: Tkn::Ident(x), .. }) => Ok(x),
        Some(token) => Err(Error::Token(*token)),
        None => Err(Error::EOF),
    }
}

fn get_type<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Type<'a>, Error<'a>> {
    panic!("{:?}", tokens.peek())
}

fn get_stmt<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Sig<'a>, Error<'a>> {
    Ok(match tokens.peek() {
        Some(Lex { token: Tkn::Obj, line }) => panic!("{:?}", Tkn::Obj),
        Some(Lex { token: Tkn::Fn, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens)?;
            let (args, return_): (Vec<Type>, Type) = {
                match get_type(tokens)? {
                    Type::Fn { args, return_ } => (args, *return_),
                    type_ => return Err(Error::Type(type_)),
                }
            };
            Sig { statement: Stmt::Fn { ident, args, return_ }, line: *line }
        }
        Some(token) => return Err(Error::Token(**token)),
        None => return Err(Error::EOF),
    })
}

fn get_sigs<'a, 'b>(tokens: &'b [Lex<'a>]) -> Result<Vec<Sig<'a>>, Error<'a>> {
    let mut sigs: Vec<Sig> = Vec::new();
    let mut tokens: Peekable<Iter<Lex>> = tokens.iter().peekable();
    while tokens.peek().is_some() {
        sigs.push(get_stmt(&mut tokens)?);
    }
    Ok(sigs)
}
