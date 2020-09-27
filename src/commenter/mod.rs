#[cfg(test)]
mod test;

use crate::tokenizer::{Count, DECIMAL};
use std::iter::Peekable;
use std::slice::Iter;
use std::str::CharIndices;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tkn<'a> {
    Arrow,
    Bool,
    Colon,
    Comma,
    Ident(&'a str),
    LBrace,
    LParen,
    Null,
    Num,
    RBrace,
    RParen,
    Str,
    Undef,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Lex<'a> {
    token: Tkn<'a>,
    pub(crate) line: Count,
}

#[derive(Debug, PartialEq)]
struct Prop<'a> {
    key: &'a str,
    value: Type<'a>,
}

#[derive(Debug, PartialEq)]
enum Type<'a> {
    Bool,
    Fn { args: Vec<Type<'a>>, return_: Box<Type<'a>> },
    Ident(&'a str),
    Null,
    Num,
    Props(Vec<Prop<'a>>),
    Str,
    Undef,
}

#[derive(Debug, PartialEq)]
enum Stmt<'a> {
    Fn { ident: &'a str, args: Vec<Type<'a>>, return_: Type<'a> },
    Obj { ident: &'a str, props: Vec<Prop<'a>> },
}

#[derive(Debug, PartialEq)]
pub(crate) struct Sig<'a> {
    statement: Stmt<'a>,
    line: Count,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error<'a> {
    EOF,
    Token(Lex<'a>),
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

macro_rules! eat_or_error {
    ($tokens:expr, $x:path $(,)?) => {
        match $tokens.next() {
            Some(Lex { token: $x, .. }) => (),
            Some(token) => return Err(Error::Token(*token)),
            None => return Err(Error::EOF),
        };
    };
}

fn get_type<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Type<'a>, Error<'a>> {
    Ok(match tokens.next() {
        Some(Lex { token: Tkn::Num, .. }) => Type::Num,
        Some(Lex { token: Tkn::Bool, .. }) => Type::Bool,
        Some(Lex { token: Tkn::Str, .. }) => Type::Str,
        Some(Lex { token: Tkn::Null, .. }) => Type::Null,
        Some(Lex { token: Tkn::Undef, .. }) => Type::Undef,
        Some(Lex { token: Tkn::Ident(x), .. }) => Type::Ident(x),
        Some(Lex { token: Tkn::LBrace, .. }) => {
            let mut props: Vec<Prop> = Vec::new();
            while let Some(token) = tokens.next() {
                match token {
                    Lex { token: Tkn::Ident(key), .. } => {
                        eat_or_error!(tokens, Tkn::Colon);
                        props.push(Prop { key, value: get_type(tokens)? });
                        match tokens.next() {
                            Some(Lex { token: Tkn::Comma, .. }) => (),
                            Some(Lex { token: Tkn::RBrace, .. }) => break,
                            Some(token) => return Err(Error::Token(*token)),
                            None => return Err(Error::EOF),
                        }
                    }
                    Lex { token: Tkn::RBrace, .. } => break,
                    token => return Err(Error::Token(*token)),
                }
            }
            Type::Props(props)
        }
        Some(Lex { token: Tkn::LParen, .. }) => {
            let mut args: Vec<Type> = Vec::new();
            while let Some(token) = tokens.peek() {
                match token {
                    Lex { token: Tkn::RParen, .. } => {
                        eat!(tokens);
                        break;
                    }
                    Lex { token: Tkn::Comma, .. } => eat!(tokens),
                    _ => args.push(get_type(tokens)?),
                }
            }
            eat_or_error!(tokens, Tkn::Arrow);
            Type::Fn { args, return_: Box::new(get_type(tokens)?) }
        }
        Some(token) => return Err(Error::Token(*token)),
        None => return Err(Error::EOF),
    })
}

fn get_stmt<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Sig<'a>, Error<'a>> {
    Ok(match tokens.next() {
        Some(Lex { token: Tkn::Ident(ident), line }) => {
            let token: Lex = match tokens.peek() {
                Some(lex) => **lex,
                None => return Err(Error::EOF),
            };
            match get_type(tokens)? {
                Type::Props(props) => {
                    Sig { statement: Stmt::Obj { ident, props }, line: *line }
                }
                Type::Fn { args, return_ } => Sig {
                    statement: Stmt::Fn { ident, args, return_: *return_ },
                    line: *line,
                },
                _ => return Err(Error::Token(token)),
            }
        }
        Some(token) => return Err(Error::Token(*token)),
        None => return Err(Error::EOF),
    })
}

pub(crate) fn get_sigs<'a, 'b>(
    comments: &'b [&'a str],
) -> Result<Vec<Sig<'a>>, Error<'a>> {
    let mut tokens: Vec<Lex> = Vec::new();
    for comment in comments {
        tokens.extend_from_slice(&get_tokens(comment));
    }
    let mut sigs: Vec<Sig> = Vec::new();
    let mut tokens: Peekable<Iter<Lex>> = tokens.iter().peekable();
    while tokens.peek().is_some() {
        sigs.push(get_stmt(&mut tokens)?);
    }
    Ok(sigs)
}
