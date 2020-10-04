#[cfg(test)]
mod test;

use crate::tokenizer::{Count, DECIMAL};
use crate::types::Type;
use std::collections::{BTreeMap, HashMap};
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
pub(crate) enum Error {
    EOF,
    Line(Count),
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
            Some(token) => return Err(Error::Line(token.line)),
            None => return Err(Error::EOF),
        };
    };
}

fn get_type<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
    types: &'c mut HashMap<&'a str, Type<'a>>,
) -> Result<Type<'a>, Error> {
    Ok(match tokens.next() {
        Some(Lex { token: Tkn::Num, .. }) => Type::Num,
        Some(Lex { token: Tkn::Bool, .. }) => Type::Bool,
        Some(Lex { token: Tkn::Str, .. }) => Type::Str,
        Some(Lex { token: Tkn::Null, .. }) => Type::Null,
        Some(Lex { token: Tkn::Undef, .. }) => Type::Undef,
        Some(Lex { token: Tkn::Ident(ident), line }) => {
            if let Some(type_) = types.get(ident) {
                type_.clone()
            } else {
                return Err(Error::Line(*line));
            }
        }
        Some(Lex { token: Tkn::LBrace, .. }) => {
            let mut props: BTreeMap<&str, Type> = BTreeMap::new();
            while let Some(token) = tokens.next() {
                match token {
                    Lex { token: Tkn::Ident(key), line } => {
                        eat_or_error!(tokens, Tkn::Colon);
                        if props
                            .insert(key, get_type(tokens, types)?)
                            .is_some()
                        {
                            return Err(Error::Line(*line));
                        }
                        match tokens.next() {
                            Some(Lex { token: Tkn::Comma, .. }) => (),
                            Some(Lex { token: Tkn::RBrace, .. }) => break,
                            Some(token) => {
                                return Err(Error::Line(token.line))
                            }
                            None => return Err(Error::EOF),
                        }
                    }
                    Lex { token: Tkn::RBrace, .. } => break,
                    token => return Err(Error::Line(token.line)),
                }
            }
            Type::Obj(props)
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
                    _ => args.push(get_type(tokens, types)?),
                }
            }
            eat_or_error!(tokens, Tkn::Arrow);
            Type::Fn(
                vec![(args, get_type(tokens, types)?)].into_iter().collect(),
            )
        }
        Some(token) => return Err(Error::Line(token.line)),
        None => return Err(Error::EOF),
    })
}

fn push_type<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
    types: &'c mut HashMap<&'a str, Type<'a>>,
) -> Result<(), Error> {
    match tokens.next() {
        Some(Lex { token: Tkn::Ident(ident), .. }) => {
            let mut new_type: Type = get_type(tokens, types)?;
            if let Some(mut type_) = types.remove(ident) {
                match (&mut type_, &mut new_type) {
                    (Type::Fn(fn_a), Type::Fn(fn_b)) => {
                        fn_a.append(fn_b);
                        let type_: Type = Type::Fn(fn_a.clone());
                        let _: Option<_> = types.insert(ident, type_.clone());
                        return Ok(());
                    }
                    _ => panic!(),
                }
            } else {
                let _: Option<_> = types.insert(ident, new_type.clone());
            }
        }
        Some(token) => return Err(Error::Line(token.line)),
        None => return Err(Error::EOF),
    }
    Ok(())
}

pub(crate) fn get_sigs<'a, 'b>(
    comments: &'b [&'a str],
) -> Result<HashMap<&'a str, Type<'a>>, Error> {
    let mut types: HashMap<&str, Type> = HashMap::new();
    let mut tokens: Vec<Lex> = Vec::new();
    for comment in comments {
        tokens.extend_from_slice(&get_tokens(comment));
    }
    let mut tokens: Peekable<Iter<Lex>> = tokens.iter().peekable();
    while tokens.peek().is_some() {
        push_type(&mut tokens, &mut types)?;
    }
    Ok(types)
}
