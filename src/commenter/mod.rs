#[cfg(test)]
mod test;

use crate::map;
use crate::tokenizer::{Count, DECIMAL};
use crate::types::{Target, Type};
use std::collections::{BTreeMap, HashMap};
use std::iter::Peekable;
use std::slice::Iter;
use std::str::CharIndices;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tkn<'a> {
    Arrow,
    At,
    Bool,
    Colon,
    Comma,
    Ident(&'a str),
    LBrace,
    LBracket,
    LParen,
    Null,
    Num,
    RBrace,
    RBracket,
    RParen,
    Str,
    Undef,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Comment<'a> {
    pub(crate) string: &'a str,
    pub(crate) line: Count,
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

fn get_tokens<'a, 'b>(comment: &'b Comment<'a>) -> Vec<Lex<'a>> {
    let mut chars: Peekable<CharIndices> =
        comment.string.char_indices().peekable();
    let mut tokens: Vec<Lex> = Vec::with_capacity(comment.string.len());
    let mut line: Count = comment.line;

    macro_rules! eat {
        () => {
            let _: Option<(usize, char)> = chars.next();
        };
    }

    macro_rules! push {
        ($token:expr $(,)?) => {{
            tokens.push(Lex { token: $token, line })
        }};
    }

    while let Some((i, x)) = chars.next() {
        match x {
            '\n' => line += 1,
            '(' => push!(Tkn::LParen),
            ')' => push!(Tkn::RParen),
            ',' => push!(Tkn::Comma),
            ':' => push!(Tkn::Colon),
            '@' => push!(Tkn::At),
            '[' => push!(Tkn::LBracket),
            ']' => push!(Tkn::RBracket),
            '{' => push!(Tkn::LBrace),
            '}' => push!(Tkn::RBrace),
            _ if x.is_alphabetic() || x == '_' => {
                let mut k: usize = i;
                loop {
                    if let Some((j, c)) = chars.peek() {
                        k = *j;
                        if c.is_alphabetic()
                            || c.is_digit(DECIMAL)
                            || *c == '_'
                        {
                            eat!();
                        } else {
                            break;
                        }
                    } else {
                        k += 1;
                        break;
                    }
                }
                let token: Tkn = match &comment.string[i..k] {
                    "number" => Tkn::Num,
                    "bool" => Tkn::Bool,
                    "string" => Tkn::Str,
                    "null" => Tkn::Null,
                    "undefined" => Tkn::Undef,
                    x => Tkn::Ident(x),
                };
                push!(token);
            }
            '-' => {
                if let Some((_, '>')) = chars.peek() {
                    push!(Tkn::Arrow);
                    eat!();
                }
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
    types: &'c mut HashMap<Target<'a>, Type<'a>>,
) -> Result<Type<'a>, Error> {
    Ok(match tokens.next() {
        Some(Lex { token: Tkn::Num, .. }) => Type::Num,
        Some(Lex { token: Tkn::Bool, .. }) => Type::Bool,
        Some(Lex { token: Tkn::Str, .. }) => Type::Str,
        Some(Lex { token: Tkn::Null, .. }) => Type::Null,
        Some(Lex { token: Tkn::Undef, .. }) => Type::Undef,
        Some(Lex { token: Tkn::Ident(ident), line }) => {
            if let Some(type_) =
                types.get(&Target { ident: vec![ident], scope: Vec::new() })
            {
                type_.clone()
            } else {
                return Err(Error::Line(*line));
            }
        }
        Some(Lex { token: Tkn::LBracket, .. }) => {
            let type_: Type = get_type(tokens, types)?;
            eat_or_error!(tokens, Tkn::RBracket);
            Type::Array(Box::new(type_))
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
            Type::Fn(map![(args, get_type(tokens, types)?)])
        }
        Some(token) => return Err(Error::Line(token.line)),
        None => return Err(Error::EOF),
    })
}

fn push_type<'a, 'b, 'c>(
    tokens: &'c mut Peekable<Iter<'b, Lex<'a>>>,
    types: &'c mut HashMap<Target<'a>, Type<'a>>,
) -> Result<(), Error> {
    match tokens.next() {
        Some(Lex { token: Tkn::Ident(first_ident), line }) => {
            let mut ident: &str = first_ident;
            let mut scope: Vec<&str> = Vec::new();
            while let Some(Lex { token: Tkn::At, .. }) = tokens.peek() {
                eat!(tokens);
                if let Some(Lex { token: Tkn::Ident(next_ident), .. }) =
                    tokens.next()
                {
                    scope.push(ident);
                    ident = next_ident;
                }
            }
            let target: Target = Target { ident: vec![ident], scope };
            let mut return_type: Type = get_type(tokens, types)?;
            if let Some(mut overload) = types.remove(&target) {
                match (&mut overload, &mut return_type) {
                    (Type::Fn(fn_a), Type::Fn(fn_b)) => {
                        if (fn_a.len() == fn_b.len())
                            && fn_a.keys().all(|key| fn_b.contains_key(key))
                        {
                            return Err(Error::Line(*line));
                        }
                        fn_a.append(fn_b);
                        let type_: Type = Type::Fn(fn_a.clone());
                        let _: Option<_> = types.insert(target, type_);
                        return Ok(());
                    }
                    _ => return Err(Error::Line(*line)),
                }
            } else {
                let _: Option<_> = types.insert(target, return_type);
            }
        }
        Some(token) => return Err(Error::Line(token.line)),
        None => return Err(Error::EOF),
    }
    Ok(())
}

pub(crate) fn get_sigs<'a, 'b>(
    comments: &'b [Comment<'a>],
) -> Result<HashMap<Target<'a>, Type<'a>>, Error> {
    let mut types: HashMap<Target, Type> = HashMap::new();
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
