#[cfg(test)]
mod test;

use std::iter::Peekable;
use std::str::CharIndices;

pub(crate) type Count = u8;

const OPS: [char; 12] =
    ['=', '.', '+', '-', '*', '%', '<', '>', '!', '&', '|', '/'];
const DECIMAL: u32 = 10;

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum Tkn<'a> {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    Semicolon,
    Comma,
    Var,
    If,
    Else,
    Ternary,
    Switch,
    Case,
    Break,
    Default,
    For,
    While,
    Fn,
    Op(&'a str),
    Ret,
    Ident(&'a str),
    Null,
    Undef,
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Illegal(&'a str),
    Comment(&'a str),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct TknPlus<'a> {
    pub(crate) token: Tkn<'a>,
    pub(crate) line: Count,
}

fn is_numeric(c: char) -> bool {
    c.is_digit(DECIMAL) || (c == '.')
}

fn is_op(c: char) -> bool {
    for op in &OPS {
        if c == *op {
            return true;
        }
    }
    false
}

pub(crate) fn get_tokens(source: &str) -> Vec<TknPlus> {
    let mut chars: Peekable<CharIndices> = source.char_indices().peekable();
    let mut tokens: Vec<TknPlus> = Vec::with_capacity(source.len());
    let mut line: Count = 0;

    macro_rules! eat {
        () => {{
            let _: Option<(usize, char)> = chars.next();
        }};
    }

    macro_rules! get_substring {
        ($f:expr, $i:expr $(,)?) => {{
            let mut k: usize = $i;
            loop {
                if let Some((j, c)) = chars.peek() {
                    k = *j;
                    if $f(*c) {
                        eat!()
                    } else {
                        break;
                    }
                } else {
                    k += 1;
                    break;
                }
            }
            &source[$i..k]
        }};
    }

    while let Some((i, c)) = chars.next() {
        match c {
            '\n' => line += 1,
            _ if c.is_whitespace() => (),
            _ if c.is_alphabetic() => {
                let ident: &str = get_substring!(
                    |c: char| c.is_alphabetic() || c.is_digit(DECIMAL),
                    i,
                );
                let token: Tkn = match ident {
                    "var" => Tkn::Var,
                    "if" => Tkn::If,
                    "else" => Tkn::Else,
                    "switch" => Tkn::Switch,
                    "case" => Tkn::Case,
                    "break" => Tkn::Break,
                    "default" => Tkn::Default,
                    "for" => Tkn::For,
                    "while" => Tkn::While,
                    "function" => Tkn::Fn,
                    "return" => Tkn::Ret,
                    "null" => Tkn::Null,
                    "undefined" => Tkn::Undef,
                    "new" => Tkn::Op(ident),
                    "true" | "false" => Tkn::Bool(ident),
                    _ => Tkn::Ident(ident),
                };
                tokens.push(TknPlus { token, line });
            }
            _ if is_numeric(c) => {
                let num: &str = get_substring!(is_numeric, i);
                if num == "." {
                    tokens.push(TknPlus { token: Tkn::Op("."), line });
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|c: char| c.is_digit(DECIMAL)).count())
                {
                    tokens.push(TknPlus { token: Tkn::Num(num), line });
                } else {
                    tokens.push(TknPlus { token: Tkn::Illegal(num), line });
                }
            }
            '/' if chars.peek() == Some(&(i + 1, '/')) => {
                eat!();
                while let Some((j, c)) = chars.next() {
                    if c == '\n' {
                        tokens.push(TknPlus {
                            token: Tkn::Comment(&source[i..j]),
                            line,
                        });
                        line += 1;
                        break;
                    }
                }
            }
            '/' if chars.peek() == Some(&(i + 1, '*')) => {
                eat!();
                let mut n: Count = 0;
                while let Some((j, c)) = chars.next() {
                    match c {
                        '*' if chars.peek() == Some(&(j + 1, '/')) => {
                            eat!();
                            tokens.push(TknPlus {
                                token: Tkn::Comment(&source[i..(j + 2)]),
                                line,
                            });
                            line += n;
                            break;
                        }
                        '\n' => n += 1,
                        _ => (),
                    }
                }
            }
            ':' => tokens.push(TknPlus { token: Tkn::Colon, line }),
            ';' => tokens.push(TknPlus { token: Tkn::Semicolon, line }),
            ',' => tokens.push(TknPlus { token: Tkn::Comma, line }),
            '{' => tokens.push(TknPlus { token: Tkn::LBrace, line }),
            '}' => tokens.push(TknPlus { token: Tkn::RBrace, line }),
            '(' => tokens.push(TknPlus { token: Tkn::LParen, line }),
            ')' => tokens.push(TknPlus { token: Tkn::RParen, line }),
            '?' => tokens.push(TknPlus { token: Tkn::Ternary, line }),
            _ if is_op(c) => {
                let op: &str = get_substring!(is_op, i);
                tokens.push(TknPlus { token: Tkn::Op(op), line });
            }
            '"' => {
                if let Some((i, _)) = chars.next() {
                    let mut k: usize = i;
                    let mut n: Count = 0;
                    while let Some((j, c)) = chars.peek() {
                        k = *j;
                        match c {
                            '"' => break,
                            '\n' => {
                                eat!();
                                n += 1;
                            }
                            _ => eat!(),
                        }
                    }
                    tokens.push(TknPlus {
                        token: Tkn::Str(&source[i..k]),
                        line,
                    });
                    line += n;
                    eat!();
                }
            }
            _ => tokens.push(TknPlus {
                token: Tkn::Illegal(&source[i..(i + 1)]),
                line,
            }),
        }
    }
    tokens
}
