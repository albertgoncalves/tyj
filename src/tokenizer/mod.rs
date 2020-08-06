#[cfg(test)]
mod test;

use std::iter::Peekable;
use std::str::CharIndices;

type Count = u8;

const OPS: [char; 9] = ['=', '.', '+', '-', '*', '/', '<', '>', '!'];
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
pub(crate) struct Lex<'a> {
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

pub(crate) fn get_tokens(string: &str) -> Vec<Lex> {
    let mut chars: Peekable<CharIndices> = string.char_indices().peekable();
    let mut tokens: Vec<Lex> = Vec::with_capacity(string.len());
    let mut line: Count = 0;

    macro_rules! eat {
        () => {{
            let _: Option<(usize, char)> = chars.next();
        }};
    }

    macro_rules! get_substring {
        ($f:expr, $i:expr $(,)?) => {{
            let mut k: usize = $i;
            let mut n: Count = 0;
            loop {
                if let Some((j, c)) = chars.peek() {
                    k = *j;
                    match c {
                        '\n' => {
                            n += 1;
                            eat!();
                        }
                        _ if $f(*c) => eat!(),
                        _ => break,
                    }
                } else {
                    k += 1;
                    break;
                }
            }
            (&string[$i..k], n)
        }};
    }

    while let Some((i, c)) = chars.next() {
        match c {
            '\n' => line += 1,
            _ if c.is_whitespace() => (),
            _ if c.is_alphabetic() => {
                let (ident, n): (&str, Count) = get_substring!(
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
                    "true" | "false" => Tkn::Bool(ident),
                    _ => Tkn::Ident(ident),
                };
                tokens.push(Lex { token, line });
                line += n;
            }
            _ if is_numeric(c) => {
                let (num, n): (&str, Count) = get_substring!(is_numeric, i);
                if num == "." {
                    tokens.push(Lex { token: Tkn::Op("."), line });
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|c: char| c.is_digit(DECIMAL)).count())
                {
                    tokens.push(Lex { token: Tkn::Num(num), line });
                } else {
                    tokens.push(Lex { token: Tkn::Illegal(num), line });
                }
                line += n;
            }
            '/' if chars.peek() == Some(&(i + 1, '/')) => {
                eat!();
                while let Some((j, c)) = chars.next() {
                    match c {
                        '\n' => {
                            tokens.push(Lex {
                                token: Tkn::Comment(&string[i..j]),
                                line,
                            });
                            line += 1;
                            break;
                        }
                        _ => (),
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
                            tokens.push(Lex {
                                token: Tkn::Comment(&string[i..(j + 2)]),
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
            ':' => tokens.push(Lex { token: Tkn::Colon, line }),
            ';' => tokens.push(Lex { token: Tkn::Semicolon, line }),
            ',' => tokens.push(Lex { token: Tkn::Comma, line }),
            '{' => tokens.push(Lex { token: Tkn::LBrace, line }),
            '}' => tokens.push(Lex { token: Tkn::RBrace, line }),
            '(' => tokens.push(Lex { token: Tkn::LParen, line }),
            ')' => tokens.push(Lex { token: Tkn::RParen, line }),
            _ if is_op(c) => {
                let (op, n): (&str, Count) = get_substring!(is_op, i);
                tokens.push(Lex { token: Tkn::Op(op), line });
                line += n;
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
                    tokens.push(Lex { token: Tkn::Str(&string[i..k]), line });
                    line += n;
                    eat!();
                }
            }
            _ => tokens
                .push(Lex { token: Tkn::Illegal(&string[i..(i + 1)]), line }),
        }
    }
    tokens
}
