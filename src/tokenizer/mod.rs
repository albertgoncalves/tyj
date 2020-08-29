#[cfg(test)]
mod test;

use std::iter::Peekable;
use std::str::CharIndices;

pub(crate) type Count = u16;

const OPS: [char; 14] =
    ['=', '.', '+', '-', '*', '%', '<', '>', '!', '~', '&', '^', '|', '/'];
const DECIMAL: u32 = 10;

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum Tkn<'a> {
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
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
pub(crate) struct Lex<'a> {
    pub(crate) token: Tkn<'a>,
    pub(crate) line: Count,
}

fn is_numeric(x: char) -> bool {
    x.is_digit(DECIMAL) || (x == '.')
}

fn is_op(x: char) -> bool {
    for op in &OPS {
        if x == *op {
            return true;
        }
    }
    false
}

pub(crate) fn get_tokens(source: &str) -> Vec<Lex> {
    let mut chars: Peekable<CharIndices> = source.char_indices().peekable();
    let mut tokens: Vec<Lex> = Vec::with_capacity(source.len());
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

    while let Some((i, x)) = chars.next() {
        match x {
            '\n' => line += 1,
            _ if x.is_whitespace() => (),
            _ if x.is_alphabetic() || x == '_' => {
                let ident: &str = get_substring!(
                    |x: char| x.is_alphabetic()
                        || x.is_digit(DECIMAL)
                        || x == '_',
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
                tokens.push(Lex { token, line });
            }
            _ if is_numeric(x) => {
                let num: &str = get_substring!(is_numeric, i);
                if num == "." {
                    tokens.push(Lex { token: Tkn::Op("."), line });
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|x: char| x.is_digit(DECIMAL)).count())
                {
                    tokens.push(Lex { token: Tkn::Num(num), line });
                } else {
                    tokens.push(Lex { token: Tkn::Illegal(num), line });
                }
            }
            '/' if chars.peek() == Some(&(i + 1, '/')) => {
                eat!();
                while let Some((j, x)) = chars.next() {
                    if x == '\n' {
                        tokens.push(Lex {
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
                while let Some((j, x)) = chars.next() {
                    match x {
                        '*' if chars.peek() == Some(&(j + 1, '/')) => {
                            eat!();
                            tokens.push(Lex {
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
            ':' => tokens.push(Lex { token: Tkn::Colon, line }),
            ';' => tokens.push(Lex { token: Tkn::Semicolon, line }),
            ',' => tokens.push(Lex { token: Tkn::Comma, line }),
            '{' => tokens.push(Lex { token: Tkn::LBrace, line }),
            '}' => tokens.push(Lex { token: Tkn::RBrace, line }),
            '(' => tokens.push(Lex { token: Tkn::LParen, line }),
            ')' => tokens.push(Lex { token: Tkn::RParen, line }),
            '[' => tokens.push(Lex { token: Tkn::LBracket, line }),
            ']' => tokens.push(Lex { token: Tkn::RBracket, line }),
            '?' => tokens.push(Lex { token: Tkn::Ternary, line }),
            _ if is_op(x) => {
                let op: &str = get_substring!(is_op, i);
                tokens.push(Lex { token: Tkn::Op(op), line });
            }
            '"' => {
                let i: usize = i + 1;
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
                tokens.push(Lex { token: Tkn::Str(&source[i..k]), line });
                line += n;
                eat!();
            }
            _ => {
                tokens.push(Lex { token: Tkn::Illegal(&source[i..=i]), line })
            }
        }
    }
    tokens
}
