#[cfg(test)]
mod test;

use std::iter::Peekable;
use std::str::CharIndices;

pub(crate) type Count = u16;

const OP_CHARS: [char; 14] =
    ['=', '.', '+', '-', '*', '%', '<', '>', '!', '~', '&', '^', '|', '/'];
pub(crate) const DECIMAL: u32 = 10;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Op {
    Add,
    And,
    BitwiseAnd,
    BitwiseNot,
    BitwiseOr,
    BitwiseXor,
    Decrement,
    Div,
    Equality,
    GreaterThan,
    GreaterThanEquals,
    Increment,
    Inequality,
    LessThan,
    LessThanEquals,
    Member,
    Mod,
    Mul,
    New,
    Not,
    Or,
    ShiftLeft,
    ShiftRight,
    Sub,
    UnsignedShiftRight,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Asn {
    Add,
    Div,
    Mul,
    Reg,
    Sub,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Tkn<'a> {
    Assign(Asn),
    Bool(&'a str),
    Break,
    Case,
    Colon,
    Comma,
    Comment(&'a str),
    Default,
    Else,
    Fn,
    For,
    Ident(&'a str),
    If,
    Illegal(&'a str),
    LBrace,
    LBracket,
    LParen,
    Null,
    Num(&'a str),
    Op(Op),
    RBrace,
    RBracket,
    Ret,
    RParen,
    Semicolon,
    Str(&'a str),
    Switch,
    Ternary,
    Undef,
    Var,
    While,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Lex<'a> {
    pub(crate) token: Tkn<'a>,
    pub(crate) line: Count,
}

fn is_numeric(x: char) -> bool {
    x.is_digit(DECIMAL) || (x == '.')
}

fn is_op(x: char) -> bool {
    for op in &OP_CHARS {
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

    macro_rules! push {
        ($token:expr $(,)?) => {{
            tokens.push(Lex { token: $token, line })
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
                    "new" => Tkn::Op(Op::New),
                    "true" | "false" => Tkn::Bool(ident),
                    _ => Tkn::Ident(ident),
                };
                push!(token);
            }
            _ if is_numeric(x) => {
                let num: &str = get_substring!(is_numeric, i);
                if num == "." {
                    push!(Tkn::Op(Op::Member));
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|x: char| x.is_digit(DECIMAL)).count())
                {
                    push!(Tkn::Num(num));
                } else {
                    push!(Tkn::Illegal(num));
                }
            }
            '/' if chars.peek() == Some(&(i + 1, '/')) => {
                eat!();
                while let Some((j, x)) = chars.next() {
                    if x == '\n' {
                        push!(Tkn::Comment(&source[i..j]));
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
                            push!(Tkn::Comment(&source[i..(j + 2)]));
                            line += n;
                            break;
                        }
                        '\n' => n += 1,
                        _ => (),
                    }
                }
            }
            ':' => push!(Tkn::Colon),
            ';' => push!(Tkn::Semicolon),
            ',' => push!(Tkn::Comma),
            '{' => push!(Tkn::LBrace),
            '}' => push!(Tkn::RBrace),
            '(' => push!(Tkn::LParen),
            ')' => push!(Tkn::RParen),
            '[' => push!(Tkn::LBracket),
            ']' => push!(Tkn::RBracket),
            '?' => push!(Tkn::Ternary),
            _ if is_op(x) => {
                let token: Tkn = match get_substring!(is_op, i) {
                    "=" => Tkn::Assign(Asn::Reg),
                    "+=" => Tkn::Assign(Asn::Add),
                    "-=" => Tkn::Assign(Asn::Sub),
                    "*=" => Tkn::Assign(Asn::Mul),
                    "/=" => Tkn::Assign(Asn::Div),
                    "." => Tkn::Op(Op::Member),
                    "!" => Tkn::Op(Op::Not),
                    "~" => Tkn::Op(Op::BitwiseNot),
                    "++" => Tkn::Op(Op::Increment),
                    "--" => Tkn::Op(Op::Decrement),
                    "+" => Tkn::Op(Op::Add),
                    "-" => Tkn::Op(Op::Sub),
                    "*" => Tkn::Op(Op::Mul),
                    "/" => Tkn::Op(Op::Div),
                    "%" => Tkn::Op(Op::Mod),
                    "<<" => Tkn::Op(Op::ShiftLeft),
                    ">>" => Tkn::Op(Op::ShiftRight),
                    ">>>" => Tkn::Op(Op::UnsignedShiftRight),
                    "<" => Tkn::Op(Op::LessThan),
                    ">" => Tkn::Op(Op::GreaterThan),
                    "<=" => Tkn::Op(Op::LessThanEquals),
                    ">=" => Tkn::Op(Op::GreaterThanEquals),
                    "===" => Tkn::Op(Op::Equality),
                    "!==" => Tkn::Op(Op::Inequality),
                    "&&" => Tkn::Op(Op::And),
                    "||" => Tkn::Op(Op::Or),
                    "&" => Tkn::Op(Op::BitwiseAnd),
                    "^" => Tkn::Op(Op::BitwiseXor),
                    "|" => Tkn::Op(Op::BitwiseOr),
                    op => Tkn::Illegal(op),
                };
                push!(token);
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
                push!(Tkn::Str(&source[i..k]));
                line += n;
                eat!();
            }
            _ => {
                push!(Tkn::Illegal(&source[i..=i]));
            }
        }
    }
    tokens
}
