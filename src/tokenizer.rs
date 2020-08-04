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

#[cfg(test)]
mod tests {
    use super::{get_tokens, Lex, Tkn};

    macro_rules! assert_token {
        ($a:expr, $b:expr $(,)?) => {
            assert_eq!(get_tokens($a), vec![$b])
        };
    }

    macro_rules! assert_tokens {
        ($a:expr, $b:expr $(,)?) => {
            assert_eq!(get_tokens($a), $b)
        };
    }

    #[test]
    fn null() {
        assert_token!("null", Lex { token: Tkn::Null, line: 0 },);
        assert_tokens!(
            "null;",
            vec![
                Lex { token: Tkn::Null, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn undefined() {
        assert_token!("undefined", Lex { token: Tkn::Undef, line: 0 },);
        assert_tokens!(
            "undefined;",
            vec![
                Lex { token: Tkn::Undef, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn number() {
        assert_token!("1", Lex { token: Tkn::Num("1"), line: 0 });
        assert_token!("10", Lex { token: Tkn::Num("10"), line: 0 });
        assert_token!(".10", Lex { token: Tkn::Num(".10"), line: 0 });
        assert_token!("1.0", Lex { token: Tkn::Num("1.0"), line: 0 });
        assert_token!("1.0.", Lex { token: Tkn::Illegal("1.0."), line: 0 });
        assert_tokens!(
            "1;",
            vec![
                Lex { token: Tkn::Num("1"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        );
        assert_tokens!(
            "10;",
            vec![
                Lex { token: Tkn::Num("10"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        );
        assert_tokens!(
            ".10;",
            vec![
                Lex { token: Tkn::Num(".10"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        );
        assert_tokens!(
            "1.0;",
            vec![
                Lex { token: Tkn::Num("1.0"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        );
        assert_tokens!(
            "1.0.;",
            vec![
                Lex { token: Tkn::Illegal("1.0."), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn string() {
        assert_token!("\"blah\"", Lex { token: Tkn::Str("blah"), line: 0 });
        assert_tokens!(
            "\"blah\";",
            vec![
                Lex { token: Tkn::Str("blah"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn bool() {
        assert_token!("true", Lex { token: Tkn::Bool("true"), line: 0 });
        assert_token!("false", Lex { token: Tkn::Bool("false"), line: 0 });
        assert_tokens!(
            "true;",
            vec![
                Lex { token: Tkn::Bool("true"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        );
        assert_tokens!(
            "false;",
            vec![
                Lex { token: Tkn::Bool("false"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn object() {
        assert_tokens!(
            "{ a: null, bc: undefined };",
            vec![
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Colon, line: 0 },
                Lex { token: Tkn::Null, line: 0 },
                Lex { token: Tkn::Comma, line: 0 },
                Lex { token: Tkn::Ident("bc"), line: 0 },
                Lex { token: Tkn::Colon, line: 0 },
                Lex { token: Tkn::Undef, line: 0 },
                Lex { token: Tkn::RBrace, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn object_fields() {
        assert_tokens!(
            "var x = { a: { b: 0 } };\n\
             x.a.b;",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Colon, line: 0 },
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Ident("b"), line: 0 },
                Lex { token: Tkn::Colon, line: 0 },
                Lex { token: Tkn::Num("0"), line: 0 },
                Lex { token: Tkn::RBrace, line: 0 },
                Lex { token: Tkn::RBrace, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 1 },
                Lex { token: Tkn::Op("."), line: 1 },
                Lex { token: Tkn::Ident("a"), line: 1 },
                Lex { token: Tkn::Op("."), line: 1 },
                Lex { token: Tkn::Ident("b"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
            ],
        )
    }

    #[test]
    fn declare() {
        assert_tokens!(
            "var a = \"null\";",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Str("null"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn multiple_lines() {
        assert_tokens!(
            "null;\n\
             undefined;\n\
             1;\n\
             10;\n\
             .10;\n\
             1.0;\n\
             \"blah\";\n\
             true;\n\
             false;\n\
             { a: 0.1 };",
            vec![
                Lex { token: Tkn::Null, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::Undef, line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::Num("1"), line: 2 },
                Lex { token: Tkn::Semicolon, line: 2 },
                Lex { token: Tkn::Num("10"), line: 3 },
                Lex { token: Tkn::Semicolon, line: 3 },
                Lex { token: Tkn::Num(".10"), line: 4 },
                Lex { token: Tkn::Semicolon, line: 4 },
                Lex { token: Tkn::Num("1.0"), line: 5 },
                Lex { token: Tkn::Semicolon, line: 5 },
                Lex { token: Tkn::Str("blah"), line: 6 },
                Lex { token: Tkn::Semicolon, line: 6 },
                Lex { token: Tkn::Bool("true"), line: 7 },
                Lex { token: Tkn::Semicolon, line: 7 },
                Lex { token: Tkn::Bool("false"), line: 8 },
                Lex { token: Tkn::Semicolon, line: 8 },
                Lex { token: Tkn::LBrace, line: 9 },
                Lex { token: Tkn::Ident("a"), line: 9 },
                Lex { token: Tkn::Colon, line: 9 },
                Lex { token: Tkn::Num("0.1"), line: 9 },
                Lex { token: Tkn::RBrace, line: 9 },
                Lex { token: Tkn::Semicolon, line: 9 },
            ],
        )
    }

    #[test]
    fn tiny_function() {
        assert_tokens!(
            "function f() {\n\
                 return 0;\n\
             }",
            vec![
                Lex { token: Tkn::Fn, line: 0 },
                Lex { token: Tkn::Ident("f"), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Ret, line: 1 },
                Lex { token: Tkn::Num("0"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::RBrace, line: 2 },
            ],
        )
    }

    #[test]
    fn small_function() {
        assert_tokens!(
            "function f(a, b, c) {\n\
                 var d = {\n\
                     a: a,\n\
                     b: b,\n\
                     c: c,\n\
                 };\n\
                 return d;\n\
             }",
            vec![
                Lex { token: Tkn::Fn, line: 0 },
                Lex { token: Tkn::Ident("f"), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Comma, line: 0 },
                Lex { token: Tkn::Ident("b"), line: 0 },
                Lex { token: Tkn::Comma, line: 0 },
                Lex { token: Tkn::Ident("c"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Var, line: 1 },
                Lex { token: Tkn::Ident("d"), line: 1 },
                Lex { token: Tkn::Op("="), line: 1 },
                Lex { token: Tkn::LBrace, line: 1 },
                Lex { token: Tkn::Ident("a"), line: 2 },
                Lex { token: Tkn::Colon, line: 2 },
                Lex { token: Tkn::Ident("a"), line: 2 },
                Lex { token: Tkn::Comma, line: 2 },
                Lex { token: Tkn::Ident("b"), line: 3 },
                Lex { token: Tkn::Colon, line: 3 },
                Lex { token: Tkn::Ident("b"), line: 3 },
                Lex { token: Tkn::Comma, line: 3 },
                Lex { token: Tkn::Ident("c"), line: 4 },
                Lex { token: Tkn::Colon, line: 4 },
                Lex { token: Tkn::Ident("c"), line: 4 },
                Lex { token: Tkn::Comma, line: 4 },
                Lex { token: Tkn::RBrace, line: 5 },
                Lex { token: Tkn::Semicolon, line: 5 },
                Lex { token: Tkn::Ret, line: 6 },
                Lex { token: Tkn::Ident("d"), line: 6 },
                Lex { token: Tkn::Semicolon, line: 6 },
                Lex { token: Tkn::RBrace, line: 7 },
            ],
        )
    }

    #[test]
    fn declare_anonymous_function() {
        assert_tokens!(
            "var f = function(x) {\n\
                 return x + 0.1;\n\
             };",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("f"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Fn, line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Ret, line: 1 },
                Lex { token: Tkn::Ident("x"), line: 1 },
                Lex { token: Tkn::Op("+"), line: 1 },
                Lex { token: Tkn::Num("0.1"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::RBrace, line: 2 },
                Lex { token: Tkn::Semicolon, line: 2 },
            ],
        )
    }

    #[test]
    fn tiny_program() {
        assert_tokens!(
            "window.onload = function() {\n\
                 var a = 0.1;\n\
                 var b = 10;\n\
                 return a + b;\n\
             };",
            vec![
                Lex { token: Tkn::Ident("window"), line: 0 },
                Lex { token: Tkn::Op("."), line: 0 },
                Lex { token: Tkn::Ident("onload"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Fn, line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LBrace, line: 0 },
                Lex { token: Tkn::Var, line: 1 },
                Lex { token: Tkn::Ident("a"), line: 1 },
                Lex { token: Tkn::Op("="), line: 1 },
                Lex { token: Tkn::Num("0.1"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::Var, line: 2 },
                Lex { token: Tkn::Ident("b"), line: 2 },
                Lex { token: Tkn::Op("="), line: 2 },
                Lex { token: Tkn::Num("10"), line: 2 },
                Lex { token: Tkn::Semicolon, line: 2 },
                Lex { token: Tkn::Ret, line: 3 },
                Lex { token: Tkn::Ident("a"), line: 3 },
                Lex { token: Tkn::Op("+"), line: 3 },
                Lex { token: Tkn::Ident("b"), line: 3 },
                Lex { token: Tkn::Semicolon, line: 3 },
                Lex { token: Tkn::RBrace, line: 4 },
                Lex { token: Tkn::Semicolon, line: 4 },
            ],
        )
    }

    #[test]
    fn prefix_operators() {
        assert_tokens!(
            "var a = !true;\n\
             var b = -1.0;",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Op("!"), line: 0 },
                Lex { token: Tkn::Bool("true"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::Var, line: 1 },
                Lex { token: Tkn::Ident("b"), line: 1 },
                Lex { token: Tkn::Op("="), line: 1 },
                Lex { token: Tkn::Op("-"), line: 1 },
                Lex { token: Tkn::Num("1.0"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
            ],
        )
    }

    #[test]
    fn nested_expression() {
        assert_tokens!(
            "var x = (a + b) + ((c + d) + e);",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Op("+"), line: 0 },
                Lex { token: Tkn::Ident("b"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::Op("+"), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("c"), line: 0 },
                Lex { token: Tkn::Op("+"), line: 0 },
                Lex { token: Tkn::Ident("d"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::Op("+"), line: 0 },
                Lex { token: Tkn::Ident("e"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn increment() {
        assert_tokens!(
            "a++;\n\
             ++b;",
            vec![
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Op("++"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::Op("++"), line: 1 },
                Lex { token: Tkn::Ident("b"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
            ],
        )
    }

    #[test]
    fn decrement() {
        assert_tokens!(
            "a--;\n\
             --b;",
            vec![
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Op("--"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::Op("--"), line: 1 },
                Lex { token: Tkn::Ident("b"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
            ],
        )
    }

    #[test]
    fn comment() {
        assert_tokens!(
            "// Comment 1\n\
             var a;\n\
             /// Comment 2\n\
             var b;",
            vec![
                Lex { token: Tkn::Comment("// Comment 1"), line: 0 },
                Lex { token: Tkn::Var, line: 1 },
                Lex { token: Tkn::Ident("a"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::Comment("/// Comment 2"), line: 2 },
                Lex { token: Tkn::Var, line: 3 },
                Lex { token: Tkn::Ident("b"), line: 3 },
                Lex { token: Tkn::Semicolon, line: 3 },
            ],
        )
    }

    #[test]
    fn multiline_comment() {
        assert_tokens!(
            "/* Comment 1 */\n\
             var a;\n\
             /* Comment 2\n\
              * ...\n\
              */\n\
             var b;",
            vec![
                Lex { token: Tkn::Comment("/* Comment 1 */"), line: 0 },
                Lex { token: Tkn::Var, line: 1 },
                Lex { token: Tkn::Ident("a"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex {
                    token: Tkn::Comment(
                        "/* Comment 2\n\
                          * ...\n\
                          */"
                    ),
                    line: 2,
                },
                Lex { token: Tkn::Var, line: 5 },
                Lex { token: Tkn::Ident("b"), line: 5 },
                Lex { token: Tkn::Semicolon, line: 5 },
            ],
        )
    }

    #[test]
    fn if_else() {
        assert_tokens!(
            "var a;
             if (true) {
                 a = 0;
             } else {
                 a = 1;
             }",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::If, line: 1 },
                Lex { token: Tkn::LParen, line: 1 },
                Lex { token: Tkn::Bool("true"), line: 1 },
                Lex { token: Tkn::RParen, line: 1 },
                Lex { token: Tkn::LBrace, line: 1 },
                Lex { token: Tkn::Ident("a"), line: 2 },
                Lex { token: Tkn::Op("="), line: 2 },
                Lex { token: Tkn::Num("0"), line: 2 },
                Lex { token: Tkn::Semicolon, line: 2 },
                Lex { token: Tkn::RBrace, line: 3 },
                Lex { token: Tkn::Else, line: 3 },
                Lex { token: Tkn::LBrace, line: 3 },
                Lex { token: Tkn::Ident("a"), line: 4 },
                Lex { token: Tkn::Op("="), line: 4 },
                Lex { token: Tkn::Num("1"), line: 4 },
                Lex { token: Tkn::Semicolon, line: 4 },
                Lex { token: Tkn::RBrace, line: 5 },
            ],
        )
    }

    #[test]
    fn switch_case() {
        assert_tokens!(
            "var x = true;
             switch (x) {
             case true: {
                 console.log(\"true\");
                 break;
             }
             case false: {
                 console.log(\"false\");
                 break;
             }
             default: {
                 console.log(\"?\");
             }
             }",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Bool("true"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::Switch, line: 1 },
                Lex { token: Tkn::LParen, line: 1 },
                Lex { token: Tkn::Ident("x"), line: 1 },
                Lex { token: Tkn::RParen, line: 1 },
                Lex { token: Tkn::LBrace, line: 1 },
                Lex { token: Tkn::Case, line: 2 },
                Lex { token: Tkn::Bool("true"), line: 2 },
                Lex { token: Tkn::Colon, line: 2 },
                Lex { token: Tkn::LBrace, line: 2 },
                Lex { token: Tkn::Ident("console"), line: 3 },
                Lex { token: Tkn::Op("."), line: 3 },
                Lex { token: Tkn::Ident("log"), line: 3 },
                Lex { token: Tkn::LParen, line: 3 },
                Lex { token: Tkn::Str("true"), line: 3 },
                Lex { token: Tkn::RParen, line: 3 },
                Lex { token: Tkn::Semicolon, line: 3 },
                Lex { token: Tkn::Break, line: 4 },
                Lex { token: Tkn::Semicolon, line: 4 },
                Lex { token: Tkn::RBrace, line: 5 },
                Lex { token: Tkn::Case, line: 6 },
                Lex { token: Tkn::Bool("false"), line: 6 },
                Lex { token: Tkn::Colon, line: 6 },
                Lex { token: Tkn::LBrace, line: 6 },
                Lex { token: Tkn::Ident("console"), line: 7 },
                Lex { token: Tkn::Op("."), line: 7 },
                Lex { token: Tkn::Ident("log"), line: 7 },
                Lex { token: Tkn::LParen, line: 7 },
                Lex { token: Tkn::Str("false"), line: 7 },
                Lex { token: Tkn::RParen, line: 7 },
                Lex { token: Tkn::Semicolon, line: 7 },
                Lex { token: Tkn::Break, line: 8 },
                Lex { token: Tkn::Semicolon, line: 8 },
                Lex { token: Tkn::RBrace, line: 9 },
                Lex { token: Tkn::Default, line: 10 },
                Lex { token: Tkn::Colon, line: 10 },
                Lex { token: Tkn::LBrace, line: 10 },
                Lex { token: Tkn::Ident("console"), line: 11 },
                Lex { token: Tkn::Op("."), line: 11 },
                Lex { token: Tkn::Ident("log"), line: 11 },
                Lex { token: Tkn::LParen, line: 11 },
                Lex { token: Tkn::Str("?"), line: 11 },
                Lex { token: Tkn::RParen, line: 11 },
                Lex { token: Tkn::Semicolon, line: 11 },
                Lex { token: Tkn::RBrace, line: 12 },
                Lex { token: Tkn::RBrace, line: 13 },
            ],
        )
    }

    #[test]
    fn r#for() {
        assert_tokens!(
            "var x = 0;
             for (var i = 0; i < 10; ++i) {
                 x += 2;
             }",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Num("0"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::For, line: 1 },
                Lex { token: Tkn::LParen, line: 1 },
                Lex { token: Tkn::Var, line: 1 },
                Lex { token: Tkn::Ident("i"), line: 1 },
                Lex { token: Tkn::Op("="), line: 1 },
                Lex { token: Tkn::Num("0"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::Ident("i"), line: 1 },
                Lex { token: Tkn::Op("<"), line: 1 },
                Lex { token: Tkn::Num("10"), line: 1 },
                Lex { token: Tkn::Semicolon, line: 1 },
                Lex { token: Tkn::Op("++"), line: 1 },
                Lex { token: Tkn::Ident("i"), line: 1 },
                Lex { token: Tkn::RParen, line: 1 },
                Lex { token: Tkn::LBrace, line: 1 },
                Lex { token: Tkn::Ident("x"), line: 2 },
                Lex { token: Tkn::Op("+="), line: 2 },
                Lex { token: Tkn::Num("2"), line: 2 },
                Lex { token: Tkn::Semicolon, line: 2 },
                Lex { token: Tkn::RBrace, line: 3 },
            ],
        )
    }

    #[test]
    fn r#while() {
        assert_tokens!(
            "var x = 0;
             while (x < 10) {
                 x += 2;
             }",
            vec![
                Lex { token: Tkn::Var, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::Op("="), line: 0 },
                Lex { token: Tkn::Num("0"), line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
                Lex { token: Tkn::While, line: 1 },
                Lex { token: Tkn::LParen, line: 1 },
                Lex { token: Tkn::Ident("x"), line: 1 },
                Lex { token: Tkn::Op("<"), line: 1 },
                Lex { token: Tkn::Num("10"), line: 1 },
                Lex { token: Tkn::RParen, line: 1 },
                Lex { token: Tkn::LBrace, line: 1 },
                Lex { token: Tkn::Ident("x"), line: 2 },
                Lex { token: Tkn::Op("+="), line: 2 },
                Lex { token: Tkn::Num("2"), line: 2 },
                Lex { token: Tkn::Semicolon, line: 2 },
                Lex { token: Tkn::RBrace, line: 3 },
            ],
        )
    }

    #[test]
    fn function_calls() {
        assert_tokens!(
            "f(a)(b);",
            vec![
                Lex { token: Tkn::Ident("f"), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("b"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }

    #[test]
    fn function_calls_nested() {
        assert_tokens!(
            "f(a(x)(y))(b);",
            vec![
                Lex { token: Tkn::Ident("f"), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("a"), line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("x"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("y"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::LParen, line: 0 },
                Lex { token: Tkn::Ident("b"), line: 0 },
                Lex { token: Tkn::RParen, line: 0 },
                Lex { token: Tkn::Semicolon, line: 0 },
            ],
        )
    }
}
