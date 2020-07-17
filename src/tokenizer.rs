use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub(crate) enum Tkn<'a> {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    Semicolon,
    Comma,
    Dot,
    Equals,
    Var,
    Ident(&'a str),
    BinOp(&'a str),
    Null,
    Undef,
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Illegal(&'a str),
}

fn is_numeric(c: char) -> bool {
    (c == '.') || c.is_digit(10)
}

fn get_tokens(string: &str) -> Vec<Tkn> {
    let mut chars: Peekable<CharIndices> = string.char_indices().peekable();
    let mut tokens: Vec<Tkn> = Vec::with_capacity(string.len());

    macro_rules! eat_char {
        () => {
            let _: Option<(usize, char)> = chars.next();
        };
    }

    macro_rules! get_str_literal {
        ($i:expr $(,)?) => {{
            let mut k: usize = $i;
            while let Some((j, c)) = chars.peek() {
                k = *j;
                if *c != '"' {
                    eat_char!();
                } else {
                    break;
                }
            }
            &string[$i..k]
        }};
    }

    macro_rules! get_substring {
        ($f:expr, $i:expr $(,)?) => {{
            let mut k: usize = $i;
            loop {
                if let Some((j, c)) = chars.peek() {
                    k = *j;
                    if $f(*c) {
                        eat_char!();
                    } else {
                        break;
                    }
                } else {
                    k += 1;
                    break;
                }
            }
            &string[$i..k]
        }};
    }

    while let Some((i, c)) = chars.next() {
        match c {
            _ if c.is_whitespace() => (),
            _ if c.is_alphabetic() => {
                let ident: &str = get_substring!(
                    |c: char| c.is_alphabetic() || c.is_digit(10),
                    i,
                );
                let token: Tkn<'_> = match ident {
                    "var" => Tkn::Var,
                    "null" => Tkn::Null,
                    "undefined" => Tkn::Undef,
                    "true" | "false" => Tkn::Bool(ident),
                    _ => Tkn::Ident(ident),
                };
                tokens.push(token);
            }
            _ if is_numeric(c) => {
                let num: &str = get_substring!(is_numeric, i);
                if num == "." {
                    tokens.push(Tkn::Dot);
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|c: char| c.is_digit(10)).count())
                {
                    tokens.push(Tkn::Num(num));
                } else {
                    tokens.push(Tkn::Illegal(num));
                }
            }
            '=' => tokens.push(Tkn::Equals),
            ':' => tokens.push(Tkn::Colon),
            ';' => tokens.push(Tkn::Semicolon),
            ',' => tokens.push(Tkn::Comma),
            '.' => tokens.push(Tkn::Dot),
            '{' => tokens.push(Tkn::LBrace),
            '}' => tokens.push(Tkn::RBrace),
            '(' => tokens.push(Tkn::LParen),
            ')' => tokens.push(Tkn::RParen),
            '+' => tokens.push(Tkn::BinOp(&string[i..(i + 1)])),
            '"' => {
                if let Some((i, _)) = chars.next() {
                    tokens.push(Tkn::Str(get_str_literal!(i)));
                    eat_char!();
                }
            }
            _ => tokens.push(Tkn::Illegal(&string[i..(i + 1)])),
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::{get_tokens, Tkn};

    macro_rules! assert_token {
        ($a:expr, $b:expr $(,)?) => {
            assert_eq!(get_tokens($a), vec![$b]);
        };
    }

    macro_rules! assert_tokens {
        ($a:expr, $b:expr $(,)?) => {
            assert_eq!(get_tokens($a), $b);
        };
    }

    #[test]
    fn null() {
        assert_token!("null", Tkn::Null);
        assert_tokens!("null;", vec![Tkn::Null, Tkn::Semicolon]);
    }

    #[test]
    fn undefined() {
        assert_token!("undefined", Tkn::Undef);
        assert_tokens!("undefined;", vec![Tkn::Undef, Tkn::Semicolon]);
    }

    #[test]
    fn number() {
        assert_token!("1", Tkn::Num("1"));
        assert_token!("10", Tkn::Num("10"));
        assert_token!(".10", Tkn::Num(".10"));
        assert_token!("1.0", Tkn::Num("1.0"));
        assert_token!("1.0.", Tkn::Illegal("1.0."));
        assert_tokens!("1;", vec![Tkn::Num("1"), Tkn::Semicolon]);
        assert_tokens!("10;", vec![Tkn::Num("10"), Tkn::Semicolon]);
        assert_tokens!(".10;", vec![Tkn::Num(".10"), Tkn::Semicolon]);
        assert_tokens!("1.0;", vec![Tkn::Num("1.0"), Tkn::Semicolon]);
        assert_tokens!("1.0.;", vec![Tkn::Illegal("1.0."), Tkn::Semicolon],);
    }

    #[test]
    fn string() {
        assert_token!("\"blah\"", Tkn::Str("blah"));
        assert_tokens!("\"blah\";", vec![Tkn::Str("blah"), Tkn::Semicolon],);
    }

    #[test]
    fn bool() {
        assert_token!("true", Tkn::Bool("true"));
        assert_token!("false", Tkn::Bool("false"));
        assert_tokens!("true;", vec![Tkn::Bool("true"), Tkn::Semicolon]);
        assert_tokens!("false;", vec![Tkn::Bool("false"), Tkn::Semicolon]);
    }

    #[test]
    fn object() {
        assert_tokens!(
            "{ a: null, bc: undefined };",
            vec![
                Tkn::LBrace,
                Tkn::Ident("a"),
                Tkn::Colon,
                Tkn::Null,
                Tkn::Comma,
                Tkn::Ident("bc"),
                Tkn::Colon,
                Tkn::Undef,
                Tkn::RBrace,
                Tkn::Semicolon
            ],
        );
    }

    #[test]
    fn declare() {
        assert_tokens!(
            "var a = \"null\";",
            vec![
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Equals,
                Tkn::Str("null"),
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn multiple_lines() {
        assert_tokens!(
            "null;\
             undefined;\
             1;\
             10;\
             .10;\
             1.0;\
             \"blah\";\
             true;\
             false;\
             { a: 0.1 };",
            vec![
                Tkn::Null,
                Tkn::Semicolon,
                Tkn::Undef,
                Tkn::Semicolon,
                Tkn::Num("1"),
                Tkn::Semicolon,
                Tkn::Num("10"),
                Tkn::Semicolon,
                Tkn::Num(".10"),
                Tkn::Semicolon,
                Tkn::Num("1.0"),
                Tkn::Semicolon,
                Tkn::Str("blah"),
                Tkn::Semicolon,
                Tkn::Bool("true"),
                Tkn::Semicolon,
                Tkn::Bool("false"),
                Tkn::Semicolon,
                Tkn::LBrace,
                Tkn::Ident("a"),
                Tkn::Colon,
                Tkn::Num("0.1"),
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn tiny_program() {
        assert_tokens!(
            "window.onload = function() {\
                var a = 0.1;\
                var b = 10;\
                var c = a + b;\
                var d = \"foo\";\
                var e = \"bar\";\
                var f = d + e;\
            };",
            vec![
                Tkn::Ident("window"),
                Tkn::Dot,
                Tkn::Ident("onload"),
                Tkn::Equals,
                Tkn::Ident("function"),
                Tkn::LParen,
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Equals,
                Tkn::Num("0.1"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Equals,
                Tkn::Num("10"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("c"),
                Tkn::Equals,
                Tkn::Ident("a"),
                Tkn::BinOp("+"),
                Tkn::Ident("b"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("d"),
                Tkn::Equals,
                Tkn::Str("foo"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("e"),
                Tkn::Equals,
                Tkn::Str("bar"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("f"),
                Tkn::Equals,
                Tkn::Ident("d"),
                Tkn::BinOp("+"),
                Tkn::Ident("e"),
                Tkn::Semicolon,
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
        );
    }
}
