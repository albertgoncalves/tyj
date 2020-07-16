use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
enum Token<'a> {
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
    Null,
    Undef,
    Ident(&'a str),
    BinOp(&'a str),
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Illegal(&'a str),
}

fn is_numeric(c: char) -> bool {
    (c == '.') || c.is_digit(10)
}

fn get_tokens(string: &str) -> Vec<Token> {
    let mut chars: Peekable<CharIndices> = string.char_indices().peekable();
    let mut tokens: Vec<Token> = Vec::with_capacity(string.len());

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
                let token: Token<'_> = match ident {
                    "var" => Token::Var,
                    "null" => Token::Null,
                    "undefined" => Token::Undef,
                    "true" | "false" => Token::Bool(ident),
                    _ => Token::Ident(ident),
                };
                tokens.push(token);
            }
            _ if is_numeric(c) => {
                let num: &str = get_substring!(is_numeric, i);
                if num == "." {
                    tokens.push(Token::Dot);
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|c: char| c.is_digit(10)).count())
                {
                    tokens.push(Token::Num(num));
                } else {
                    tokens.push(Token::Illegal(num));
                }
            }
            '=' => tokens.push(Token::Equals),
            ':' => tokens.push(Token::Colon),
            ';' => tokens.push(Token::Semicolon),
            ',' => tokens.push(Token::Comma),
            '.' => tokens.push(Token::Dot),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '+' => tokens.push(Token::BinOp(&string[i..(i + 1)])),
            '"' => {
                if let Some((i, _)) = chars.next() {
                    tokens.push(Token::Str(get_str_literal!(i)));
                    eat_char!();
                }
            }
            _ => tokens.push(Token::Illegal(&string[i..(i + 1)])),
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::{get_tokens, Token};

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
        assert_token!("null", Token::Null);
        assert_tokens!("null;", vec![Token::Null, Token::Semicolon]);
    }

    #[test]
    fn undefined() {
        assert_token!("undefined", Token::Undef);
        assert_tokens!("undefined;", vec![Token::Undef, Token::Semicolon]);
    }

    #[test]
    fn number() {
        assert_token!("1", Token::Num("1"));
        assert_token!("10", Token::Num("10"));
        assert_token!(".10", Token::Num(".10"));
        assert_token!("1.0", Token::Num("1.0"));
        assert_token!("1.0.", Token::Illegal("1.0."));
        assert_tokens!("1;", vec![Token::Num("1"), Token::Semicolon]);
        assert_tokens!("10;", vec![Token::Num("10"), Token::Semicolon]);
        assert_tokens!(".10;", vec![Token::Num(".10"), Token::Semicolon]);
        assert_tokens!("1.0;", vec![Token::Num("1.0"), Token::Semicolon]);
        assert_tokens!(
            "1.0.;",
            vec![Token::Illegal("1.0."), Token::Semicolon],
        );
    }

    #[test]
    fn string() {
        assert_token!("\"blah\"", Token::Str("blah"));
        assert_tokens!(
            "\"blah\";",
            vec![Token::Str("blah"), Token::Semicolon],
        );
    }

    #[test]
    fn bool() {
        assert_token!("true", Token::Bool("true"));
        assert_token!("false", Token::Bool("false"));
        assert_tokens!("true;", vec![Token::Bool("true"), Token::Semicolon]);
        assert_tokens!("false;", vec![Token::Bool("false"), Token::Semicolon]);
    }

    #[test]
    fn object() {
        assert_tokens!(
            "{ a: null, bc: undefined };",
            vec![
                Token::LBrace,
                Token::Ident("a"),
                Token::Colon,
                Token::Null,
                Token::Comma,
                Token::Ident("bc"),
                Token::Colon,
                Token::Undef,
                Token::RBrace,
                Token::Semicolon
            ],
        );
    }

    #[test]
    fn declare() {
        assert_tokens!(
            "var a = \"null\";",
            vec![
                Token::Var,
                Token::Ident("a"),
                Token::Equals,
                Token::Str("null"),
                Token::Semicolon,
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
                Token::Null,
                Token::Semicolon,
                Token::Undef,
                Token::Semicolon,
                Token::Num("1"),
                Token::Semicolon,
                Token::Num("10"),
                Token::Semicolon,
                Token::Num(".10"),
                Token::Semicolon,
                Token::Num("1.0"),
                Token::Semicolon,
                Token::Str("blah"),
                Token::Semicolon,
                Token::Bool("true"),
                Token::Semicolon,
                Token::Bool("false"),
                Token::Semicolon,
                Token::LBrace,
                Token::Ident("a"),
                Token::Colon,
                Token::Num("0.1"),
                Token::RBrace,
                Token::Semicolon,
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
                Token::Ident("window"),
                Token::Dot,
                Token::Ident("onload"),
                Token::Equals,
                Token::Ident("function"),
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::Var,
                Token::Ident("a"),
                Token::Equals,
                Token::Num("0.1"),
                Token::Semicolon,
                Token::Var,
                Token::Ident("b"),
                Token::Equals,
                Token::Num("10"),
                Token::Semicolon,
                Token::Var,
                Token::Ident("c"),
                Token::Equals,
                Token::Ident("a"),
                Token::BinOp("+"),
                Token::Ident("b"),
                Token::Semicolon,
                Token::Var,
                Token::Ident("d"),
                Token::Equals,
                Token::Str("foo"),
                Token::Semicolon,
                Token::Var,
                Token::Ident("e"),
                Token::Equals,
                Token::Str("bar"),
                Token::Semicolon,
                Token::Var,
                Token::Ident("f"),
                Token::Equals,
                Token::Ident("d"),
                Token::BinOp("+"),
                Token::Ident("e"),
                Token::Semicolon,
                Token::RBrace,
                Token::Semicolon,
            ],
        );
    }
}
