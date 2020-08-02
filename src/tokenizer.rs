use std::iter::Peekable;
use std::str::CharIndices;

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

pub(crate) fn get_tokens(string: &str) -> Vec<Tkn> {
    let mut chars: Peekable<CharIndices> = string.char_indices().peekable();
    let mut tokens: Vec<Tkn> = Vec::with_capacity(string.len());

    macro_rules! eat {
        () => {
            if let Some(_) = chars.next() {
                ()
            } else {
                panic!()
            }
        };
    }

    macro_rules! get_substring {
        ($f:expr, $i:expr $(,)?) => {{
            let mut k: usize = $i;
            loop {
                if let Some((j, c)) = chars.peek() {
                    k = *j;
                    if $f(*c) {
                        eat!();
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
                    |c: char| c.is_alphabetic() || c.is_digit(DECIMAL),
                    i,
                );
                let token: Tkn = match ident {
                    "var" => Tkn::Var,
                    "function" => Tkn::Fn,
                    "return" => Tkn::Ret,
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
                    tokens.push(Tkn::Op("."));
                } else if (num.matches('.').count() < 2)
                    && (0 < num.matches(|c: char| c.is_digit(DECIMAL)).count())
                {
                    tokens.push(Tkn::Num(num));
                } else {
                    tokens.push(Tkn::Illegal(num));
                }
            }
            '/' if chars.peek() == Some(&(i + 1, '/')) => {
                eat!();
                while let Some((j, c)) = chars.next() {
                    match c {
                        '\n' => {
                            tokens.push(Tkn::Comment(&string[i..j]));
                            break;
                        }
                        _ => (),
                    }
                }
            }
            '/' if chars.peek() == Some(&(i + 1, '*')) => {
                eat!();
                while let Some((j, c)) = chars.next() {
                    match c {
                        '*' if chars.peek() == Some(&(j + 1, '/')) => {
                            eat!();
                            tokens.push(Tkn::Comment(&string[i..(j + 2)]));
                            break;
                        }
                        _ => (),
                    }
                }
            }
            ':' => tokens.push(Tkn::Colon),
            ';' => tokens.push(Tkn::Semicolon),
            ',' => tokens.push(Tkn::Comma),
            '{' => tokens.push(Tkn::LBrace),
            '}' => tokens.push(Tkn::RBrace),
            '(' => tokens.push(Tkn::LParen),
            ')' => tokens.push(Tkn::RParen),
            _ if is_op(c) => tokens.push(Tkn::Op(get_substring!(is_op, i))),
            '"' => {
                if let Some((i, _)) = chars.next() {
                    let mut k: usize = i;
                    while let Some((j, c)) = chars.peek() {
                        k = *j;
                        if *c != '"' {
                            eat!();
                        } else {
                            break;
                        }
                    }
                    tokens.push(Tkn::Str(&string[i..k]));
                    eat!();
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
        assert_token!("null", Tkn::Null);
        assert_tokens!("null;", vec![Tkn::Null, Tkn::Semicolon])
    }

    #[test]
    fn undefined() {
        assert_token!("undefined", Tkn::Undef);
        assert_tokens!("undefined;", vec![Tkn::Undef, Tkn::Semicolon])
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
        assert_tokens!("1.0.;", vec![Tkn::Illegal("1.0."), Tkn::Semicolon])
    }

    #[test]
    fn string() {
        assert_token!("\"blah\"", Tkn::Str("blah"));
        assert_tokens!("\"blah\";", vec![Tkn::Str("blah"), Tkn::Semicolon])
    }

    #[test]
    fn bool() {
        assert_token!("true", Tkn::Bool("true"));
        assert_token!("false", Tkn::Bool("false"));
        assert_tokens!("true;", vec![Tkn::Bool("true"), Tkn::Semicolon]);
        assert_tokens!("false;", vec![Tkn::Bool("false"), Tkn::Semicolon])
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
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn object_fields() {
        assert_tokens!(
            "var x = { a: { b: 0 } };\n\
             x.a.b;",
            vec![
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::LBrace,
                Tkn::Ident("a"),
                Tkn::Colon,
                Tkn::LBrace,
                Tkn::Ident("b"),
                Tkn::Colon,
                Tkn::Num("0"),
                Tkn::RBrace,
                Tkn::RBrace,
                Tkn::Semicolon,
                Tkn::Ident("x"),
                Tkn::Op("."),
                Tkn::Ident("a"),
                Tkn::Op("."),
                Tkn::Ident("b"),
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn declare() {
        assert_tokens!(
            "var a = \"null\";",
            vec![
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Op("="),
                Tkn::Str("null"),
                Tkn::Semicolon,
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
    fn tiny_function() {
        assert_tokens!(
            "function f() {\n\
                 return 0;\n\
             }",
            vec![
                Tkn::Fn,
                Tkn::Ident("f"),
                Tkn::LParen,
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::Ret,
                Tkn::Num("0"),
                Tkn::Semicolon,
                Tkn::RBrace,
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
                Tkn::Fn,
                Tkn::Ident("f"),
                Tkn::LParen,
                Tkn::Ident("a"),
                Tkn::Comma,
                Tkn::Ident("b"),
                Tkn::Comma,
                Tkn::Ident("c"),
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::Var,
                Tkn::Ident("d"),
                Tkn::Op("="),
                Tkn::LBrace,
                Tkn::Ident("a"),
                Tkn::Colon,
                Tkn::Ident("a"),
                Tkn::Comma,
                Tkn::Ident("b"),
                Tkn::Colon,
                Tkn::Ident("b"),
                Tkn::Comma,
                Tkn::Ident("c"),
                Tkn::Colon,
                Tkn::Ident("c"),
                Tkn::Comma,
                Tkn::RBrace,
                Tkn::Semicolon,
                Tkn::Ret,
                Tkn::Ident("d"),
                Tkn::Semicolon,
                Tkn::RBrace,
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
                Tkn::Var,
                Tkn::Ident("f"),
                Tkn::Op("="),
                Tkn::Fn,
                Tkn::LParen,
                Tkn::Ident("x"),
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::Ret,
                Tkn::Ident("x"),
                Tkn::Op("+"),
                Tkn::Num("0.1"),
                Tkn::Semicolon,
                Tkn::RBrace,
                Tkn::Semicolon,
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
                Tkn::Ident("window"),
                Tkn::Op("."),
                Tkn::Ident("onload"),
                Tkn::Op("="),
                Tkn::Fn,
                Tkn::LParen,
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Op("="),
                Tkn::Num("0.1"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Op("="),
                Tkn::Num("10"),
                Tkn::Semicolon,
                Tkn::Ret,
                Tkn::Ident("a"),
                Tkn::Op("+"),
                Tkn::Ident("b"),
                Tkn::Semicolon,
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn prefix_operators() {
        assert_tokens!(
            "var a = !true;\n\
             var b = -1.0;",
            vec![
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Op("="),
                Tkn::Op("!"),
                Tkn::Bool("true"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Op("="),
                Tkn::Op("-"),
                Tkn::Num("1.0"),
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn nested_expression() {
        assert_tokens!(
            "var x = (a + b) + ((c + d) + e);",
            vec![
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::LParen,
                Tkn::Ident("a"),
                Tkn::Op("+"),
                Tkn::Ident("b"),
                Tkn::RParen,
                Tkn::Op("+"),
                Tkn::LParen,
                Tkn::LParen,
                Tkn::Ident("c"),
                Tkn::Op("+"),
                Tkn::Ident("d"),
                Tkn::RParen,
                Tkn::Op("+"),
                Tkn::Ident("e"),
                Tkn::RParen,
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn increment() {
        assert_tokens!(
            "a++;\n\
             ++b;",
            vec![
                Tkn::Ident("a"),
                Tkn::Op("++"),
                Tkn::Semicolon,
                Tkn::Op("++"),
                Tkn::Ident("b"),
                Tkn::Semicolon,
            ],
        )
    }

    #[test]
    fn decrement() {
        assert_tokens!(
            "a--;\n\
             --b;",
            vec![
                Tkn::Ident("a"),
                Tkn::Op("--"),
                Tkn::Semicolon,
                Tkn::Op("--"),
                Tkn::Ident("b"),
                Tkn::Semicolon,
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
                Tkn::Comment("// Comment 1"),
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Semicolon,
                Tkn::Comment("/// Comment 2"),
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Semicolon,
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
                Tkn::Comment("/* Comment 1 */"),
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Semicolon,
                Tkn::Comment(
                    "/* Comment 2\n\
                      * ...\n\
                      */"
                ),
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Semicolon,
            ],
        )
    }
}
