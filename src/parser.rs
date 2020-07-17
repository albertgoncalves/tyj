use crate::tokenizer::Tkn;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
struct Prop<'a> {
    key: &'a str,
    value: Expr<'a>,
}

#[derive(Debug, PartialEq)]
enum Expr<'a> {
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Obj(Vec<Prop<'a>>),
    Null,
    Undef,
}

#[derive(Debug, PartialEq)]
enum Stmt<'a> {
    Var(&'a str, Expr<'a>),
}

macro_rules! eat {
    ($tokens:expr $(,)?) => {{
        let _: Option<&Tkn> = $tokens.next();
    }};
}

macro_rules! eat_or_panic {
    ($tokens:expr, $x:path $(,)?) => {
        let _: () = if let Some($x) = $tokens.next() {
            ()
        } else {
            panic!()
        };
    };
}

fn get_ident<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> &'a str {
    if let Some(Tkn::Ident(x)) = tokens.next() {
        x
    } else {
        panic!()
    }
}

fn get_prop<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Prop<'a> {
    let key: &str = get_ident(tokens);
    eat_or_panic!(tokens, Tkn::Colon);
    Prop {
        key,
        value: get_expr(tokens),
    }
}

fn get_expr<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Expr<'a> {
    match tokens.next() {
        Some(Tkn::Num(x)) => return Expr::Num(x),
        Some(Tkn::Str(x)) => return Expr::Str(x),
        Some(Tkn::Bool(x)) => return Expr::Bool(x),
        Some(Tkn::Null) => return Expr::Null,
        Some(Tkn::Undef) => return Expr::Undef,
        Some(Tkn::LBrace) => {
            let mut props: Vec<Prop> = Vec::new();
            while let Some(t) = tokens.peek() {
                match t {
                    Tkn::Ident(_) => {
                        props.push(get_prop(tokens));
                        match tokens.next() {
                            Some(Tkn::Comma) => (),
                            Some(Tkn::RBrace) => return Expr::Obj(props),
                            _ => break,
                        }
                    }
                    Tkn::RBrace => {
                        eat!(tokens);
                        return Expr::Obj(props);
                    }
                    _ => break,
                }
            }
        }
        _ => (),
    }
    panic!()
}

fn get_var<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Stmt<'a> {
    let ident: &str = get_ident(tokens);
    eat_or_panic!(tokens, Tkn::Equals);
    let var: Stmt = Stmt::Var(ident, get_expr(tokens));
    eat_or_panic!(tokens, Tkn::Semicolon);
    var
}

fn get_ast<'a>(tokens: &[Tkn<'a>]) -> Vec<Stmt<'a>> {
    let mut ast: Vec<Stmt> = Vec::new();
    let mut tokens: Peekable<Iter<'_, Tkn<'_>>> = tokens.iter().peekable();
    while let Some(t) = tokens.next() {
        match t {
            Tkn::Var => ast.push(get_var(&mut tokens)),
            _ => panic!(),
        }
    }
    ast
}

#[cfg(test)]
mod tests {
    use super::{get_ast, Expr, Prop, Stmt};
    use crate::tokenizer::Tkn;

    macro_rules! assert_ast {
        ($a:expr, $b:expr $(,)?) => {
            assert_eq!(get_ast($a), $b)
        };
    }

    #[test]
    fn var_number() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Num(".1"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Num(".1"))],
        )
    }

    #[test]
    fn var_string() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Str("blah blah"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Str("blah blah"))],
        );
    }

    #[test]
    fn var_bool() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Bool("true"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Bool("true"))],
        );
    }

    #[test]
    fn var_null() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Null,
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Null)],
        );
    }

    #[test]
    fn var_undefined() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Undef,
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Undef)],
        );
    }

    #[test]
    fn var_object() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
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
            vec![Stmt::Var(
                "x",
                Expr::Obj(vec![
                    Prop {
                        key: "a",
                        value: Expr::Null,
                    },
                    Prop {
                        key: "bc",
                        value: Expr::Undef,
                    },
                ]),
            )],
        );
    }

    #[test]
    fn var_object_trailing_comma() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::LBrace,
                Tkn::Ident("a"),
                Tkn::Colon,
                Tkn::Null,
                Tkn::Comma,
                Tkn::Ident("bc"),
                Tkn::Colon,
                Tkn::Undef,
                Tkn::Comma,
                Tkn::RBrace,
                Tkn::Semicolon
            ],
            vec![Stmt::Var(
                "x",
                Expr::Obj(vec![
                    Prop {
                        key: "a",
                        value: Expr::Null,
                    },
                    Prop {
                        key: "bc",
                        value: Expr::Undef,
                    },
                ]),
            )],
        );
    }

    #[test]
    #[should_panic]
    fn var_object_missing_comma() {
        let _: Vec<Stmt> = get_ast(&[
            Tkn::Var,
            Tkn::Ident("x"),
            Tkn::Equals,
            Tkn::LBrace,
            Tkn::Ident("a"),
            Tkn::Colon,
            Tkn::Null,
            Tkn::Ident("bc"),
            Tkn::Colon,
            Tkn::Undef,
            Tkn::RBrace,
            Tkn::Semicolon,
        ]);
    }

    #[test]
    fn multiple_vars() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Equals,
                Tkn::Num("1."),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Equals,
                Tkn::Str("blah"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("c"),
                Tkn::Equals,
                Tkn::Bool("false"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("d"),
                Tkn::Equals,
                Tkn::Null,
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("e"),
                Tkn::Equals,
                Tkn::Undef,
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("f"),
                Tkn::Equals,
                Tkn::LBrace,
                Tkn::Ident("key"),
                Tkn::Colon,
                Tkn::Str("value"),
                Tkn::RBrace,
                Tkn::Semicolon
            ],
            vec![
                Stmt::Var("a", Expr::Num("1.")),
                Stmt::Var("b", Expr::Str("blah")),
                Stmt::Var("c", Expr::Bool("false")),
                Stmt::Var("d", Expr::Null),
                Stmt::Var("e", Expr::Undef),
                Stmt::Var(
                    "f",
                    Expr::Obj(vec![Prop {
                        key: "key",
                        value: Expr::Str("value"),
                    }]),
                ),
            ],
        )
    }
}
