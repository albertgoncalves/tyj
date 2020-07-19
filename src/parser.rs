use crate::tokenizer::Tkn;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
pub(crate) struct Prop<'a> {
    pub(crate) key: &'a str,
    pub(crate) value: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr<'a> {
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Obj(Vec<Prop<'a>>),
    Ref(&'a str),
    Infix {
        op: &'a str,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Null,
    Undef,
    Uninit,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'a> {
    Decl {
        name: &'a str,
        value: Expr<'a>,
    },
    Assign {
        name: &'a str,
        value: Expr<'a>,
    },
    Ret(Expr<'a>),
    Fn {
        name: &'a str,
        args: Vec<&'a str>,
        body: Vec<Stmt<'a>>,
    },
    Effect(Expr<'a>),
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
        value: get_expr(tokens, 0),
    }
}

fn get_infix_binding_power(op: &str) -> (u8, u8) {
    match op {
        "." => (9, 10),
        _ => panic!(),
    }
}

fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Tkn<'a>>>,
    precedence: u8,
) -> Expr<'a> {
    let mut expr: Expr = match tokens.next() {
        Some(Tkn::Num(x)) => Expr::Num(x),
        Some(Tkn::Str(x)) => Expr::Str(x),
        Some(Tkn::Bool(x)) => Expr::Bool(x),
        Some(Tkn::Ident(x)) => Expr::Ref(x),
        Some(Tkn::Null) => Expr::Null,
        Some(Tkn::Undef) => Expr::Undef,
        Some(Tkn::LBrace) => {
            let mut props: Vec<Prop> = Vec::new();
            while let Some(t) = tokens.peek() {
                match t {
                    Tkn::Ident(_) => {
                        props.push(get_prop(tokens));
                        match tokens.next() {
                            Some(Tkn::Comma) => (),
                            Some(Tkn::RBrace) => break,
                            _ => panic!(),
                        }
                    }
                    Tkn::RBrace => {
                        eat!(tokens);
                        break;
                    }
                    _ => panic!(),
                }
            }
            Expr::Obj(props)
        }
        _ => panic!(),
    };

    macro_rules! set_infix {
        ($op:expr $(,)?) => {{
            let (l_power, r_power): (u8, u8) = get_infix_binding_power($op);
            if l_power < precedence {
                break;
            }
            eat!(tokens);
            expr = Expr::Infix {
                op: $op,
                left: Box::new(expr),
                right: Box::new(get_expr(tokens, r_power)),
            };
        }};
    }

    while let Some(t) = tokens.peek() {
        match t {
            Tkn::Dot => set_infix!("."),
            _ => break,
        }
    }
    expr
}

fn get_fn<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Stmt<'a> {
    let ident: &str = get_ident(tokens);
    eat_or_panic!(tokens, Tkn::LParen);
    let mut args: Vec<&str> = Vec::new();
    while let Some(t) = tokens.next() {
        match t {
            Tkn::Ident(x) => {
                args.push(x);
                match tokens.next() {
                    Some(Tkn::Comma) => (),
                    Some(Tkn::RParen) => break,
                    _ => panic!(),
                }
            }
            Tkn::RParen => break,
            _ => panic!(),
        }
    }
    let mut stmts: Vec<Stmt> = Vec::new();
    eat_or_panic!(tokens, Tkn::LBrace);
    while let Some(t) = tokens.peek() {
        match t {
            Tkn::RBrace => {
                eat!(tokens);
                break;
            }
            _ => stmts.push(get_stmt(tokens)),
        }
    }
    Stmt::Fn {
        name: ident,
        args,
        body: stmts,
    }
}

fn get_stmt<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Stmt<'a> {
    match tokens.peek() {
        Some(Tkn::Fn) => {
            eat!(tokens);
            get_fn(tokens)
        }
        Some(Tkn::Var) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            match tokens.next() {
                Some(Tkn::Semicolon) => Stmt::Decl {
                    name: ident,
                    value: Expr::Uninit,
                },
                Some(Tkn::Equals) => {
                    let var: Stmt = Stmt::Decl {
                        name: ident,
                        value: get_expr(tokens, 0),
                    };
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    var
                }
                _ => panic!(),
            }
        }
        Some(Tkn::Ret) => {
            eat!(tokens);
            let expr: Expr = match tokens.peek() {
                Some(Tkn::Semicolon) => {
                    eat!(tokens);
                    Expr::Undef
                }
                _ => {
                    let expr: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    expr
                }
            };
            Stmt::Ret(expr)
        }
        _ => {
            let expr: Expr = get_expr(tokens, 0);
            match (&expr, tokens.peek()) {
                (Expr::Ref(x), Some(Tkn::Equals)) => {
                    eat!(tokens);
                    let expr: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    Stmt::Assign {
                        name: x,
                        value: expr,
                    }
                }
                _ => {
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    Stmt::Effect(expr)
                }
            }
        }
    }
}

pub(crate) fn get_ast<'a>(tokens: &[Tkn<'a>]) -> Vec<Stmt<'a>> {
    let mut ast: Vec<Stmt> = Vec::new();
    let mut tokens: Peekable<Iter<'_, Tkn<'_>>> = tokens.iter().peekable();
    while let Some(_) = tokens.peek() {
        ast.push(get_stmt(&mut tokens));
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
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Num(".1"),
            }],
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
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Str("blah blah"),
            }],
        )
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
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Bool("true"),
            }],
        )
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
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Null,
            }],
        )
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
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Undef,
            }],
        )
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
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Obj(vec![
                    Prop {
                        key: "a",
                        value: Expr::Null,
                    },
                    Prop {
                        key: "bc",
                        value: Expr::Undef,
                    },
                ]),
            }],
        )
    }

    #[test]
    fn var_empty_object() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::LBrace,
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Obj(Vec::new()),
            }],
        )
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
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                name: "x",
                value: Expr::Obj(vec![
                    Prop {
                        key: "a",
                        value: Expr::Null,
                    },
                    Prop {
                        key: "bc",
                        value: Expr::Undef,
                    },
                ]),
            }],
        )
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
    fn var_declare_assign() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Semicolon,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Null,
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Decl {
                    name: "x",
                    value: Expr::Uninit,
                },
                Stmt::Assign {
                    name: "x",
                    value: Expr::Null,
                },
            ],
        )
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
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Decl {
                    name: "a",
                    value: Expr::Num("1."),
                },
                Stmt::Decl {
                    name: "b",
                    value: Expr::Str("blah"),
                },
                Stmt::Decl {
                    name: "c",
                    value: Expr::Bool("false"),
                },
                Stmt::Decl {
                    name: "d",
                    value: Expr::Null,
                },
                Stmt::Decl {
                    name: "e",
                    value: Expr::Undef,
                },
                Stmt::Decl {
                    name: "f",
                    value: Expr::Obj(vec![Prop {
                        key: "key",
                        value: Expr::Str("value"),
                    }]),
                },
            ],
        )
    }

    #[test]
    fn return_nothing() {
        assert_ast!(&[Tkn::Ret, Tkn::Semicolon], vec![Stmt::Ret(Expr::Undef)])
    }

    #[test]
    fn return_object() {
        assert_ast!(
            &[
                Tkn::Ret,
                Tkn::LBrace,
                Tkn::Ident("ab"),
                Tkn::Colon,
                Tkn::Null,
                Tkn::Comma,
                Tkn::Ident("cd"),
                Tkn::Colon,
                Tkn::Undef,
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
            vec![Stmt::Ret(Expr::Obj(vec![
                Prop {
                    key: "ab",
                    value: Expr::Null,
                },
                Prop {
                    key: "cd",
                    value: Expr::Undef,
                },
            ]))],
        )
    }

    #[test]
    fn return_empty_object() {
        assert_ast!(
            &[Tkn::Ret, Tkn::LBrace, Tkn::RBrace, Tkn::Semicolon],
            vec![Stmt::Ret(Expr::Obj(Vec::new()))],
        )
    }

    #[test]
    fn function_nothing() {
        assert_ast!(
            &[
                Tkn::Fn,
                Tkn::Ident("f"),
                Tkn::LParen,
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::RBrace,
            ],
            vec![Stmt::Fn {
                name: "f",
                args: Vec::new(),
                body: Vec::new(),
            }],
        )
    }

    #[test]
    fn function_return_nothing() {
        assert_ast!(
            &[
                Tkn::Fn,
                Tkn::Ident("f"),
                Tkn::LParen,
                Tkn::Ident("x"),
                Tkn::Comma,
                Tkn::Ident("y"),
                Tkn::RParen,
                Tkn::LBrace,
                Tkn::Ret,
                Tkn::Semicolon,
                Tkn::RBrace,
            ],
            vec![Stmt::Fn {
                name: "f",
                args: vec!["x", "y"],
                body: vec![Stmt::Ret(Expr::Undef)],
            }],
        )
    }

    #[test]
    fn function_multiple_lines() {
        assert_ast!(
            &[
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
                Tkn::Equals,
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
            vec![Stmt::Fn {
                name: "f",
                args: vec!["a", "b", "c"],
                body: vec![
                    Stmt::Decl {
                        name: "d",
                        value: Expr::Obj(vec![
                            Prop {
                                key: "a",
                                value: Expr::Ref("a"),
                            },
                            Prop {
                                key: "b",
                                value: Expr::Ref("b"),
                            },
                            Prop {
                                key: "c",
                                value: Expr::Ref("c"),
                            },
                        ]),
                    },
                    Stmt::Ret(Expr::Ref("d")),
                ],
            }],
        )
    }

    #[test]
    fn object_fields() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
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
                Tkn::Dot,
                Tkn::Ident("a"),
                Tkn::Dot,
                Tkn::Ident("b"),
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Decl {
                    name: "x",
                    value: Expr::Obj(vec![Prop {
                        key: "a",
                        value: Expr::Obj(vec![Prop {
                            key: "b",
                            value: Expr::Num("0"),
                        }]),
                    }]),
                },
                Stmt::Effect(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ref("x")),
                        right: Box::new(Expr::Ref("a")),
                    }),
                    right: Box::new(Expr::Ref("b")),
                }),
            ],
        )
    }
}
