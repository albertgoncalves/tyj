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
    Prefix {
        op: &'a str,
        expr: Box<Expr<'a>>,
    },
    Infix {
        op: &'a str,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Postfix {
        op: &'a str,
        expr: Box<Expr<'a>>,
    },
    Fn {
        args: Vec<&'a str>,
        body: Vec<Stmt<'a>>,
    },
    Null,
    Undef,
    Uninit,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'a> {
    Decl {
        ident: &'a str,
        expr: Expr<'a>,
    },
    Assign {
        r#ref: Expr<'a>,
        expr: Expr<'a>,
    },
    Ret(Expr<'a>),
    Fn {
        ident: &'a str,
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

fn get_args<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Vec<&'a str> {
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
    args
}

fn get_body<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Tkn<'a>>>,
) -> Vec<Stmt<'a>> {
    eat_or_panic!(tokens, Tkn::LBrace);
    let mut body: Vec<Stmt> = Vec::new();
    while let Some(t) = tokens.peek() {
        match t {
            Tkn::RBrace => {
                eat!(tokens);
                break;
            }
            _ => body.push(get_stmt(tokens)),
        }
    }
    body
}

fn get_fn<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Expr<'a> {
    let args: Vec<&str> = get_args(tokens);
    Expr::Fn {
        args,
        body: get_body(tokens),
    }
}

fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Tkn<'a>>>,
    precedence: u8,
) -> Expr<'a> {
    macro_rules! set_prefix {
        ($op:expr $(,)?) => {{
            let power: u8 = match $op {
                "-" | "!" | "++" | "--" => 9,
                _ => panic!(),
            };
            Expr::Prefix {
                op: $op,
                expr: Box::new(get_expr(tokens, power)),
            }
        }};
    }

    let mut expr: Expr = match tokens.next() {
        Some(Tkn::LParen) => {
            let expr: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            expr
        }
        Some(Tkn::Op(x)) if *x != "=" => set_prefix!(*x),
        Some(Tkn::Num(x)) => Expr::Num(x),
        Some(Tkn::Str(x)) => Expr::Str(x),
        Some(Tkn::Bool(x)) => Expr::Bool(x),
        Some(Tkn::Ident(x)) => Expr::Ref(x),
        Some(Tkn::Fn) => get_fn(tokens),
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

    macro_rules! set_postfix_or_infix {
        ($op:expr $(,)?) => {{
            let power: Option<u8> = match $op {
                "++" | "--" => Some(9),
                _ => None,
            };
            if let Some(power) = power {
                if power < precedence {
                    break;
                }
                eat!(tokens);
                expr = Expr::Postfix {
                    op: $op,
                    expr: Box::new(expr),
                };
            } else {
                let (l_power, r_power): (u8, u8) = match $op {
                    "." => (11, 12),
                    "+" | "-" => (5, 6),
                    _ => panic!(),
                };
                if l_power < precedence {
                    break;
                }
                eat!(tokens);
                expr = Expr::Infix {
                    op: $op,
                    left: Box::new(expr),
                    right: Box::new(get_expr(tokens, r_power)),
                };
            }
        }};
    }

    while let Some(t) = tokens.peek() {
        match t {
            Tkn::Op(x) if *x != "=" => set_postfix_or_infix!(*x),
            _ => break,
        }
    }
    expr
}

fn get_stmt<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Stmt<'a> {
    match tokens.peek() {
        Some(Tkn::Fn) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let args: Vec<&str> = get_args(tokens);
            Stmt::Fn {
                ident,
                args,
                body: get_body(tokens),
            }
        }
        Some(Tkn::Var) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            match tokens.next() {
                Some(Tkn::Semicolon) => Stmt::Decl {
                    ident,
                    expr: Expr::Uninit,
                },
                Some(Tkn::Op("=")) => {
                    let var: Stmt = Stmt::Decl {
                        ident,
                        expr: get_expr(tokens, 0),
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
            let a: Expr = get_expr(tokens, 0);
            match tokens.next() {
                Some(Tkn::Op("=")) => {
                    let b: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    Stmt::Assign { r#ref: a, expr: b }
                }
                Some(Tkn::Semicolon) => Stmt::Effect(a),
                _ => panic!(),
            }
        }
    }
}

pub(crate) fn get_ast<'a>(tokens: &[Tkn<'a>]) -> Vec<Stmt<'a>> {
    let mut ast_tokens: Vec<Tkn> = Vec::new();
    let mut comments: Vec<&str> = Vec::new();
    for token in tokens {
        match token {
            Tkn::Comment(x) => comments.push(x),
            _ => ast_tokens.push(*token),
        }
    }
    let mut ast: Vec<Stmt> = Vec::new();
    let mut ast_tokens: Peekable<Iter<Tkn>> = ast_tokens.iter().peekable();
    while let Some(_) = ast_tokens.peek() {
        ast.push(get_stmt(&mut ast_tokens));
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
    fn declare_number() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::Num(".1"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Num(".1"),
            }],
        )
    }

    #[test]
    fn declare_string() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::Str("blah blah"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Str("blah blah"),
            }],
        )
    }

    #[test]
    fn declare_bool() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::Bool("true"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Bool("true"),
            }],
        )
    }

    #[test]
    fn declare_null() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::Null,
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Null,
            }],
        )
    }

    #[test]
    fn declare_undefined() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::Undef,
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Undef,
            }],
        )
    }

    #[test]
    fn declare_object() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
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
                ident: "x",
                expr: Expr::Obj(vec![
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
    fn declare_empty_object() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::LBrace,
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Obj(Vec::new()),
            }],
        )
    }

    #[test]
    fn declare_object_trailing_comma() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Op("="),
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
                ident: "x",
                expr: Expr::Obj(vec![
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
    fn declare_object_missing_comma() {
        let _: Vec<Stmt> = get_ast(&[
            Tkn::Var,
            Tkn::Ident("x"),
            Tkn::Op("="),
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
    fn declare_assign() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Semicolon,
                Tkn::Ident("x"),
                Tkn::Op("="),
                Tkn::Null,
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Decl {
                    ident: "x",
                    expr: Expr::Uninit,
                },
                Stmt::Assign {
                    r#ref: Expr::Ref("x"),
                    expr: Expr::Null,
                },
            ],
        )
    }

    #[test]
    fn multiple_declares() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("a"),
                Tkn::Op("="),
                Tkn::Num("1."),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("b"),
                Tkn::Op("="),
                Tkn::Str("blah"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("c"),
                Tkn::Op("="),
                Tkn::Bool("false"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("d"),
                Tkn::Op("="),
                Tkn::Null,
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("e"),
                Tkn::Op("="),
                Tkn::Undef,
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("f"),
                Tkn::Op("="),
                Tkn::LBrace,
                Tkn::Ident("key"),
                Tkn::Colon,
                Tkn::Str("value"),
                Tkn::RBrace,
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Decl {
                    ident: "a",
                    expr: Expr::Num("1."),
                },
                Stmt::Decl {
                    ident: "b",
                    expr: Expr::Str("blah"),
                },
                Stmt::Decl {
                    ident: "c",
                    expr: Expr::Bool("false"),
                },
                Stmt::Decl {
                    ident: "d",
                    expr: Expr::Null,
                },
                Stmt::Decl {
                    ident: "e",
                    expr: Expr::Undef,
                },
                Stmt::Decl {
                    ident: "f",
                    expr: Expr::Obj(vec![Prop {
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
                ident: "f",
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
                ident: "f",
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
            vec![Stmt::Fn {
                ident: "f",
                args: vec!["a", "b", "c"],
                body: vec![
                    Stmt::Decl {
                        ident: "d",
                        expr: Expr::Obj(vec![
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
            vec![
                Stmt::Decl {
                    ident: "x",
                    expr: Expr::Obj(vec![Prop {
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

    #[test]
    fn declare_anonymous_function() {
        assert_ast!(
            &[
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
            vec![Stmt::Decl {
                ident: "f",
                expr: Expr::Fn {
                    args: vec!["x"],
                    body: vec![Stmt::Ret(Expr::Infix {
                        op: "+",
                        left: Box::new(Expr::Ref("x")),
                        right: Box::new(Expr::Num("0.1")),
                    })],
                }
            }],
        )
    }

    #[test]
    fn tiny_program() {
        assert_ast!(
            &[
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
            vec![Stmt::Assign {
                r#ref: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ref("window")),
                    right: Box::new(Expr::Ref("onload")),
                },
                expr: Expr::Fn {
                    args: Vec::new(),
                    body: vec![
                        Stmt::Decl {
                            ident: "a",
                            expr: Expr::Num("0.1"),
                        },
                        Stmt::Decl {
                            ident: "b",
                            expr: Expr::Num("10"),
                        },
                        Stmt::Ret(Expr::Infix {
                            op: "+",
                            left: Box::new(Expr::Ref("a")),
                            right: Box::new(Expr::Ref("b")),
                        }),
                    ],
                },
            }],
        )
    }

    #[test]
    fn prefix_operators() {
        assert_ast!(
            &[
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
            vec![
                Stmt::Decl {
                    ident: "a",
                    expr: Expr::Prefix {
                        op: "!",
                        expr: Box::new(Expr::Bool("true")),
                    },
                },
                Stmt::Decl {
                    ident: "b",
                    expr: Expr::Prefix {
                        op: "-",
                        expr: Box::new(Expr::Num("1.0")),
                    },
                },
            ],
        )
    }

    #[test]
    fn nested_expression() {
        assert_ast!(
            &[
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
            vec![Stmt::Decl {
                ident: "x",
                expr: Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Infix {
                        op: "+",
                        left: Box::new(Expr::Ref("a")),
                        right: Box::new(Expr::Ref("b")),
                    }),
                    right: Box::new(Expr::Infix {
                        op: "+",
                        left: Box::new(Expr::Infix {
                            op: "+",
                            left: Box::new(Expr::Ref("c")),
                            right: Box::new(Expr::Ref("d")),
                        }),
                        right: Box::new(Expr::Ref("e")),
                    }),
                },
            }],
        )
    }

    #[test]
    fn increment() {
        assert_ast!(
            &[
                Tkn::Ident("a"),
                Tkn::Op("++"),
                Tkn::Semicolon,
                Tkn::Op("++"),
                Tkn::Ident("b"),
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Effect(Expr::Postfix {
                    op: "++",
                    expr: Box::new(Expr::Ref("a")),
                }),
                Stmt::Effect(Expr::Prefix {
                    op: "++",
                    expr: Box::new(Expr::Ref("b")),
                }),
            ],
        )
    }

    #[test]
    fn decrement() {
        assert_ast!(
            &[
                Tkn::Ident("a"),
                Tkn::Op("--"),
                Tkn::Semicolon,
                Tkn::Op("--"),
                Tkn::Ident("b"),
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Effect(Expr::Postfix {
                    op: "--",
                    expr: Box::new(Expr::Ref("a")),
                }),
                Stmt::Effect(Expr::Prefix {
                    op: "--",
                    expr: Box::new(Expr::Ref("b")),
                }),
            ],
        )
    }
}
