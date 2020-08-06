use super::{get_ast, Case, Expr, Prop, Stmt};
use crate::tokenizer::{Lex, Tkn};

macro_rules! assert_ast {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_ast($a), $b)
    };
}

#[test]
fn declare_number() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Num(".1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl { ident: "x", expr: Expr::Num(".1") }],
    )
}

#[test]
fn declare_string() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Str("blah blah"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl { ident: "x", expr: Expr::Str("blah blah") }],
    )
}

#[test]
fn declare_bool() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl { ident: "x", expr: Expr::Bool("true") }],
    )
}

#[test]
fn declare_null() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl { ident: "x", expr: Expr::Null }],
    )
}

#[test]
fn declare_undefined() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Undef, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl { ident: "x", expr: Expr::Undef }],
    )
}

#[test]
fn declare_object() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
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
        vec![Stmt::Decl {
            ident: "x",
            expr: Expr::Obj(vec![
                Prop { key: "a", value: Expr::Null },
                Prop { key: "bc", value: Expr::Undef },
            ]),
        }],
    )
}

#[test]
fn declare_empty_object() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl { ident: "x", expr: Expr::Obj(Vec::new()) }],
    )
}

#[test]
fn declare_object_trailing_comma() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("bc"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Undef, line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Decl {
            ident: "x",
            expr: Expr::Obj(vec![
                Prop { key: "a", value: Expr::Null },
                Prop { key: "bc", value: Expr::Undef },
            ]),
        }],
    )
}

#[test]
#[should_panic]
fn declare_object_missing_comma() {
    let _: Vec<Stmt> = get_ast(&[
        Lex { token: Tkn::Var, line: 0 },
        Lex { token: Tkn::Ident("x"), line: 0 },
        Lex { token: Tkn::Op("="), line: 0 },
        Lex { token: Tkn::LBrace, line: 0 },
        Lex { token: Tkn::Ident("a"), line: 0 },
        Lex { token: Tkn::Colon, line: 0 },
        Lex { token: Tkn::Null, line: 0 },
        Lex { token: Tkn::Ident("bc"), line: 0 },
        Lex { token: Tkn::Colon, line: 0 },
        Lex { token: Tkn::Undef, line: 0 },
        Lex { token: Tkn::RBrace, line: 0 },
        Lex { token: Tkn::Semicolon, line: 0 },
    ]);
}

#[test]
fn declare_assign() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            Stmt::Decl { ident: "x", expr: Expr::Uninit },
            Stmt::Assign { r#ref: Expr::Ref("x"), expr: Expr::Null },
        ],
    )
}

#[test]
fn multiple_declares() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Num("1."), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Str("blah"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("c"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Bool("false"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("d"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("e"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Undef, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("key"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Str("value"), line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            Stmt::Decl { ident: "a", expr: Expr::Num("1.") },
            Stmt::Decl { ident: "b", expr: Expr::Str("blah") },
            Stmt::Decl { ident: "c", expr: Expr::Bool("false") },
            Stmt::Decl { ident: "d", expr: Expr::Null },
            Stmt::Decl { ident: "e", expr: Expr::Undef },
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
    assert_ast!(
        &[
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Ret(Expr::Undef)]
    )
}

#[test]
fn return_object() {
    assert_ast!(
        &[
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("ab"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("cd"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Undef, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Ret(Expr::Obj(vec![
            Prop { key: "ab", value: Expr::Null },
            Prop { key: "cd", value: Expr::Undef },
        ]))],
    )
}

#[test]
fn return_empty_object() {
    assert_ast!(
        &[
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Ret(Expr::Obj(Vec::new()))],
    )
}

#[test]
fn function_nothing() {
    assert_ast!(
        &[
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
        ],
        vec![Stmt::Fn { ident: "f", args: Vec::new(), body: Vec::new() }],
    )
}

#[test]
fn function_return_nothing() {
    assert_ast!(
        &[
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("y"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
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
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("d"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("c"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Ident("c"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::Ident("d"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
        ],
        vec![Stmt::Fn {
            ident: "f",
            args: vec!["a", "b", "c"],
            body: vec![
                Stmt::Decl {
                    ident: "d",
                    expr: Expr::Obj(vec![
                        Prop { key: "a", value: Expr::Ref("a") },
                        Prop { key: "b", value: Expr::Ref("b") },
                        Prop { key: "c", value: Expr::Ref("c") },
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
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("."), line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("."), line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
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
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("+"), line: 0 },
            Lex { token: Tkn::Num("0.1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
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
            Lex { token: Tkn::Ident("window"), line: 0 },
            Lex { token: Tkn::Op("."), line: 0 },
            Lex { token: Tkn::Ident("onload"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Num("0.1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Num("10"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("+"), line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
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
                    Stmt::Decl { ident: "a", expr: Expr::Num("0.1") },
                    Stmt::Decl { ident: "b", expr: Expr::Num("10") },
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
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Op("!"), line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Op("-"), line: 0 },
            Lex { token: Tkn::Num("1.0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
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
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("++"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Op("++"), line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
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
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("--"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Op("--"), line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
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

#[test]
fn r#if() {
    assert_ast!(
        &[
            Lex { token: Tkn::If, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ret, line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
        ],
        vec![Stmt::Cond {
            condition: Expr::Bool("true"),
            r#if: vec![Stmt::Ret(Expr::Num("0"))],
            r#else: Vec::new(),
        }],
    )
}

#[test]
fn if_else() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::If, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Else, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Num("1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
        ],
        vec![
            Stmt::Decl { ident: "a", expr: Expr::Uninit },
            Stmt::Cond {
                condition: Expr::Bool("true"),
                r#if: vec![Stmt::Assign {
                    r#ref: Expr::Ref("a"),
                    expr: Expr::Num("0"),
                }],
                r#else: vec![Stmt::Assign {
                    r#ref: Expr::Ref("a"),
                    expr: Expr::Num("1"),
                }],
            },
        ],
    )
}

#[test]
fn function_calls() {
    assert_ast!(
        &[
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Effect(Expr::Call {
            expr: Box::new(Expr::Call {
                expr: Box::new(Expr::Ref("f")),
                args: vec![Expr::Ref("a")],
            }),
            args: vec![Expr::Ref("b")],
        })],
    )
}

#[test]
fn function_calls_more_parens() {
    assert_ast!(
        &[
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
        vec![Stmt::Effect(Expr::Call {
            expr: Box::new(Expr::Call {
                expr: Box::new(Expr::Ref("f")),
                args: vec![Expr::Ref("a")],
            }),
            args: vec![Expr::Ref("b")],
        })],
    )
}

#[test]
fn function_calls_nested() {
    assert_ast!(
        &[
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
        vec![Stmt::Effect(Expr::Call {
            expr: Box::new(Expr::Call {
                expr: Box::new(Expr::Ref("f")),
                args: vec![Expr::Call {
                    expr: Box::new(Expr::Call {
                        expr: Box::new(Expr::Ref("a")),
                        args: vec![Expr::Ref("x")],
                    }),
                    args: vec![Expr::Ref("y")],
                }],
            }),
            args: vec![Expr::Ref("b")],
        })],
    )
}

#[test]
fn switch() {
    assert_ast!(
        &[
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Op("="), line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Switch, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Case, line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("console"), line: 0 },
            Lex { token: Tkn::Op("."), line: 0 },
            Lex { token: Tkn::Ident("log"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Str("true"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Break, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Case, line: 0 },
            Lex { token: Tkn::Bool("false"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("console"), line: 0 },
            Lex { token: Tkn::Op("."), line: 0 },
            Lex { token: Tkn::Ident("log"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Str("false"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Break, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::Default, line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("console"), line: 0 },
            Lex { token: Tkn::Op("."), line: 0 },
            Lex { token: Tkn::Ident("log"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Str("?"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
            Lex { token: Tkn::RBrace, line: 0 },
        ],
        vec![
            Stmt::Decl { ident: "x", expr: Expr::Bool("true") },
            Stmt::Switch {
                expr: Expr::Ref("x"),
                cases: vec![
                    Case {
                        expr: Expr::Bool("true"),
                        body: vec![
                            Stmt::Effect(Expr::Call {
                                expr: Box::new(Expr::Infix {
                                    op: ".",
                                    left: Box::new(Expr::Ref("console")),
                                    right: Box::new(Expr::Ref("log")),
                                }),
                                args: vec![Expr::Str("true")],
                            }),
                            Stmt::Break,
                        ],
                    },
                    Case {
                        expr: Expr::Bool("false"),
                        body: vec![
                            Stmt::Effect(Expr::Call {
                                expr: Box::new(Expr::Infix {
                                    op: ".",
                                    left: Box::new(Expr::Ref("console")),
                                    right: Box::new(Expr::Ref("log")),
                                }),
                                args: vec![Expr::Str("false")],
                            }),
                            Stmt::Break,
                        ],
                    },
                ],
                default: vec![Stmt::Effect(Expr::Call {
                    expr: Box::new(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ref("console")),
                        right: Box::new(Expr::Ref("log")),
                    }),
                    args: vec![Expr::Str("?")],
                })],
            },
        ],
    )
}
