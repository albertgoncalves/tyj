use super::{get_ast, Case, Expr, Prop, Stmt, StmtPlus};
use crate::tokenizer::{Tkn, TknPlus};

macro_rules! assert_ast {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_ast($a), $b)
    };
}

#[test]
fn declare_number() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Num(".1"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Num(".1") },
            line: 0,
        }],
    )
}

#[test]
fn declare_string() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Str("blah blah"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Str("blah blah") },
            line: 0,
        }],
    )
}

#[test]
fn declare_bool() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Bool("true"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Bool("true") },
            line: 0,
        }],
    )
}

#[test]
fn declare_null() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Null, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Null },
            line: 0,
        }],
    )
}

#[test]
fn declare_undefined() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Undef, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Undef },
            line: 0,
        }],
    )
}

#[test]
fn declare_object() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Null, line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("bc"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Undef, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl {
                ident: "x",
                expr: Expr::Obj(vec![
                    Prop { key: "a", value: Expr::Null },
                    Prop { key: "bc", value: Expr::Undef },
                ]),
            },
            line: 0,
        }],
    )
}

#[test]
fn declare_empty_object() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Obj(Vec::new()) },
            line: 0,
        }],
    )
}

#[test]
fn declare_object_trailing_comma() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Null, line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("bc"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Undef, line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl {
                ident: "x",
                expr: Expr::Obj(vec![
                    Prop { key: "a", value: Expr::Null },
                    Prop { key: "bc", value: Expr::Undef },
                ]),
            },
            line: 0,
        }],
    )
}

#[test]
#[should_panic]
fn declare_object_missing_comma() {
    let _: Vec<StmtPlus> = get_ast(&[
        TknPlus { token: Tkn::Var, line: 0 },
        TknPlus { token: Tkn::Ident("x"), line: 0 },
        TknPlus { token: Tkn::Op("="), line: 0 },
        TknPlus { token: Tkn::LBrace, line: 0 },
        TknPlus { token: Tkn::Ident("a"), line: 0 },
        TknPlus { token: Tkn::Colon, line: 0 },
        TknPlus { token: Tkn::Null, line: 0 },
        TknPlus { token: Tkn::Ident("bc"), line: 0 },
        TknPlus { token: Tkn::Colon, line: 0 },
        TknPlus { token: Tkn::Undef, line: 0 },
        TknPlus { token: Tkn::RBrace, line: 0 },
        TknPlus { token: Tkn::Semicolon, line: 0 },
    ]);
}

#[test]
fn declare_assign() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Null, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "x", expr: Expr::Uninit },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Assign {
                    r#ref: Expr::Ref("x"),
                    expr: Expr::Null,
                },
                line: 0,
            },
        ],
    )
}

#[test]
fn multiple_declares() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Num("1."), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Str("blah"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("c"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Bool("false"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("d"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Null, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("e"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Undef, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("key"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Str("value"), line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "a", expr: Expr::Num("1.") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "b", expr: Expr::Str("blah") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "c",
                    expr: Expr::Bool("false")
                },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "d", expr: Expr::Null },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "e", expr: Expr::Undef },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "f",
                    expr: Expr::Obj(vec![Prop {
                        key: "key",
                        value: Expr::Str("value"),
                    }]),
                },
                line: 0,
            },
        ],
    )
}

#[test]
fn return_nothing() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus { statement: Stmt::Ret(Expr::Undef), line: 0 }],
    )
}

#[test]
fn return_object() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("ab"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Null, line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("cd"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Undef, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Ret(Expr::Obj(vec![
                Prop { key: "ab", value: Expr::Null },
                Prop { key: "cd", value: Expr::Undef },
            ])),
            line: 0,
        }],
    )
}

#[test]
fn return_empty_object() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Ret(Expr::Obj(Vec::new())),
            line: 0,
        }],
    )
}

#[test]
fn function_nothing() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Fn, line: 0 },
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Fn {
                ident: "f",
                args: Vec::new(),
                body: Vec::new(),
            },
            line: 0,
        }],
    )
}

#[test]
fn function_return_nothing() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Fn, line: 0 },
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("y"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["x", "y"],
                body: vec![StmtPlus {
                    statement: Stmt::Ret(Expr::Undef),
                    line: 0,
                }],
            },
            line: 0,
        }],
    )
}

#[test]
fn function_multiple_lines() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Fn, line: 0 },
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("c"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("d"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::Ident("c"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Ident("c"), line: 0 },
            TknPlus { token: Tkn::Comma, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::Ident("d"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["a", "b", "c"],
                body: vec![
                    StmtPlus {
                        statement: Stmt::Decl {
                            ident: "d",
                            expr: Expr::Obj(vec![
                                Prop { key: "a", value: Expr::Ref("a") },
                                Prop { key: "b", value: Expr::Ref("b") },
                                Prop { key: "c", value: Expr::Ref("c") },
                            ]),
                        },
                        line: 0,
                    },
                    StmtPlus { statement: Stmt::Ret(Expr::Ref("d")), line: 0 },
                ],
            },
            line: 0,
        }],
    )
}

#[test]
fn object_fields() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::Num("0"), line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("."), line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("."), line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Obj(vec![Prop {
                        key: "a",
                        value: Expr::Obj(vec![Prop {
                            key: "b",
                            value: Expr::Num("0"),
                        }]),
                    }]),
                },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ref("x")),
                        right: Box::new(Expr::Ref("a")),
                    }),
                    right: Box::new(Expr::Ref("b")),
                }),
                line: 0,
            },
        ],
    )
}

#[test]
fn declare_anonymous_function() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Fn, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("+"), line: 0 },
            TknPlus { token: Tkn::Num("0.1"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl {
                ident: "f",
                expr: Expr::Fn {
                    args: vec!["x"],
                    body: vec![StmtPlus {
                        statement: Stmt::Ret(Expr::Infix {
                            op: "+",
                            left: Box::new(Expr::Ref("x")),
                            right: Box::new(Expr::Num("0.1")),
                        }),
                        line: 0,
                    }],
                }
            },
            line: 0,
        }],
    )
}

#[test]
fn tiny_program() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ident("window"), line: 0 },
            TknPlus { token: Tkn::Op("."), line: 0 },
            TknPlus { token: Tkn::Ident("onload"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Fn, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Num("0.1"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Num("10"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("+"), line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Assign {
                r#ref: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ref("window")),
                    right: Box::new(Expr::Ref("onload")),
                },
                expr: Expr::Fn {
                    args: Vec::new(),
                    body: vec![
                        StmtPlus {
                            statement: Stmt::Decl {
                                ident: "a",
                                expr: Expr::Num("0.1"),
                            },
                            line: 0,
                        },
                        StmtPlus {
                            statement: Stmt::Decl {
                                ident: "b",
                                expr: Expr::Num("10"),
                            },
                            line: 0,
                        },
                        StmtPlus {
                            statement: Stmt::Ret(Expr::Infix {
                                op: "+",
                                left: Box::new(Expr::Ref("a")),
                                right: Box::new(Expr::Ref("b")),
                            }),
                            line: 0,
                        },
                    ],
                },
            },
            line: 0,
        }],
    )
}

#[test]
fn prefix_operators() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Op("!"), line: 0 },
            TknPlus { token: Tkn::Bool("true"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Op("-"), line: 0 },
            TknPlus { token: Tkn::Num("1.0"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "a",
                    expr: Expr::Prefix {
                        op: "!",
                        expr: Box::new(Expr::Bool("true")),
                    },
                },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "b",
                    expr: Expr::Prefix {
                        op: "-",
                        expr: Box::new(Expr::Num("1.0")),
                    },
                },
                line: 0,
            },
        ],
    )
}

#[test]
fn nested_expression() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("+"), line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Op("+"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("c"), line: 0 },
            TknPlus { token: Tkn::Op("+"), line: 0 },
            TknPlus { token: Tkn::Ident("d"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Op("+"), line: 0 },
            TknPlus { token: Tkn::Ident("e"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Decl {
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
            },
            line: 0,
        }],
    )
}

#[test]
fn increment() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("++"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Op("++"), line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Effect(Expr::Postfix {
                    op: "++",
                    expr: Box::new(Expr::Ref("a")),
                }),
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Prefix {
                    op: "++",
                    expr: Box::new(Expr::Ref("b")),
                }),
                line: 0,
            },
        ],
    )
}

#[test]
fn decrement() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("--"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Op("--"), line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Effect(Expr::Postfix {
                    op: "--",
                    expr: Box::new(Expr::Ref("a")),
                }),
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Prefix {
                    op: "--",
                    expr: Box::new(Expr::Ref("b")),
                }),
                line: 0,
            },
        ],
    )
}

#[test]
fn r#if() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::If, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Bool("true"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ret, line: 0 },
            TknPlus { token: Tkn::Num("0"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Cond {
                condition: Expr::Bool("true"),
                r#if: vec![StmtPlus {
                    statement: Stmt::Ret(Expr::Num("0")),
                    line: 0,
                }],
                r#else: Vec::new(),
            },
            line: 0,
        }],
    )
}

#[test]
fn if_else() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::If, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Bool("true"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Num("0"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Else, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Num("1"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "a", expr: Expr::Uninit },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Cond {
                    condition: Expr::Bool("true"),
                    r#if: vec![StmtPlus {
                        statement: Stmt::Assign {
                            r#ref: Expr::Ref("a"),
                            expr: Expr::Num("0"),
                        },
                        line: 0,
                    }],
                    r#else: vec![StmtPlus {
                        statement: Stmt::Assign {
                            r#ref: Expr::Ref("a"),
                            expr: Expr::Num("1"),
                        },
                        line: 0,
                    }],
                },
                line: 0,
            },
        ],
    )
}

#[test]
fn function_calls() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ref("f")),
                    args: vec![Expr::Ref("a")],
                }),
                args: vec![Expr::Ref("b")],
            }),
            line: 0,
        }],
    )
}

#[test]
fn function_calls_more_parens() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ref("f")),
                    args: vec![Expr::Ref("a")],
                }),
                args: vec![Expr::Ref("b")],
            }),
            line: 0,
        }],
    )
}

#[test]
fn function_calls_nested() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Ident("f"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("a"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("y"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("b"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
        ],
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Call {
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
            }),
            line: 0,
        }],
    )
}

#[test]
fn switch() {
    assert_ast!(
        &[
            TknPlus { token: Tkn::Var, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::Op("="), line: 0 },
            TknPlus { token: Tkn::Bool("true"), line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Switch, line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Ident("x"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Case, line: 0 },
            TknPlus { token: Tkn::Bool("true"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("console"), line: 0 },
            TknPlus { token: Tkn::Op("."), line: 0 },
            TknPlus { token: Tkn::Ident("log"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Str("true"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Break, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Case, line: 0 },
            TknPlus { token: Tkn::Bool("false"), line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("console"), line: 0 },
            TknPlus { token: Tkn::Op("."), line: 0 },
            TknPlus { token: Tkn::Ident("log"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Str("false"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::Break, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::Default, line: 0 },
            TknPlus { token: Tkn::Colon, line: 0 },
            TknPlus { token: Tkn::LBrace, line: 0 },
            TknPlus { token: Tkn::Ident("console"), line: 0 },
            TknPlus { token: Tkn::Op("."), line: 0 },
            TknPlus { token: Tkn::Ident("log"), line: 0 },
            TknPlus { token: Tkn::LParen, line: 0 },
            TknPlus { token: Tkn::Str("?"), line: 0 },
            TknPlus { token: Tkn::RParen, line: 0 },
            TknPlus { token: Tkn::Semicolon, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
            TknPlus { token: Tkn::RBrace, line: 0 },
        ],
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "x", expr: Expr::Bool("true") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Switch {
                    expr: Expr::Ref("x"),
                    cases: vec![
                        Case {
                            expr: Expr::Bool("true"),
                            body: vec![
                                StmtPlus {
                                    statement: Stmt::Effect(Expr::Call {
                                        expr: Box::new(Expr::Infix {
                                            op: ".",
                                            left: Box::new(Expr::Ref(
                                                "console",
                                            )),
                                            right: Box::new(Expr::Ref("log")),
                                        }),
                                        args: vec![Expr::Str("true")],
                                    }),
                                    line: 0,
                                },
                                StmtPlus { statement: Stmt::Break, line: 0 },
                            ],
                        },
                        Case {
                            expr: Expr::Bool("false"),
                            body: vec![
                                StmtPlus {
                                    statement: Stmt::Effect(Expr::Call {
                                        expr: Box::new(Expr::Infix {
                                            op: ".",
                                            left: Box::new(Expr::Ref(
                                                "console",
                                            )),
                                            right: Box::new(Expr::Ref("log")),
                                        }),
                                        args: vec![Expr::Str("false")],
                                    }),
                                    line: 0,
                                },
                                StmtPlus { statement: Stmt::Break, line: 0 },
                            ],
                        },
                    ],
                    default: vec![StmtPlus {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("console")),
                                right: Box::new(Expr::Ref("log")),
                            }),
                            args: vec![Expr::Str("?")],
                        }),
                        line: 0,
                    }],
                },
                line: 0,
            },
        ],
    )
}
