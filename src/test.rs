use crate::parser::{get_ast, Case, Expr, Prop, Stmt, StmtPlus};
use crate::tokenizer::get_tokens;

#[test]
fn parse_small_function() {
    assert_eq!(
        get_ast(&get_tokens(
            "// ...
             function f(a, b, c) {
                 var d = {
                     a: a,
                     b: b,
                     c: c,
                 };
                 return d.a;
             }",
        )),
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
                        line: 2,
                    },
                    StmtPlus {
                        statement: Stmt::Ret(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ref("d")),
                            right: Box::new(Expr::Ref("a")),
                        }),
                        line: 7,
                    },
                ],
            },
            line: 1,
        }],
    );
}

#[test]
fn parse_operator_precedence() {
    assert_eq!(
        get_ast(&get_tokens(
            "/* ...
              */
             var x = {
                 a: {
                     b: 10,
                 },
             };
             // ...
             x.a.b++ + .01;
             ++x.a.b + .01;
             /* ... */
             .01 + x.a.b++;
             .01 + ++x.a.b;",
        )),
        vec![
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Obj(vec![Prop {
                        key: "a",
                        value: Expr::Obj(vec![Prop {
                            key: "b",
                            value: Expr::Num("10"),
                        }]),
                    }]),
                },
                line: 2,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Postfix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("x")),
                                right: Box::new(Expr::Ref("a")),
                            }),
                            right: Box::new(Expr::Ref("b")),
                        }),
                    }),
                    right: Box::new(Expr::Num(".01")),
                }),
                line: 8,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Prefix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("x")),
                                right: Box::new(Expr::Ref("a")),
                            }),
                            right: Box::new(Expr::Ref("b")),
                        }),
                    }),
                    right: Box::new(Expr::Num(".01")),
                }),
                line: 9,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Num(".01")),
                    right: Box::new(Expr::Postfix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("x")),
                                right: Box::new(Expr::Ref("a")),
                            }),
                            right: Box::new(Expr::Ref("b")),
                        }),
                    }),
                }),
                line: 11,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Num(".01")),
                    right: Box::new(Expr::Prefix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("x")),
                                right: Box::new(Expr::Ref("a")),
                            }),
                            right: Box::new(Expr::Ref("b")),
                        }),
                    }),
                }),
                line: 12,
            },
        ],
    );
}

#[test]
fn parse_return_function() {
    assert_eq!(
        get_ast(&get_tokens(
            "function f(a, b) {
                 return function(c) {
                     return {
                         a: a,
                         b: b,
                         c: c,
                     };
                 };
             }",
        )),
        vec![StmtPlus {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["a", "b"],
                body: vec![StmtPlus {
                    statement: Stmt::Ret(Expr::Fn {
                        args: vec!["c"],
                        body: vec![StmtPlus {
                            statement: Stmt::Ret(Expr::Obj(vec![
                                Prop { key: "a", value: Expr::Ref("a") },
                                Prop { key: "b", value: Expr::Ref("b") },
                                Prop { key: "c", value: Expr::Ref("c") },
                            ])),
                            line: 2,
                        }],
                    }),
                    line: 1,
                }],
            },
            line: 0,
        }],
    );
}

#[test]
fn parse_if_else_chain() {
    assert_eq!(
        get_ast(&get_tokens(
            "var x = 0;
             var y;
             if (x === 0) {
                 y = 0;
             } else if (x === -1) {
                 y = 1;
             } else {
                 y = 2;
             }",
        )),
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "x", expr: Expr::Num("0") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "y", expr: Expr::Uninit },
                line: 1,
            },
            StmtPlus {
                statement: Stmt::Cond {
                    condition: Expr::Infix {
                        op: "===",
                        left: Box::new(Expr::Ref("x")),
                        right: Box::new(Expr::Num("0")),
                    },
                    r#if: vec![StmtPlus {
                        statement: Stmt::Assign {
                            r#ref: Expr::Ref("y"),
                            expr: Expr::Num("0"),
                        },
                        line: 3,
                    }],
                    r#else: vec![StmtPlus {
                        statement: Stmt::Cond {
                            condition: Expr::Infix {
                                op: "===",
                                left: Box::new(Expr::Ref("x")),
                                right: Box::new(Expr::Prefix {
                                    op: "-",
                                    expr: Box::new(Expr::Num("1")),
                                }),
                            },
                            r#if: vec![StmtPlus {
                                statement: Stmt::Assign {
                                    r#ref: Expr::Ref("y"),
                                    expr: Expr::Num("1"),
                                },
                                line: 5,
                            }],
                            r#else: vec![StmtPlus {
                                statement: Stmt::Assign {
                                    r#ref: Expr::Ref("y"),
                                    expr: Expr::Num("2"),
                                },
                                line: 7,
                            }],
                        },
                        line: 4,
                    }],
                },
                line: 2,
            },
        ],
    );
}

#[test]
fn parse_switch() {
    assert_eq!(
        get_ast(&get_tokens(
            "var x = 0;
             var y;
             var a = 0;
             var b = 1;
             switch (x) {
             case a: {
                 y = \"0\";
                 break;
             }
             case b: {
                 y = \"1\";
                 break;
             }
             default: {
                 y = undefined;
             }
             }
             console.log(y);",
        )),
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "x", expr: Expr::Num("0") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "y", expr: Expr::Uninit },
                line: 1,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "a", expr: Expr::Num("0") },
                line: 2,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "b", expr: Expr::Num("1") },
                line: 3,
            },
            StmtPlus {
                statement: Stmt::Switch {
                    expr: Expr::Ref("x"),
                    cases: vec![
                        Case {
                            expr: Expr::Ref("a"),
                            body: vec![
                                StmtPlus {
                                    statement: Stmt::Assign {
                                        r#ref: Expr::Ref("y"),
                                        expr: Expr::Str("0"),
                                    },
                                    line: 6,
                                },
                                StmtPlus { statement: Stmt::Break, line: 7 },
                            ],
                        },
                        Case {
                            expr: Expr::Ref("b"),
                            body: vec![
                                StmtPlus {
                                    statement: Stmt::Assign {
                                        r#ref: Expr::Ref("y"),
                                        expr: Expr::Str("1"),
                                    },
                                    line: 10,
                                },
                                StmtPlus { statement: Stmt::Break, line: 11 },
                            ],
                        },
                    ],
                    default: vec![StmtPlus {
                        statement: Stmt::Assign {
                            r#ref: Expr::Ref("y"),
                            expr: Expr::Undef,
                        },
                        line: 14,
                    }],
                },
                line: 4,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Call {
                    expr: Box::new(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ref("console")),
                        right: Box::new(Expr::Ref("log")),
                    }),
                    args: vec![Expr::Ref("y")],
                }),
                line: 17,
            },
        ],
    );
}

#[test]
fn parse_console_log() {
    assert_eq!(
        get_ast(&get_tokens("console.log(\"Hello, world!\");")),
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ref("console")),
                    right: Box::new(Expr::Ref("log")),
                }),
                args: vec![Expr::Str("Hello, world!")],
            }),
            line: 0,
        }],
    );
}
