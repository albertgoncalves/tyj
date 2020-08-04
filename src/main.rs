mod parser;
mod tokenizer;

#[cfg(test)]
mod tests {
    use crate::parser::{get_ast, Case, Expr, Prop, Stmt};
    use crate::tokenizer::get_tokens;

    #[test]
    fn parse_small_function() {
        assert_eq!(
            get_ast(&get_tokens(
                "// ...\n\
                 function f(a, b, c) {\n\
                     var d = {\n\
                         a: a,\n\
                         b: b,\n\
                         c: c,\n\
                     };\n\
                     return d.a;\n\
                 }",
            )),
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
                    Stmt::Ret(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ref("d")),
                        right: Box::new(Expr::Ref("a")),
                    }),
                ],
            }],
        );
    }

    #[test]
    fn parse_operator_precedence() {
        assert_eq!(
            get_ast(&get_tokens(
                "/* ...\n\
                  */\n\
                 var x = {\n\
                     a: {\n\
                         b: 10,\n\
                     },\n\
                 };\n\
                 // ...\n\
                 x.a.b++ + .01;\n\
                 ++x.a.b + .01;\n\
                 /* ... */\n\
                 .01 + x.a.b++;\n\
                 .01 + ++x.a.b;"
            )),
            vec![
                Stmt::Decl {
                    ident: "x",
                    expr: Expr::Obj(vec![Prop {
                        key: "a",
                        value: Expr::Obj(vec![Prop {
                            key: "b",
                            value: Expr::Num("10"),
                        }]),
                    }]),
                },
                Stmt::Effect(Expr::Infix {
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
                Stmt::Effect(Expr::Infix {
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
                Stmt::Effect(Expr::Infix {
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
                Stmt::Effect(Expr::Infix {
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
                 }"
            )),
            vec![Stmt::Fn {
                ident: "f",
                args: vec!["a", "b"],
                body: vec![Stmt::Ret(Expr::Fn {
                    args: vec!["c"],
                    body: vec![Stmt::Ret(Expr::Obj(vec![
                        Prop { key: "a", value: Expr::Ref("a") },
                        Prop { key: "b", value: Expr::Ref("b") },
                        Prop { key: "c", value: Expr::Ref("c") },
                    ]))],
                })],
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
                 }"
            )),
            vec![
                Stmt::Decl { ident: "x", expr: Expr::Num("0") },
                Stmt::Decl { ident: "y", expr: Expr::Uninit },
                Stmt::Cond {
                    condition: Expr::Infix {
                        op: "===",
                        left: Box::new(Expr::Ref("x")),
                        right: Box::new(Expr::Num("0")),
                    },
                    r#if: vec![Stmt::Assign {
                        r#ref: Expr::Ref("y"),
                        expr: Expr::Num("0"),
                    }],
                    r#else: vec![Stmt::Cond {
                        condition: Expr::Infix {
                            op: "===",
                            left: Box::new(Expr::Ref("x")),
                            right: Box::new(Expr::Prefix {
                                op: "-",
                                expr: Box::new(Expr::Num("1")),
                            })
                        },
                        r#if: vec![Stmt::Assign {
                            r#ref: Expr::Ref("y"),
                            expr: Expr::Num("1"),
                        }],
                        r#else: vec![Stmt::Assign {
                            r#ref: Expr::Ref("y"),
                            expr: Expr::Num("2"),
                        }],
                    }],
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
                Stmt::Decl {
                    ident: "x",
                    expr: Expr::Num("0"),
                },
                Stmt::Decl {
                    ident: "y",
                    expr: Expr::Uninit,
                },
                Stmt::Decl {
                    ident: "a",
                    expr: Expr::Num("0"),
                },
                Stmt::Decl {
                    ident: "b",
                    expr: Expr::Num("1"),
                },
                Stmt::Switch {
                    expr: Expr::Ref("x"),
                    cases: vec![
                        Case {
                            expr: Expr::Ref("a"),
                            body: vec![
                                Stmt::Assign {
                                    r#ref: Expr::Ref("y"),
                                    expr: Expr::Str("0"),
                                },
                                Stmt::Break,
                            ],
                        },
                        Case {
                            expr: Expr::Ref("b"),
                            body: vec![
                                Stmt::Assign {
                                    r#ref: Expr::Ref("y"),
                                    expr: Expr::Str("1"),
                                },
                                Stmt::Break,
                            ],
                        },
                    ],
                    default: vec![
                        Stmt::Assign {
                            r#ref: Expr::Ref("y"),
                            expr: Expr::Undef,
                        },
                    ],
                },
                Stmt::Effect(
                    Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ref("console")),
                            right: Box::new(Expr::Ref("log")),
                        }),
                        args: vec![Expr::Ref("y")],
                    }
                ),
            ],
        );
    }

    #[test]
    fn parse_console_log() {
        assert_eq!(
            get_ast(&get_tokens("console.log(\"Hello, world!\");")),
            vec![Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ref("console")),
                    right: Box::new(Expr::Ref("log")),
                }),
                args: vec![Expr::Str("Hello, world!")],
            })],
        );
    }
}
