mod parser;
mod tokenizer;

#[cfg(test)]
mod tests {
    use crate::parser::{get_ast, Expr, Prop, Stmt};
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
                    ]))],
                })],
            }],
        );
    }
}
