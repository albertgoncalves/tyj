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
                "function f(a, b, c) {
                    var d = {
                        a: a,
                        b: b,
                        c: c,
                    };
                    return d.a;
                }"
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
                "var x = {
                    a: {
                        b: 10,
                    },
                };
                x.a.b++ + .01;
                ++x.a.b + .01;
                .01 + x.a.b++;
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
}
