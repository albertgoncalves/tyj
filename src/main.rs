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
                    Stmt::Ret(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ref("d")),
                        right: Box::new(Expr::Ref("a")),
                    }),
                ],
            }],
        );
    }
}
