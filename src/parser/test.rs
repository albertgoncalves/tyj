use super::{get_ast, Case, Expr, Prop, Stmt, StmtPlus};
use crate::tokenizer::get_tokens;

macro_rules! assert_ast {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_ast(&get_tokens($a)), $b)
    };
}

#[test]
fn declare_number() {
    assert_ast!(
        "var x = .1;",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Num(".1") },
            line: 0,
        }],
    )
}

#[test]
fn declare_string() {
    assert_ast!(
        "var x = \"blah blah\";",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Str("blah blah") },
            line: 0,
        }],
    )
}

#[test]
fn declare_bool() {
    assert_ast!(
        "var x = true;",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Bool("true") },
            line: 0,
        }],
    )
}

#[test]
fn declare_null() {
    assert_ast!(
        "var x = null;",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Null },
            line: 0,
        }],
    )
}

#[test]
fn declare_undefined() {
    assert_ast!(
        "var x = undefined;",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Undef },
            line: 0,
        }],
    )
}

#[test]
fn declare_object() {
    assert_ast!(
        "var x = { a: null, bc: undefined };",
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
        "var x = {};",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Obj(Vec::new()) },
            line: 0,
        }],
    )
}

#[test]
fn declare_object_trailing_comma() {
    assert_ast!(
        "var x = {
             a: null,
             bc: undefined,
         };",
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
    let _: Vec<StmtPlus> =
        get_ast(&get_tokens("var x = { a: null bc: undefined };"));
}

#[test]
fn declare_assign() {
    assert_ast!(
        "var x;
         x = null;",
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "x", expr: Expr::Uninit },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Assign {
                    op: "=",
                    r#ref: Expr::Ref("x"),
                    expr: Expr::Null,
                },
                line: 1,
            },
        ],
    )
}

#[test]
fn multiple_declares() {
    assert_ast!(
        "var a = 1.;
         var b = \"blah\";
         var c = false;
         var d = null;
         var e = undefined;
         var f = {
             key: \"value\",
         };",
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "a", expr: Expr::Num("1.") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "b", expr: Expr::Str("blah") },
                line: 1,
            },
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "c",
                    expr: Expr::Bool("false")
                },
                line: 2,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "d", expr: Expr::Null },
                line: 3,
            },
            StmtPlus {
                statement: Stmt::Decl { ident: "e", expr: Expr::Undef },
                line: 4,
            },
            StmtPlus {
                statement: Stmt::Decl {
                    ident: "f",
                    expr: Expr::Obj(vec![Prop {
                        key: "key",
                        value: Expr::Str("value"),
                    }]),
                },
                line: 5,
            },
        ],
    )
}

#[test]
fn return_nothing() {
    assert_ast!(
        "return;",
        vec![StmtPlus { statement: Stmt::Ret(Expr::Undef), line: 0 }],
    )
}

#[test]
fn return_object() {
    assert_ast!(
        "return {
             ab: null,
             cd: undefined
         };",
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
        "return {};",
        vec![StmtPlus {
            statement: Stmt::Ret(Expr::Obj(Vec::new())),
            line: 0,
        }],
    )
}

#[test]
fn function_nothing() {
    assert_ast!(
        "function f() {}",
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
        "function f(x, y) {
             return;
         }",
        vec![StmtPlus {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["x", "y"],
                body: vec![StmtPlus {
                    statement: Stmt::Ret(Expr::Undef),
                    line: 1,
                }],
            },
            line: 0,
        }],
    )
}

#[test]
fn function_multiple_lines() {
    assert_ast!(
        "function f(a, b, c) {
             var d = {
                 a: a,
                 b: b,
                 c: c,
             };
             return d;
         }",
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
                        line: 1,
                    },
                    StmtPlus { statement: Stmt::Ret(Expr::Ref("d")), line: 6 },
                ],
            },
            line: 0,
        }],
    )
}

#[test]
fn object_fields() {
    assert_ast!(
        "var x = {
             a: {
                 b: 0,
             },
         };
         x.a.b;",
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
                line: 5,
            },
        ],
    )
}

#[test]
fn declare_anonymous_function() {
    assert_ast!(
        "var f = function(x) {
             return x + 0.1;
         };",
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
                        line: 1,
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
        "window.onload = function() {
             var a = 0.1;
             var b = 10;
             return a + b;
         };",
        vec![StmtPlus {
            statement: Stmt::Assign {
                op: "=",
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
                            line: 1,
                        },
                        StmtPlus {
                            statement: Stmt::Decl {
                                ident: "b",
                                expr: Expr::Num("10"),
                            },
                            line: 2,
                        },
                        StmtPlus {
                            statement: Stmt::Ret(Expr::Infix {
                                op: "+",
                                left: Box::new(Expr::Ref("a")),
                                right: Box::new(Expr::Ref("b")),
                            }),
                            line: 3,
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
        "var a = !true;
         var b = -1.0;",
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
                line: 1,
            },
        ],
    )
}

#[test]
fn nested_expression() {
    assert_ast!(
        "var x = (a + b) + ((c + d) + e);",
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
        "a++;
         ++b;",
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
                line: 1,
            },
        ],
    )
}

#[test]
fn decrement() {
    assert_ast!(
        "a--;
         --b;",
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
                line: 1,
            },
        ],
    )
}

#[test]
fn r#if() {
    assert_ast!(
        "if (true) {
             return 0;
         }",
        vec![StmtPlus {
            statement: Stmt::Cond {
                condition: Expr::Bool("true"),
                r#if: vec![StmtPlus {
                    statement: Stmt::Ret(Expr::Num("0")),
                    line: 1,
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
        "var a;
         if (true) {
             a = 0;
         } else {
             a = 1;
         }",
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
                            op: "=",
                            r#ref: Expr::Ref("a"),
                            expr: Expr::Num("0"),
                        },
                        line: 2,
                    }],
                    r#else: vec![StmtPlus {
                        statement: Stmt::Assign {
                            op: "=",
                            r#ref: Expr::Ref("a"),
                            expr: Expr::Num("1"),
                        },
                        line: 4,
                    }],
                },
                line: 1,
            },
        ],
    )
}

#[test]
fn function_calls() {
    assert_ast!(
        "f(a)(b);",
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
        "((f(a))(b));",
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
        "f(a(x)(y))(b);",
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
fn small_function() {
    assert_ast!(
        "// ...
         function f(a, b, c) {
             var d = {
                 a: a,
                 b: b,
                 c: c,
             };
             return d.a;
         }",
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
    )
}

#[test]
fn operator_precedence() {
    assert_ast!(
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
    )
}

#[test]
fn return_function() {
    assert_ast!(
        "function f(a, b) {
             return function(c) {
                 return {
                     a: a,
                     b: b,
                     c: c,
                 };
             };
         }",
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
    )
}

#[test]
fn parse_if_else_chain() {
    assert_ast!(
        "var x = 0;
         var y;
         if (x === 0) {
             y = 0;
         } else if (x === -1) {
             y = 1;
         } else {
             y = 2;
         }",
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
                            op: "=",
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
                                    op: "=",
                                    r#ref: Expr::Ref("y"),
                                    expr: Expr::Num("1"),
                                },
                                line: 5,
                            }],
                            r#else: vec![StmtPlus {
                                statement: Stmt::Assign {
                                    op: "=",
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
    )
}

#[test]
fn switch_simple() {
    assert_ast!(
        "var x = true;
         switch (x) {
         case true: {
             console.log(\"true\");
             break;
         }
         case false: {
             console.log(\"false\");
             break;
         }
         default: {
             console.log(\"?\");
         }
         }",
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
                                    line: 3,
                                },
                                StmtPlus { statement: Stmt::Break, line: 4 },
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
                                    line: 7,
                                },
                                StmtPlus { statement: Stmt::Break, line: 8 },
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
                        line: 11,
                    }],
                },
                line: 1,
            },
        ],
    )
}

#[test]
fn switch() {
    assert_ast!(
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
                                        op: "=",
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
                                        op: "=",
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
                            op: "=",
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
    )
}

#[test]
fn console_log() {
    assert_ast!(
        "console.log(\"Hello, world!\");",
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
    )
}

#[test]
fn scopes() {
    assert_ast!(
        "{
             {
                 var x = null;
             }
         }",
        vec![StmtPlus {
            statement: Stmt::Decl { ident: "x", expr: Expr::Null },
            line: 2,
        }],
    )
}

#[test]
fn ternary_operator() {
    assert_ast!(
        "var x = y === 0 ? 0 : 1;",
        vec![StmtPlus {
            statement: Stmt::Decl {
                ident: "x",
                expr: Expr::Ternary {
                    condition: Box::new(Expr::Infix {
                        op: "===",
                        left: Box::new(Expr::Ref("y")),
                        right: Box::new(Expr::Num("0")),
                    }),
                    r#if: Box::new(Expr::Num("0")),
                    r#else: Box::new(Expr::Num("1")),
                },
            },
            line: 0,
        }],
    )
}

#[test]
fn modulo_operator() {
    assert_ast!(
        "10 % 9;",
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Infix {
                op: "%",
                left: Box::new(Expr::Num("10")),
                right: Box::new(Expr::Num("9")),
            }),
            line: 0,
        }],
    )
}

#[test]
fn boolean_operators() {
    assert_ast!(
        "10 % 9 === 1 || true && false;",
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Infix {
                op: "||",
                left: Box::new(Expr::Infix {
                    op: "===",
                    left: Box::new(Expr::Infix {
                        op: "%",
                        left: Box::new(Expr::Num("10")),
                        right: Box::new(Expr::Num("9")),
                    }),
                    right: Box::new(Expr::Num("1")),
                }),
                right: Box::new(Expr::Infix {
                    op: "&&",
                    left: Box::new(Expr::Bool("true")),
                    right: Box::new(Expr::Bool("false")),
                }),
            }),
            line: 0,
        }],
    )
}

#[test]
fn negate_call() {
    assert_ast!(
        "!f();",
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Prefix {
                op: "!",
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ref("f")),
                    args: Vec::new(),
                }),
            }),
            line: 0,
        }],
    )
}

#[test]
fn new() {
    assert_ast!(
        "new Uint8Array(buffer, 0, 13);",
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Prefix {
                op: "new",
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ref("Uint8Array")),
                    args: vec![
                        Expr::Ref("buffer"),
                        Expr::Num("0"),
                        Expr::Num("13"),
                    ],
                }),
            }),
            line: 0,
        }],
    )
}

#[test]
fn add_nested_functions() {
    assert_ast!(
        "f(g()) + g(f());",
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Infix {
                op: "+",
                left: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ref("f")),
                    args: vec![Expr::Call {
                        expr: Box::new(Expr::Ref("g")),
                        args: Vec::new(),
                    }],
                }),
                right: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ref("g")),
                    args: vec![Expr::Call {
                        expr: Box::new(Expr::Ref("f")),
                        args: Vec::new(),
                    }],
                }),
            }),
            line: 0,
        }],
    )
}

#[test]
fn call_method_chain() {
    assert_ast!(
        "f().x().y(g().z);",
        vec![StmtPlus {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Call {
                                expr: Box::new(Expr::Ref("f")),
                                args: Vec::new(),
                            }),
                            right: Box::new(Expr::Ref("x")),
                        }),
                        args: Vec::new(),
                    }),
                    right: Box::new(Expr::Ref("y")),
                }),
                args: vec![Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Call {
                        expr: Box::new(Expr::Ref("g")),
                        args: Vec::new(),
                    }),
                    right: Box::new(Expr::Ref("z")),
                }],
            }),
            line: 0,
        }],
    )
}

#[test]
fn promise() {
    assert_ast!(
        "window.onload = function() {
             module.init(fetch(\"./source\")).then(function(object) {});
         };",
        vec![StmtPlus {
            statement: Stmt::Assign {
                op: "=",
                r#ref: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ref("window")),
                    right: Box::new(Expr::Ref("onload")),
                },
                expr: Expr::Fn {
                    args: Vec::new(),
                    body: vec![StmtPlus {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Call {
                                    expr: Box::new(Expr::Infix {
                                        op: ".",
                                        left: Box::new(Expr::Ref("module")),
                                        right: Box::new(Expr::Ref("init")),
                                    }),
                                    args: vec![Expr::Call {
                                        expr: Box::new(Expr::Ref("fetch")),
                                        args: vec![Expr::Str("./source")],
                                    }],
                                }),
                                right: Box::new(Expr::Ref("then")),
                            }),
                            args: vec![Expr::Fn {
                                args: vec!["object"],
                                body: Vec::new(),
                            }],
                        }),
                        line: 1,
                    }],
                },
            },
            line: 0,
        }],
    )
}

#[test]
fn brackets() {
    assert_ast!(
        "array[0].x = null;",
        vec![StmtPlus {
            statement: Stmt::Assign {
                op: "=",
                r#ref: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Access {
                        expr: Box::new(Expr::Ref("array")),
                        index: Box::new(Expr::Num("0")),
                    }),
                    right: Box::new(Expr::Ref("x")),
                },
                expr: Expr::Null,
            },
            line: 0,
        }],
    )
}

#[test]
fn bit_operators() {
    assert_ast!(
        "~0;
         1 & 1;
         2 | 2;
         3 ^ 3;
         4 << 4;
         5 >> 5;
         6 >>> 6;",
        vec![
            StmtPlus {
                statement: Stmt::Effect(Expr::Prefix {
                    op: "~",
                    expr: Box::new(Expr::Num("0")),
                }),
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "&",
                    left: Box::new(Expr::Num("1")),
                    right: Box::new(Expr::Num("1")),
                }),
                line: 1,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "|",
                    left: Box::new(Expr::Num("2")),
                    right: Box::new(Expr::Num("2")),
                }),
                line: 2,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "^",
                    left: Box::new(Expr::Num("3")),
                    right: Box::new(Expr::Num("3")),
                }),
                line: 3,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: "<<",
                    left: Box::new(Expr::Num("4")),
                    right: Box::new(Expr::Num("4")),
                }),
                line: 4,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: ">>",
                    left: Box::new(Expr::Num("5")),
                    right: Box::new(Expr::Num("5")),
                }),
                line: 5,
            },
            StmtPlus {
                statement: Stmt::Effect(Expr::Infix {
                    op: ">>>",
                    left: Box::new(Expr::Num("6")),
                    right: Box::new(Expr::Num("6")),
                }),
                line: 6,
            },
        ],
    )
}

#[test]
fn update_assign() {
    assert_ast!(
        "a += 1;
         b -= 1;
         c *= 2;
         d /= 2;",
        vec![
            StmtPlus {
                statement: Stmt::Assign {
                    op: "+=",
                    r#ref: Expr::Ref("a"),
                    expr: Expr::Num("1"),
                },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::Assign {
                    op: "-=",
                    r#ref: Expr::Ref("b"),
                    expr: Expr::Num("1"),
                },
                line: 1,
            },
            StmtPlus {
                statement: Stmt::Assign {
                    op: "*=",
                    r#ref: Expr::Ref("c"),
                    expr: Expr::Num("2"),
                },
                line: 2,
            },
            StmtPlus {
                statement: Stmt::Assign {
                    op: "/=",
                    r#ref: Expr::Ref("d"),
                    expr: Expr::Num("2"),
                },
                line: 3,
            },
        ],
    )
}

#[test]
fn r#while() {
    assert_ast!(
        "var i = 0;
         while (i < 10) {
             console.log(i++);
         }",
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "i", expr: Expr::Num("0") },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::While {
                    condition: Expr::Infix {
                        op: "<",
                        left: Box::new(Expr::Ref("i")),
                        right: Box::new(Expr::Num("10")),
                    },
                    body: vec![StmtPlus {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("console")),
                                right: Box::new(Expr::Ref("log")),
                            }),
                            args: vec![Expr::Postfix {
                                op: "++",
                                expr: Box::new(Expr::Ref("i")),
                            }],
                        }),
                        line: 2,
                    }],
                },
                line: 1,
            },
        ],
    )
}

#[test]
fn r#for() {
    assert_ast!(
        "for (var i = 0; i < 10; ++i) {
             console.log(i);
         }",
        vec![StmtPlus {
            statement: Stmt::For {
                init: Some(Box::new(StmtPlus {
                    statement: Stmt::Decl { ident: "i", expr: Expr::Num("0") },
                    line: 0,
                })),
                condition: Some(Expr::Infix {
                    op: "<",
                    left: Box::new(Expr::Ref("i")),
                    right: Box::new(Expr::Num("10")),
                }),
                update: Some(Box::new(StmtPlus {
                    statement: Stmt::Effect(Expr::Prefix {
                        op: "++",
                        expr: Box::new(Expr::Ref("i")),
                    }),
                    line: 0,
                })),
                body: vec![StmtPlus {
                    statement: Stmt::Effect(Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ref("console")),
                            right: Box::new(Expr::Ref("log")),
                        }),
                        args: vec![Expr::Ref("i")],
                    }),
                    line: 1,
                }],
            },
            line: 0,
        }],
    )
}

#[test]
fn for_empty() {
    assert_ast!(
        "for (;;) {
             console.log(i);
         }",
        vec![StmtPlus {
            statement: Stmt::For {
                init: None,
                condition: None,
                update: None,
                body: vec![StmtPlus {
                    statement: Stmt::Effect(Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ref("console")),
                            right: Box::new(Expr::Ref("log")),
                        }),
                        args: vec![Expr::Ref("i")],
                    }),
                    line: 1,
                }],
            },
            line: 0,
        }],
    )
}

#[test]
fn for_update() {
    assert_ast!(
        "var i;
         for (i = 0; i < 10; i += 2) {
             console.log(i);
         }",
        vec![
            StmtPlus {
                statement: Stmt::Decl { ident: "i", expr: Expr::Uninit },
                line: 0,
            },
            StmtPlus {
                statement: Stmt::For {
                    init: Some(Box::new(StmtPlus {
                        statement: Stmt::Assign {
                            op: "=",
                            r#ref: Expr::Ref("i"),
                            expr: Expr::Num("0"),
                        },
                        line: 1,
                    })),
                    condition: Some(Expr::Infix {
                        op: "<",
                        left: Box::new(Expr::Ref("i")),
                        right: Box::new(Expr::Num("10")),
                    }),
                    update: Some(Box::new(StmtPlus {
                        statement: Stmt::Assign {
                            op: "+=",
                            r#ref: Expr::Ref("i"),
                            expr: Expr::Num("2"),
                        },
                        line: 1,
                    })),
                    body: vec![StmtPlus {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ref("console")),
                                right: Box::new(Expr::Ref("log")),
                            }),
                            args: vec![Expr::Ref("i")],
                        }),
                        line: 2,
                    }],
                },
                line: 1,
            },
        ],
    )
}
