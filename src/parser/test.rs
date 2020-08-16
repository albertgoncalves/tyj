use super::{get_ast, Case, Expr, Prop, Stmt, Syntax};
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
        vec![Syntax {
            statement: Stmt::Decl { ident: "x", expr: Expr::Num(".1") },
            line: 0,
        }],
    )
}

#[test]
fn declare_string() {
    assert_ast!(
        "var x = \"blah blah\";",
        vec![Syntax {
            statement: Stmt::Decl { ident: "x", expr: Expr::Str("blah blah") },
            line: 0,
        }],
    )
}

#[test]
fn declare_bool() {
    assert_ast!(
        "var x = true;",
        vec![Syntax {
            statement: Stmt::Decl { ident: "x", expr: Expr::Bool("true") },
            line: 0,
        }],
    )
}

#[test]
fn declare_null() {
    assert_ast!(
        "var x = null;",
        vec![Syntax {
            statement: Stmt::Decl { ident: "x", expr: Expr::Null },
            line: 0,
        }],
    )
}

#[test]
fn declare_undefined() {
    assert_ast!(
        "var x = undefined;",
        vec![Syntax {
            statement: Stmt::Decl { ident: "x", expr: Expr::Undef },
            line: 0,
        }],
    )
}

#[test]
fn declare_object() {
    assert_ast!(
        "var x = { a: null, bc: undefined };",
        vec![Syntax {
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
        vec![Syntax {
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
        vec![Syntax {
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
    let _: Vec<Syntax> =
        get_ast(&get_tokens("var x = { a: null bc: undefined };"));
}

#[test]
fn declare_assign() {
    assert_ast!(
        "var x;
         x = null;",
        vec![
            Syntax {
                statement: Stmt::Decl { ident: "x", expr: Expr::Uninit },
                line: 0,
            },
            Syntax {
                statement: Stmt::Assign {
                    op: "=",
                    ident: Expr::Ident("x"),
                    expr: Expr::Null,
                },
                line: 1,
            },
        ],
    )
}

#[test]
fn mixed_declares() {
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
            Syntax {
                statement: Stmt::Decl { ident: "a", expr: Expr::Num("1.") },
                line: 0,
            },
            Syntax {
                statement: Stmt::Decl { ident: "b", expr: Expr::Str("blah") },
                line: 1,
            },
            Syntax {
                statement: Stmt::Decl {
                    ident: "c",
                    expr: Expr::Bool("false")
                },
                line: 2,
            },
            Syntax {
                statement: Stmt::Decl { ident: "d", expr: Expr::Null },
                line: 3,
            },
            Syntax {
                statement: Stmt::Decl { ident: "e", expr: Expr::Undef },
                line: 4,
            },
            Syntax {
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
        vec![Syntax { statement: Stmt::Ret(Expr::Undef), line: 0 }],
    )
}

#[test]
fn return_object() {
    assert_ast!(
        "return {
             ab: null,
             cd: undefined
         };",
        vec![Syntax {
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
        vec![Syntax { statement: Stmt::Ret(Expr::Obj(Vec::new())), line: 0 }],
    )
}

#[test]
fn function_nothing() {
    assert_ast!(
        "function f() {}",
        vec![Syntax {
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
        vec![Syntax {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["x", "y"],
                body: vec![Syntax {
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
        vec![Syntax {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["a", "b", "c"],
                body: vec![
                    Syntax {
                        statement: Stmt::Decl {
                            ident: "d",
                            expr: Expr::Obj(vec![
                                Prop { key: "a", value: Expr::Ident("a") },
                                Prop { key: "b", value: Expr::Ident("b") },
                                Prop { key: "c", value: Expr::Ident("c") },
                            ]),
                        },
                        line: 1,
                    },
                    Syntax { statement: Stmt::Ret(Expr::Ident("d")), line: 6 },
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
            Syntax {
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
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ident("x")),
                        right: Box::new(Expr::Ident("a")),
                    }),
                    right: Box::new(Expr::Ident("b")),
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
        vec![Syntax {
            statement: Stmt::Decl {
                ident: "f",
                expr: Expr::Fn {
                    args: vec!["x"],
                    body: vec![Syntax {
                        statement: Stmt::Ret(Expr::Infix {
                            op: "+",
                            left: Box::new(Expr::Ident("x")),
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
        vec![Syntax {
            statement: Stmt::Assign {
                op: "=",
                ident: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ident("window")),
                    right: Box::new(Expr::Ident("onload")),
                },
                expr: Expr::Fn {
                    args: Vec::new(),
                    body: vec![
                        Syntax {
                            statement: Stmt::Decl {
                                ident: "a",
                                expr: Expr::Num("0.1"),
                            },
                            line: 1,
                        },
                        Syntax {
                            statement: Stmt::Decl {
                                ident: "b",
                                expr: Expr::Num("10"),
                            },
                            line: 2,
                        },
                        Syntax {
                            statement: Stmt::Ret(Expr::Infix {
                                op: "+",
                                left: Box::new(Expr::Ident("a")),
                                right: Box::new(Expr::Ident("b")),
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
            Syntax {
                statement: Stmt::Decl {
                    ident: "a",
                    expr: Expr::Prefix {
                        op: "!",
                        expr: Box::new(Expr::Bool("true")),
                    },
                },
                line: 0,
            },
            Syntax {
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
        vec![Syntax {
            statement: Stmt::Decl {
                ident: "x",
                expr: Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Infix {
                        op: "+",
                        left: Box::new(Expr::Ident("a")),
                        right: Box::new(Expr::Ident("b")),
                    }),
                    right: Box::new(Expr::Infix {
                        op: "+",
                        left: Box::new(Expr::Infix {
                            op: "+",
                            left: Box::new(Expr::Ident("c")),
                            right: Box::new(Expr::Ident("d")),
                        }),
                        right: Box::new(Expr::Ident("e")),
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
            Syntax {
                statement: Stmt::Effect(Expr::Postfix {
                    op: "++",
                    expr: Box::new(Expr::Ident("a")),
                }),
                line: 0,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Prefix {
                    op: "++",
                    expr: Box::new(Expr::Ident("b")),
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
            Syntax {
                statement: Stmt::Effect(Expr::Postfix {
                    op: "--",
                    expr: Box::new(Expr::Ident("a")),
                }),
                line: 0,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Prefix {
                    op: "--",
                    expr: Box::new(Expr::Ident("b")),
                }),
                line: 1,
            },
        ],
    )
}

#[test]
fn if_() {
    assert_ast!(
        "if (true) {
             return 0;
         }",
        vec![Syntax {
            statement: Stmt::Cond {
                condition: Expr::Bool("true"),
                if_: vec![Syntax {
                    statement: Stmt::Ret(Expr::Num("0")),
                    line: 1,
                }],
                else_: Vec::new(),
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
            Syntax {
                statement: Stmt::Decl { ident: "a", expr: Expr::Uninit },
                line: 0,
            },
            Syntax {
                statement: Stmt::Cond {
                    condition: Expr::Bool("true"),
                    if_: vec![Syntax {
                        statement: Stmt::Assign {
                            op: "=",
                            ident: Expr::Ident("a"),
                            expr: Expr::Num("0"),
                        },
                        line: 2,
                    }],
                    else_: vec![Syntax {
                        statement: Stmt::Assign {
                            op: "=",
                            ident: Expr::Ident("a"),
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
        vec![Syntax {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("f")),
                    args: vec![Expr::Ident("a")],
                }),
                args: vec![Expr::Ident("b")],
            }),
            line: 0,
        }],
    )
}

#[test]
fn function_calls_more_parens() {
    assert_ast!(
        "((f(a))(b));",
        vec![Syntax {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("f")),
                    args: vec![Expr::Ident("a")],
                }),
                args: vec![Expr::Ident("b")],
            }),
            line: 0,
        }],
    )
}

#[test]
fn function_calls_nested() {
    assert_ast!(
        "f(a(x)(y))(b);",
        vec![Syntax {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("f")),
                    args: vec![Expr::Call {
                        expr: Box::new(Expr::Call {
                            expr: Box::new(Expr::Ident("a")),
                            args: vec![Expr::Ident("x")],
                        }),
                        args: vec![Expr::Ident("y")],
                    }],
                }),
                args: vec![Expr::Ident("b")],
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
        vec![Syntax {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["a", "b", "c"],
                body: vec![
                    Syntax {
                        statement: Stmt::Decl {
                            ident: "d",
                            expr: Expr::Obj(vec![
                                Prop { key: "a", value: Expr::Ident("a") },
                                Prop { key: "b", value: Expr::Ident("b") },
                                Prop { key: "c", value: Expr::Ident("c") },
                            ]),
                        },
                        line: 2,
                    },
                    Syntax {
                        statement: Stmt::Ret(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ident("d")),
                            right: Box::new(Expr::Ident("a")),
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
            Syntax {
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
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Postfix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("x")),
                                right: Box::new(Expr::Ident("a")),
                            }),
                            right: Box::new(Expr::Ident("b")),
                        }),
                    }),
                    right: Box::new(Expr::Num(".01")),
                }),
                line: 8,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Prefix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("x")),
                                right: Box::new(Expr::Ident("a")),
                            }),
                            right: Box::new(Expr::Ident("b")),
                        }),
                    }),
                    right: Box::new(Expr::Num(".01")),
                }),
                line: 9,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Num(".01")),
                    right: Box::new(Expr::Postfix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("x")),
                                right: Box::new(Expr::Ident("a")),
                            }),
                            right: Box::new(Expr::Ident("b")),
                        }),
                    }),
                }),
                line: 11,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "+",
                    left: Box::new(Expr::Num(".01")),
                    right: Box::new(Expr::Prefix {
                        op: "++",
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("x")),
                                right: Box::new(Expr::Ident("a")),
                            }),
                            right: Box::new(Expr::Ident("b")),
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
        vec![Syntax {
            statement: Stmt::Fn {
                ident: "f",
                args: vec!["a", "b"],
                body: vec![Syntax {
                    statement: Stmt::Ret(Expr::Fn {
                        args: vec!["c"],
                        body: vec![Syntax {
                            statement: Stmt::Ret(Expr::Obj(vec![
                                Prop { key: "a", value: Expr::Ident("a") },
                                Prop { key: "b", value: Expr::Ident("b") },
                                Prop { key: "c", value: Expr::Ident("c") },
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
            Syntax {
                statement: Stmt::Decl { ident: "x", expr: Expr::Num("0") },
                line: 0,
            },
            Syntax {
                statement: Stmt::Decl { ident: "y", expr: Expr::Uninit },
                line: 1,
            },
            Syntax {
                statement: Stmt::Cond {
                    condition: Expr::Infix {
                        op: "===",
                        left: Box::new(Expr::Ident("x")),
                        right: Box::new(Expr::Num("0")),
                    },
                    if_: vec![Syntax {
                        statement: Stmt::Assign {
                            op: "=",
                            ident: Expr::Ident("y"),
                            expr: Expr::Num("0"),
                        },
                        line: 3,
                    }],
                    else_: vec![Syntax {
                        statement: Stmt::Cond {
                            condition: Expr::Infix {
                                op: "===",
                                left: Box::new(Expr::Ident("x")),
                                right: Box::new(Expr::Prefix {
                                    op: "-",
                                    expr: Box::new(Expr::Num("1")),
                                }),
                            },
                            if_: vec![Syntax {
                                statement: Stmt::Assign {
                                    op: "=",
                                    ident: Expr::Ident("y"),
                                    expr: Expr::Num("1"),
                                },
                                line: 5,
                            }],
                            else_: vec![Syntax {
                                statement: Stmt::Assign {
                                    op: "=",
                                    ident: Expr::Ident("y"),
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
            Syntax {
                statement: Stmt::Decl { ident: "x", expr: Expr::Bool("true") },
                line: 0,
            },
            Syntax {
                statement: Stmt::Switch {
                    expr: Expr::Ident("x"),
                    cases: vec![
                        Case {
                            expr: Expr::Bool("true"),
                            body: vec![
                                Syntax {
                                    statement: Stmt::Effect(Expr::Call {
                                        expr: Box::new(Expr::Infix {
                                            op: ".",
                                            left: Box::new(Expr::Ident(
                                                "console",
                                            )),
                                            right: Box::new(Expr::Ident(
                                                "log",
                                            )),
                                        }),
                                        args: vec![Expr::Str("true")],
                                    }),
                                    line: 3,
                                },
                                Syntax { statement: Stmt::Break, line: 4 },
                            ],
                        },
                        Case {
                            expr: Expr::Bool("false"),
                            body: vec![
                                Syntax {
                                    statement: Stmt::Effect(Expr::Call {
                                        expr: Box::new(Expr::Infix {
                                            op: ".",
                                            left: Box::new(Expr::Ident(
                                                "console",
                                            )),
                                            right: Box::new(Expr::Ident(
                                                "log",
                                            )),
                                        }),
                                        args: vec![Expr::Str("false")],
                                    }),
                                    line: 7,
                                },
                                Syntax { statement: Stmt::Break, line: 8 },
                            ],
                        },
                    ],
                    default: vec![Syntax {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("console")),
                                right: Box::new(Expr::Ident("log")),
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
            Syntax {
                statement: Stmt::Decl { ident: "x", expr: Expr::Num("0") },
                line: 0,
            },
            Syntax {
                statement: Stmt::Decl { ident: "y", expr: Expr::Uninit },
                line: 1,
            },
            Syntax {
                statement: Stmt::Decl { ident: "a", expr: Expr::Num("0") },
                line: 2,
            },
            Syntax {
                statement: Stmt::Decl { ident: "b", expr: Expr::Num("1") },
                line: 3,
            },
            Syntax {
                statement: Stmt::Switch {
                    expr: Expr::Ident("x"),
                    cases: vec![
                        Case {
                            expr: Expr::Ident("a"),
                            body: vec![
                                Syntax {
                                    statement: Stmt::Assign {
                                        op: "=",
                                        ident: Expr::Ident("y"),
                                        expr: Expr::Str("0"),
                                    },
                                    line: 6,
                                },
                                Syntax { statement: Stmt::Break, line: 7 },
                            ],
                        },
                        Case {
                            expr: Expr::Ident("b"),
                            body: vec![
                                Syntax {
                                    statement: Stmt::Assign {
                                        op: "=",
                                        ident: Expr::Ident("y"),
                                        expr: Expr::Str("1"),
                                    },
                                    line: 10,
                                },
                                Syntax { statement: Stmt::Break, line: 11 },
                            ],
                        },
                    ],
                    default: vec![Syntax {
                        statement: Stmt::Assign {
                            op: "=",
                            ident: Expr::Ident("y"),
                            expr: Expr::Undef,
                        },
                        line: 14,
                    }],
                },
                line: 4,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Call {
                    expr: Box::new(Expr::Infix {
                        op: ".",
                        left: Box::new(Expr::Ident("console")),
                        right: Box::new(Expr::Ident("log")),
                    }),
                    args: vec![Expr::Ident("y")],
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
        vec![Syntax {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ident("console")),
                    right: Box::new(Expr::Ident("log")),
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
        vec![Syntax {
            statement: Stmt::Scope(vec![Syntax {
                statement: Stmt::Scope(vec![Syntax {
                    statement: Stmt::Decl { ident: "x", expr: Expr::Null },
                    line: 2,
                }]),
                line: 1,
            }]),
            line: 0,
        }],
    )
}

#[test]
fn ternary_operator() {
    assert_ast!(
        "var x = y === 0 ? 0 : 1;",
        vec![Syntax {
            statement: Stmt::Decl {
                ident: "x",
                expr: Expr::Ternary {
                    condition: Box::new(Expr::Infix {
                        op: "===",
                        left: Box::new(Expr::Ident("y")),
                        right: Box::new(Expr::Num("0")),
                    }),
                    if_: Box::new(Expr::Num("0")),
                    else_: Box::new(Expr::Num("1")),
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
        vec![Syntax {
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
        vec![Syntax {
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
        vec![Syntax {
            statement: Stmt::Effect(Expr::Prefix {
                op: "!",
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("f")),
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
        vec![Syntax {
            statement: Stmt::Effect(Expr::Prefix {
                op: "new",
                expr: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("Uint8Array")),
                    args: vec![
                        Expr::Ident("buffer"),
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
        vec![Syntax {
            statement: Stmt::Effect(Expr::Infix {
                op: "+",
                left: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("f")),
                    args: vec![Expr::Call {
                        expr: Box::new(Expr::Ident("g")),
                        args: Vec::new(),
                    }],
                }),
                right: Box::new(Expr::Call {
                    expr: Box::new(Expr::Ident("g")),
                    args: vec![Expr::Call {
                        expr: Box::new(Expr::Ident("f")),
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
        vec![Syntax {
            statement: Stmt::Effect(Expr::Call {
                expr: Box::new(Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Call {
                                expr: Box::new(Expr::Ident("f")),
                                args: Vec::new(),
                            }),
                            right: Box::new(Expr::Ident("x")),
                        }),
                        args: Vec::new(),
                    }),
                    right: Box::new(Expr::Ident("y")),
                }),
                args: vec![Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Call {
                        expr: Box::new(Expr::Ident("g")),
                        args: Vec::new(),
                    }),
                    right: Box::new(Expr::Ident("z")),
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
        vec![Syntax {
            statement: Stmt::Assign {
                op: "=",
                ident: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Ident("window")),
                    right: Box::new(Expr::Ident("onload")),
                },
                expr: Expr::Fn {
                    args: Vec::new(),
                    body: vec![Syntax {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Call {
                                    expr: Box::new(Expr::Infix {
                                        op: ".",
                                        left: Box::new(Expr::Ident("module")),
                                        right: Box::new(Expr::Ident("init")),
                                    }),
                                    args: vec![Expr::Call {
                                        expr: Box::new(Expr::Ident("fetch")),
                                        args: vec![Expr::Str("./source")],
                                    }],
                                }),
                                right: Box::new(Expr::Ident("then")),
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
        vec![Syntax {
            statement: Stmt::Assign {
                op: "=",
                ident: Expr::Infix {
                    op: ".",
                    left: Box::new(Expr::Access {
                        expr: Box::new(Expr::Ident("array")),
                        index: Box::new(Expr::Num("0")),
                    }),
                    right: Box::new(Expr::Ident("x")),
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
            Syntax {
                statement: Stmt::Effect(Expr::Prefix {
                    op: "~",
                    expr: Box::new(Expr::Num("0")),
                }),
                line: 0,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "&",
                    left: Box::new(Expr::Num("1")),
                    right: Box::new(Expr::Num("1")),
                }),
                line: 1,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "|",
                    left: Box::new(Expr::Num("2")),
                    right: Box::new(Expr::Num("2")),
                }),
                line: 2,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "^",
                    left: Box::new(Expr::Num("3")),
                    right: Box::new(Expr::Num("3")),
                }),
                line: 3,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: "<<",
                    left: Box::new(Expr::Num("4")),
                    right: Box::new(Expr::Num("4")),
                }),
                line: 4,
            },
            Syntax {
                statement: Stmt::Effect(Expr::Infix {
                    op: ">>",
                    left: Box::new(Expr::Num("5")),
                    right: Box::new(Expr::Num("5")),
                }),
                line: 5,
            },
            Syntax {
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
            Syntax {
                statement: Stmt::Assign {
                    op: "+=",
                    ident: Expr::Ident("a"),
                    expr: Expr::Num("1"),
                },
                line: 0,
            },
            Syntax {
                statement: Stmt::Assign {
                    op: "-=",
                    ident: Expr::Ident("b"),
                    expr: Expr::Num("1"),
                },
                line: 1,
            },
            Syntax {
                statement: Stmt::Assign {
                    op: "*=",
                    ident: Expr::Ident("c"),
                    expr: Expr::Num("2"),
                },
                line: 2,
            },
            Syntax {
                statement: Stmt::Assign {
                    op: "/=",
                    ident: Expr::Ident("d"),
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
            Syntax {
                statement: Stmt::Decl { ident: "i", expr: Expr::Num("0") },
                line: 0,
            },
            Syntax {
                statement: Stmt::While {
                    condition: Expr::Infix {
                        op: "<",
                        left: Box::new(Expr::Ident("i")),
                        right: Box::new(Expr::Num("10")),
                    },
                    body: vec![Syntax {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("console")),
                                right: Box::new(Expr::Ident("log")),
                            }),
                            args: vec![Expr::Postfix {
                                op: "++",
                                expr: Box::new(Expr::Ident("i")),
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
        vec![Syntax {
            statement: Stmt::For {
                init: Some(Box::new(Syntax {
                    statement: Stmt::Decl { ident: "i", expr: Expr::Num("0") },
                    line: 0,
                })),
                condition: Some(Expr::Infix {
                    op: "<",
                    left: Box::new(Expr::Ident("i")),
                    right: Box::new(Expr::Num("10")),
                }),
                update: Some(Box::new(Syntax {
                    statement: Stmt::Effect(Expr::Prefix {
                        op: "++",
                        expr: Box::new(Expr::Ident("i")),
                    }),
                    line: 0,
                })),
                body: vec![Syntax {
                    statement: Stmt::Effect(Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ident("console")),
                            right: Box::new(Expr::Ident("log")),
                        }),
                        args: vec![Expr::Ident("i")],
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
        vec![Syntax {
            statement: Stmt::For {
                init: None,
                condition: None,
                update: None,
                body: vec![Syntax {
                    statement: Stmt::Effect(Expr::Call {
                        expr: Box::new(Expr::Infix {
                            op: ".",
                            left: Box::new(Expr::Ident("console")),
                            right: Box::new(Expr::Ident("log")),
                        }),
                        args: vec![Expr::Ident("i")],
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
            Syntax {
                statement: Stmt::Decl { ident: "i", expr: Expr::Uninit },
                line: 0,
            },
            Syntax {
                statement: Stmt::For {
                    init: Some(Box::new(Syntax {
                        statement: Stmt::Assign {
                            op: "=",
                            ident: Expr::Ident("i"),
                            expr: Expr::Num("0"),
                        },
                        line: 1,
                    })),
                    condition: Some(Expr::Infix {
                        op: "<",
                        left: Box::new(Expr::Ident("i")),
                        right: Box::new(Expr::Num("10")),
                    }),
                    update: Some(Box::new(Syntax {
                        statement: Stmt::Assign {
                            op: "+=",
                            ident: Expr::Ident("i"),
                            expr: Expr::Num("2"),
                        },
                        line: 1,
                    })),
                    body: vec![Syntax {
                        statement: Stmt::Effect(Expr::Call {
                            expr: Box::new(Expr::Infix {
                                op: ".",
                                left: Box::new(Expr::Ident("console")),
                                right: Box::new(Expr::Ident("log")),
                            }),
                            args: vec![Expr::Ident("i")],
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
fn array_literal_empty() {
    assert_ast!(
        "[];",
        vec![Syntax {
            statement: Stmt::Effect(Expr::Array(Vec::new())),
            line: 0,
        }],
    )
}

#[test]
fn array_literal() {
    assert_ast!(
        "[1, 2, 3];",
        vec![Syntax {
            statement: Stmt::Effect(Expr::Array(vec![
                Expr::Num("1"),
                Expr::Num("2"),
                Expr::Num("3"),
            ])),
            line: 0,
        }],
    )
}

#[test]
fn scoped_array_access() {
    assert_ast!(
        "// ...
         {
             x[2] = 1;
             x[3] = 2;
         }",
        vec![Syntax {
            statement: Stmt::Scope(vec![
                Syntax {
                    statement: Stmt::Assign {
                        op: "=",
                        ident: Expr::Access {
                            expr: Box::new(Expr::Ident("x")),
                            index: Box::new(Expr::Num("2")),
                        },
                        expr: Expr::Num("1"),
                    },
                    line: 2,
                },
                Syntax {
                    statement: Stmt::Assign {
                        op: "=",
                        ident: Expr::Access {
                            expr: Box::new(Expr::Ident("x")),
                            index: Box::new(Expr::Num("3")),
                        },
                        expr: Expr::Num("2"),
                    },
                    line: 3,
                },
            ]),
            line: 1,
        }],
    )
}

#[test]
fn multiple_declares() {
    assert_ast!(
        "var x, y, z;",
        vec![Syntax { statement: Stmt::Decls(vec!["x", "y", "z"]), line: 0 }],
    )
}
