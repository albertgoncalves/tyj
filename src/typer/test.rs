use super::{get_types, Error, Message};
use crate::commenter::get_sigs;
use crate::parser::{get_ast, Expr, Prop, Stmt, Syntax};
use crate::tokenizer::{get_tokens, Asn, Op};
use crate::types::{Target, Type};
use std::collections::{BTreeMap, HashMap};

macro_rules! assert_types {
    ($a:expr, $b:expr $(,)?) => {{
        let (ast, comments): (Vec<Syntax>, Vec<&str>) =
            get_ast(&get_tokens($a)).unwrap();
        let mut sigs: HashMap<Target, Type> = get_sigs(&comments).unwrap();
        assert_eq!(get_types(&ast, &mut sigs), $b)
    }};
}

#[test]
fn declares() {
    assert_types!(
        "var a = \"?\";
         var b = 0;
         var c = true;
         var d = null;
         var e = undefined;",
        Ok(vec![
            (Target { ident: vec!["a"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["b"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["c"], scope: Vec::new() }, Type::Bool),
            (Target { ident: vec!["d"], scope: Vec::new() }, Type::Null),
            (Target { ident: vec!["e"], scope: Vec::new() }, Type::Undef),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_shadow_ident() {
    assert_types!(
        "var x = true;
         var x = false;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Bool("false"),
                },
                line: 1,
            },
            message: Message::IdentShadow,
        }),
    )
}

#[test]
fn declare_unknown_ident() {
    assert_types!(
        "var y = x;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl { ident: "y", expr: Expr::Ident("x") },
                line: 0,
            },
            message: Message::IdentUnknown,
        }),
    )
}

#[test]
fn declare_ident() {
    assert_types!(
        "var x = 0;
         var y = x;",
        Ok(vec![
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Num),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_object() {
    assert_types!(
        "var x = 0;
         var y = { a: x };",
        Ok(vec![
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["y", "a"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["y"], scope: Vec::new() },
                Type::Obj(vec![("a", Type::Num)].into_iter().collect()),
            ),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_empty_object() {
    assert_types!(
        "var x = {};",
        Ok(vec![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Obj(BTreeMap::new()),
        )]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_nested_object() {
    let props: BTreeMap<&str, Type> =
        vec![("a", Type::Num)].into_iter().collect();
    assert_types!(
        "var x = {
             a: 0,
             b: \"?\",
             c: true,
             d: null,
             e: undefined,
             f: {
                 a: 0,
             },
         };",
        Ok(vec![
            (Target { ident: vec!["x", "a"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["x", "b"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["x", "c"], scope: Vec::new() }, Type::Bool),
            (Target { ident: vec!["x", "d"], scope: Vec::new() }, Type::Null),
            (Target { ident: vec!["x", "e"], scope: Vec::new() }, Type::Undef),
            (
                Target { ident: vec!["x", "f", "a"], scope: Vec::new() },
                Type::Num,
            ),
            (
                Target { ident: vec!["x", "f"], scope: Vec::new() },
                Type::Obj(props.clone()),
            ),
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(
                    vec![
                        ("a", Type::Num),
                        ("b", Type::Str),
                        ("c", Type::Bool),
                        ("d", Type::Null),
                        ("e", Type::Undef),
                        ("f", Type::Obj(props)),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_object_duplicate_keys() {
    assert_types!(
        "var x = {
             a: true,
             a: false,
         };",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Obj(vec![
                        Prop { key: "a", value: Expr::Bool("true") },
                        Prop { key: "a", value: Expr::Bool("false") },
                    ]),
                },
                line: 0,
            },
            message: Message::ObjDuplicateKeys,
        }),
    )
}

#[test]
fn declare_array() {
    assert_types!(
        "var x = [0, 1, 2, 3];",
        Ok(vec![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Array(Box::new(Type::Num)),
        )]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_array_empty() {
    assert_types!(
        "var x = [];",
        Ok(vec![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::EmptyArray,
        )]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_array_obj() {
    assert_types!(
        "var a = {
             x: 0,
         };
         var b = {
             x: 1,
         };
         var xs = [a, b];",
        Ok(vec![
            (Target { ident: vec!["a", "x"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["a"], scope: Vec::new() },
                Type::Obj(vec![("x", Type::Num)].into_iter().collect()),
            ),
            (Target { ident: vec!["b", "x"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["b"], scope: Vec::new() },
                Type::Obj(vec![("x", Type::Num)].into_iter().collect()),
            ),
            (
                Target { ident: vec!["xs"], scope: Vec::new() },
                Type::Array(Box::new(Type::Obj(
                    vec![("x", Type::Num)].into_iter().collect(),
                ))),
            ),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_array_err() {
    assert_types!(
        "var x = [0, \"0\"];",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Array(vec![Expr::Num("0"), Expr::Str("0")]),
                },
                line: 0,
            },
            message: Message::ArrayMultiType,
        }),
    )
}

#[test]
fn declare_uninit_ident_err() {
    assert_types!(
        "var x;
         var y = x;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl { ident: "y", expr: Expr::Ident("x") },
                line: 1,
            },
            message: Message::IdentUninit,
        }),
    )
}

#[test]
fn declare_prefix() {
    assert_types!(
        "var a = !false;
         var b = -0;
         var c = ~0;
         var d = ++b;
         var e = --c;",
        Ok(vec![
            (Target { ident: vec!["a"], scope: Vec::new() }, Type::Bool),
            (Target { ident: vec!["b"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["c"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["d"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["e"], scope: Vec::new() }, Type::Num),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_prefix_err() {
    assert_types!(
        "var a = null;
         var b = !a;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "b",
                    expr: Expr::Prefix {
                        op: Op::Not,
                        expr: Box::new(Expr::Ident("a")),
                    }
                },
                line: 1,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn declare_infix_member() {
    let props: BTreeMap<&str, Type> =
        vec![("y", Type::Num)].into_iter().collect();
    assert_types!(
        "var a = {
             x: {
                 y: 0,
             },
         };
         var x = null;
         var y = \"\";
         var b = a.x.y;",
        Ok(vec![
            (
                Target { ident: vec!["a", "x", "y"], scope: Vec::new() },
                Type::Num,
            ),
            (
                Target { ident: vec!["a", "x"], scope: Vec::new() },
                Type::Obj(props.clone()),
            ),
            (
                Target { ident: vec!["a"], scope: Vec::new() },
                Type::Obj(vec![("x", Type::Obj(props))].into_iter().collect()),
            ),
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Null),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["b"], scope: Vec::new() }, Type::Num),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_infix_member_err() {
    assert_types!(
        "var a = {
             x: {
                 y: 0,
             },
         };
         var b = a.\"x\".y;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "b",
                    expr: Expr::Infix {
                        op: Op::Member,
                        left: Box::new(Expr::Infix {
                            op: Op::Member,
                            left: Box::new(Expr::Ident("a")),
                            right: Box::new(Expr::Str("x")),
                        }),
                        right: Box::new(Expr::Ident("y")),
                    },
                },
                line: 5,
            },
            message: Message::NonIdentMember,
        }),
    )
}

#[test]
fn declare_update() {
    assert_types!(
        "var x = 0;
         x = 1;",
        Ok(vec![(Target { ident: vec!["x"], scope: Vec::new() }, Type::Num)]
            .into_iter()
            .collect()),
    )
}

#[test]
fn declare_update_err() {
    assert_types!(
        "var x = 0;
         x = \"?\";",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Ident("x"),
                    expr: Expr::Str("?"),
                },
                line: 1,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn declare_uninit() {
    assert_types!(
        "var x;
         x = 1;
         x = 2;",
        Ok(vec![(Target { ident: vec!["x"], scope: Vec::new() }, Type::Num)]
            .into_iter()
            .collect()),
    )
}

#[test]
fn declare_uninit_err() {
    assert_types!(
        "var x;
         x = \"?\";
         x = 2;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Ident("x"),
                    expr: Expr::Num("2"),
                },
                line: 2,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn assign_err() {
    assert_types!(
        "var x;
         \"x\" = 0;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Str("x"),
                    expr: Expr::Num("0"),
                },
                line: 1,
            },
            message: Message::AssignNonIdent,
        }),
    )
}

#[test]
fn assign_obj() {
    assert_types!(
        "var x = { a: 0 };
         var y = 1;
         x.a = y;",
        Ok(vec![
            (Target { ident: vec!["x", "a"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(vec![("a", Type::Num)].into_iter().collect()),
            ),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Num),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn assign_obj_type_err() {
    assert_types!(
        "var x = { a: 0 };
         x.a = \"0\";",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Infix {
                        op: Op::Member,
                        left: Box::new(Expr::Ident("x")),
                        right: Box::new(Expr::Ident("a")),
                    },
                    expr: Expr::Str("0"),
                },
                line: 1,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn assign_obj_key_err() {
    assert_types!(
        "var x = { a: 0 };
         x.b = 1;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Infix {
                        op: Op::Member,
                        left: Box::new(Expr::Ident("x")),
                        right: Box::new(Expr::Ident("b")),
                    },
                    expr: Expr::Num("1"),
                },
                line: 1,
            },
            message: Message::IdentUnknown,
        }),
    )
}

#[test]
fn assign_uninit_ident_err() {
    assert_types!(
        "var x;
         var y;
         y = x;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Ident("y"),
                    expr: Expr::Ident("x"),
                },
                line: 2,
            },
            message: Message::IdentUninit,
        }),
    )
}

#[test]
fn access() {
    assert_types!(
        "var xs = [1];
         var x = xs[0];",
        Ok(vec![
            (
                Target { ident: vec!["xs"], scope: Vec::new() },
                Type::Array(Box::new(Type::Num)),
            ),
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Num),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn access_non_array_err() {
    assert_types!(
        "var xs = \"[1]\";
         var x = xs[0];",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Access {
                        expr: Box::new(Expr::Ident("xs")),
                        index: Box::new(Expr::Num("0")),
                    },
                },
                line: 1,
            },
            message: Message::AccessNonArray,
        }),
    )
}

#[test]
fn access_non_index_err() {
    assert_types!(
        "var xs = [1];
         var x = xs[\"0\"];",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Access {
                        expr: Box::new(Expr::Ident("xs")),
                        index: Box::new(Expr::Str("0")),
                    },
                },
                line: 1,
            },
            message: Message::AccessNonIndex,
        }),
    )
}

#[test]
fn declare_functions() {
    let obj: Type = Type::Obj(vec![("a", Type::Bool)].into_iter().collect());
    assert_types!(
        "/* x { a: bool }
          * f(number, string) -> number
          * f(string, bool) -> string
          * f(string, null) -> string
          * f(x, undefined) -> x
          * g(number) -> number
          * g(string) -> number
          * g(x) -> number
          */

         var x = \"???\";
         var y = null;

         function f(x, _) {
             return x;
         }

         function g(_) {
             var y = 0;
             return y;
         }

         /* h(x) -> bool */
         function h(x) {
             return x.a;
         }

         // i(number) -> undefined
         // i(string) -> undefined
         // i(null) -> undefined
         // i(bool) -> undefined
         // i(undefined) -> undefined
         function i(_) {}",
        Ok(vec![
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Null),
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(
                    vec![
                        (vec![Type::Num, Type::Str], Type::Num),
                        (vec![Type::Str, Type::Bool], Type::Str),
                        (vec![Type::Str, Type::Null], Type::Str),
                        (vec![obj.clone(), Type::Undef], obj.clone()),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
            (
                Target { ident: vec!["g"], scope: Vec::new() },
                Type::Fn(
                    vec![
                        (vec![Type::Num], Type::Num),
                        (vec![Type::Str], Type::Num),
                        (vec![obj.clone()], Type::Num),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
            (
                Target { ident: vec!["h"], scope: Vec::new() },
                Type::Fn(vec![(vec![obj], Type::Bool)].into_iter().collect()),
            ),
            (
                Target { ident: vec!["i"], scope: Vec::new() },
                Type::Fn(
                    vec![
                        (vec![Type::Num], Type::Undef),
                        (vec![Type::Str], Type::Undef),
                        (vec![Type::Null], Type::Undef),
                        (vec![Type::Bool], Type::Undef),
                        (vec![Type::Undef], Type::Undef),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
        ]
        .into_iter()
        .collect()),
    )
}

#[test]
fn declare_scoped_function() {
    let obj: Type = Type::Obj(vec![("a", Type::Num)].into_iter().collect());
    assert_types!(
        "/* x {
          *     a: number,
          * }
          * f(x) -> number
          * h(x) -> undefined
          * h @ g() -> undefined
          * h @ g @ f() -> number
          */

         function f(x) {
             return x.a;
         }

         function h(x) {
             function g() {
                 function f() {
                     return x.a;
                 }
             }
         }",
        Ok(vec![
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(
                    vec![(vec![obj.clone()], Type::Num),]
                        .into_iter()
                        .collect(),
                ),
            ),
            (
                Target { ident: vec!["h"], scope: Vec::new() },
                Type::Fn(
                    vec![(vec![obj.clone()], Type::Undef)]
                        .into_iter()
                        .collect(),
                ),
            ),
            (
                Target { ident: vec!["g"], scope: vec!["h"] },
                Type::Fn(
                    vec![(Vec::new(), Type::Undef)].into_iter().collect(),
                ),
            ),
            (
                Target { ident: vec!["f"], scope: vec!["h", "g"] },
                Type::Fn(vec![(Vec::new(), Type::Num)].into_iter().collect()),
            ),
        ]
        .into_iter()
        .collect()),
    )
}
