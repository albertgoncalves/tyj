use super::{get_types, Error, Message, Type};
use crate::parser::{get_ast, Expr, Prop, Stmt, Syntax};
use crate::tokenizer::get_tokens;
use std::collections::BTreeMap;
use std::rc::Rc;

macro_rules! assert_types {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_types(&get_ast(&get_tokens($a)).unwrap()), $b)
    };
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
            (vec!["a"], Type::Str),
            (vec!["b"], Type::Num),
            (vec!["c"], Type::Bool),
            (vec!["d"], Type::Null),
            (vec!["e"], Type::Undef),
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
        Ok(vec![(vec!["x"], Type::Num), (vec!["y"], Type::Num)]
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
            (vec!["x"], Type::Num),
            (vec!["y", "a"], Type::Num),
            (
                vec!["y"],
                Type::Obj(Rc::new(
                    vec![("a", Type::Num)].into_iter().collect()
                )),
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
        Ok(vec![(vec!["x"], Type::Obj(Rc::new(BTreeMap::new())))]
            .into_iter()
            .collect()),
    )
}

#[test]
fn declare_nested_object() {
    let props: Rc<BTreeMap<&str, Type>> =
        Rc::new(vec![("a", Type::Num)].into_iter().collect());
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
            (vec!["x", "a"], Type::Num),
            (vec!["x", "b"], Type::Str),
            (vec!["x", "c"], Type::Bool),
            (vec!["x", "d"], Type::Null),
            (vec!["x", "e"], Type::Undef),
            (vec!["x", "f", "a"], Type::Num),
            (vec!["x", "f"], Type::Obj(props.clone())),
            (
                vec!["x"],
                Type::Obj(Rc::new(
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
                )),
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
        Ok(vec![(vec!["x"], Type::Array(Rc::new(Type::Num)))]
            .into_iter()
            .collect()),
    )
}

#[test]
fn declare_array_empty() {
    assert_types!(
        "var x = [];",
        Ok(vec![(vec!["x"], Type::EmptyArray)].into_iter().collect()),
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
            (vec!["a", "x"], Type::Num),
            (
                vec!["a"],
                Type::Obj(Rc::new(
                    vec![("x", Type::Num)].into_iter().collect()
                )),
            ),
            (vec!["b", "x"], Type::Num),
            (
                vec!["b"],
                Type::Obj(Rc::new(
                    vec![("x", Type::Num)].into_iter().collect()
                )),
            ),
            (
                vec!["xs"],
                Type::Array(Rc::new(Type::Obj(Rc::new(
                    vec![("x", Type::Num)].into_iter().collect()
                )))),
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
