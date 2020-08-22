use super::{
    get_types, Error, Table, Type, DUPLICATE_KEYS, SHADOW_IDENT, UNKNOWN_IDENT,
};
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
        Ok(Table {
            types: vec![
                Type::Str,
                Type::Num,
                Type::Bool,
                Type::Null,
                Type::Undef,
            ],
            indices: vec![
                (vec!["a"], 0),
                (vec!["b"], 1),
                (vec!["c"], 2),
                (vec!["d"], 3),
                (vec!["e"], 4),
            ]
            .into_iter()
            .collect(),
        }),
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
            message: SHADOW_IDENT,
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
            message: UNKNOWN_IDENT,
        }),
    )
}

#[test]
fn declare_ident() {
    assert_types!(
        "var x = 0;
         var y = x;",
        Ok(Table {
            types: vec![Type::Num],
            indices: vec![(vec!["x"], 0), (vec!["y"], 0)]
                .into_iter()
                .collect(),
        }),
    )
}

#[test]
fn declare_object() {
    assert_types!(
        "var x = 0;
         var y = { a: x };",
        Ok(Table {
            types: vec![
                Type::Num,
                Type::Obj(Rc::new(
                    vec![("a", Type::Num)].into_iter().collect()
                )),
            ],
            indices: vec![(vec!["x"], 0), (vec!["y", "a"], 0), (vec!["y"], 1)]
                .into_iter()
                .collect(),
        }),
    )
}

#[test]
fn declare_empty_object() {
    assert_types!(
        "var x = {};",
        Ok(Table {
            types: vec![Type::Obj(Rc::new(BTreeMap::new()))],
            indices: vec![(vec!["x"], 0)].into_iter().collect(),
        }),
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
        Ok(Table {
            types: vec![
                Type::Num,
                Type::Str,
                Type::Bool,
                Type::Null,
                Type::Undef,
                Type::Num,
                Type::Obj(props.clone()),
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
            ],
            indices: vec![
                (vec!["x", "a"], 0),
                (vec!["x", "b"], 1),
                (vec!["x", "c"], 2),
                (vec!["x", "d"], 3),
                (vec!["x", "e"], 4),
                (vec!["x", "f", "a"], 5),
                (vec!["x", "f"], 6),
                (vec!["x"], 7),
            ]
            .into_iter()
            .collect(),
        }),
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
            message: DUPLICATE_KEYS,
        }),
    )
}
