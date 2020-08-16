use super::{get_types, Key, Type, Value};
use crate::parser::get_ast;
use crate::tokenizer::get_tokens;
use std::collections::{BTreeMap, BTreeSet, HashMap};

macro_rules! array {
    ($items:expr $(,)?) => {
        Type::Array($items.into_iter().collect())
    };
}

macro_rules! obj {
    ($pairs:expr $(,)?) => {
        Type::Obj($pairs.into_iter().collect())
    };
}

macro_rules! assert_types {
    ($a:expr, $b:expr $(,)?) => {{
        assert_eq!(
            get_types(&get_ast(&get_tokens($a))),
            $b.into_iter().collect(),
        );
    }};
}

#[test]
fn declares() {
    assert_types!(
        "var a = 0;
         var b = \"Hello, world!\";
         var c = true;
         var d = null;
         var e = undefined;
         var f = {};
         var g = {
             _a: 0,
             _b: \"Hello, world!\",
             _c: true,
             _d: null,
             _e: undefined,
             _f: {},
             _g: { __a: 0 },
         };
         var h = [];
         var i = [1, 2, 3];
         var j = [\"a\", \"b\", \"c\"];
         var k = [true, false];
         var l = [null, null];
         var m = [undefined, undefined];",
        vec![
            (
                Key::Var("a"),
                Value { scope: Vec::new(), type_: Type::Num, line: 0 },
            ),
            (
                Key::Var("b"),
                Value { scope: Vec::new(), type_: Type::Str, line: 1 },
            ),
            (
                Key::Var("c"),
                Value { scope: Vec::new(), type_: Type::Bool, line: 2 },
            ),
            (
                Key::Var("d"),
                Value { scope: Vec::new(), type_: Type::Null, line: 3 },
            ),
            (
                Key::Var("e"),
                Value { scope: Vec::new(), type_: Type::Undef, line: 4 },
            ),
            (
                Key::Var("f"),
                Value {
                    scope: Vec::new(),
                    type_: Type::Obj(BTreeMap::new()),
                    line: 5,
                },
            ),
            (
                Key::Var("g"),
                Value {
                    scope: Vec::new(),
                    type_: obj!(vec![
                        ("_a", Type::Num),
                        ("_b", Type::Str),
                        ("_c", Type::Bool),
                        ("_d", Type::Null),
                        ("_e", Type::Undef),
                        ("_f", Type::Obj(BTreeMap::new())),
                        ("_g", obj!(vec![("__a", Type::Num)])),
                    ]),
                    line: 6,
                },
            ),
            (
                Key::Var("h"),
                Value {
                    scope: Vec::new(),
                    type_: Type::Array(BTreeSet::new()),
                    line: 15,
                },
            ),
            (
                Key::Var("i"),
                Value {
                    scope: Vec::new(),
                    type_: array!(vec![Type::Num]),
                    line: 16,
                },
            ),
            (
                Key::Var("j"),
                Value {
                    scope: Vec::new(),
                    type_: array!(vec![Type::Str]),
                    line: 17,
                },
            ),
            (
                Key::Var("k"),
                Value {
                    scope: Vec::new(),
                    type_: array!(vec![Type::Bool]),
                    line: 18,
                },
            ),
            (
                Key::Var("l"),
                Value {
                    scope: Vec::new(),
                    type_: array!(vec![Type::Null]),
                    line: 19,
                },
            ),
            (
                Key::Var("m"),
                Value {
                    scope: Vec::new(),
                    type_: array!(vec![Type::Undef]),
                    line: 20,
                },
            ),
        ],
    )
}

#[test]
#[should_panic]
fn assign_without_declare() {
    let _: HashMap<Key, Value> =
        get_types(&get_ast(&get_tokens("x = \"?\";")));
}

#[test]
#[should_panic]
fn declare_assign_unmatched_type() {
    let _: HashMap<Key, Value> = get_types(&get_ast(&get_tokens(
        "var x = 0;
         x = \"?\";",
    )));
}

#[test]
#[should_panic]
fn declare_assign_undeclared_key() {
    let _: HashMap<Key, Value> = get_types(&get_ast(&get_tokens(
        "var x = {};
         x = {
             a: 0,
         };",
    )));
}

#[test]
fn declare_assign() {
    assert_types!(
        "var x = {
             a: 0,
         };
         x = {
             a: 1,
         };",
        vec![(
            Key::Var("x"),
            Value {
                scope: Vec::new(),
                type_: obj!(vec![("a", Type::Num)]),
                line: 0,
            },
        )],
    )
}

#[test]
fn declare_assign_object() {
    assert_types!(
        "var x = {
             a: 0,
             b: \"\",
         };
         x.a = 1;
         x.b = \"?\";",
        vec![(
            Key::Var("x"),
            Value {
                scope: Vec::new(),
                type_: obj!(vec![("a", Type::Num), ("b", Type::Str)]),
                line: 0,
            },
        )],
    )
}

#[test]
fn declare_assign_nested_objects() {
    assert_types!(
        "var x = {
             a: {
                 b: true,
             },
         };
         x.a = {
             b: false,
         };",
        vec![(
            Key::Var("x"),
            Value {
                scope: Vec::new(),
                type_: obj!(vec![("a", obj!(vec![("b", Type::Bool)]))]),
                line: 0,
            },
        )],
    )
}

#[test]
#[should_panic]
fn declare_assign_object_unmatched_type() {
    let _: HashMap<Key, Value> = get_types(&get_ast(&get_tokens(
        "var x = {
             a: 0,
             b: \"\",
         };
         x.a = 1;
         x.b = 2;",
    )));
}

#[test]
fn declare_assign_ref() {
    assert_types!(
        "var x = {
             a: 0,
         };
         var y = 1;
         x.a = y;",
        vec![
            (
                Key::Var("x"),
                Value {
                    scope: Vec::new(),
                    type_: obj!(vec![("a", Type::Num)]),
                    line: 0,
                },
            ),
            (
                Key::Var("y"),
                Value { scope: Vec::new(), type_: Type::Num, line: 3 },
            ),
        ],
    )
}

#[test]
fn declare_assign_object_ref() {
    assert_types!(
        "var x = 0;
         var y = {
             a: 1,
         };
         x = y.a;",
        vec![
            (
                Key::Var("x"),
                Value { scope: Vec::new(), type_: Type::Num, line: 0 },
            ),
            (
                Key::Var("y"),
                Value {
                    scope: Vec::new(),
                    type_: obj!(vec![("a", Type::Num)]),
                    line: 1,
                },
            ),
        ],
    )
}

#[test]
fn declare_assign_prop_to_prop() {
    assert_types!(
        "var x = {
             a: true,
         };
         var y = {
             a: false,
         };
         x.a = y.a;",
        vec![
            (
                Key::Var("x"),
                Value {
                    scope: Vec::new(),
                    type_: obj!(vec![("a", Type::Bool)]),
                    line: 0,
                },
            ),
            (
                Key::Var("y"),
                Value {
                    scope: Vec::new(),
                    type_: obj!(vec![("a", Type::Bool)]),
                    line: 3,
                },
            ),
        ],
    )
}
