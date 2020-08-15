use super::{get_types, Label, Prop, Type};
use crate::parser::get_ast;
use crate::tokenizer::get_tokens;

macro_rules! assert_types {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_types(&get_ast(&get_tokens($a))), $b)
    };
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
            Label {
                scope: Vec::new(),
                ident: vec!["a"],
                r#type: Type::Num,
                line: 0,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["b"],
                r#type: Type::Str,
                line: 1,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["c"],
                r#type: Type::Bool,
                line: 2,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["d"],
                r#type: Type::Null,
                line: 3,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["e"],
                r#type: Type::Undef,
                line: 4,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["f"],
                r#type: Type::Obj(Vec::new()),
                line: 5,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["g"],
                r#type: Type::Obj(vec![
                    Prop { key: "_a", value: Type::Num },
                    Prop { key: "_b", value: Type::Str },
                    Prop { key: "_c", value: Type::Bool },
                    Prop { key: "_d", value: Type::Null },
                    Prop { key: "_e", value: Type::Undef },
                    Prop { key: "_f", value: Type::Obj(Vec::new()) },
                    Prop {
                        key: "_g",
                        value: Type::Obj(vec![Prop {
                            key: "__a",
                            value: Type::Num,
                        }]),
                    },
                ]),
                line: 6,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["h"],
                r#type: Type::Array(Vec::new()),
                line: 15,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["i"],
                r#type: Type::Array(vec![Type::Num]),
                line: 16,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["j"],
                r#type: Type::Array(vec![Type::Str]),
                line: 17,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["k"],
                r#type: Type::Array(vec![Type::Bool]),
                line: 18,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["l"],
                r#type: Type::Array(vec![Type::Null]),
                line: 19,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["m"],
                r#type: Type::Array(vec![Type::Undef]),
                line: 20,
            },
        ],
    )
}

#[test]
fn declare_assign() {
    assert_types!(
        "var x = 0;
         x = 1;
         x = \"?\";",
        vec![
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Num,
                line: 0,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Num,
                line: 1,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Str,
                line: 2,
            },
        ],
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
        vec![
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Obj(vec![
                    Prop { key: "a", value: Type::Num },
                    Prop { key: "b", value: Type::Str },
                ]),
                line: 0,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["x", "a"],
                r#type: Type::Num,
                line: 4,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["x", "b"],
                r#type: Type::Str,
                line: 5,
            },
        ],
    )
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
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Obj(vec![Prop { key: "a", value: Type::Num }]),
                line: 0,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["y"],
                r#type: Type::Num,
                line: 3,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["x", "a"],
                r#type: Type::Ref(vec!["y"]),
                line: 4,
            },
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
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Num,
                line: 0,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["y"],
                r#type: Type::Obj(vec![Prop { key: "a", value: Type::Num }]),
                line: 1,
            },
            Label {
                scope: Vec::new(),
                ident: vec!["x"],
                r#type: Type::Ref(vec!["y", "a"]),
                line: 4,
            },
        ],
    )
}
