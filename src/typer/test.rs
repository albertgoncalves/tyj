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
            Label { ident: "a", r#type: Type::Num, line: 0 },
            Label { ident: "b", r#type: Type::Str, line: 1 },
            Label { ident: "c", r#type: Type::Bool, line: 2 },
            Label { ident: "d", r#type: Type::Null, line: 3 },
            Label { ident: "e", r#type: Type::Undef, line: 4 },
            Label { ident: "f", r#type: Type::Obj(Vec::new()), line: 5 },
            Label {
                ident: "g",
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
            Label { ident: "h", r#type: Type::Array(Vec::new()), line: 15 },
            Label {
                ident: "i",
                r#type: Type::Array(vec![Type::Num]),
                line: 16,
            },
            Label {
                ident: "j",
                r#type: Type::Array(vec![Type::Str]),
                line: 17,
            },
            Label {
                ident: "k",
                r#type: Type::Array(vec![Type::Bool]),
                line: 18,
            },
            Label {
                ident: "l",
                r#type: Type::Array(vec![Type::Null]),
                line: 19,
            },
            Label {
                ident: "m",
                r#type: Type::Array(vec![Type::Undef]),
                line: 20,
            },
        ],
    )
}