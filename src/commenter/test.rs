use super::{get_sigs, get_tokens, Comment, Error, Lex, Tkn};
use crate::map;
use crate::types::{Target, Type};
use std::collections::BTreeMap;

macro_rules! assert_tokens {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_tokens(&$a), $b)
    };
}

#[test]
fn tokenize_empty() {
    assert_tokens!(Comment { string: "/* \n *\n */", line: 0 }, Vec::new());
    assert_tokens!(Comment { string: "//", line: 0 }, Vec::new());
}

#[test]
fn tokenize_function() {
    assert_tokens!(
        Comment { string: "// f(number, number) -> number", line: 0 },
        vec![
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Num, line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Num, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Arrow, line: 0 },
            Lex { token: Tkn::Num, line: 0 },
        ],
    )
}

#[test]
fn tokenize_object() {
    let string: &str = "/* x {
                         *     a: number,
                         *     b: string,
                         *     c: bool,
                         *     d: null,
                         *     e: undefined,
                         * }
                         */";
    assert_tokens!(
        Comment { string, line: 0 },
        vec![
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 1 },
            Lex { token: Tkn::Colon, line: 1 },
            Lex { token: Tkn::Num, line: 1 },
            Lex { token: Tkn::Comma, line: 1 },
            Lex { token: Tkn::Ident("b"), line: 2 },
            Lex { token: Tkn::Colon, line: 2 },
            Lex { token: Tkn::Str, line: 2 },
            Lex { token: Tkn::Comma, line: 2 },
            Lex { token: Tkn::Ident("c"), line: 3 },
            Lex { token: Tkn::Colon, line: 3 },
            Lex { token: Tkn::Bool, line: 3 },
            Lex { token: Tkn::Comma, line: 3 },
            Lex { token: Tkn::Ident("d"), line: 4 },
            Lex { token: Tkn::Colon, line: 4 },
            Lex { token: Tkn::Null, line: 4 },
            Lex { token: Tkn::Comma, line: 4 },
            Lex { token: Tkn::Ident("e"), line: 5 },
            Lex { token: Tkn::Colon, line: 5 },
            Lex { token: Tkn::Undef, line: 5 },
            Lex { token: Tkn::Comma, line: 5 },
            Lex { token: Tkn::RBrace, line: 6 },
        ],
    )
}

macro_rules! assert_sigs {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_sigs(&[$a]), $b)
    };
}

#[test]
fn parse_fn() {
    assert_sigs!(
        Comment { string: "// f(null, undefined) -> null", line: 0 },
        Ok(map![(
            Target { ident: vec!["f"], scope: Vec::new() },
            Type::Fn(map![(vec![Type::Null, Type::Undef], Type::Null)]),
        )]),
    )
}

#[test]
fn parse_fn_empty_args() {
    assert_sigs!(
        Comment { string: "/* f() -> undefined */", line: 0 },
        Ok(map![(
            Target { ident: vec!["f"], scope: Vec::new() },
            Type::Fn(map![(Vec::new(), Type::Undef)]),
        )]),
    )
}

#[test]
fn parse_obj() {
    let string: &str = "/* x {
                         *     a: number,
                         *     b: string,
                         *     c: bool,
                         *     d: null,
                         *     e: undefined,
                         *     f: { g: null },
                         * }
                         */";
    assert_sigs!(
        Comment { string, line: 0 },
        Ok(map![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Obj(map![
                ("a", Type::Num),
                ("b", Type::Str),
                ("c", Type::Bool),
                ("d", Type::Null),
                ("e", Type::Undef),
                ("f", Type::Obj(map![("g", Type::Null)])),
            ]),
        )]),
    )
}

#[test]
fn parse_obj_empty() {
    assert_sigs!(
        Comment { string: "// x {}", line: 0 },
        Ok(map![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Obj(BTreeMap::new()),
        )]),
    )
}

#[test]
fn parse_combined() {
    let obj: Type = Type::Obj(map![("a", Type::Num)]);
    let string: &str = "/* x { a: number }
                         * f(x) -> { b: bool }
                         */";
    assert_sigs!(
        Comment { string, line: 0 },
        Ok(map![
            (Target { ident: vec!["x"], scope: Vec::new() }, obj.clone()),
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(map![(
                    vec![obj],
                    Type::Obj(map![("b", Type::Bool)]),
                )]),
            ),
        ]),
    )
}

#[test]
fn parse_overload() {
    let string: &str = "// f(bool, bool) -> bool
                        // f(null, null) -> null
                        // x {}
                        // f(number, number) -> number";
    assert_sigs!(
        Comment { string, line: 0 },
        Ok(map![
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(map![
                    (vec![Type::Bool, Type::Bool], Type::Bool),
                    (vec![Type::Null, Type::Null], Type::Null),
                    (vec![Type::Num, Type::Num], Type::Num),
                ]),
            ),
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(BTreeMap::new()),
            ),
        ]),
    )
}

#[test]
fn parse_scopes() {
    let obj: Type = Type::Obj(map![("a", Type::Num)]);
    let string: &str = "/* x {
                         *     a: number,
                         * }
                         * f(x) -> number
                         * g(x) -> undefined
                         * g @ f() -> number
                         */";
    assert_sigs!(
        Comment { string, line: 0 },
        Ok(map![
            (Target { ident: vec!["x"], scope: Vec::new() }, obj.clone()),
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(map![(vec![obj.clone()], Type::Num)]),
            ),
            (
                Target { ident: vec!["g"], scope: Vec::new() },
                Type::Fn(map![(vec![obj.clone()], Type::Undef)]),
            ),
            (
                Target { ident: vec!["f"], scope: vec!["g"] },
                Type::Fn(map![(Vec::new(), Type::Num)]),
            ),
        ]),
    )
}

#[test]
fn syntax_err() {
    let string: &str = "/* f(string) -> string
                         * f(number) -> number
                         * f(bool -> bool
                         */";
    assert_sigs!(Comment { string, line: 0 }, Err(Error::Line(2)))
}

#[test]
fn overloads_err() {
    let string: &str = "// f() -> string
                        // f() -> number";
    assert_sigs!(Comment { string, line: 0 }, Err(Error::Line(1)))
}

#[test]
fn nested_overloads_err() {
    let string: &str = "// g(string) -> () -> string
                        // g(number) -> () -> number
                        // f() -> string
                        // f() -> number";
    assert_sigs!(Comment { string, line: 0 }, Err(Error::Line(3)))
}

#[test]
fn array() {
    let string: &str = "// f(string, number) -> [string]
                        // f([number], number) -> [[number]]";
    assert_sigs!(
        Comment { string, line: 0 },
        Ok(map![(
            Target { ident: vec!["f"], scope: Vec::new() },
            Type::Fn(map![
                (vec![Type::Str, Type::Num], Type::Array(Box::new(Type::Str))),
                (
                    vec![Type::Array(Box::new(Type::Num)), Type::Num],
                    Type::Array(Box::new(Type::Array(Box::new(Type::Num)))),
                ),
            ]),
        )]),
    )
}
