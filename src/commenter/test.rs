use super::{get_sigs, get_tokens, Lex, Tkn, Type};
use std::collections::BTreeMap;

macro_rules! assert_tokens {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_tokens($a), $b)
    };
}

#[test]
fn tokenize_empty() {
    assert_tokens!("/* \n *\n */", Vec::new());
    assert_tokens!("//", Vec::new());
}

#[test]
fn tokenize_function() {
    assert_tokens!(
        "// f(number, number) -> number",
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
    assert_tokens!(
        "/* x {
          *     a: number,
          *     b: string,
          *     c: bool,
          *     d: null,
          *     e: undefined,
          * }
          */",
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
        "// f(null, undefined) -> null",
        Ok(vec![(
            "f",
            Type::Fn(
                vec![(vec![Type::Null, Type::Undef], Type::Null)]
                    .into_iter()
                    .collect(),
            ),
        )]
        .into_iter()
        .collect()),
    )
}

#[test]
fn parse_fn_empty_args() {
    assert_sigs!(
        "/* f() -> undefined */",
        Ok(vec![(
            "f",
            Type::Fn(vec![(Vec::new(), Type::Undef)].into_iter().collect()),
        )]
        .into_iter()
        .collect()),
    )
}

#[test]
fn parse_obj() {
    assert_sigs!(
        "/* x {
          *     a: number,
          *     b: string,
          *     c: bool,
          *     d: null,
          *     e: undefined,
          *     f: { g: null },
          * }
          */",
        Ok(vec![(
            "x",
            Type::Obj(
                vec![
                    ("a", Type::Num),
                    ("b", Type::Str),
                    ("c", Type::Bool),
                    ("d", Type::Null),
                    ("e", Type::Undef),
                    (
                        "f",
                        Type::Obj(
                            vec![("g", Type::Null)].into_iter().collect(),
                        ),
                    ),
                ]
                .into_iter()
                .collect(),
            )
        )]
        .into_iter()
        .collect()),
    )
}

#[test]
fn parse_obj_empty() {
    assert_sigs!(
        "// x {}",
        Ok(vec![("x", Type::Obj(BTreeMap::new()))].into_iter().collect()),
    )
}

#[test]
fn parse_combined() {
    assert_sigs!(
        "/* x { a: number }
          * f(x) -> { b: bool }
          */",
        Ok(vec![
            ("x", Type::Obj(vec![("a", Type::Num)].into_iter().collect(),)),
            (
                "f",
                Type::Fn(
                    vec![(
                        vec![Type::Obj(
                            vec![("a", Type::Num)].into_iter().collect(),
                        )],
                        Type::Obj(
                            vec![("b", Type::Bool)].into_iter().collect(),
                        )
                    )]
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
fn parse_overload() {
    assert_sigs!(
        "// f(bool, bool) -> bool
         // f(null, null) -> null
         // x {}
         // f(number, number) -> number",
        Ok(vec![
            (
                "f",
                Type::Fn(
                    vec![
                        (vec![Type::Bool, Type::Bool], Type::Bool),
                        (vec![Type::Null, Type::Null], Type::Null),
                        (vec![Type::Num, Type::Num], Type::Num),
                    ]
                    .into_iter()
                    .collect(),
                ),
            ),
            ("x", Type::Obj(BTreeMap::new())),
        ]
        .into_iter()
        .collect()),
    )
}
