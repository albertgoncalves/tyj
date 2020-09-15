use super::{get_sigs, get_tokens, Lex, Prop, Sig, Stmt, Tkn, Type};

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
             a: number,
             b: string,
             c: bool,
             d: null,
             e: undefined
         } */",
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
        "f(null, undefined) -> null",
        Ok(vec![Sig {
            statement: Stmt::Fn {
                ident: "f",
                args: vec![Type::Null, Type::Undef],
                return_: Type::Null
            },
            line: 0,
        }]),
    )
}

#[test]
fn parse_fn_empty_args() {
    assert_sigs!(
        "f() -> undefined",
        Ok(vec![Sig {
            statement: Stmt::Fn {
                ident: "f",
                args: Vec::new(),
                return_: Type::Undef,
            },
            line: 0,
        }]),
    )
}

#[test]
fn parse_obj() {
    assert_sigs!(
        "x {
             a: number,
             b: string,
             c: bool,
             d: null,
             e: undefined,
             f: { g: null },
         }",
        Ok(vec![Sig {
            statement: Stmt::Obj {
                ident: "x",
                props: vec![
                    Prop { key: "a", value: Type::Num },
                    Prop { key: "b", value: Type::Str },
                    Prop { key: "c", value: Type::Bool },
                    Prop { key: "d", value: Type::Null },
                    Prop { key: "e", value: Type::Undef },
                    Prop {
                        key: "f",
                        value: Type::Props(vec![Prop {
                            key: "g",
                            value: Type::Null,
                        }]),
                    },
                ],
            },
            line: 0,
        }]),
    )
}

#[test]
fn parse_obj_empty() {
    assert_sigs!(
        "x {}",
        Ok(vec![Sig {
            statement: Stmt::Obj { ident: "x", props: Vec::new() },
            line: 0,
        }]),
    )
}

#[test]
fn parse_combined() {
    assert_sigs!(
        "x { a: number }
         f(a) -> { b: bool }",
        Ok(vec![
            Sig {
                statement: Stmt::Obj {
                    ident: "x",
                    props: vec![Prop { key: "a", value: Type::Num }],
                },
                line: 0,
            },
            Sig {
                statement: Stmt::Fn {
                    ident: "f",
                    args: vec![Type::Ident("a")],
                    return_: Type::Props(vec![Prop {
                        key: "b",
                        value: Type::Bool
                    }]),
                },
                line: 1,
            },
        ]),
    )
}
