use super::{get_tokens, Lex, Tkn};

macro_rules! assert_tokens {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_tokens($a), $b)
    };
}

#[test]
fn empty() {
    assert_tokens!("/* \n *\n */", Vec::new());
    assert_tokens!("//", Vec::new());
}

#[test]
fn function() {
    assert_tokens!(
        "// fn f(number, number) -> number",
        vec![
            Lex { token: Tkn::Fn, line: 0 },
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
fn object() {
    assert_tokens!(
        "/* obj x {
             a: number,
             b: string,
             c: bool,
             d: null,
             e: undefined
         } */",
        vec![
            Lex { token: Tkn::Obj, line: 0 },
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
