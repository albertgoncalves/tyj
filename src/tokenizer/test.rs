use super::{get_tokens, Asn, Lex, Op, Tkn};

macro_rules! assert_token {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_tokens($a), vec![$b])
    };
}

macro_rules! assert_tokens {
    ($a:expr, $b:expr $(,)?) => {
        assert_eq!(get_tokens($a), $b)
    };
}

#[test]
fn null() {
    assert_token!("null", Lex { token: Tkn::Null, line: 0 });
    assert_tokens!(
        "null;",
        vec![
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn undefined() {
    assert_token!("undefined", Lex { token: Tkn::Undef, line: 0 });
    assert_tokens!(
        "undefined;",
        vec![
            Lex { token: Tkn::Undef, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn number() {
    assert_token!("1", Lex { token: Tkn::Num("1"), line: 0 });
    assert_token!("10", Lex { token: Tkn::Num("10"), line: 0 });
    assert_token!(".10", Lex { token: Tkn::Num(".10"), line: 0 });
    assert_token!("1.0", Lex { token: Tkn::Num("1.0"), line: 0 });
    assert_tokens!(
        "1.0.",
        vec![
            Lex { token: Tkn::Num("1.0"), line: 0 },
            Lex { token: Tkn::Op(Op::Member), line: 0 },
        ],
    );
    assert_tokens!(
        "1;",
        vec![
            Lex { token: Tkn::Num("1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    );
    assert_tokens!(
        "10;",
        vec![
            Lex { token: Tkn::Num("10"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    );
    assert_tokens!(
        ".10;",
        vec![
            Lex { token: Tkn::Num(".10"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    );
    assert_tokens!(
        "1.0;",
        vec![
            Lex { token: Tkn::Num("1.0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    );
    assert_tokens!(
        "1.0.;",
        vec![
            Lex { token: Tkn::Num("1.0"), line: 0 },
            Lex { token: Tkn::Op(Op::Member), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn string() {
    assert_token!("\"blah\"", Lex { token: Tkn::Str("blah"), line: 0 });
    assert_tokens!(
        "\"blah\";",
        vec![
            Lex { token: Tkn::Str("blah"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn bool() {
    assert_token!("true", Lex { token: Tkn::Bool("true"), line: 0 });
    assert_token!("false", Lex { token: Tkn::Bool("false"), line: 0 });
    assert_tokens!(
        "true;",
        vec![
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    );
    assert_tokens!(
        "false;",
        vec![
            Lex { token: Tkn::Bool("false"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn object() {
    assert_tokens!(
        "{

             a: null,

             bc: undefined

         };",
        vec![
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 2 },
            Lex { token: Tkn::Colon, line: 2 },
            Lex { token: Tkn::Null, line: 2 },
            Lex { token: Tkn::Comma, line: 2 },
            Lex { token: Tkn::Ident("bc"), line: 4 },
            Lex { token: Tkn::Colon, line: 4 },
            Lex { token: Tkn::Undef, line: 4 },
            Lex { token: Tkn::RBrace, line: 6 },
            Lex { token: Tkn::Semicolon, line: 6 },
        ],
    )
}

#[test]
fn object_fields() {
    assert_tokens!(
        "var x = {
             a: {
                 b: 0
             }
         };
         x.a.b;",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 1 },
            Lex { token: Tkn::Colon, line: 1 },
            Lex { token: Tkn::LBrace, line: 1 },
            Lex { token: Tkn::Ident("b"), line: 2 },
            Lex { token: Tkn::Colon, line: 2 },
            Lex { token: Tkn::Num("0"), line: 2 },
            Lex { token: Tkn::RBrace, line: 3 },
            Lex { token: Tkn::RBrace, line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
            Lex { token: Tkn::Ident("x"), line: 5 },
            Lex { token: Tkn::Op(Op::Member), line: 5 },
            Lex { token: Tkn::Ident("a"), line: 5 },
            Lex { token: Tkn::Op(Op::Member), line: 5 },
            Lex { token: Tkn::Ident("b"), line: 5 },
            Lex { token: Tkn::Semicolon, line: 5 },
        ],
    )
}

#[test]
fn declare() {
    assert_tokens!(
        "var a = \"null\";",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Str("null"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn multiple_lines() {
    assert_tokens!(
        "null;
         undefined;
         1;
         10;
         .10;
         1.0;
         \"blah\";
         true;
         false;
         { a: 0.1 };",
        vec![
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Undef, line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Num("1"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::Num("10"), line: 3 },
            Lex { token: Tkn::Semicolon, line: 3 },
            Lex { token: Tkn::Num(".10"), line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
            Lex { token: Tkn::Num("1.0"), line: 5 },
            Lex { token: Tkn::Semicolon, line: 5 },
            Lex { token: Tkn::Str("blah"), line: 6 },
            Lex { token: Tkn::Semicolon, line: 6 },
            Lex { token: Tkn::Bool("true"), line: 7 },
            Lex { token: Tkn::Semicolon, line: 7 },
            Lex { token: Tkn::Bool("false"), line: 8 },
            Lex { token: Tkn::Semicolon, line: 8 },
            Lex { token: Tkn::LBrace, line: 9 },
            Lex { token: Tkn::Ident("a"), line: 9 },
            Lex { token: Tkn::Colon, line: 9 },
            Lex { token: Tkn::Num("0.1"), line: 9 },
            Lex { token: Tkn::RBrace, line: 9 },
            Lex { token: Tkn::Semicolon, line: 9 },
        ],
    )
}

#[test]
fn tiny_function() {
    assert_tokens!(
        "function f() {
             return 0;
         }",
        vec![
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ret, line: 1 },
            Lex { token: Tkn::Num("0"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::RBrace, line: 2 },
        ],
    )
}

#[test]
fn small_function() {
    assert_tokens!(
        "function f(a, b, c) {
             var d = {
                 a: a,
                 b: b,
                 c: c,
             };
             return d;
         }",
        vec![
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Ident("c"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Var, line: 1 },
            Lex { token: Tkn::Ident("d"), line: 1 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 1 },
            Lex { token: Tkn::LBrace, line: 1 },
            Lex { token: Tkn::Ident("a"), line: 2 },
            Lex { token: Tkn::Colon, line: 2 },
            Lex { token: Tkn::Ident("a"), line: 2 },
            Lex { token: Tkn::Comma, line: 2 },
            Lex { token: Tkn::Ident("b"), line: 3 },
            Lex { token: Tkn::Colon, line: 3 },
            Lex { token: Tkn::Ident("b"), line: 3 },
            Lex { token: Tkn::Comma, line: 3 },
            Lex { token: Tkn::Ident("c"), line: 4 },
            Lex { token: Tkn::Colon, line: 4 },
            Lex { token: Tkn::Ident("c"), line: 4 },
            Lex { token: Tkn::Comma, line: 4 },
            Lex { token: Tkn::RBrace, line: 5 },
            Lex { token: Tkn::Semicolon, line: 5 },
            Lex { token: Tkn::Ret, line: 6 },
            Lex { token: Tkn::Ident("d"), line: 6 },
            Lex { token: Tkn::Semicolon, line: 6 },
            Lex { token: Tkn::RBrace, line: 7 },
        ],
    )
}

#[test]
fn declare_anonymous_function() {
    assert_tokens!(
        "var f = function(x) {
             return x + 0.1;
         };",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Ret, line: 1 },
            Lex { token: Tkn::Ident("x"), line: 1 },
            Lex { token: Tkn::Op(Op::Add), line: 1 },
            Lex { token: Tkn::Num("0.1"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::RBrace, line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
        ],
    )
}

#[test]
fn tiny_program() {
    assert_tokens!(
        "window.onload = function() {
             var a = 0.1;
             var b = 10;
             return a + b;
         };",
        vec![
            Lex { token: Tkn::Ident("window"), line: 0 },
            Lex { token: Tkn::Op(Op::Member), line: 0 },
            Lex { token: Tkn::Ident("onload"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Fn, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LBrace, line: 0 },
            Lex { token: Tkn::Var, line: 1 },
            Lex { token: Tkn::Ident("a"), line: 1 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 1 },
            Lex { token: Tkn::Num("0.1"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Var, line: 2 },
            Lex { token: Tkn::Ident("b"), line: 2 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 2 },
            Lex { token: Tkn::Num("10"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::Ret, line: 3 },
            Lex { token: Tkn::Ident("a"), line: 3 },
            Lex { token: Tkn::Op(Op::Add), line: 3 },
            Lex { token: Tkn::Ident("b"), line: 3 },
            Lex { token: Tkn::Semicolon, line: 3 },
            Lex { token: Tkn::RBrace, line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
        ],
    )
}

#[test]
fn prefix_operators() {
    assert_tokens!(
        "var a = !true;
         var b = -1.0;",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Op(Op::Not), line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Var, line: 1 },
            Lex { token: Tkn::Ident("b"), line: 1 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 1 },
            Lex { token: Tkn::Op(Op::Sub), line: 1 },
            Lex { token: Tkn::Num("1.0"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
        ],
    )
}

#[test]
fn nested_expression() {
    assert_tokens!(
        "var x = (a + b) + ((c + d) + e);",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op(Op::Add), line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Op(Op::Add), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("c"), line: 0 },
            Lex { token: Tkn::Op(Op::Add), line: 0 },
            Lex { token: Tkn::Ident("d"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Op(Op::Add), line: 0 },
            Lex { token: Tkn::Ident("e"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn increment() {
    assert_tokens!(
        "a++;
         ++b;",
        vec![
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op(Op::Increment), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Op(Op::Increment), line: 1 },
            Lex { token: Tkn::Ident("b"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
        ],
    )
}

#[test]
fn decrement() {
    assert_tokens!(
        "a--;
         --b;",
        vec![
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Op(Op::Decrement), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Op(Op::Decrement), line: 1 },
            Lex { token: Tkn::Ident("b"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
        ],
    )
}

#[test]
fn comment() {
    assert_tokens!(
        "//! Comment 1
         var a;
         /*! Comment 2 */
         // Ignored Comment 3
         var b;
         /* Ignored Comment 4 */",
        vec![
            Lex { token: Tkn::Comment("//! Comment 1"), line: 0 },
            Lex { token: Tkn::Var, line: 1 },
            Lex { token: Tkn::Ident("a"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Comment("/*! Comment 2 */"), line: 2 },
            Lex { token: Tkn::Var, line: 4 },
            Lex { token: Tkn::Ident("b"), line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
        ],
    )
}

#[test]
fn multiline_comment() {
    assert_tokens!(
        "/*! Comment 1 */
         var a;
         /*! Comment 2\n\
          * ...\n\
          */\n\
         var b;",
        vec![
            Lex { token: Tkn::Comment("/*! Comment 1 */"), line: 0 },
            Lex { token: Tkn::Var, line: 1 },
            Lex { token: Tkn::Ident("a"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Comment("/*! Comment 2\n* ...\n*/"), line: 2 },
            Lex { token: Tkn::Var, line: 5 },
            Lex { token: Tkn::Ident("b"), line: 5 },
            Lex { token: Tkn::Semicolon, line: 5 },
        ],
    )
}

#[test]
fn if_else() {
    assert_tokens!(
        "var a;
         if (true) {
             a = 0;
         } else {
             a = 1;
         }",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::If, line: 1 },
            Lex { token: Tkn::LParen, line: 1 },
            Lex { token: Tkn::Bool("true"), line: 1 },
            Lex { token: Tkn::RParen, line: 1 },
            Lex { token: Tkn::LBrace, line: 1 },
            Lex { token: Tkn::Ident("a"), line: 2 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 2 },
            Lex { token: Tkn::Num("0"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::RBrace, line: 3 },
            Lex { token: Tkn::Else, line: 3 },
            Lex { token: Tkn::LBrace, line: 3 },
            Lex { token: Tkn::Ident("a"), line: 4 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 4 },
            Lex { token: Tkn::Num("1"), line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
            Lex { token: Tkn::RBrace, line: 5 },
        ],
    )
}

#[test]
fn switch_case() {
    assert_tokens!(
        "var x = true;
         switch (x) {
         case true: {
             console.log(\"true\");
             break;
         }
         case false: {
             console.log(\"false\");
             break;
         }
         default: {
             console.log(\"?\");
         }
         }",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Bool("true"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Switch, line: 1 },
            Lex { token: Tkn::LParen, line: 1 },
            Lex { token: Tkn::Ident("x"), line: 1 },
            Lex { token: Tkn::RParen, line: 1 },
            Lex { token: Tkn::LBrace, line: 1 },
            Lex { token: Tkn::Case, line: 2 },
            Lex { token: Tkn::Bool("true"), line: 2 },
            Lex { token: Tkn::Colon, line: 2 },
            Lex { token: Tkn::LBrace, line: 2 },
            Lex { token: Tkn::Ident("console"), line: 3 },
            Lex { token: Tkn::Op(Op::Member), line: 3 },
            Lex { token: Tkn::Ident("log"), line: 3 },
            Lex { token: Tkn::LParen, line: 3 },
            Lex { token: Tkn::Str("true"), line: 3 },
            Lex { token: Tkn::RParen, line: 3 },
            Lex { token: Tkn::Semicolon, line: 3 },
            Lex { token: Tkn::Break, line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
            Lex { token: Tkn::RBrace, line: 5 },
            Lex { token: Tkn::Case, line: 6 },
            Lex { token: Tkn::Bool("false"), line: 6 },
            Lex { token: Tkn::Colon, line: 6 },
            Lex { token: Tkn::LBrace, line: 6 },
            Lex { token: Tkn::Ident("console"), line: 7 },
            Lex { token: Tkn::Op(Op::Member), line: 7 },
            Lex { token: Tkn::Ident("log"), line: 7 },
            Lex { token: Tkn::LParen, line: 7 },
            Lex { token: Tkn::Str("false"), line: 7 },
            Lex { token: Tkn::RParen, line: 7 },
            Lex { token: Tkn::Semicolon, line: 7 },
            Lex { token: Tkn::Break, line: 8 },
            Lex { token: Tkn::Semicolon, line: 8 },
            Lex { token: Tkn::RBrace, line: 9 },
            Lex { token: Tkn::Default, line: 10 },
            Lex { token: Tkn::Colon, line: 10 },
            Lex { token: Tkn::LBrace, line: 10 },
            Lex { token: Tkn::Ident("console"), line: 11 },
            Lex { token: Tkn::Op(Op::Member), line: 11 },
            Lex { token: Tkn::Ident("log"), line: 11 },
            Lex { token: Tkn::LParen, line: 11 },
            Lex { token: Tkn::Str("?"), line: 11 },
            Lex { token: Tkn::RParen, line: 11 },
            Lex { token: Tkn::Semicolon, line: 11 },
            Lex { token: Tkn::RBrace, line: 12 },
            Lex { token: Tkn::RBrace, line: 13 },
        ],
    )
}

#[test]
fn for_() {
    assert_tokens!(
        "var x = 0;
         for (var i = 0; i < 10; ++i) {
             x += 2;
         }",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::For, line: 1 },
            Lex { token: Tkn::LParen, line: 1 },
            Lex { token: Tkn::Var, line: 1 },
            Lex { token: Tkn::Ident("i"), line: 1 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 1 },
            Lex { token: Tkn::Num("0"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Ident("i"), line: 1 },
            Lex { token: Tkn::Op(Op::LessThan), line: 1 },
            Lex { token: Tkn::Num("10"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Op(Op::Increment), line: 1 },
            Lex { token: Tkn::Ident("i"), line: 1 },
            Lex { token: Tkn::RParen, line: 1 },
            Lex { token: Tkn::LBrace, line: 1 },
            Lex { token: Tkn::Ident("x"), line: 2 },
            Lex { token: Tkn::Assign(Asn::Add), line: 2 },
            Lex { token: Tkn::Num("2"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::RBrace, line: 3 },
        ],
    )
}

#[test]
fn while_() {
    assert_tokens!(
        "var x = 0;
         while (x < 10) {
             x += 2;
         }",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::While, line: 1 },
            Lex { token: Tkn::LParen, line: 1 },
            Lex { token: Tkn::Ident("x"), line: 1 },
            Lex { token: Tkn::Op(Op::LessThan), line: 1 },
            Lex { token: Tkn::Num("10"), line: 1 },
            Lex { token: Tkn::RParen, line: 1 },
            Lex { token: Tkn::LBrace, line: 1 },
            Lex { token: Tkn::Ident("x"), line: 2 },
            Lex { token: Tkn::Assign(Asn::Add), line: 2 },
            Lex { token: Tkn::Num("2"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::RBrace, line: 3 },
        ],
    )
}

#[test]
fn function_calls() {
    assert_tokens!(
        "f(a)(b);",
        vec![
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn function_calls_nested() {
    assert_tokens!(
        "f(a(x)(y))(b);",
        vec![
            Lex { token: Tkn::Ident("f"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("y"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn ternary_operator() {
    assert_tokens!(
        "var x = y ? 0 : 1;",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Ident("y"), line: 0 },
            Lex { token: Tkn::Ternary, line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Colon, line: 0 },
            Lex { token: Tkn::Num("1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn new() {
    assert_tokens!(
        "new Uint8Array(buffer, 0, 13);",
        vec![
            Lex { token: Tkn::Op(Op::New), line: 0 },
            Lex { token: Tkn::Ident("Uint8Array"), line: 0 },
            Lex { token: Tkn::LParen, line: 0 },
            Lex { token: Tkn::Ident("buffer"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Comma, line: 0 },
            Lex { token: Tkn::Num("13"), line: 0 },
            Lex { token: Tkn::RParen, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn brackets() {
    assert_tokens!(
        "array[0] = null;",
        vec![
            Lex { token: Tkn::Ident("array"), line: 0 },
            Lex { token: Tkn::LBracket, line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::RBracket, line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Null, line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn bit_operators() {
    assert_tokens!(
        "~0;
         1 & 1;
         2 | 2;
         3 ^ 3;
         4 << 4;
         5 >> 5;
         6 >>> 6;",
        vec![
            Lex { token: Tkn::Op(Op::BitwiseNot), line: 0 },
            Lex { token: Tkn::Num("0"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Num("1"), line: 1 },
            Lex { token: Tkn::Op(Op::BitwiseAnd), line: 1 },
            Lex { token: Tkn::Num("1"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Num("2"), line: 2 },
            Lex { token: Tkn::Op(Op::BitwiseOr), line: 2 },
            Lex { token: Tkn::Num("2"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::Num("3"), line: 3 },
            Lex { token: Tkn::Op(Op::BitwiseXor), line: 3 },
            Lex { token: Tkn::Num("3"), line: 3 },
            Lex { token: Tkn::Semicolon, line: 3 },
            Lex { token: Tkn::Num("4"), line: 4 },
            Lex { token: Tkn::Op(Op::ShiftLeft), line: 4 },
            Lex { token: Tkn::Num("4"), line: 4 },
            Lex { token: Tkn::Semicolon, line: 4 },
            Lex { token: Tkn::Num("5"), line: 5 },
            Lex { token: Tkn::Op(Op::ShiftRight), line: 5 },
            Lex { token: Tkn::Num("5"), line: 5 },
            Lex { token: Tkn::Semicolon, line: 5 },
            Lex { token: Tkn::Num("6"), line: 6 },
            Lex { token: Tkn::Op(Op::UnsignedShiftRight), line: 6 },
            Lex { token: Tkn::Num("6"), line: 6 },
            Lex { token: Tkn::Semicolon, line: 6 },
        ],
    )
}

#[test]
fn update_assign() {
    assert_tokens!(
        "a += 1;
         b -= 1;
         c *= 2;
         d /= 2;",
        vec![
            Lex { token: Tkn::Ident("a"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Add), line: 0 },
            Lex { token: Tkn::Num("1"), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
            Lex { token: Tkn::Ident("b"), line: 1 },
            Lex { token: Tkn::Assign(Asn::Sub), line: 1 },
            Lex { token: Tkn::Num("1"), line: 1 },
            Lex { token: Tkn::Semicolon, line: 1 },
            Lex { token: Tkn::Ident("c"), line: 2 },
            Lex { token: Tkn::Assign(Asn::Mul), line: 2 },
            Lex { token: Tkn::Num("2"), line: 2 },
            Lex { token: Tkn::Semicolon, line: 2 },
            Lex { token: Tkn::Ident("d"), line: 3 },
            Lex { token: Tkn::Assign(Asn::Div), line: 3 },
            Lex { token: Tkn::Num("2"), line: 3 },
            Lex { token: Tkn::Semicolon, line: 3 },
        ],
    )
}

#[test]
fn empty_string() {
    assert_tokens!(
        "var x = \"\";",
        vec![
            Lex { token: Tkn::Var, line: 0 },
            Lex { token: Tkn::Ident("x"), line: 0 },
            Lex { token: Tkn::Assign(Asn::Reg), line: 0 },
            Lex { token: Tkn::Str(""), line: 0 },
            Lex { token: Tkn::Semicolon, line: 0 },
        ],
    )
}

#[test]
fn dots() {
    assert_tokens!(
        "100.000.",
        vec![
            Lex { token: Tkn::Num("100.000"), line: 0 },
            Lex { token: Tkn::Op(Op::Member), line: 0 },
        ],
    );
    assert_tokens!(
        ".100.000",
        vec![
            Lex { token: Tkn::Num(".100"), line: 0 },
            Lex { token: Tkn::Num(".000"), line: 0 },
        ],
    );
    assert_tokens!(
        ".100 000.",
        vec![
            Lex { token: Tkn::Num(".100"), line: 0 },
            Lex { token: Tkn::Num("000."), line: 0 },
        ],
    );
    assert_tokens!(
        ".100 000..",
        vec![
            Lex { token: Tkn::Num(".100"), line: 0 },
            Lex { token: Tkn::Num("000."), line: 0 },
            Lex { token: Tkn::Op(Op::Member), line: 0 },
        ],
    )
}
