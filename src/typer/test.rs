use super::{get_types, Error, Type};
use crate::parser::{get_ast, Expr, Stmt, Syntax};
use crate::tokenizer::get_tokens;

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
        Ok((
            vec![Type::Str, Type::Num, Type::Bool, Type::Null, Type::Undef],
            vec![
                (vec!["a"], 0),
                (vec!["b"], 1),
                (vec!["c"], 2),
                (vec!["d"], 3),
                (vec!["e"], 4),
            ]
            .into_iter()
            .collect(),
        )),
    )
}

#[test]
fn declare_shadow_ident() {
    assert_types!(
        "var x = true;
         var x = false;",
        Err(Error::ShadowIdent(&Syntax {
            statement: Stmt::Decl { ident: "x", expr: Expr::Bool("false") },
            line: 1,
        })),
    )
}

#[test]
fn declare_unknown_ident() {
    assert_types!(
        "var y = x;",
        Err(Error::UnknownIdent(&Syntax {
            statement: Stmt::Decl { ident: "y", expr: Expr::Ident("x") },
            line: 0,
        })),
    )
}

#[test]
fn declare_ident() {
    assert_types!(
        "var x = 0;
         var y = x;",
        Ok((
            vec![Type::Num],
            vec![(vec!["x"], 0), (vec!["y"], 0)].into_iter().collect(),
        )),
    )
}
