use crate::tokenizer::Tkn;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
enum Expr<'a> {
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Null,
    Undef,
}

#[derive(Debug, PartialEq)]
enum Stmt<'a> {
    Var(&'a str, Expr<'a>),
}

macro_rules! eat_or_panic {
    ($tokens:expr, $x:path $(,)?) => {
        let _: () = if let Some($x) = $tokens.next() {
            ()
        } else {
            panic!()
        };
    };
}

fn get_expr<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Expr<'a> {
    match tokens.next() {
        Some(Tkn::Num(x)) => Expr::Num(x),
        Some(Tkn::Str(x)) => Expr::Str(x),
        Some(Tkn::Bool(x)) => Expr::Bool(x),
        Some(Tkn::Null) => Expr::Null,
        Some(Tkn::Undef) => Expr::Undef,
        _ => panic!(),
    }
}

fn get_ident<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> &'a str {
    if let Some(Tkn::Ident(x)) = tokens.next() {
        x
    } else {
        panic!()
    }
}

fn get_var<'a, 'b>(tokens: &mut Peekable<Iter<'b, Tkn<'a>>>) -> Stmt<'a> {
    let ident: &str = get_ident(tokens);
    eat_or_panic!(tokens, Tkn::Equals);
    let var: Stmt = Stmt::Var(ident, get_expr(tokens));
    eat_or_panic!(tokens, Tkn::Semicolon);
    var
}

fn get_ast<'a>(tokens: &[Tkn<'a>]) -> Vec<Stmt<'a>> {
    let mut ast: Vec<Stmt> = Vec::new();
    let mut tokens: Peekable<Iter<'_, Tkn<'_>>> = tokens.iter().peekable();
    while let Some(t) = tokens.next() {
        match t {
            Tkn::Var => ast.push(get_var(&mut tokens)),
            _ => panic!(),
        }
    }
    ast
}

#[cfg(test)]
mod tests {
    use super::{get_ast, Expr, Stmt};
    use crate::tokenizer::Tkn;

    macro_rules! assert_ast {
        ($a:expr, $b:expr $(,)?) => {
            assert_eq!(get_ast($a), $b)
        };
    }

    #[test]
    fn var_number() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Num(".1"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Num(".1"))],
        )
    }

    #[test]
    fn var_string() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Str("blah blah"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Str("blah blah"))],
        );
    }

    #[test]
    fn var_bool() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Bool("true"),
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Bool("true"))],
        );
    }

    #[test]
    fn var_null() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Null,
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Null)],
        );
    }

    #[test]
    fn var_undefined() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Undef,
                Tkn::Semicolon,
            ],
            vec![Stmt::Var("x", Expr::Undef)],
        );
    }

    #[test]
    fn multiple_vars() {
        assert_ast!(
            &[
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Num("1."),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Str("blah"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Bool("false"),
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Null,
                Tkn::Semicolon,
                Tkn::Var,
                Tkn::Ident("x"),
                Tkn::Equals,
                Tkn::Undef,
                Tkn::Semicolon,
            ],
            vec![
                Stmt::Var("x", Expr::Num("1.")),
                Stmt::Var("x", Expr::Str("blah")),
                Stmt::Var("x", Expr::Bool("false")),
                Stmt::Var("x", Expr::Null),
                Stmt::Var("x", Expr::Undef),
            ],
        )
    }
}
