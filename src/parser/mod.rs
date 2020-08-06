#[cfg(test)]
mod test;

use crate::tokenizer::{Lex, Tkn};
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
pub(crate) struct Prop<'a> {
    pub(crate) key: &'a str,
    pub(crate) value: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr<'a> {
    Num(&'a str),
    Str(&'a str),
    Bool(&'a str),
    Obj(Vec<Prop<'a>>),
    Ref(&'a str),
    Prefix { op: &'a str, expr: Box<Expr<'a>> },
    Infix { op: &'a str, left: Box<Expr<'a>>, right: Box<Expr<'a>> },
    Postfix { op: &'a str, expr: Box<Expr<'a>> },
    Fn { args: Vec<&'a str>, body: Vec<Stmt<'a>> },
    Call { expr: Box<Expr<'a>>, args: Vec<Expr<'a>> },
    Null,
    Undef,
    Uninit,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Case<'a> {
    pub(crate) expr: Expr<'a>,
    pub(crate) body: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'a> {
    Decl { ident: &'a str, expr: Expr<'a> },
    Assign { r#ref: Expr<'a>, expr: Expr<'a> },
    Ret(Expr<'a>),
    Fn { ident: &'a str, args: Vec<&'a str>, body: Vec<Stmt<'a>> },
    Cond { condition: Expr<'a>, r#if: Vec<Stmt<'a>>, r#else: Vec<Stmt<'a>> },
    Switch { expr: Expr<'a>, cases: Vec<Case<'a>>, default: Vec<Stmt<'a>> },
    Break,
    Effect(Expr<'a>),
}

macro_rules! eat {
    ($tokens:expr $(,)?) => {{
        let _: Option<&Lex> = $tokens.next();
    }};
}

macro_rules! eat_or_panic {
    ($tokens:expr, $x:path $(,)?) => {
        let lex: Option<&Lex> = $tokens.next();
        let _: () = if let Some(Lex { token: $x, .. }) = lex {
            ()
        } else {
            panic!(format!("{:?}", lex))
        };
    };
}

fn get_ident<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> &'a str {
    let lex: Option<&Lex> = tokens.next();
    if let Some(Lex { token: Tkn::Ident(x), .. }) = lex {
        x
    } else {
        panic!(format!("{:?}", lex))
    }
}

fn get_prop<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> Prop<'a> {
    let key: &str = get_ident(tokens);
    eat_or_panic!(tokens, Tkn::Colon);
    Prop { key, value: get_expr(tokens, 0) }
}

fn get_args<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> Vec<&'a str> {
    eat_or_panic!(tokens, Tkn::LParen);
    let mut args: Vec<&str> = Vec::new();
    while let Some(lex) = tokens.next() {
        match lex {
            Lex { token: Tkn::Ident(x), .. } => {
                args.push(x);
                let lex: Option<&Lex> = tokens.next();
                match lex {
                    Some(Lex { token: Tkn::Comma, .. }) => (),
                    Some(Lex { token: Tkn::RParen, .. }) => break,
                    _ => panic!(format!("{:?}", lex)),
                }
            }
            Lex { token: Tkn::RParen, .. } => break,
            _ => panic!(format!("{:?}", lex)),
        }
    }
    args
}

fn get_body<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Vec<Stmt<'a>> {
    eat_or_panic!(tokens, Tkn::LBrace);
    let mut body: Vec<Stmt> = Vec::new();
    while let Some(lex) = tokens.peek() {
        match lex {
            Lex { token: Tkn::RBrace, .. } => {
                eat!(tokens);
                break;
            }
            _ => body.push(get_stmt(tokens)),
        }
    }
    body
}

fn get_fn<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> Expr<'a> {
    let args: Vec<&str> = get_args(tokens);
    Expr::Fn { args, body: get_body(tokens) }
}

fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
    precedence: u8,
) -> Expr<'a> {
    let lex: Option<&Lex> = tokens.next();

    macro_rules! set_prefix {
        ($op:expr $(,)?) => {{
            let power: u8 = match $op {
                "-" | "!" | "++" | "--" => 9,
                _ => panic!(format!("{:?}", lex)),
            };
            Expr::Prefix { op: $op, expr: Box::new(get_expr(tokens, power)) }
        }};
    }

    let mut expr: Expr = match lex {
        Some(Lex { token: Tkn::LParen, .. }) => {
            let expr: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            expr
        }
        Some(Lex { token: Tkn::Op(x), .. }) if *x != "=" => set_prefix!(*x),
        Some(Lex { token: Tkn::Num(x), .. }) => Expr::Num(x),
        Some(Lex { token: Tkn::Str(x), .. }) => Expr::Str(x),
        Some(Lex { token: Tkn::Bool(x), .. }) => Expr::Bool(x),
        Some(Lex { token: Tkn::Ident(x), .. }) => Expr::Ref(x),
        Some(Lex { token: Tkn::Fn, .. }) => get_fn(tokens),
        Some(Lex { token: Tkn::Null, .. }) => Expr::Null,
        Some(Lex { token: Tkn::Undef, .. }) => Expr::Undef,
        Some(Lex { token: Tkn::LBrace, .. }) => {
            let mut props: Vec<Prop> = Vec::new();
            while let Some(lex) = tokens.peek() {
                match lex {
                    Lex { token: Tkn::Ident(_), .. } => {
                        props.push(get_prop(tokens));
                        let lex: Option<&Lex> = tokens.next();
                        match lex {
                            Some(Lex { token: Tkn::Comma, .. }) => (),
                            Some(Lex { token: Tkn::RBrace, .. }) => break,
                            _ => panic!(format!("{:?}", lex)),
                        }
                    }
                    Lex { token: Tkn::RBrace, .. } => {
                        eat!(tokens);
                        break;
                    }
                    _ => panic!(format!("{:?}", lex)),
                }
            }
            Expr::Obj(props)
        }
        _ => panic!(format!("{:?}", lex)),
    };

    macro_rules! set_postfix_or_infix {
        ($lex:expr, $op:expr $(,)?) => {{
            let power: Option<u8> = match $op {
                "++" | "--" => Some(9),
                _ => None,
            };
            if let Some(power) = power {
                if power < precedence {
                    break;
                }
                eat!(tokens);
                expr = Expr::Postfix { op: $op, expr: Box::new(expr) };
            } else {
                let (l_power, r_power): (u8, u8) = match $op {
                    "." => (11, 12),
                    "*" | "/" => (7, 8),
                    "+" | "-" => (5, 6),
                    "<" | ">" | "<=" | ">=" => (3, 4),
                    "===" | "!==" => (1, 2),
                    _ => panic!(format!("{:?}", $lex)),
                };
                if l_power < precedence {
                    break;
                }
                eat!(tokens);
                expr = Expr::Infix {
                    op: $op,
                    left: Box::new(expr),
                    right: Box::new(get_expr(tokens, r_power)),
                };
            }
        }};
    }

    while let Some(lex) = tokens.peek() {
        match lex {
            Lex { token: Tkn::Op(x), .. } if *x != "=" => {
                set_postfix_or_infix!(lex, *x)
            }
            _ => break,
        }
    }
    if precedence == 0 {
        while let Some(Lex { token: Tkn::LParen, .. }) = tokens.peek() {
            eat!(tokens);
            let mut args: Vec<Expr> = Vec::new();
            while let Some(lex) = tokens.peek() {
                match lex {
                    Lex { token: Tkn::Comma, .. } => eat!(tokens),
                    Lex { token: Tkn::RParen, .. } => {
                        eat!(tokens);
                        break;
                    }
                    _ => args.push(get_expr(tokens, 0)),
                }
            }
            expr = Expr::Call { expr: Box::new(expr), args }
        }
    }
    expr
}

fn get_stmt<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> Stmt<'a> {
    match tokens.peek() {
        Some(Lex { token: Tkn::Fn, .. }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let args: Vec<&str> = get_args(tokens);
            Stmt::Fn { ident, args, body: get_body(tokens) }
        }
        Some(Lex { token: Tkn::If, .. }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            let r#if: Vec<Stmt> = get_body(tokens);
            if let Some(Lex { token: Tkn::Else, .. }) = tokens.peek() {
                eat!(tokens);
                match tokens.peek() {
                    Some(Lex { token: Tkn::If, .. }) => {
                        return Stmt::Cond {
                            condition,
                            r#if,
                            r#else: vec![get_stmt(tokens)],
                        };
                    }
                    Some(Lex { token: Tkn::LBrace, .. }) => {
                        return Stmt::Cond {
                            condition,
                            r#if,
                            r#else: get_body(tokens),
                        };
                    }
                    _ => panic!(format!("{:?}", tokens.peek())),
                }
            }
            Stmt::Cond { condition, r#if, r#else: Vec::new() }
        }
        Some(Lex { token: Tkn::Switch, .. }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let expr: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            eat_or_panic!(tokens, Tkn::LBrace);
            let mut cases: Vec<Case> = Vec::new();
            while let Some(Lex { token: Tkn::Case, .. }) = tokens.peek() {
                eat!(tokens);
                let expr: Expr = get_expr(tokens, 0);
                eat_or_panic!(tokens, Tkn::Colon);
                cases.push(Case { expr, body: get_body(tokens) });
            }
            let default: Vec<Stmt> =
                if let Some(Lex { token: Tkn::Default, .. }) = tokens.peek() {
                    eat!(tokens);
                    eat_or_panic!(tokens, Tkn::Colon);
                    get_body(tokens)
                } else {
                    Vec::new()
                };
            eat_or_panic!(tokens, Tkn::RBrace);
            Stmt::Switch { expr, cases, default }
        }
        Some(Lex { token: Tkn::Var, .. }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let lex: Option<&Lex> = tokens.next();
            match lex {
                Some(Lex { token: Tkn::Semicolon, .. }) => {
                    Stmt::Decl { ident, expr: Expr::Uninit }
                }
                Some(Lex { token: Tkn::Op("="), .. }) => {
                    let var: Stmt =
                        Stmt::Decl { ident, expr: get_expr(tokens, 0) };
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    var
                }
                _ => panic!(format!("{:?}", lex)),
            }
        }
        Some(Lex { token: Tkn::Ret, .. }) => {
            eat!(tokens);
            let expr: Expr = match tokens.peek() {
                Some(Lex { token: Tkn::Semicolon, .. }) => {
                    eat!(tokens);
                    Expr::Undef
                }
                _ => {
                    let expr: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    expr
                }
            };
            Stmt::Ret(expr)
        }
        Some(Lex { token: Tkn::Break, .. }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::Semicolon);
            Stmt::Break
        }
        _ => {
            let a: Expr = get_expr(tokens, 0);
            let lex: Option<&Lex> = tokens.next();
            match lex {
                Some(Lex { token: Tkn::Op("="), .. }) => {
                    let b: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    Stmt::Assign { r#ref: a, expr: b }
                }
                Some(Lex { token: Tkn::Semicolon, .. }) => Stmt::Effect(a),
                _ => panic!(format!("{:?}", lex)),
            }
        }
    }
}

pub(crate) fn get_ast<'a>(tokens: &[Lex<'a>]) -> Vec<Stmt<'a>> {
    let mut ast_tokens: Vec<Lex> = Vec::new();
    let mut comments: Vec<&str> = Vec::new();
    for token in tokens {
        match token {
            Lex { token: Tkn::Comment(x), .. } => comments.push(x),
            _ => ast_tokens.push(*token),
        }
    }
    let mut ast: Vec<Stmt> = Vec::new();
    let mut ast_tokens: Peekable<Iter<Lex>> = ast_tokens.iter().peekable();
    while let Some(_) = ast_tokens.peek() {
        ast.push(get_stmt(&mut ast_tokens));
    }
    ast
}
