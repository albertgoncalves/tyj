#[cfg(test)]
mod test;

use crate::tokenizer::{Count, Tkn, TknPlus};
use std::iter::Peekable;
use std::slice::Iter;

const ASSIGN_OPS: [&str; 5] = ["=", "+=", "-=", "*=", "/="];

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
    Array(Vec<Expr<'a>>),
    Ref(&'a str),
    Prefix {
        op: &'a str,
        expr: Box<Expr<'a>>,
    },
    Infix {
        op: &'a str,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Postfix {
        op: &'a str,
        expr: Box<Expr<'a>>,
    },
    Ternary {
        condition: Box<Expr<'a>>,
        r#if: Box<Expr<'a>>,
        r#else: Box<Expr<'a>>,
    },
    Fn {
        args: Vec<&'a str>,
        body: Vec<StmtPlus<'a>>,
    },
    Call {
        expr: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
    Access {
        expr: Box<Expr<'a>>,
        index: Box<Expr<'a>>,
    },
    Null,
    Undef,
    Uninit,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Case<'a> {
    pub(crate) expr: Expr<'a>,
    pub(crate) body: Vec<StmtPlus<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'a> {
    Decl {
        ident: &'a str,
        expr: Expr<'a>,
    },
    Assign {
        op: &'a str,
        r#ref: Expr<'a>,
        expr: Expr<'a>,
    },
    Ret(Expr<'a>),
    Fn {
        ident: &'a str,
        args: Vec<&'a str>,
        body: Vec<StmtPlus<'a>>,
    },
    Cond {
        condition: Expr<'a>,
        r#if: Vec<StmtPlus<'a>>,
        r#else: Vec<StmtPlus<'a>>,
    },
    While {
        condition: Expr<'a>,
        body: Vec<StmtPlus<'a>>,
    },
    For {
        init: Option<Box<StmtPlus<'a>>>,
        condition: Option<Expr<'a>>,
        update: Option<Box<StmtPlus<'a>>>,
        body: Vec<StmtPlus<'a>>,
    },
    Switch {
        expr: Expr<'a>,
        cases: Vec<Case<'a>>,
        default: Vec<StmtPlus<'a>>,
    },
    Break,
    Scope(Vec<StmtPlus<'a>>),
    Effect(Expr<'a>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct StmtPlus<'a> {
    pub(crate) statement: Stmt<'a>,
    pub(crate) line: Count,
}

macro_rules! eat {
    ($tokens:expr $(,)?) => {{
        let _: Option<&TknPlus> = $tokens.next();
    }};
}

macro_rules! eat_or_panic {
    ($tokens:expr, $x:path $(,)?) => {
        let token: Option<&TknPlus> = $tokens.next();
        let _: () = if let Some(TknPlus { token: $x, .. }) = token {
            ()
        } else {
            panic!(format!("{:?}", token))
        };
    };
}

fn get_ident<'a, 'b>(tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>) -> &'a str {
    let token: Option<&TknPlus> = tokens.next();
    if let Some(TknPlus { token: Tkn::Ident(x), .. }) = token {
        x
    } else {
        panic!(format!("{:?}", token))
    }
}

fn get_prop<'a, 'b>(tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>) -> Prop<'a> {
    let key: &str = get_ident(tokens);
    eat_or_panic!(tokens, Tkn::Colon);
    Prop { key, value: get_expr(tokens, 0) }
}

fn get_args<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>,
) -> Vec<&'a str> {
    eat_or_panic!(tokens, Tkn::LParen);
    let mut args: Vec<&str> = Vec::new();
    while let Some(token) = tokens.next() {
        match token {
            TknPlus { token: Tkn::Ident(x), .. } => {
                args.push(x);
                let token: Option<&TknPlus> = tokens.next();
                match token {
                    Some(TknPlus { token: Tkn::Comma, .. }) => (),
                    Some(TknPlus { token: Tkn::RParen, .. }) => break,
                    _ => panic!(format!("{:?}", token)),
                }
            }
            TknPlus { token: Tkn::RParen, .. } => break,
            _ => panic!(format!("{:?}", token)),
        }
    }
    args
}

fn get_body<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>,
) -> Vec<StmtPlus<'a>> {
    eat_or_panic!(tokens, Tkn::LBrace);
    let mut body: Vec<StmtPlus> = Vec::new();
    while let Some(token) = tokens.peek() {
        if let TknPlus { token: Tkn::RBrace, .. } = token {
            eat!(tokens);
            break;
        } else {
            body.push(get_stmt(tokens))
        }
    }
    body
}

fn get_fn<'a, 'b>(tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>) -> Expr<'a> {
    let args: Vec<&str> = get_args(tokens);
    Expr::Fn { args, body: get_body(tokens) }
}

fn is_assign_op(x: &str) -> bool {
    for op in &ASSIGN_OPS {
        if x == *op {
            return true;
        }
    }
    false
}

fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>,
    precedence: u8,
) -> Expr<'a> {
    let token: Option<&TknPlus> = tokens.next();
    let mut expr: Expr = match token {
        Some(TknPlus { token: Tkn::LParen, .. }) => {
            let expr: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            expr
        }
        Some(TknPlus { token: Tkn::LBracket, .. }) => {
            let mut exprs = Vec::new();
            while let Some(token) = tokens.peek() {
                match token {
                    TknPlus { token: Tkn::Comma, .. } => eat!(tokens),
                    TknPlus { token: Tkn::RBracket, .. } => break,
                    _ => exprs.push(get_expr(tokens, 0)),
                }
            }
            eat_or_panic!(tokens, Tkn::RBracket);
            Expr::Array(exprs)
        }
        Some(TknPlus { token: Tkn::Op(x), .. }) if !is_assign_op(*x) => {
            let power: u8 = match *x {
                "new" => 25,
                "-" | "~" | "!" | "++" | "--" => 21,
                _ => panic!(format!("{:?}", token)),
            };
            Expr::Prefix { op: x, expr: Box::new(get_expr(tokens, power)) }
        }
        Some(TknPlus { token: Tkn::Num(x), .. }) => Expr::Num(x),
        Some(TknPlus { token: Tkn::Str(x), .. }) => Expr::Str(x),
        Some(TknPlus { token: Tkn::Bool(x), .. }) => Expr::Bool(x),
        Some(TknPlus { token: Tkn::Ident(x), .. }) => Expr::Ref(x),
        Some(TknPlus { token: Tkn::Fn, .. }) => get_fn(tokens),
        Some(TknPlus { token: Tkn::Null, .. }) => Expr::Null,
        Some(TknPlus { token: Tkn::Undef, .. }) => Expr::Undef,
        Some(TknPlus { token: Tkn::LBrace, .. }) => {
            let mut props: Vec<Prop> = Vec::new();
            while let Some(token) = tokens.peek() {
                match token {
                    TknPlus { token: Tkn::Ident(_), .. } => {
                        props.push(get_prop(tokens));
                        let token: Option<&TknPlus> = tokens.next();
                        match token {
                            Some(TknPlus { token: Tkn::Comma, .. }) => (),
                            Some(TknPlus { token: Tkn::RBrace, .. }) => break,
                            _ => panic!(format!("{:?}", token)),
                        }
                    }
                    TknPlus { token: Tkn::RBrace, .. } => {
                        eat!(tokens);
                        break;
                    }
                    _ => panic!(format!("{:?}", token)),
                }
            }
            Expr::Obj(props)
        }
        _ => panic!(format!("{:?}", token)),
    };
    loop {
        match tokens.peek() {
            Some(TknPlus { token: Tkn::Op(x), .. }) if !is_assign_op(*x) => {
                let power: Option<u8> = match *x {
                    "++" | "--" => Some(23),
                    _ => None,
                };
                if let Some(power) = power {
                    if power < precedence {
                        break;
                    }
                    eat!(tokens);
                    expr = Expr::Postfix { op: x, expr: Box::new(expr) };
                } else {
                    let (l_power, r_power): (u8, u8) = match *x {
                        "." => (25, 26),
                        "*" | "/" | "%" => (19, 20),
                        "+" | "-" => (17, 18),
                        "<<" | ">>" | ">>>" => (15, 16),
                        "<" | ">" | "<=" | ">=" => (13, 14),
                        "===" | "!==" => (11, 12),
                        "&" => (9, 10),
                        "^" => (7, 8),
                        "|" => (5, 6),
                        "&&" => (3, 4),
                        "||" => (1, 2),
                        _ => panic!(format!("{:?}", token)),
                    };
                    if l_power < precedence {
                        break;
                    }
                    eat!(tokens);
                    expr = Expr::Infix {
                        op: x,
                        left: Box::new(expr),
                        right: Box::new(get_expr(tokens, r_power)),
                    };
                }
            }
            Some(TknPlus { token: Tkn::LParen, .. }) if precedence < 26 => {
                eat!(tokens);
                let mut args: Vec<Expr> = Vec::new();
                while let Some(token) = tokens.peek() {
                    match token {
                        TknPlus { token: Tkn::Comma, .. } => eat!(tokens),
                        TknPlus { token: Tkn::RParen, .. } => {
                            eat!(tokens);
                            break;
                        }
                        _ => args.push(get_expr(tokens, 0)),
                    }
                }
                expr = Expr::Call { expr: Box::new(expr), args };
            }
            Some(TknPlus { token: Tkn::LBracket, .. }) if precedence < 26 => {
                eat!(tokens);
                let index: Expr = get_expr(tokens, 0);
                eat_or_panic!(tokens, Tkn::RBracket);
                expr = Expr::Access {
                    expr: Box::new(expr),
                    index: Box::new(index),
                };
            }
            Some(TknPlus { token: Tkn::Ternary, .. }) if precedence == 0 => {
                if let Some(TknPlus { token: Tkn::Ternary, .. }) =
                    tokens.peek()
                {
                    eat!(tokens);
                    let r#if: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Colon);
                    let r#else: Expr = get_expr(tokens, 0);
                    expr = Expr::Ternary {
                        condition: Box::new(expr),
                        r#if: Box::new(r#if),
                        r#else: Box::new(r#else),
                    }
                }
            }
            _ => break,
        }
    }
    expr
}

fn get_stmt<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, TknPlus<'a>>>,
) -> StmtPlus<'a> {
    let token: Option<&&TknPlus> = tokens.peek();
    match token {
        Some(TknPlus { token: Tkn::Fn, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let args: Vec<&str> = get_args(tokens);
            StmtPlus {
                statement: Stmt::Fn { ident, args, body: get_body(tokens) },
                line: *line,
            }
        }
        Some(TknPlus { token: Tkn::If, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            let r#if: Vec<StmtPlus> = get_body(tokens);
            if let Some(TknPlus { token: Tkn::Else, .. }) = tokens.peek() {
                eat!(tokens);
                match tokens.peek() {
                    Some(TknPlus { token: Tkn::If, .. }) => {
                        return StmtPlus {
                            statement: Stmt::Cond {
                                condition,
                                r#if,
                                r#else: vec![get_stmt(tokens)],
                            },
                            line: *line,
                        };
                    }
                    Some(TknPlus { token: Tkn::LBrace, .. }) => {
                        return StmtPlus {
                            statement: Stmt::Cond {
                                condition,
                                r#if,
                                r#else: get_body(tokens),
                            },
                            line: *line,
                        };
                    }
                    _ => panic!(format!("{:?}", tokens.peek())),
                }
            }
            StmtPlus {
                statement: Stmt::Cond { condition, r#if, r#else: Vec::new() },
                line: *line,
            }
        }
        Some(TknPlus { token: Tkn::While, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            let body: Vec<StmtPlus> = get_body(tokens);
            StmtPlus {
                statement: Stmt::While { condition, body },
                line: *line,
            }
        }
        Some(TknPlus { token: Tkn::For, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let init: Option<Box<StmtPlus>> = match tokens.peek() {
                Some(TknPlus { token: Tkn::Semicolon, .. }) => {
                    eat!(tokens);
                    None
                }
                _ => Some(Box::new(get_stmt(tokens))),
            };
            let condition: Option<Expr> = match tokens.peek() {
                Some(TknPlus { token: Tkn::Semicolon, .. }) => None,
                _ => Some(get_expr(tokens, 0)),
            };
            eat_or_panic!(tokens, Tkn::Semicolon);
            let update: Option<Box<StmtPlus>> = {
                let token: Option<&&TknPlus> = tokens.peek();
                match token {
                    Some(TknPlus { token: Tkn::RParen, .. }) => None,
                    Some(TknPlus { line, .. }) => {
                        let a: Expr = get_expr(tokens, 0);
                        let token: Option<&&TknPlus> = tokens.peek();
                        match token {
                            Some(TknPlus { token: Tkn::Op(x), .. })
                                if is_assign_op(*x) =>
                            {
                                eat!(tokens);
                                let b: Expr = get_expr(tokens, 0);
                                Some(Box::new(StmtPlus {
                                    statement: Stmt::Assign {
                                        op: x,
                                        r#ref: a,
                                        expr: b,
                                    },
                                    line: *line,
                                }))
                            }
                            Some(TknPlus { token: Tkn::RParen, .. }) => {
                                Some(Box::new(StmtPlus {
                                    statement: Stmt::Effect(a),
                                    line: *line,
                                }))
                            }
                            _ => panic!(format!("{:?}", token)),
                        }
                    }
                    _ => panic!(format!("{:?}", token)),
                }
            };
            eat_or_panic!(tokens, Tkn::RParen);
            let body: Vec<StmtPlus> = get_body(tokens);
            StmtPlus {
                statement: Stmt::For { init, condition, update, body },
                line: *line,
            }
        }
        Some(TknPlus { token: Tkn::Switch, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let expr: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            eat_or_panic!(tokens, Tkn::LBrace);
            let mut cases: Vec<Case> = Vec::new();
            while let Some(TknPlus { token: Tkn::Case, .. }) = tokens.peek() {
                eat!(tokens);
                let expr: Expr = get_expr(tokens, 0);
                eat_or_panic!(tokens, Tkn::Colon);
                cases.push(Case { expr, body: get_body(tokens) });
            }
            let default: Vec<StmtPlus> =
                if let Some(TknPlus { token: Tkn::Default, .. }) =
                    tokens.peek()
                {
                    eat!(tokens);
                    eat_or_panic!(tokens, Tkn::Colon);
                    get_body(tokens)
                } else {
                    Vec::new()
                };
            eat_or_panic!(tokens, Tkn::RBrace);
            StmtPlus {
                statement: Stmt::Switch { expr, cases, default },
                line: *line,
            }
        }
        Some(TknPlus { token: Tkn::Var, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let token: Option<&TknPlus> = tokens.next();
            match token {
                Some(TknPlus { token: Tkn::Semicolon, .. }) => StmtPlus {
                    statement: Stmt::Decl { ident, expr: Expr::Uninit },
                    line: *line,
                },
                Some(TknPlus { token: Tkn::Op("="), .. }) => {
                    let var: StmtPlus = StmtPlus {
                        statement: Stmt::Decl {
                            ident,
                            expr: get_expr(tokens, 0),
                        },
                        line: *line,
                    };
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    var
                }
                _ => panic!(format!("{:?}", token)),
            }
        }
        Some(TknPlus { token: Tkn::Ret, line }) => {
            eat!(tokens);
            let expr: Expr = {
                if let Some(TknPlus { token: Tkn::Semicolon, .. }) =
                    tokens.peek()
                {
                    eat!(tokens);
                    Expr::Undef
                } else {
                    let expr: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    expr
                }
            };
            StmtPlus { statement: Stmt::Ret(expr), line: *line }
        }
        Some(TknPlus { token: Tkn::Break, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::Semicolon);
            StmtPlus { statement: Stmt::Break, line: *line }
        }
        Some(TknPlus { token: Tkn::LBrace, line }) => {
            StmtPlus { statement: Stmt::Scope(get_body(tokens)), line: *line }
        }
        Some(TknPlus { line, .. }) => {
            let a: Expr = get_expr(tokens, 0);
            let token: Option<&TknPlus> = tokens.next();
            match token {
                Some(TknPlus { token: Tkn::Op(x), .. })
                    if is_assign_op(*x) =>
                {
                    let b: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    StmtPlus {
                        statement: Stmt::Assign { op: x, r#ref: a, expr: b },
                        line: *line,
                    }
                }
                Some(TknPlus { token: Tkn::Semicolon, .. }) => {
                    StmtPlus { statement: Stmt::Effect(a), line: *line }
                }
                _ => panic!(format!("{:?}", token)),
            }
        }
        _ => panic!(format!("{:?}", token)),
    }
}

pub(crate) fn get_ast<'a>(tokens: &[TknPlus<'a>]) -> Vec<StmtPlus<'a>> {
    let mut ast_tokens: Vec<TknPlus> = Vec::new();
    let mut comments: Vec<&str> = Vec::new();
    for token in tokens {
        if let TknPlus { token: Tkn::Comment(x), .. } = token {
            comments.push(x)
        } else {
            ast_tokens.push(*token)
        }
    }
    let mut ast: Vec<StmtPlus> = Vec::new();
    let mut ast_tokens: Peekable<Iter<TknPlus>> = ast_tokens.iter().peekable();
    while let Some(_) = ast_tokens.peek() {
        ast.push(get_stmt(&mut ast_tokens));
    }
    ast
}
