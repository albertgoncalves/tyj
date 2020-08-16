#[cfg(test)]
mod test;

use crate::tokenizer::{Count, Lex, Tkn};
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
    Ident(&'a str),
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
        if_: Box<Expr<'a>>,
        else_: Box<Expr<'a>>,
    },
    Fn {
        args: Vec<&'a str>,
        body: Vec<Syntax<'a>>,
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
    expr: Expr<'a>,
    body: Vec<Syntax<'a>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'a> {
    Decl {
        ident: &'a str,
        expr: Expr<'a>,
    },
    Decls(Vec<&'a str>),
    Assign {
        op: &'a str,
        ident: Expr<'a>,
        expr: Expr<'a>,
    },
    Ret(Expr<'a>),
    Fn {
        ident: &'a str,
        args: Vec<&'a str>,
        body: Vec<Syntax<'a>>,
    },
    Cond {
        condition: Expr<'a>,
        if_: Vec<Syntax<'a>>,
        else_: Vec<Syntax<'a>>,
    },
    While {
        condition: Expr<'a>,
        body: Vec<Syntax<'a>>,
    },
    For {
        init: Option<Box<Syntax<'a>>>,
        condition: Option<Expr<'a>>,
        update: Option<Box<Syntax<'a>>>,
        body: Vec<Syntax<'a>>,
    },
    Switch {
        expr: Expr<'a>,
        cases: Vec<Case<'a>>,
        default: Vec<Syntax<'a>>,
    },
    Break,
    Scope(Vec<Syntax<'a>>),
    Effect(Expr<'a>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Syntax<'a> {
    pub(crate) statement: Stmt<'a>,
    pub(crate) line: Count,
}

macro_rules! eat {
    ($tokens:expr $(,)?) => {{
        let _: Option<&Lex> = $tokens.next();
    }};
}

macro_rules! eat_or_panic {
    ($tokens:expr, $x:path $(,)?) => {
        let token: Option<&Lex> = $tokens.next();
        let _: () = if let Some(Lex { token: $x, .. }) = token {
            ()
        } else {
            panic!(format!("{:?}", token))
        };
    };
}

fn get_ident<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> &'a str {
    let token: Option<&Lex> = tokens.next();
    if let Some(Lex { token: Tkn::Ident(x), .. }) = token {
        x
    } else {
        panic!(format!("{:?}", token))
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
    while let Some(token) = tokens.next() {
        match token {
            Lex { token: Tkn::Ident(x), .. } => {
                args.push(x);
                let token: Option<&Lex> = tokens.next();
                match token {
                    Some(Lex { token: Tkn::Comma, .. }) => (),
                    Some(Lex { token: Tkn::RParen, .. }) => break,
                    _ => panic!(format!("{:?}", token)),
                }
            }
            Lex { token: Tkn::RParen, .. } => break,
            _ => panic!(format!("{:?}", token)),
        }
    }
    args
}

fn get_body<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Vec<Syntax<'a>> {
    eat_or_panic!(tokens, Tkn::LBrace);
    let mut body: Vec<Syntax> = Vec::new();
    while let Some(token) = tokens.peek() {
        if let Lex { token: Tkn::RBrace, .. } = token {
            eat!(tokens);
            break;
        } else {
            body.push(get_stmt(tokens))
        }
    }
    body
}

fn get_fn<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> Expr<'a> {
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
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
    precedence: u8,
) -> Expr<'a> {
    let token: Option<&Lex> = tokens.next();
    let mut expr: Expr = match token {
        Some(Lex { token: Tkn::LParen, .. }) => {
            let expr: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            expr
        }
        Some(Lex { token: Tkn::LBracket, .. }) => {
            let mut exprs = Vec::new();
            while let Some(token) = tokens.peek() {
                match token {
                    Lex { token: Tkn::Comma, .. } => eat!(tokens),
                    Lex { token: Tkn::RBracket, .. } => break,
                    _ => exprs.push(get_expr(tokens, 0)),
                }
            }
            eat_or_panic!(tokens, Tkn::RBracket);
            Expr::Array(exprs)
        }
        Some(Lex { token: Tkn::Op(x), .. }) if !is_assign_op(*x) => {
            let power: u8 = match *x {
                "new" => 25,
                "-" | "~" | "!" | "++" | "--" => 21,
                _ => panic!(format!("{:?}", token)),
            };
            Expr::Prefix { op: x, expr: Box::new(get_expr(tokens, power)) }
        }
        Some(Lex { token: Tkn::Num(x), .. }) => Expr::Num(x),
        Some(Lex { token: Tkn::Str(x), .. }) => Expr::Str(x),
        Some(Lex { token: Tkn::Bool(x), .. }) => Expr::Bool(x),
        Some(Lex { token: Tkn::Ident(x), .. }) => Expr::Ident(x),
        Some(Lex { token: Tkn::Fn, .. }) => get_fn(tokens),
        Some(Lex { token: Tkn::Null, .. }) => Expr::Null,
        Some(Lex { token: Tkn::Undef, .. }) => Expr::Undef,
        Some(Lex { token: Tkn::LBrace, .. }) => {
            let mut props: Vec<Prop> = Vec::new();
            while let Some(token) = tokens.peek() {
                match token {
                    Lex { token: Tkn::Ident(_), .. } => {
                        props.push(get_prop(tokens));
                        let token: Option<&Lex> = tokens.next();
                        match token {
                            Some(Lex { token: Tkn::Comma, .. }) => (),
                            Some(Lex { token: Tkn::RBrace, .. }) => break,
                            _ => panic!(format!("{:?}", token)),
                        }
                    }
                    Lex { token: Tkn::RBrace, .. } => {
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
            Some(Lex { token: Tkn::Op(x), .. }) if !is_assign_op(*x) => {
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
            Some(Lex { token: Tkn::LParen, .. }) if precedence < 26 => {
                eat!(tokens);
                let mut args: Vec<Expr> = Vec::new();
                while let Some(token) = tokens.peek() {
                    match token {
                        Lex { token: Tkn::Comma, .. } => eat!(tokens),
                        Lex { token: Tkn::RParen, .. } => {
                            eat!(tokens);
                            break;
                        }
                        _ => args.push(get_expr(tokens, 0)),
                    }
                }
                expr = Expr::Call { expr: Box::new(expr), args };
            }
            Some(Lex { token: Tkn::LBracket, .. }) if precedence < 26 => {
                eat!(tokens);
                let index: Expr = get_expr(tokens, 0);
                eat_or_panic!(tokens, Tkn::RBracket);
                expr = Expr::Access {
                    expr: Box::new(expr),
                    index: Box::new(index),
                };
            }
            Some(Lex { token: Tkn::Ternary, .. }) if precedence == 0 => {
                if let Some(Lex { token: Tkn::Ternary, .. }) = tokens.peek() {
                    eat!(tokens);
                    let if_: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Colon);
                    let else_: Expr = get_expr(tokens, 0);
                    expr = Expr::Ternary {
                        condition: Box::new(expr),
                        if_: Box::new(if_),
                        else_: Box::new(else_),
                    }
                }
            }
            _ => break,
        }
    }
    expr
}

fn get_stmt<'a, 'b>(tokens: &mut Peekable<Iter<'b, Lex<'a>>>) -> Syntax<'a> {
    let token: Option<&&Lex> = tokens.peek();
    match token {
        Some(Lex { token: Tkn::Fn, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let args: Vec<&str> = get_args(tokens);
            Syntax {
                statement: Stmt::Fn { ident, args, body: get_body(tokens) },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::If, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            let if_: Vec<Syntax> = get_body(tokens);
            if let Some(Lex { token: Tkn::Else, .. }) = tokens.peek() {
                eat!(tokens);
                match tokens.peek() {
                    Some(Lex { token: Tkn::If, .. }) => {
                        return Syntax {
                            statement: Stmt::Cond {
                                condition,
                                if_,
                                else_: vec![get_stmt(tokens)],
                            },
                            line: *line,
                        };
                    }
                    Some(Lex { token: Tkn::LBrace, .. }) => {
                        return Syntax {
                            statement: Stmt::Cond {
                                condition,
                                if_,
                                else_: get_body(tokens),
                            },
                            line: *line,
                        };
                    }
                    _ => panic!(format!("{:?}", tokens.peek())),
                }
            }
            Syntax {
                statement: Stmt::Cond { condition, if_, else_: Vec::new() },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::While, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0);
            eat_or_panic!(tokens, Tkn::RParen);
            let body: Vec<Syntax> = get_body(tokens);
            Syntax { statement: Stmt::While { condition, body }, line: *line }
        }
        Some(Lex { token: Tkn::For, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::LParen);
            let init: Option<Box<Syntax>> = match tokens.peek() {
                Some(Lex { token: Tkn::Semicolon, .. }) => {
                    eat!(tokens);
                    None
                }
                _ => Some(Box::new(get_stmt(tokens))),
            };
            let condition: Option<Expr> = match tokens.peek() {
                Some(Lex { token: Tkn::Semicolon, .. }) => None,
                _ => Some(get_expr(tokens, 0)),
            };
            eat_or_panic!(tokens, Tkn::Semicolon);
            let update: Option<Box<Syntax>> = {
                let token: Option<&&Lex> = tokens.peek();
                match token {
                    Some(Lex { token: Tkn::RParen, .. }) => None,
                    Some(Lex { line, .. }) => {
                        let a: Expr = get_expr(tokens, 0);
                        let token: Option<&&Lex> = tokens.peek();
                        match token {
                            Some(Lex { token: Tkn::Op(x), .. })
                                if is_assign_op(*x) =>
                            {
                                eat!(tokens);
                                let b: Expr = get_expr(tokens, 0);
                                Some(Box::new(Syntax {
                                    statement: Stmt::Assign {
                                        op: x,
                                        ident: a,
                                        expr: b,
                                    },
                                    line: *line,
                                }))
                            }
                            Some(Lex { token: Tkn::RParen, .. }) => {
                                Some(Box::new(Syntax {
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
            let body: Vec<Syntax> = get_body(tokens);
            Syntax {
                statement: Stmt::For { init, condition, update, body },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::Switch, line }) => {
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
            let default: Vec<Syntax> =
                if let Some(Lex { token: Tkn::Default, .. }) = tokens.peek() {
                    eat!(tokens);
                    eat_or_panic!(tokens, Tkn::Colon);
                    get_body(tokens)
                } else {
                    Vec::new()
                };
            eat_or_panic!(tokens, Tkn::RBrace);
            Syntax {
                statement: Stmt::Switch { expr, cases, default },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::Var, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens);
            let token: Option<&Lex> = tokens.next();
            match token {
                Some(Lex { token: Tkn::Comma, .. }) => {
                    let mut idents: Vec<&str> = vec![ident];
                    while let Some(token) = tokens.peek() {
                        match token {
                            Lex { token: Tkn::Comma, .. } => eat!(tokens),
                            Lex { token: Tkn::Semicolon, .. } => break,
                            _ => idents.push(get_ident(tokens)),
                        }
                    }
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    Syntax { statement: Stmt::Decls(idents), line: *line }
                }
                Some(Lex { token: Tkn::Semicolon, .. }) => Syntax {
                    statement: Stmt::Decl { ident, expr: Expr::Uninit },
                    line: *line,
                },
                Some(Lex { token: Tkn::Op("="), .. }) => {
                    let var: Syntax = Syntax {
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
        Some(Lex { token: Tkn::Ret, line }) => {
            eat!(tokens);
            let expr: Expr = {
                if let Some(Lex { token: Tkn::Semicolon, .. }) = tokens.peek()
                {
                    eat!(tokens);
                    Expr::Undef
                } else {
                    let expr: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    expr
                }
            };
            Syntax { statement: Stmt::Ret(expr), line: *line }
        }
        Some(Lex { token: Tkn::Break, line }) => {
            eat!(tokens);
            eat_or_panic!(tokens, Tkn::Semicolon);
            Syntax { statement: Stmt::Break, line: *line }
        }
        Some(Lex { token: Tkn::LBrace, line }) => {
            Syntax { statement: Stmt::Scope(get_body(tokens)), line: *line }
        }
        Some(Lex { line, .. }) => {
            let a: Expr = get_expr(tokens, 0);
            let token: Option<&Lex> = tokens.next();
            match token {
                Some(Lex { token: Tkn::Op(x), .. }) if is_assign_op(*x) => {
                    let b: Expr = get_expr(tokens, 0);
                    eat_or_panic!(tokens, Tkn::Semicolon);
                    Syntax {
                        statement: Stmt::Assign { op: x, ident: a, expr: b },
                        line: *line,
                    }
                }
                Some(Lex { token: Tkn::Semicolon, .. }) => {
                    Syntax { statement: Stmt::Effect(a), line: *line }
                }
                _ => panic!(format!("{:?}", token)),
            }
        }
        _ => panic!(format!("{:?}", token)),
    }
}

pub(crate) fn get_ast<'a>(tokens: &[Lex<'a>]) -> Vec<Syntax<'a>> {
    let mut ast_tokens: Vec<Lex> = Vec::new();
    let mut comments: Vec<&str> = Vec::new();
    for token in tokens {
        if let Lex { token: Tkn::Comment(x), .. } = token {
            comments.push(x)
        } else {
            ast_tokens.push(*token)
        }
    }
    let mut ast: Vec<Syntax> = Vec::new();
    let mut ast_tokens: Peekable<Iter<Lex>> = ast_tokens.iter().peekable();
    while let Some(_) = ast_tokens.peek() {
        ast.push(get_stmt(&mut ast_tokens));
    }
    ast
}
