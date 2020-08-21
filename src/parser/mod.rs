#[cfg(test)]
mod test;

use crate::tokenizer::{Count, Lex, Tkn};
use std::collections::BTreeSet;
use std::iter::Peekable;
use std::slice::Iter;

const ASSIGN_OPS: [&str; 5] = ["=", "+=", "-=", "*=", "/="];

#[derive(Debug, PartialEq)]
pub(crate) enum Error<'a> {
    Token(Lex<'a>),
    EOF,
}

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
        if let Some(_) = $tokens.next() {
            ()
        } else {
            return Err(Error::EOF);
        }
    }};
}

macro_rules! eat_or_error {
    ($tokens:expr, $x:path $(,)?) => {
        match $tokens.next() {
            Some(Lex { token: $x, .. }) => (),
            Some(token) => return Err(Error::Token(*token)),
            None => return Err(Error::EOF),
        };
    };
}

fn get_ident<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<&'a str, Error<'a>> {
    match tokens.next() {
        Some(Lex { token: Tkn::Ident(x), .. }) => Ok(x),
        Some(token) => Err(Error::Token(*token)),
        None => Err(Error::EOF),
    }
}

fn get_prop<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Prop<'a>, Error<'a>> {
    let key: &str = get_ident(tokens)?;
    eat_or_error!(tokens, Tkn::Colon);
    Ok(Prop { key, value: get_expr(tokens, 0)? })
}

fn get_args<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Vec<&'a str>, Error<'a>> {
    eat_or_error!(tokens, Tkn::LParen);
    let mut args: Vec<&str> = Vec::new();
    while let Some(token) = tokens.next() {
        match token {
            Lex { token: Tkn::Ident(x), .. } => {
                args.push(x);
                match tokens.next() {
                    Some(Lex { token: Tkn::Comma, .. }) => (),
                    Some(Lex { token: Tkn::RParen, .. }) => break,
                    Some(token) => return Err(Error::Token(*token)),
                    None => return Err(Error::EOF),
                }
            }
            Lex { token: Tkn::RParen, .. } => break,
            _ => return Err(Error::Token(*token)),
        }
    }
    Ok(args)
}

fn get_body<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Vec<Syntax<'a>>, Error<'a>> {
    eat_or_error!(tokens, Tkn::LBrace);
    let mut body: Vec<Syntax> = Vec::new();
    while let Some(token) = tokens.peek() {
        if let Lex { token: Tkn::RBrace, .. } = token {
            eat!(tokens);
            break;
        } else {
            body.push(get_stmt(tokens)?)
        }
    }
    Ok(body)
}

fn get_fn<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Expr<'a>, Error<'a>> {
    let args: Vec<&str> = get_args(tokens)?;
    Ok(Expr::Fn { args, body: get_body(tokens)? })
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
) -> Result<Expr<'a>, Error<'a>> {
    let mut expr: Expr = if let Some(token) = tokens.next() {
        match token {
            Lex { token: Tkn::LParen, .. } => {
                let expr: Expr = get_expr(tokens, 0)?;
                eat_or_error!(tokens, Tkn::RParen);
                expr
            }
            Lex { token: Tkn::LBracket, .. } => {
                let mut exprs = Vec::new();
                while let Some(token) = tokens.peek() {
                    match token {
                        Lex { token: Tkn::Comma, .. } => eat!(tokens),
                        Lex { token: Tkn::RBracket, .. } => break,
                        _ => exprs.push(get_expr(tokens, 0)?),
                    }
                }
                eat_or_error!(tokens, Tkn::RBracket);
                Expr::Array(exprs)
            }
            Lex { token: Tkn::Op(x), .. } if !is_assign_op(*x) => {
                let power: u8 = match *x {
                    "new" => 25,
                    "-" | "~" | "!" | "++" | "--" => 21,
                    _ => return Err(Error::Token(*token)),
                };
                Expr::Prefix {
                    op: x,
                    expr: Box::new(get_expr(tokens, power)?),
                }
            }
            Lex { token: Tkn::Num(x), .. } => Expr::Num(x),
            Lex { token: Tkn::Str(x), .. } => Expr::Str(x),
            Lex { token: Tkn::Bool(x), .. } => Expr::Bool(x),
            Lex { token: Tkn::Ident(x), .. } => Expr::Ident(x),
            Lex { token: Tkn::Fn, .. } => get_fn(tokens)?,
            Lex { token: Tkn::Null, .. } => Expr::Null,
            Lex { token: Tkn::Undef, .. } => Expr::Undef,
            Lex { token: Tkn::LBrace, .. } => {
                let mut props: Vec<Prop> = Vec::new();
                while let Some(token) = tokens.peek() {
                    match token {
                        Lex { token: Tkn::Ident(_), .. } => {
                            props.push(get_prop(tokens)?);
                            match tokens.next() {
                                Some(Lex { token: Tkn::Comma, .. }) => (),
                                Some(Lex { token: Tkn::RBrace, .. }) => break,
                                Some(token) => {
                                    return Err(Error::Token(*token))
                                }
                                None => return Err(Error::EOF),
                            }
                        }
                        Lex { token: Tkn::RBrace, .. } => {
                            eat!(tokens);
                            break;
                        }
                        token => return Err(Error::Token(**token)),
                    }
                }
                let mut keys: BTreeSet<&str> = BTreeSet::new();
                for Prop { key, .. } in &props {
                    if !keys.insert(key) {
                        return Err(Error::Token(*token));
                    }
                }
                Expr::Obj(props)
            }
            token => return Err(Error::Token(*token)),
        }
    } else {
        return Err(Error::EOF);
    };
    loop {
        if let Some(token) = tokens.peek() {
            match token {
                Lex { token: Tkn::Op(x), .. } if !is_assign_op(*x) => {
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
                            _ => return Err(Error::Token(**token)),
                        };
                        if l_power < precedence {
                            break;
                        }
                        eat!(tokens);
                        expr = Expr::Infix {
                            op: x,
                            left: Box::new(expr),
                            right: Box::new(get_expr(tokens, r_power)?),
                        };
                    }
                }
                Lex { token: Tkn::LParen, .. } if precedence < 26 => {
                    eat!(tokens);
                    let mut args: Vec<Expr> = Vec::new();
                    while let Some(token) = tokens.peek() {
                        match token {
                            Lex { token: Tkn::Comma, .. } => eat!(tokens),
                            Lex { token: Tkn::RParen, .. } => {
                                eat!(tokens);
                                break;
                            }
                            _ => args.push(get_expr(tokens, 0)?),
                        }
                    }
                    expr = Expr::Call { expr: Box::new(expr), args };
                }
                Lex { token: Tkn::LBracket, .. } if precedence < 26 => {
                    eat!(tokens);
                    let index: Expr = get_expr(tokens, 0)?;
                    eat_or_error!(tokens, Tkn::RBracket);
                    expr = Expr::Access {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Lex { token: Tkn::Ternary, .. } if precedence == 0 => {
                    if let Some(Lex { token: Tkn::Ternary, .. }) =
                        tokens.peek()
                    {
                        eat!(tokens);
                        let if_: Expr = get_expr(tokens, 0)?;
                        eat_or_error!(tokens, Tkn::Colon);
                        let else_: Expr = get_expr(tokens, 0)?;
                        expr = Expr::Ternary {
                            condition: Box::new(expr),
                            if_: Box::new(if_),
                            else_: Box::new(else_),
                        }
                    }
                }
                _ => break,
            }
        } else {
            return Err(Error::EOF);
        }
    }
    Ok(expr)
}

fn get_stmt<'a, 'b>(
    tokens: &mut Peekable<Iter<'b, Lex<'a>>>,
) -> Result<Syntax<'a>, Error<'a>> {
    Ok(match tokens.peek() {
        Some(Lex { token: Tkn::Fn, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens)?;
            let args: Vec<&str> = get_args(tokens)?;
            Syntax {
                statement: Stmt::Fn { ident, args, body: get_body(tokens)? },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::If, line }) => {
            eat!(tokens);
            eat_or_error!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0)?;
            eat_or_error!(tokens, Tkn::RParen);
            let if_: Vec<Syntax> = get_body(tokens)?;
            if let Some(Lex { token: Tkn::Else, .. }) = tokens.peek() {
                eat!(tokens);
                match tokens.peek() {
                    Some(Lex { token: Tkn::If, .. }) => {
                        return Ok(Syntax {
                            statement: Stmt::Cond {
                                condition,
                                if_,
                                else_: vec![get_stmt(tokens)?],
                            },
                            line: *line,
                        });
                    }
                    Some(Lex { token: Tkn::LBrace, .. }) => {
                        return Ok(Syntax {
                            statement: Stmt::Cond {
                                condition,
                                if_,
                                else_: get_body(tokens)?,
                            },
                            line: *line,
                        });
                    }
                    Some(token) => return Err(Error::Token(**token)),
                    None => return Err(Error::EOF),
                }
            }
            Syntax {
                statement: Stmt::Cond { condition, if_, else_: Vec::new() },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::While, line }) => {
            eat!(tokens);
            eat_or_error!(tokens, Tkn::LParen);
            let condition: Expr = get_expr(tokens, 0)?;
            eat_or_error!(tokens, Tkn::RParen);
            let body: Vec<Syntax> = get_body(tokens)?;
            Syntax { statement: Stmt::While { condition, body }, line: *line }
        }
        Some(Lex { token: Tkn::For, line }) => {
            eat!(tokens);
            eat_or_error!(tokens, Tkn::LParen);
            let init: Option<Box<Syntax>> = match tokens.peek() {
                Some(Lex { token: Tkn::Semicolon, .. }) => {
                    eat!(tokens);
                    None
                }
                _ => Some(Box::new(get_stmt(tokens)?)),
            };
            let condition: Option<Expr> = match tokens.peek() {
                Some(Lex { token: Tkn::Semicolon, .. }) => None,
                _ => Some(get_expr(tokens, 0)?),
            };
            eat_or_error!(tokens, Tkn::Semicolon);
            let update: Option<Box<Syntax>> = {
                match tokens.peek() {
                    Some(Lex { token: Tkn::RParen, .. }) => None,
                    Some(Lex { line, .. }) => {
                        let a: Expr = get_expr(tokens, 0)?;
                        match tokens.peek() {
                            Some(Lex { token: Tkn::Op(x), .. })
                                if is_assign_op(*x) =>
                            {
                                eat!(tokens);
                                let b: Expr = get_expr(tokens, 0)?;
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
                            Some(token) => return Err(Error::Token(**token)),
                            None => return Err(Error::EOF),
                        }
                    }
                    None => return Err(Error::EOF),
                }
            };
            eat_or_error!(tokens, Tkn::RParen);
            let body: Vec<Syntax> = get_body(tokens)?;
            Syntax {
                statement: Stmt::For { init, condition, update, body },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::Switch, line }) => {
            eat!(tokens);
            eat_or_error!(tokens, Tkn::LParen);
            let expr: Expr = get_expr(tokens, 0)?;
            eat_or_error!(tokens, Tkn::RParen);
            eat_or_error!(tokens, Tkn::LBrace);
            let mut cases: Vec<Case> = Vec::new();
            while let Some(Lex { token: Tkn::Case, .. }) = tokens.peek() {
                eat!(tokens);
                let expr: Expr = get_expr(tokens, 0)?;
                eat_or_error!(tokens, Tkn::Colon);
                cases.push(Case { expr, body: get_body(tokens)? });
            }
            let default: Vec<Syntax> =
                if let Some(Lex { token: Tkn::Default, .. }) = tokens.peek() {
                    eat!(tokens);
                    eat_or_error!(tokens, Tkn::Colon);
                    get_body(tokens)?
                } else {
                    Vec::new()
                };
            eat_or_error!(tokens, Tkn::RBrace);
            Syntax {
                statement: Stmt::Switch { expr, cases, default },
                line: *line,
            }
        }
        Some(Lex { token: Tkn::Var, line }) => {
            eat!(tokens);
            let ident: &str = get_ident(tokens)?;
            match tokens.next() {
                Some(Lex { token: Tkn::Comma, .. }) => {
                    let mut idents: Vec<&str> = vec![ident];
                    while let Some(token) = tokens.peek() {
                        match token {
                            Lex { token: Tkn::Comma, .. } => eat!(tokens),
                            Lex { token: Tkn::Semicolon, .. } => break,
                            _ => idents.push(get_ident(tokens)?),
                        }
                    }
                    eat_or_error!(tokens, Tkn::Semicolon);
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
                            expr: get_expr(tokens, 0)?,
                        },
                        line: *line,
                    };
                    eat_or_error!(tokens, Tkn::Semicolon);
                    var
                }
                Some(token) => return Err(Error::Token(*token)),
                None => return Err(Error::EOF),
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
                    let expr: Expr = get_expr(tokens, 0)?;
                    eat_or_error!(tokens, Tkn::Semicolon);
                    expr
                }
            };
            Syntax { statement: Stmt::Ret(expr), line: *line }
        }
        Some(Lex { token: Tkn::Break, line }) => {
            eat!(tokens);
            eat_or_error!(tokens, Tkn::Semicolon);
            Syntax { statement: Stmt::Break, line: *line }
        }
        Some(Lex { token: Tkn::LBrace, line }) => {
            Syntax { statement: Stmt::Scope(get_body(tokens)?), line: *line }
        }
        Some(Lex { line, .. }) => {
            let expr_a: Expr = get_expr(tokens, 0)?;
            match tokens.next() {
                Some(Lex { token: Tkn::Op(x), .. }) if is_assign_op(*x) => {
                    let expr_b: Expr = get_expr(tokens, 0)?;
                    eat_or_error!(tokens, Tkn::Semicolon);
                    Syntax {
                        statement: Stmt::Assign {
                            op: x,
                            ident: expr_a,
                            expr: expr_b,
                        },
                        line: *line,
                    }
                }
                Some(Lex { token: Tkn::Semicolon, .. }) => {
                    Syntax { statement: Stmt::Effect(expr_a), line: *line }
                }
                Some(token) => return Err(Error::Token(*token)),
                None => return Err(Error::EOF),
            }
        }
        None => return Err(Error::EOF),
    })
}

pub(crate) fn get_ast<'a>(
    tokens: &[Lex<'a>],
) -> Result<Vec<Syntax<'a>>, Error<'a>> {
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
        ast.push(get_stmt(&mut ast_tokens)?);
    }
    Ok(ast)
}
