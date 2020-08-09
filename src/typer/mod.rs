#[cfg(test)]
mod test;

use crate::parser::{Expr, Prop as ParserProp, Stmt, Syntax};
use crate::tokenizer::Count;

#[derive(Debug, Eq, Ord, PartialOrd, PartialEq)]
pub(crate) struct Prop<'a> {
    key: &'a str,
    value: Type<'a>,
}

#[derive(Debug, Eq, Ord, PartialOrd, PartialEq)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(Vec<Prop<'a>>),
    Array(Vec<Type<'a>>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Label<'a> {
    ident: &'a str,
    r#type: Type<'a>,
    line: Count,
}

fn get_props<'a>(parser_props: &'a [ParserProp]) -> Option<Type<'a>> {
    let mut type_props: Vec<Prop<'a>> = Vec::new();
    for prop in parser_props {
        if let Some(r#type) = get_expr(&prop.value) {
            type_props.push(Prop { key: prop.key, value: r#type })
        } else {
            return None;
        }
    }
    Some(Type::Obj(type_props))
}

fn get_array<'a>(exprs: &'a [Expr]) -> Option<Type<'a>> {
    let mut types: Vec<Type> = Vec::new();
    for expr in exprs {
        if let Some(r#type) = get_expr(expr) {
            types.push(r#type)
        }
    }
    types.sort_unstable();
    types.dedup();
    Some(Type::Array(types))
}

fn get_expr<'a>(expr: &'a Expr<'a>) -> Option<Type<'a>> {
    match expr {
        Expr::Num(_) => Some(Type::Num),
        Expr::Str(_) => Some(Type::Str),
        Expr::Bool(_) => Some(Type::Bool),
        Expr::Null => Some(Type::Null),
        Expr::Undef => Some(Type::Undef),
        Expr::Obj(x) => get_props(x),
        Expr::Array(x) => get_array(x),
        _ => None,
    }
}

pub(crate) fn get_types<'a>(ast: &'a [Syntax]) -> Vec<Label<'a>> {
    let mut labels: Vec<Label> = Vec::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                if let Some(r#type) = get_expr(expr) {
                    labels.push(Label { ident, r#type, line: syntax.line });
                } else {
                    panic!("{:?}", syntax);
                }
            }
            _ => panic!("{:?}", syntax),
        }
    }
    labels
}
