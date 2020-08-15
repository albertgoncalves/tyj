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
    Ref(Vec<&'a str>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Label<'a> {
    scope: Vec<&'a str>,
    ident: Vec<&'a str>,
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
        } else {
            return None;
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
        Expr::Ref(_) | Expr::Infix { .. } => {
            if let Some(ident) = get_ref(expr) {
                Some(Type::Ref(ident))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn get_ref<'a>(r#ref: &'a Expr<'a>) -> Option<Vec<&'a str>> {
    match r#ref {
        Expr::Ref(ident) => Some(vec![ident]),
        Expr::Infix { op: ".", left, right } => {
            if let (Some(mut left), Some(mut right)) =
                (get_ref(left), get_ref(right))
            {
                left.append(&mut right);
                Some(left)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub(crate) fn get_types<'a>(ast: &'a [Syntax]) -> Vec<Label<'a>> {
    let mut labels: Vec<Label> = Vec::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                if let Some(r#type) = get_expr(expr) {
                    labels.push(Label {
                        scope: Vec::new(),
                        ident: vec![ident],
                        r#type,
                        line: syntax.line,
                    });
                } else {
                    panic!("{:?}", syntax);
                }
            }
            Stmt::Assign { op: "=", r#ref, expr } => {
                if let (Some(r#type), Some(ident)) =
                    (get_expr(expr), get_ref(r#ref))
                {
                    labels.push(Label {
                        scope: Vec::new(),
                        ident,
                        r#type,
                        line: syntax.line,
                    });
                } else {
                    panic!("{:?}", syntax);
                }
            }
            _ => panic!("{:?}", syntax),
        }
    }
    labels
}
