#[cfg(test)]
mod test;

use crate::parser::{Expr, Stmt, Syntax};
use std::collections::HashMap;

const SHADOW_IDENT: &str = "Shadowed Identifier";
const UNKNOWN_IDENT: &str = "Unknown Identifier";
const UNHANDLED_EXPR: &str = "Unhandled Expression";
const UNHANDLED_SYNTAX: &str = "Unhandled Syntax";

#[derive(Debug, PartialEq)]
pub(crate) struct Error<'a, 'b, 'c> {
    pub(crate) syntax: &'b Syntax<'a>,
    pub(crate) message: &'c str,
}

type TypeIndex = usize;

#[derive(Debug, PartialEq)]
pub(crate) struct Prop<'a> {
    key: &'a str,
    value: TypeIndex,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(Vec<Prop<'a>>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Table<'a> {
    pub(crate) types: Vec<Type<'a>>,
    pub(crate) indices: HashMap<Vec<&'a str>, TypeIndex>,
}

pub(crate) fn get_types<'a, 'b, 'c>(
    ast: &'b [Syntax<'a>],
) -> Result<Table<'a>, Error<'a, 'b, 'c>> {
    let mut types: Vec<Type> = Vec::new();
    let mut indices: HashMap<Vec<&str>, TypeIndex> = HashMap::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                let key: Vec<&str> = vec![ident];
                if indices.contains_key(&key) {
                    return Err(Error { syntax, message: SHADOW_IDENT });
                }
                let mut index: TypeIndex = types.len();
                match expr {
                    Expr::Num(_) => types.push(Type::Num),
                    Expr::Str(_) => types.push(Type::Str),
                    Expr::Bool(_) => types.push(Type::Bool),
                    Expr::Null => types.push(Type::Null),
                    Expr::Undef => types.push(Type::Undef),
                    Expr::Ident(x) => {
                        if let Some(i) = indices.get(&vec![*x]) {
                            index = *i;
                        } else {
                            return Err(Error {
                                syntax,
                                message: UNKNOWN_IDENT,
                            });
                        }
                    }
                    _ => {
                        return Err(Error { syntax, message: UNHANDLED_EXPR })
                    }
                }
                let _: Option<_> = indices.insert(key, index);
            }
            _ => return Err(Error { syntax, message: UNHANDLED_SYNTAX }),
        }
    }
    Ok(Table { types, indices })
}
