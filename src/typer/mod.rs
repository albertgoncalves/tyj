#[cfg(test)]
mod test;

use crate::parser::{Expr, Stmt, Syntax};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub(crate) enum Error<'a, 'b> {
    ShadowIdent(&'b Syntax<'a>),
    UnknownIdent(&'b Syntax<'a>),
    UnhandledExpr(&'b Syntax<'a>),
    UnhandledSyntax(&'b Syntax<'a>),
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

pub(crate) fn get_types<'a, 'b>(
    ast: &'b [Syntax<'a>],
) -> Result<(Vec<Type<'a>>, HashMap<Vec<&'a str>, TypeIndex>), Error<'a, 'b>> {
    let mut types: Vec<Type> = Vec::new();
    let mut indices: HashMap<Vec<&str>, TypeIndex> = HashMap::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                let key: Vec<&str> = vec![ident];
                if indices.contains_key(&key) {
                    return Err(Error::ShadowIdent(syntax));
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
                            return Err(Error::UnknownIdent(syntax));
                        }
                    }
                    _ => return Err(Error::UnhandledExpr(syntax)),
                }
                let _: Option<_> = indices.insert(key, index);
            }
            _ => return Err(Error::UnhandledSyntax(syntax)),
        }
    }
    Ok((types, indices))
}
