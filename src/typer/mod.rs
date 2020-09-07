#[cfg(test)]
mod test;

use crate::parser::{Expr, Stmt, Syntax};
use crate::tokenizer::{Asn, Op};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub(crate) enum Message {
    ArrayMultiType,
    AssignNonIdent,
    IdentShadow,
    IdentUninit,
    IdentUnknown,
    IncompatibleTypes,
    NonIdentMember,
    ObjDuplicateKeys,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Error<'a> {
    pub(crate) syntax: &'a Syntax<'a>,
    pub(crate) message: Message,
}

struct Ident<'a, 'b> {
    unwrap: &'b [&'a str],
}

struct Scope<'a, 'b> {
    unwrap: &'b [&'a str],
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Target<'a> {
    ident: Vec<&'a str>,
    scope: Vec<&'a str>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(Rc<BTreeMap<&'a str, Type<'a>>>),
    EmptyArray,
    Array(Rc<Type<'a>>),
    Uninit,
}

fn get_idents<'a>(expr: &'a Expr<'a>) -> Result<Vec<&'a str>, Message> {
    Ok(match expr {
        Expr::Ident(ident) => vec![ident],
        Expr::Infix { op: Op::Member, left, right } => {
            let mut idents: Vec<&str> = get_idents(left)?;
            idents.extend_from_slice(&get_idents(right)?);
            idents
        }
        _ => return Err(Message::NonIdentMember),
    })
}

fn deref_ident<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    ident: &'b Ident<'a, 'b>,
    types: &'b HashMap<Target<'a>, Type<'a>>,
) -> Option<&'b Type<'a>> {
    let mut scope: Vec<&str> = scope.unwrap.to_vec();
    loop {
        let type_: Option<&Type> = types.get(&Target {
            ident: ident.unwrap.to_vec(),
            scope: scope.to_vec(),
        });
        if type_.is_some() || scope.pop().is_none() {
            return type_;
        }
    }
}

fn get_expr<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    types: &'b HashMap<Target<'a>, Type<'a>>,
    expr: &'a Expr<'a>,
) -> Result<Type<'a>, Message> {
    macro_rules! deref_ident {
        ($ident:expr $(,)?) => {
            match deref_ident(scope, $ident, types) {
                Some(Type::Uninit) => return Err(Message::IdentUninit),
                Some(type_) => type_.clone(),
                None => return Err(Message::IdentUnknown),
            }
        };
    }

    Ok(match expr {
        Expr::Ident(ident) => deref_ident!(&Ident { unwrap: &[*ident] }),
        Expr::Num(_) => Type::Num,
        Expr::Str(_) => Type::Str,
        Expr::Bool(_) => Type::Bool,
        Expr::Null => Type::Null,
        Expr::Undef => Type::Undef,
        Expr::Uninit => Type::Uninit,
        Expr::Obj(parse_props) => {
            let mut type_props: BTreeMap<&str, Type> = BTreeMap::new();
            for prop in parse_props {
                if type_props
                    .insert(prop.key, get_expr(scope, types, &prop.value)?)
                    .is_some()
                {
                    return Err(Message::ObjDuplicateKeys);
                }
            }
            Type::Obj(Rc::new(type_props))
        }
        Expr::Array(elems) => {
            let mut type_elems: BTreeSet<Type> = BTreeSet::new();
            for elem in elems {
                let _: bool = type_elems.insert(get_expr(scope, types, elem)?);
            }
            let mut type_: Type = Type::EmptyArray;
            for elem in &type_elems {
                match type_ {
                    Type::EmptyArray => {
                        type_ = Type::Array(Rc::new(elem.clone()))
                    }
                    _ => return Err(Message::ArrayMultiType),
                }
            }
            type_
        }
        Expr::Prefix { op, expr } => {
            match (get_expr(scope, types, expr)?, op) {
                (Type::Num, Op::BitwiseNot)
                | (Type::Num, Op::Sub)
                | (Type::Num, Op::Increment)
                | (Type::Num, Op::Decrement) => Type::Num,
                (Type::Bool, Op::Not) => Type::Bool,
                (_, Op::New) => panic!("{:#?} {:#?}", op, expr),
                _ => return Err(Message::IncompatibleTypes),
            }
        }
        Expr::Infix { op: Op::Member, .. } => {
            deref_ident!(&Ident { unwrap: &get_idents(expr)? })
        }
        _ => panic!("{:#?}", expr),
    })
}

fn set_type<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    ident: &'b Ident<'a, 'b>,
    types: &'b mut HashMap<Target<'a>, Type<'a>>,
    type_: &'b Type<'a>,
) -> Result<(), Message> {
    if let Type::Obj(props) = type_ {
        for (key, value) in props.iter() {
            let mut ident: Vec<&str> = ident.unwrap.to_vec();
            ident.push(key);
            set_type(scope, &Ident { unwrap: &ident }, types, &value)?;
        }
    }
    if types
        .insert(
            Target {
                ident: ident.unwrap.to_vec(),
                scope: scope.unwrap.to_vec(),
            },
            type_.clone(),
        )
        .is_some()
    {
        Err(Message::ObjDuplicateKeys)
    } else {
        Ok(())
    }
}

fn set_assign<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    ident_expr: &'a Expr<'a>,
    types: &'b mut HashMap<Target<'a>, Type<'a>>,
    value_expr: &'a Expr<'a>,
) -> Result<(), Message> {
    let ident: Vec<&str> = match ident_expr {
        Expr::Ident(ident) => vec![ident],
        Expr::Infix { op: Op::Member, .. } => get_idents(ident_expr)?,
        _ => return Err(Message::AssignNonIdent),
    };
    let expr_type: Type = get_expr(scope, &types, value_expr)?;
    let ident_type: Type = match types
        .get(&Target { ident: ident.to_vec(), scope: scope.unwrap.to_vec() })
    {
        Some(type_) => type_.clone(),
        None => return Err(Message::IdentUnknown),
    };
    match ident_type {
        Type::Uninit => {
            let _: Option<_> = types.insert(
                Target { ident, scope: scope.unwrap.to_vec() },
                expr_type.clone(),
            );
        }
        ident_type => {
            if ident_type != expr_type {
                return Err(Message::IncompatibleTypes);
            }
        }
    }
    Ok(())
}

pub(crate) fn get_types<'a>(
    ast: &'a [Syntax<'a>],
) -> Result<HashMap<Target<'a>, Type<'a>>, Error<'a>> {
    let mut types: HashMap<Target, Type> = HashMap::new();
    let scope: Scope = Scope { unwrap: &Vec::new() };
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                let ident: Vec<&str> = vec![ident];
                if types.contains_key(&Target {
                    ident: ident.to_vec(),
                    scope: scope.unwrap.to_vec(),
                }) {
                    return Err(Error {
                        syntax,
                        message: Message::IdentShadow,
                    });
                }
                match get_expr(&scope, &types, expr) {
                    Err(message) => return Err(Error { syntax, message }),
                    Ok(type_) => {
                        if let Err(message) = set_type(
                            &scope,
                            &Ident { unwrap: &ident },
                            &mut types,
                            &type_,
                        ) {
                            return Err(Error { syntax, message });
                        }
                    }
                }
            }
            Stmt::Assign { op, ident, expr } => match op {
                Asn::Reg => {
                    if let Err(message) =
                        set_assign(&scope, ident, &mut types, expr)
                    {
                        return Err(Error { syntax, message });
                    }
                }
                _ => panic!("{:#?}", syntax),
            },
            _ => panic!("{:#?}", syntax),
        }
    }
    Ok(types)
}
