#[cfg(test)]
mod test;

use crate::map;
use crate::parser::{Case, Expr, Stmt, Syntax};
use crate::tokenizer::{Asn, Op};
use crate::types::{Target, Type};
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, PartialEq)]
pub(crate) enum Message {
    AccessNonArray,
    AccessNonIndex,
    ArrayMultiType,
    AssignNonIdent,
    CallNonFn,
    FnIncompatArgs,
    FnMissingReturn,
    FnMissingSig,
    FnWrongNumArgs,
    FnWrongReturn,
    IdentShadow,
    IdentUninit,
    IdentUnknown,
    IncompatibleTypes,
    NonIdentMember,
    ObjDuplicateKeys,
    SwitchEmpty,
    SwitchMissingCaseBreak,
    Unreachable,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Error<'a> {
    pub(crate) syntax: &'a Syntax<'a>,
    pub(crate) message: Message,
}

struct Ident<'a, 'b>(&'b [&'a str]);

struct Scope<'a, 'b>(&'b [&'a str]);

enum Return<'a> {
    Always(Type<'a>),
    Sometimes(Type<'a>),
    Empty,
}

macro_rules! get_infix_idents {
    ($left:expr, $right:expr $(,)?) => {{
        let mut idents: Vec<&str> = get_idents($left)?;
        idents.extend_from_slice(&get_idents($right)?);
        idents
    }};
}

fn get_idents<'a>(expr: &'a Expr<'a>) -> Result<Vec<&'a str>, Message> {
    Ok(match expr {
        Expr::Ident(ident) => vec![ident],
        Expr::Infix { op: Op::Member, left, right } => {
            get_infix_idents!(left, right)
        }
        _ => return Err(Message::NonIdentMember),
    })
}

fn deref_ident<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    ident: &'b Ident<'a, 'b>,
    types: &'b HashMap<Target<'a>, Type<'a>>,
) -> Option<&'b Type<'a>> {
    let mut scope: Vec<&str> = scope.0.to_vec();
    loop {
        let type_: Option<&Type> = types
            .get(&Target { ident: ident.0.to_vec(), scope: scope.clone() });
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
    macro_rules! check_ident {
        ($ident:expr $(,)?) => {
            match deref_ident(scope, $ident, types) {
                Some(Type::Uninit) => return Err(Message::IdentUninit),
                Some(type_) => type_.clone(),
                None => return Err(Message::IdentUnknown),
            }
        };
    }

    Ok(match expr {
        Expr::Ident(ident) => check_ident!(&Ident(&[*ident])),
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
            Type::Obj(type_props)
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
                        type_ = Type::Array(Box::new(elem.clone()))
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
        Expr::Infix { op, left, right } => match op {
            Op::Member => {
                check_ident!(&Ident(&get_infix_idents!(left, right)))
            }
            Op::Add | Op::Sub | Op::Mul | Op::Div => {
                match (
                    get_expr(scope, types, left)?,
                    get_expr(scope, types, right)?,
                ) {
                    (Type::Num, Type::Num) => Type::Num,
                    _ => panic!("{:#?} {:#?}", left, right),
                }
            }
            _ => panic!("{:#?} {:#?}", left, right),
        },
        Expr::Access { expr, index } => {
            let expr: Type = get_expr(scope, types, expr)?;
            let index: Type = get_expr(scope, types, index)?;
            match (expr, index) {
                (Type::Array(type_), Type::Num) => *type_,
                (Type::Array(_), _) => return Err(Message::AccessNonIndex),
                _ => return Err(Message::AccessNonArray),
            }
        }
        Expr::Call { expr, args } => {
            let fn_: BTreeMap<Vec<Type>, Type> =
                match get_expr(scope, types, expr)? {
                    Type::Fn(fn_) => fn_,
                    _ => return Err(Message::CallNonFn),
                };
            let mut arg_types: Vec<Type> = Vec::with_capacity(args.len());
            for arg in args {
                arg_types.push(get_expr(scope, types, arg)?);
            }
            match fn_.get(&arg_types) {
                Some(type_) => type_.clone(),
                None => return Err(Message::FnIncompatArgs),
            }
        }
        _ => panic!("{:#?}", expr),
    })
}

fn set_array_methods<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    ident: &'b Ident<'a, 'b>,
    types: &'b mut HashMap<Target<'a>, Type<'a>>,
    type_: &'b Type<'a>,
) -> Result<(), Message> {
    let mut ident: Vec<&str> = ident.0.to_vec();
    ident.push("push");
    set_type(
        scope,
        &Ident(&ident),
        types,
        &Type::Fn(map![(vec![type_.clone()], Type::Undef)]),
    )
}

fn set_type<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    ident: &'b Ident<'a, 'b>,
    types: &'b mut HashMap<Target<'a>, Type<'a>>,
    type_: &'b Type<'a>,
) -> Result<(), Message> {
    match type_ {
        Type::Obj(props) => {
            for (key, value) in props.iter() {
                let mut ident: Vec<&str> = ident.0.to_vec();
                ident.push(key);
                set_type(scope, &Ident(&ident), types, &value)?;
            }
        }
        Type::Array(type_) => set_array_methods(scope, ident, types, type_)?,
        _ => (),
    }
    let target: Target =
        Target { ident: ident.0.to_vec(), scope: scope.0.to_vec() };
    if types.insert(target, type_.clone()).is_some() {
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
        Expr::Infix { op: Op::Member, left, right } => {
            get_infix_idents!(left, right)
        }
        _ => return Err(Message::AssignNonIdent),
    };
    let expr_type: Type = get_expr(scope, &types, value_expr)?;
    let ident_type: Type = match deref_ident(scope, &Ident(&ident), types) {
        Some(type_) => type_.clone(),
        None => return Err(Message::IdentUnknown),
    };
    match ident_type {
        Type::Uninit => {
            let _: Option<_> = types.remove(&Target {
                ident: ident.clone(),
                scope: scope.0.to_vec(),
            });
            set_type(scope, &Ident(&ident), types, &expr_type)?;
        }
        ident_type => {
            if ident_type != expr_type {
                return Err(Message::IncompatibleTypes);
            }
        }
    }
    Ok(())
}

macro_rules! error {
    ($syntax:expr, $message:expr $(,)?) => {
        return Err(Error { syntax: $syntax, message: $message });
    };
}

fn get_returns<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    types: &'b mut HashMap<Target<'a>, Type<'a>>,
    body: &'a [Syntax<'a>],
) -> Result<Return<'a>, Error<'a>> {
    if let Some((last, body)) = body.split_last() {
        let mut return_type: Option<Type> = None;
        for syntax in body {
            match syntax.statement {
                Stmt::Ret(_) | Stmt::Break | Stmt::Continue => {
                    error!(syntax, Message::Unreachable)
                }
                _ => (),
            }
            match get_return(scope, types, syntax) {
                Ok(Return::Always(_)) => error!(syntax, Message::Unreachable),
                Ok(Return::Sometimes(next_type)) => match &return_type {
                    Some(prev_type) => {
                        if *prev_type != next_type {
                            error!(syntax, Message::FnWrongReturn)
                        }
                    }
                    None => return_type = Some(next_type),
                },
                Ok(Return::Empty) => {
                    if let Some(prev_type) = &return_type {
                        if *prev_type != Type::Undef {
                            error!(syntax, Message::FnWrongReturn)
                        }
                    }
                }
                Err(error) => return Err(error),
            }
        }
        match (return_type, get_return(scope, types, last)) {
            (None, last) => last,
            (Some(prev_type), Ok(Return::Always(next_type))) => {
                if prev_type != next_type {
                    error!(last, Message::FnWrongReturn)
                }
                Ok(Return::Always(next_type))
            }
            (Some(prev_type), Ok(Return::Sometimes(next_type))) => {
                if prev_type != next_type {
                    error!(last, Message::FnWrongReturn)
                }
                Ok(Return::Sometimes(next_type))
            }
            (Some(next_type), Ok(Return::Empty)) => {
                if Type::Undef != next_type {
                    error!(last, Message::FnMissingReturn)
                }
                Ok(Return::Empty)
            }
            (_, Err(error)) => Err(error),
        }
    } else {
        Ok(Return::Empty)
    }
}

fn get_return<'a, 'b>(
    scope: &'b Scope<'a, 'b>,
    types: &'b mut HashMap<Target<'a>, Type<'a>>,
    syntax: &'a Syntax<'a>,
) -> Result<Return<'a>, Error<'a>> {
    match &syntax.statement {
        Stmt::Decl { ident, expr } => {
            let idents: Vec<&str> = vec![ident];
            if types.contains_key(&Target {
                ident: idents.clone(),
                scope: scope.0.to_vec(),
            }) {
                error!(syntax, Message::IdentShadow);
            }
            match get_expr(scope, types, expr) {
                Err(message) => return Err(Error { syntax, message }),
                Ok(type_) => {
                    let current_scope: &[&str] = scope.0;
                    let n: usize = current_scope.len();
                    if 0 < n {
                        let mut purges: Vec<Target> = Vec::new();
                        for target in types.keys() {
                            let target_scope: &[&str] = &target.scope;
                            /* NOTE: `target.ident` should *never* be empty! */
                            if (&target.ident[0] == ident)
                                && (&current_scope[0..(n - 1)] == target_scope)
                            {
                                purges.push(target.clone());
                            }
                        }
                        for purge in purges {
                            let _: Option<_> = types.remove(&purge);
                        }
                    }
                    if let Err(message) =
                        set_type(&scope, &Ident(&idents), types, &type_)
                    {
                        error!(syntax, message);
                    }
                }
            }
        }
        Stmt::Assign { op: Asn::Reg, ident, expr } => {
            if let Err(message) = set_assign(&scope, ident, types, expr) {
                error!(syntax, message);
            }
        }
        Stmt::Ret(expr) => match get_expr(scope, types, expr) {
            Ok(type_) => return Ok(Return::Always(type_)),
            Err(message) => error!(syntax, message),
        },
        Stmt::Effect(expr) => {
            if let Err(message) = get_expr(scope, types, expr) {
                return Err(Error { syntax, message });
            }
        }
        Stmt::Fn { ident, args: arg_idents, body } => {
            let parent_scope: Vec<&str> = scope.0.to_vec();
            let mut fn_scope: Vec<&str> =
                Vec::with_capacity(parent_scope.len() + 1);
            fn_scope.append(&mut scope.0.to_vec());
            fn_scope.push(ident);
            if let Some(Type::Fn(overloads)) =
                deref_ident(&Scope(&parent_scope), &Ident(&[ident]), types)
            {
                for (arg_types, return_) in overloads {
                    if arg_idents.len() != arg_types.len() {
                        error!(syntax, Message::FnWrongNumArgs);
                    }
                    let mut types: HashMap<Target, Type> = types.clone();
                    for (arg_ident, arg_type) in
                        arg_idents.iter().zip(arg_types)
                    {
                        if let Err(message) = set_type(
                            &Scope(&fn_scope),
                            &Ident(&[*arg_ident]),
                            &mut types,
                            arg_type,
                        ) {
                            error!(syntax, message);
                        }
                    }
                    if let Some((last, body)) = body.split_last() {
                        for syntax in body {
                            match get_return(
                                &Scope(&fn_scope),
                                &mut types,
                                syntax,
                            ) {
                                Ok(Return::Sometimes(type_)) => {
                                    if *return_ != type_ {
                                        error!(syntax, Message::FnWrongReturn);
                                    }
                                }
                                Ok(Return::Always(_)) => {
                                    error!(syntax, Message::Unreachable);
                                }
                                Err(error) => return Err(error),
                                Ok(_) => (),
                            }
                        }
                        match get_return(&Scope(&fn_scope), &mut types, last) {
                            Ok(Return::Always(type_)) => {
                                if *return_ != type_ {
                                    error!(last, Message::FnWrongReturn);
                                }
                            }
                            Ok(_) => {
                                if *return_ != Type::Undef {
                                    error!(last, Message::FnMissingReturn);
                                }
                            }
                            Err(error) => return Err(error),
                        }
                    } else if *return_ != Type::Undef {
                        error!(syntax, Message::FnMissingReturn);
                    }
                }
            } else {
                error!(syntax, Message::FnMissingSig);
            }
        }
        Stmt::Switch { expr: switch_expr, cases, default } => {
            if cases.is_empty() && default.is_empty() {
                error!(syntax, Message::SwitchEmpty)
            }
            let switch_type: Type = match get_expr(scope, types, switch_expr) {
                Ok(type_) => type_,
                Err(message) => error!(syntax, message),
            };
            let mut return_type: Option<Type> = None;
            for Case { expr: case_expr, body } in cases {
                match get_expr(scope, types, case_expr) {
                    Ok(type_) => {
                        if switch_type != type_ {
                            error!(syntax, Message::IncompatibleTypes);
                        }
                    }
                    Err(message) => error!(syntax, message),
                }
                match (return_type.clone(), get_returns(scope, types, body)) {
                    (None, Ok(Return::Always(type_)))
                    | (None, Ok(Return::Sometimes(type_))) => {
                        return_type = Some(type_);
                    }
                    (Some(prev_type), Ok(Return::Always(next_type)))
                    | (Some(prev_type), Ok(Return::Sometimes(next_type))) => {
                        if prev_type != next_type {
                            error!(syntax, Message::IncompatibleTypes)
                        }
                    }
                    (_, Ok(Return::Empty)) => (),
                    (_, error) => return error,
                }
                if let Some(last) = body.last() {
                    match last.statement {
                        Stmt::Break | Stmt::Ret(_) => (),
                        _ => error!(syntax, Message::SwitchMissingCaseBreak),
                    }
                } else {
                    error!(syntax, Message::SwitchMissingCaseBreak);
                }
            }
            if default.is_empty() {
                if let Some(type_) = return_type {
                    return Ok(Return::Sometimes(type_));
                }
            } else {
                match (return_type, get_returns(scope, types, default)) {
                    (None, Ok(Return::Always(next_type))) => {
                        if cases.is_empty() {
                            return Ok(Return::Always(next_type));
                        }
                        return Ok(Return::Sometimes(next_type));
                    }
                    (None, Ok(Return::Sometimes(next_type))) => {
                        return Ok(Return::Sometimes(next_type));
                    }
                    (None, Ok(Return::Empty)) => (),
                    (Some(prev_type), Ok(Return::Always(next_type))) => {
                        if prev_type != next_type {
                            error!(syntax, Message::IncompatibleTypes)
                        }
                        return Ok(Return::Always(next_type));
                    }
                    (Some(prev_type), Ok(Return::Sometimes(next_type))) => {
                        if prev_type != next_type {
                            error!(syntax, Message::IncompatibleTypes)
                        }
                        return Ok(Return::Sometimes(next_type));
                    }
                    (Some(prev_type), Ok(Return::Empty)) => {
                        if prev_type != Type::Undef {
                            error!(syntax, Message::IncompatibleTypes)
                        }
                        return Ok(Return::Always(Type::Undef));
                    }
                    (_, error) => return error,
                }
            }
        }
        Stmt::Break => (),
        _ => panic!("{:#?}", syntax),
    }
    Ok(Return::Empty)
}

pub(crate) fn get_types<'a>(
    ast: &'a [Syntax<'a>],
    sigs: &'a mut HashMap<Target<'a>, Type<'a>>,
) -> Result<HashMap<Target<'a>, Type<'a>>, Error<'a>> {
    let mut types: HashMap<Target, Type> = HashMap::new();
    for (target, type_) in sigs.drain() {
        if let Type::Fn(_) = type_ {
            let _: Option<_> = types.insert(target, type_);
        }
    }
    let scope: Vec<&str> = Vec::new();
    let scope: Scope = Scope(&scope);
    for syntax in ast {
        let _: Return = get_return(&scope, &mut types, syntax)?;
    }
    Ok(types)
}
