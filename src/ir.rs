use crate::ast::Type;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum IRType {
    Int,
    I32,
    I64,
    Float,
    F32,
    F64,
    Func(Vec<IRType>, Box<IRType>),
    Void,
    Any,
    Error,
}


#[derive(Debug, PartialEq, Clone)]
pub enum IR {
    Error,
    Drop,
    LitInt(i64),
    LitFloat(f64),
    LitStr(String),
    Call(String),
    GlobalGet(String),
    GlobalSet(String),
    LocalGet(String),
    LocalSet(String),
    Add,
    Sub,
    // signedp
    Mul(bool),
    Div(bool),
    Neg,
    Not,
    // from
    Cast(IRType),
}


#[derive(Debug, PartialEq)]
pub enum TopLevelIR {
    GlobalVar(String, (IRType, IR)),
    // name, args, return, export, locals, body
    Proc(String, Vec<(String, IRType)>, IRType, Option<String>, Vec<(String, IRType)>, IRList),
}
pub type ImportIR<'a> = (Vec<&'a str>, String, IRType);

pub type IRList = Vec<(IRType, IR)>;
pub type TopLevelIRList = Vec<TopLevelIR>;
pub type ImportIRList<'a> = Vec<ImportIR<'a>>;


pub fn asttype_to_irtype(s: Type) -> IRType {
    match s {
        Type::I32  => IRType::I32,
        Type::I64  => IRType::I64,
        Type::F32  => IRType::F32,
        Type::F64  => IRType::F64,
        Type::Void => IRType::Void,
        Type::Proc(args, ret) =>
            IRType::Func(args.into_iter().map(asttype_to_irtype).collect(),
                         Box::new(asttype_to_irtype(*ret))),
    }
}


pub fn irlist_type(lst: &IRList) -> IRType {
    match lst.last() {
        Some((t, _)) => t.clone(),
        None => IRType::Void,
    }
}


impl fmt::Display for IRType  {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRType::Int => write!(f, "int"),
            IRType::I32 => write!(f, "i32"),
            IRType::I64 => write!(f, "i64"),
            IRType::Float => write!(f, "float"),
            IRType::F32 => write!(f, "f32"),
            IRType::F64 => write!(f, "f64"),
            IRType::Func(args, ret) => {
                let args_str = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({}) -> {}", args_str, ret)
            },
            IRType::Void => write!(f, "void"),
            IRType::Any => write!(f, "any"),
            IRType::Error => write!(f, "error"),
        }
    }
}


pub fn default_irtype_val(typ: &IRType) -> (IRType, IR) {
    match typ {
        IRType::Int | IRType::Any | IRType::I32 | IRType::I64
            => (typ.clone(), IR::LitInt(0)),
        IRType::Float | IRType::F32 | IRType::F64
            => (typ.clone(), IR::LitFloat(0.0)),
        IRType::Func(_, _) => (typ.clone(), IR::Error),
        IRType::Void => (typ.clone(), IR::Drop),
        IRType::Error => (typ.clone(), IR::Error),
    }
}


// insert automatic cast where needed
pub fn ir_resolve_types(got: (IRType, IR), expected: IRType) -> IRList {
    let current = got.0.clone();
    match expected {
        IRType::I32 => match current {
            IRType::I32 | IRType::Int | IRType::Any => vec![got],
            IRType::I64 => vec![got, (IRType::I32, IR::Cast(IRType::I64))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::I64 => match current {
            IRType::I64 | IRType::Int | IRType::Any => vec![got],
            IRType::I32 => vec![got, (IRType::I64, IR::Cast(IRType::I32))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::F32 => match current {
            IRType::F32 | IRType::Float | IRType::Any => vec![got],
            IRType::F64 => vec![got, (IRType::F32, IR::Cast(IRType::F64))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::F64 => match current {
            IRType::F64 | IRType::Float | IRType::Any => vec![got],
            IRType::F32 => vec![got, (IRType::F64, IR::Cast(IRType::F32))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::Void => match current {
            IRType::Void | IRType::Any => vec![got],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::Any => vec![got],

        _ => vec![(IRType::Error, IR::Error)]
    }
}
