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


#[derive(Debug, PartialEq)]
pub enum IR<'a> {
    Error,
    Drop,
    LitInt(i64),
    LitFloat(f64),
    LitStr(&'a str),
    Call(&'a str),
    GlobalGet(&'a str),
    GlobalSet(&'a str),
    LocalGet(&'a str),
    LocalSet(&'a str),
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
pub enum TopLevelIR<'a> {
    GlobalVar(&'a str, IR<'a>),
    Import(Vec<&'a str>, &'a str, IRType),
}

pub type IRList<'a> = Vec<(IRType, IR<'a>)>;

pub fn irlist_type<'a>(lst: &IRList<'a>) -> IRType {
    match lst.last() {
        Some((t, _)) => t.clone(),
        None => IRType::Void,
    }
}


// insert automatic cast where needed
pub fn ir_resolve_types<'a>(got: (IRType, IR<'a>), expected: IRType) -> IRList<'a> {
    let current = got.0.clone();
    match expected {
        IRType::I32 => match current {
            IRType::I32 | IRType::Int => vec![got],
            IRType::I64 => vec![got, (IRType::I32, IR::Cast(IRType::I64))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::I64 => match current {
            IRType::I64 | IRType::Int => vec![got],
            IRType::I32 => vec![got, (IRType::I64, IR::Cast(IRType::I32))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::F32 => match current {
            IRType::F32 | IRType::Float => vec![got],
            IRType::F64 => vec![got, (IRType::F32, IR::Cast(IRType::F64))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        IRType::F64 => match current {
            IRType::F64 | IRType::Float => vec![got],
            IRType::F32 => vec![got, (IRType::F64, IR::Cast(IRType::F32))],
            _ => vec![(IRType::Error, IR::Error)]
        },

        _ => vec![(IRType::Error, IR::Error)]
    }
}
