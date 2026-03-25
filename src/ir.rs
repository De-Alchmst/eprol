#[derive(Debug, PartialEq)]
pub enum IRBaseType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, PartialEq)]
pub enum IRExtType {
    Int,
    I32,
    I64,
    Float,
    F32,
    F64,
    Func(Vec<IRBaseType>, Option<IRBaseType>),
    Void,
    Any,
}


#[derive(Debug, PartialEq)]
pub enum IR<'a> {
    Error,
    Drop,
    LitInt(IRBaseType, i64),
    LitFloat(IRBaseType, f64),
    LitStr(&'a str),
    Call(&'a str),
    GlobalGet(&'a str),
    GlovalSet(&'a str),
    LocalGet(&'a str),
    LocalSet(&'a str),
    Add(IRBaseType, bool),
    Sub(IRBaseType, bool),
    Mul(IRBaseType, bool),
    Div(IRBaseType, bool),
    Neg(IRBaseType),
}


#[derive(Debug, PartialEq)]
pub enum TopLevelIR<'a> {
    GlobalVar(&'a str, IR<'a>),
    Import(Vec<&'a str>, &'a str, IRExtType),
}
