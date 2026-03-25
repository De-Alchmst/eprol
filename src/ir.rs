#[derive(Debug, PartialEq)]
pub enum IRBaseType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, PartialEq)]
pub enum IRExtType {
    I32,
    I64,
    F32,
    F64,
    Void,
    Func(Vec<IRBaseType>, Option<IRBaseType>),
}


#[derive(Debug, PartialEq)]
pub enum IR<'a> {
    Drop,
    LiteralInt(IRBaseType, i64),
    LiteralFloat(IRBaseType, f64),
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
