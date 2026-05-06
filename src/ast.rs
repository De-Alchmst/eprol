use chumsky::span::SimpleSpan;

// phony span
pub static PS: SimpleSpan = SimpleSpan { start: 0, end: 0, context: () };

#[derive(Debug, PartialEq, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
    pub namespace: Vec<&'a str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Accessor<'a> {
    pub typ: Type,
    pub offset_len: i64,
    pub offset: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32, I64,
    F32, F64,
    I8,  I16, U8, U16, U32, U64,
    Void,
    // args, return
    Proc(Vec<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Xor,
    Nand,
    Nor,
    Nxor,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal<'a> {
    Int(i64),
    Float(f64),
    Str(&'a str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Unop {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Lit(SimpleSpan, Literal<'a>),
    Unop(SimpleSpan, Unop, Box<Expr<'a>>),
    Binop(SimpleSpan, Binop, Box<Expr<'a>>, Box<Expr<'a>>),
    Ident(SimpleSpan, Ident<'a>),
    ProcCall(SimpleSpan, Ident<'a>, Vec<Expr<'a>>),
    RawAccess(SimpleSpan, Box<Expr<'a>>, Box<Accessor<'a>>),
    NamedAccess(SimpleSpan, Box<Expr<'a>>, Ident<'a>),
    Malformed,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LeftValue<'a> {
    Ident(SimpleSpan, Ident<'a>),
    RawAccess(SimpleSpan, Expr<'a>, Accessor<'a>),
    NamedAccess(SimpleSpan, Expr<'a>, Ident<'a>),
    Malformed
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Assign(LeftValue<'a>, Expr<'a>),
    Return(SimpleSpan, Expr<'a>),
    VoidReturn(SimpleSpan),
    Malformed,
}

pub type VarDeclBlock<'a> = (Type, Vec<(SimpleSpan, &'a str, Option<Expr<'a>>)>);
pub type ConstDeclBlock<'a> = Vec<(SimpleSpan, &'a str, Expr<'a>)>;

// type, args
pub type ProcArgs<'a> = (Type, Vec<(SimpleSpan, &'a str)>);
#[derive(Debug, PartialEq)]
pub enum ProcDeclBlock<'a> {
    Var(Vec<VarDeclBlock<'a>>),
    Const(ConstDeclBlock<'a>),
}

#[derive(Debug, PartialEq)]
pub enum TopLevel<'a> {
    ConstDecl(Vec<&'a str>, ConstDeclBlock<'a>),
    VarDecl(Vec<&'a str>, Vec<VarDeclBlock<'a>>),
    // name, args, return type, export name, decls, body
    ProcDecl((SimpleSpan, Ident<'a>), Vec<ProcArgs<'a>>, Type, Option<&'a str>, Vec<ProcDeclBlock<'a>>, Vec<Stmt<'a>>),
    // outer name, inner name, type
    Import(SimpleSpan, Vec<&'a str>, Ident<'a>, Type),
    AccessorDecl(SimpleSpan, Ident<'a>, Accessor<'a>),
}

pub type Program<'a> = Vec<TopLevel<'a>>;


pub fn expr2span(expr: &Expr) -> SimpleSpan {
    match expr {
        Expr::Lit(span, _) | 
        Expr::Unop(span, _, _) |
        Expr::Binop(span, _, _, _) |
        Expr::Ident(span, _) |
        Expr::ProcCall(span, _, _) |
        Expr::RawAccess(span, _, _) |
        Expr::NamedAccess(span, _, _) => *span,
        Expr::Malformed => PS,
    }
}


pub fn type_len(typ: Type) -> i64 {
    match typ {
        Type::I8  | Type::U8  => 1,
        Type::I16 | Type::U16 => 2,
        Type::I32 | Type::F32 | Type::U32 => 4,
        Type::I64 | Type::F64 | Type::U64 => 8,
        Type::Void => 0,
        Type::Proc(_, _) => unreachable!(),
    }
}
