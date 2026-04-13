use chumsky::span::SimpleSpan;

// phony span
pub static PS: SimpleSpan = SimpleSpan { start: 0, end: 0, context: () };

#[derive(Debug, PartialEq, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
    pub namespace: Vec<&'a str>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32, I64,
    F32, F64,
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
    Malformed,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Assign(Ident<'a>, Expr<'a>),
    Return(Expr<'a>),
    Malformed,
}

pub type VarDeclBlock<'a> = (Type, Vec<(&'a str, Option<Expr<'a>>)>);
pub type ConstDeclBlock<'a> = Vec<(&'a str, Expr<'a>)>;

// type, args
pub type ProcArgs<'a> = (Type, Vec<&'a str>);
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
    ProcDecl(Ident<'a>, Vec<ProcArgs<'a>>, Type, Option<&'a str>, Vec<ProcDeclBlock<'a>>, Vec<Stmt<'a>>),
    // outer name, inner name, type
    Import(Vec<&'a str>, Ident<'a>, Type),
}

pub type Program<'a> = Vec<TopLevel<'a>>;
