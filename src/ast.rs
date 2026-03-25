#[derive(Debug, PartialEq, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
    pub namespace: Vec<&'a str>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Int(i64),
    Float(f64),
    Str(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum Unop {
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Lit(Literal<'a>),
    Unop(Unop, Box<Expr<'a>>),
    Binop(Binop, Box<Expr<'a>>, Box<Expr<'a>>),
    Ident(Ident<'a>),
    ProcCall(Ident<'a>, Vec<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Assign(Ident<'a>, Expr<'a>),
    Return(Expr<'a>),
}

pub type VarDeclBlock<'a> = (&'a str, Vec<(Ident<'a>, Option<Expr<'a>>)>);
pub type ConstDeclBlock<'a> = Vec<(Ident<'a>, Expr<'a>)>;

// type, args
pub type ProcArgs<'a> = (&'a str, Vec<Ident<'a>>);
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
    ProcDecl(Ident<'a>, Vec<ProcArgs<'a>>, &'a str, Option<&'a str>, Vec<ProcDeclBlock<'a>>, Vec<Stmt<'a>>),
    // outer name, inner name, type
    Import(Vec<&'a str>, Ident<'a>, &'a str),
}

pub type Program<'a> = Vec<TopLevel<'a>>;
