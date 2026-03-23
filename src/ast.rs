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
pub enum Expr<'a> {
    Lit(Literal<'a>),
    Binop(Binop, Box<Expr<'a>>, Box<Expr<'a>>),
    Ident(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Assign(&'a str, Expr<'a>),
}

#[derive(Debug, PartialEq)]
pub enum TopLevel<'a> {
    ConstDecl,
    VarDecl,
    FuncDecl,
    Stmt(Stmt<'a>),
}

type Program<'a> = Vec<TopLevel<'a>>;
