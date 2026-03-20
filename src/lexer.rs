use logos::Logos;
use std::fmt;

#[derive(Logos, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
    Error,

    #[token("PROC")]
    Proc,
    #[token("DO")]
    Do,
    #[token("END")]
    End,
    #[token("IF")]
    If,
    #[token("ELIF")]
    Elif,
    #[token("ELSE")]
    Else,
    #[token("WHILE")]
    While,
    #[token("LOOP")]
    Loop,
    #[token("FOR")]
    For,
    #[token("FROM")]
    From,
    #[token("TO")]
    To,
    #[token("DOWNTO")]
    Downto,
    #[token("UNTIL")]
    Until,
    #[token("STEP")]
    Step,
    #[token("CASE")]
    Case,
    #[token("OF")]
    Of,
    #[token("IN")]
    In,
    #[token("RETURN")]
    Return,
    #[token("BREAK")]
    Break,
    #[token("NEXT")]
    Next,
    #[token("GIVE")]
    Give,
    #[token("VAR")]
    Var,
    #[token("CONST")]
    Const,
    #[token("STATIC")]
    Static,
    #[token("ACCESSOR")]
    Accessor,
    #[token("ENUM")]
    Enum,
    #[token("RECORD")]
    Record,
    #[token("EXTENDS")]
    Extends,
    #[token("EXPORT")]
    Export,
    #[token("USE")]
    Use,
    #[token("IMPORT")]
    Import,
    #[token("AS")]
    As,
    #[token("NOT")]
    Not,
    #[token("BOOL")]
    Bool,
    #[token("NBOOL")]
    Nbool,
    #[token("AND")]
    And,
    #[token("OR")]
    Or,
    #[token("Xor")]
    Xor,
    #[token("NAND")]
    Nand,
    #[token("NOR")]
    Nor,
    #[token("NXOR")]
    Nxor,
    #[token("TRUE")]
    True,
    #[token("FALSE")]
    False,

    #[regex(r"[iIfFuUsS](8|16|32|64)", priority=20)]
    Type(&'a str),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok(), priority=30)]
    Float(f64),
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok(), priority=20)]
    Int(i64),

    #[regex(r#""(\\"|[^"])*""#)]
    String(&'a str),

    #[regex(r"[_\p{Alphabetic}][_\p{Alphabetic}0-9]+", priority=1)]
    Ident(&'a str),

    #[token(";")]
    Semicolon,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("|")]
    Pipe,
    #[token("@")]
    AtSym,
    #[token("^")]
    Caret,
    #[token("(")]
    LRound,
    #[token(")")]
    RRound,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Equal,
    #[token("<>")]
    NotEqual,
    #[token("<")]
    LesserThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LesserEqualThan,
    #[token(":=")]
    Assign,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Float(s) => write!(f, "{s}"),
            Self::Error => write!(f, "<error>"),
            _ => write!(f, "UNIMPLEMENTED")
        }
    }
}
