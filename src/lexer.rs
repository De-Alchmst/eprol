use logos::Logos;
use std::fmt;

// TODO: unclosed comments currently just ignore rest of flle, maybe not deal behavior
fn comment_callback<'a, 'b : 'a>(
    lex: &'a mut logos::Lexer<'b, Token<'b>>
) -> logos::Skip {
    let mut depth = 1;
    let mut remainder = lex.remainder();

    lex.bump(2); // Skip the initial "(*"
    while depth > 0 && remainder.len() > 1 {
        if remainder.starts_with("(*") {
            depth += 1;
            lex.bump(2);
        } else if remainder.starts_with("*)") {
            depth -= 1;
            lex.bump(2);
        } else {
            lex.bump(1);
        }

        remainder = lex.remainder();
    }

    logos::Skip
}

#[derive(Logos, Clone, PartialEq, Debug)]
#[logos(skip(r"[ \t\n\f]+"))]
#[logos(skip(r"\(\*", comment_callback))]
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

    #[regex(r"[IFUS](8|16|32|64)", priority=20)]
    Type(&'a str),

    #[regex(r"[0-9]+\.[0-9]+", priority=30)]
    Float(&'a str),
    #[regex(r"[0-9]+", priority=20)]
    Int(&'a str),

    #[regex(r#""(\\"|[^"])*""#, |lex| { 
        let s = lex.slice();
        &s[1..s.len() - 1]
    })]
    String(&'a str),

    #[regex(r"[_\p{Alphabetic}][_\p{Alphabetic}0-9]*", priority=1)]
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
    #[token(">=")]
    GreaterEqualThan,
    #[token(":=")]
    Assign,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Error => write!(f, "<erreor>"),
            Self::Proc => write!(f, "PROC"),
            Self::Do => write!(f, "DO"),
            Self::End => write!(f, "END"),
            Self::If => write!(f, "IF"),
            Self::Elif => write!(f, "ELIF"),
            Self::Else => write!(f, "ELSE"),
            Self::While => write!(f, "WHILE"),
            Self::Loop => write!(f, "LOOP"),
            Self::For => write!(f, "FOR"),
            Self::From => write!(f, "FROM"),
            Self::To => write!(f, "TO"),
            Self::Downto => write!(f, "DOWNTO"),
            Self::Until => write!(f, "UNTIL"),
            Self::Step => write!(f, "STEP"),
            Self::Case => write!(f, "CASE"),
            Self::Of => write!(f, "OF"),
            Self::In => write!(f, "IN"),
            Self::Return => write!(f, "RETURN"),
            Self::Break => write!(f, "BREAK"),
            Self::Next => write!(f, "NEXT"),
            Self::Give => write!(f, "GIVE"),
            Self::Var => write!(f, "VAR"),
            Self::Const => write!(f, "CONST"),
            Self::Static => write!(f, "STATIC"),
            Self::Accessor => write!(f, "ACCESSOR"),
            Self::Enum => write!(f, "ENUM"),
            Self::Record => write!(f, "RECORD"),
            Self::Extends => write!(f, "EXTENDS"),
            Self::Export => write!(f, "EXPORT"),
            Self::Use => write!(f, "USE"),
            Self::Import => write!(f, "IMPORT"),
            Self::As => write!(f, "AS"),
            Self::Not => write!(f, "NOT"),
            Self::Bool => write!(f, "BOOL"),
            Self::Nbool => write!(f, "NBOOL"),
            Self::And => write!(f, "AND"),
            Self::Or => write!(f, "OR"),
            Self::Xor => write!(f, "XOR"),
            Self::Nand => write!(f, "NAND"),
            Self::Nor => write!(f, "NOR"),
            Self::Nxor => write!(f, "NXOR"),
            Self::True => write!(f, "TRUE"),
            Self::False => write!(f, "FALSE"),
            Self::Type(s) => write!(f, "{s}"),
            Self::Float(s) => write!(f, "{s}"),
            Self::Int(s) => write!(f, "{s}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Ident(s) => write!(f, "{s}"),
            Self::Semicolon => write!(f, ";"),
            Self::Period => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Pipe => write!(f, "|"),
            Self::AtSym => write!(f, "@"),
            Self::Caret => write!(f, "^"),
            Self::LRound => write!(f, "("),
            Self::RRound => write!(f, ")"),
            Self::LCurly => write!(f, "{}", '{'),
            Self::RCurly => write!(f, "{}", '}'),
            Self::LSquare => write!(f, "["),
            Self::RSquare => write!(f, "]"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Equal => write!(f, "="),
            Self::NotEqual => write!(f, "<>"),
            Self::LesserThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),
            Self::LesserEqualThan => write!(f, "<="),
            Self::GreaterEqualThan => write!(f, ">="),
            Self::Assign => write!(f, ":="),
            // _ => write!(f, "UNIMPLEMENTED")
        }
    }
}
