use crate::{
    ast::*,
    lexer::Token,
};
use logos::Logos;
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};

pub fn parse_str_expr(input: &str) -> Result<Expr<'_>, Vec<Rich<'_, Token<'_>, SimpleSpan<usize, ()>>>>   {
    // Create a logos lexer over the source code
    let token_iter = Token::lexer(input)
        .spanned()
        // Convert logos errors into tokens. We want parsing to be recoverable
        // and not fail at the lexing stage, so we have a dedicated
        // `Token::Error` variant that represents a token error that was
        // previously encountered
        .map(|(tok, span)| match tok {
            // Turn the `Range<usize>` spans logos gives us into chumsky's
            // `SimpleSpan` via `Into`, because it's easier to work with
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

    // Turn the token iterator into a stream that chumsky can use for things
    // like backtracking
    let token_stream = Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts
        // so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a
        // zero-width span at the end of the string
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    expr().parse(token_stream).into_result()
}

pub fn parse_str_stmt(input: &str) -> Result<Stmt<'_>, Vec<Rich<'_, Token<'_>, SimpleSpan<usize, ()>>>>   {
    let token_iter = Token::lexer(input)
        .spanned() .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });
    let token_stream = Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    stmt().parse(token_stream).into_result()
}

pub fn parse_str_top_level(input: &str) -> Result<TopLevel<'_>, Vec<Rich<'_, Token<'_>, SimpleSpan<usize, ()>>>>   {
    let token_iter = Token::lexer(input)
        .spanned() .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });
    let token_stream = Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    top_level().parse(token_stream).into_result()
}

pub fn parse_str_program(input: &str) -> Result<Program<'_>, Vec<Rich<'_, Token<'_>, SimpleSpan<usize, ()>>>>   {
    let token_iter = Token::lexer(input)
        .spanned() .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });
    let token_stream = Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    program().parse(token_stream).into_result()
}



fn ident<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Ident<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    select! { Token::Ident(s) => s }
        .separated_by(just(Token::Period))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|mut parts| {
            let name = parts.remove(0); // safe: at_least(1)
            Ident { name, namespace: parts }
        })
}


fn expr<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Expr<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|exp| {
        let end_node = choice((
            // LITERAL
            select!{
                Token::Int(n) => Expr::Lit(Literal::Int(n.parse().unwrap_or(0))),
                Token::Float(n) => Expr::Lit(Literal::Float(n.parse().unwrap_or(0.0))),
                Token::True => Expr::Lit(Literal::Int(1)),
                Token::False => Expr::Lit(Literal::Int(0)),
                Token::String(s) => Expr::Lit(Literal::Str(s))
            },

            // PARENTHESISED
            exp.clone().delimited_by(just(Token::LRound), just(Token::RRound)),
           
            // PROC CALL
            ident()
                .then(
                    exp.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LRound), just(Token::RRound)))
                .map(|(proc, args)|
                    Expr::ProcCall(proc, args)),

            // IDENT
            ident().map(Expr::Ident),
        )).boxed();

        let unop = choice((
            just(Token::Minus),
            just(Token::Plus),
            just(Token::Not)
        ))
            .repeated()
            .foldr(end_node, |op, rhs|
                match op {
                    Token::Minus => Expr::Unop(Unop::Neg, Box::new(rhs)),
                    Token::Not   => Expr::Unop(Unop::Not, Box::new(rhs)),
                    Token::Plus  => rhs,
                    _ => unreachable!(),
                });

        let binop_product = unop.clone().foldl(
            choice((
                just(Token::Star),
                just(Token::Slash),
            ))
            .then(unop)
            .repeated(),
            |lhs, (op, rhs)|
            Expr::Binop(match op {
                Token::Star  => Binop::Mul,
                Token::Slash => Binop::Div,
                _ => unreachable!(),
            }, Box::new(lhs), Box::new(rhs))
        );

        let binop_sum = binop_product.clone().foldl(
            choice((
                just(Token::Plus),
                just(Token::Minus),
            ))
            .then(binop_product)
            .repeated(),
            |lhs, (op, rhs)|
            Expr::Binop(match op {
                Token::Plus  => Binop::Add,
                Token::Minus => Binop::Sub,
                _ => unreachable!(),
            }, Box::new(lhs), Box::new(rhs))
        );

        binop_sum
    })
}


fn stmt<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Stmt<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        // ASSIGN
        ident()
            .then_ignore(just(Token::Assign))
            .then(expr())
            .map(|(idnt, exp)|
                Stmt::Assign(idnt, exp)),

        // RETURN
        just(Token::Return)
        .ignore_then(expr())
        .map(|e| Stmt::Return(e)),

        // JUST EXPR
        expr().map(|e| Stmt::Expr(e)),
    ))
}


fn stmt_vect<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Stmt<'src>>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    stmt()
        .separated_by(
            just(Token::Semicolon)
            .repeated()
            .at_least(1))
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
}


fn simple_type<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Type, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    select!{Token::Type(s) => match s {
        "I32"|"i32" => Type::I32,
        "I64"|"i64" => Type::I64,
        "F32"|"f32" => Type::F32,
        "F64"|"f64" => Type::F64,
        _ => unreachable!(),
    }}
}


fn import_type<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Type, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        // procedure type
        simple_type()
            // args
            .then(
                select!{ Token::Ident(s) => s }
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>()
            )
            // replace arg names with types
            .map(|(typ, names)| names.iter().map(|_| typ.clone()).collect::<Vec<_>>())
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LRound), just(Token::RRound))
            // optional return type
            .then(choice((
                just(Token::Colon).ignore_then(simple_type()),
                empty().to(Type::Void),
            )))
            .map(|(arg_types, typ)|
                Type::Proc(arg_types.into_iter().flatten().collect::<Vec<_>>(),
                           Box::new(typ))),

        simple_type(),
    ))
}


fn declare_namespace<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<&'src str>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    just(Token::Colon)
        .ignore_then(
            select! { Token::Ident(s) => s }
            .separated_by(just(Token::Period))
            .at_least(1)
            .collect::<Vec<_>>()
        )
}

fn optional_declare_namespace<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<&'src str>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    declare_namespace()
        .or(empty().to(vec![]))
}


fn name_optional_namespace_declare<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Ident<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        select! { Token::Ident(s) => s }
        .then(declare_namespace())
        .map(|(idnt, nmspc)| {
            Ident {
                name: idnt,
                namespace: nmspc
            }
        }),
        select! { Token::Ident(s) => Ident { name: s, namespace: vec![] } }
    ))
}


fn var_decl_body<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<VarDeclBlock<'src>>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let decl_block = simple_type()
        .then(
            choice((
                select!{ Token::Ident(s) => s }
                    .then_ignore(just(Token::Assign))
                    .then(expr())
                    .map(|(i, exp)| (i, Some(exp))),
                select!{ Token::Ident(s) => (s, None) }
            ))
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
        );

    let decl_body = decl_block
        .repeated()
        .collect::<Vec<_>>();

    decl_body
}


fn const_decl_body<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, ConstDeclBlock<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    select!{ Token::Ident(s) => s }
        .then_ignore(just(Token::Equal))
        .then(expr())
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}


fn optional_export<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Option<&'src str>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        just(Token::Export)
            .ignore_then(select!{Token::String(s) => s})
            .map(Some),
        empty().to(None),
    ))
}


fn proc_args_decl<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<ProcArgs<'src>>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        simple_type()
            .then(
                select!{ Token::Ident(s) => s }
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<_>>()
            )
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LRound), just(Token::RRound)),
        empty().to(vec![]),
    ))
}


fn proc_decl_blocks<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<ProcDeclBlock<'src>>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    var_decl_body()
        .then(
            choice((
                just(Token::Var)
                    .ignore_then(var_decl_body())
                    .map(ProcDeclBlock::Var),
                just(Token::Const)
                    .ignore_then(const_decl_body())
                    .map(ProcDeclBlock::Const),
            ))
            .repeated()
            .collect::<Vec<_>>()
        )
        .map(|(initial, mut rest)| {
            if initial.len() > 0 {
                rest.insert(0, ProcDeclBlock::Var(initial));
            }
            rest
        })
}


fn top_level<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, TopLevel<'tokens>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        // IMPORT
        just(Token::Import)
            .ignore_then(
                select!{Token::String(s) => s}
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
            )
            .then_ignore(just(Token::As))
            .then(name_optional_namespace_declare())
            .then(import_type())
            .map(|((strings, ident), typ)|
                TopLevel::Import(strings, ident, typ)),

        // VAR
        just(Token::Var)
            .ignore_then(optional_declare_namespace())
            .then(var_decl_body())
            .then_ignore(just(Token::End))
            .map(|(nmsp, body)|
                TopLevel::VarDecl(nmsp, body)),

        // CONST
        just(Token::Const)
            .ignore_then(optional_declare_namespace())
            .then(const_decl_body())
            .then_ignore(just(Token::End))
            .map(|(nmsp, body)|
                TopLevel::ConstDecl(nmsp, body)),

        // PROC
        just(Token::Proc)
            .ignore_then(name_optional_namespace_declare())
            .then(proc_args_decl())
            .then(choice((
                just(Token::Colon).ignore_then(simple_type()),
                empty().to(Type::Void),
            )))
            .then(optional_export())
            .then(proc_decl_blocks())
            .then_ignore(just(Token::Do))
            .then(stmt_vect())
            .then_ignore(just(Token::End))
            .map(|(((((name, args), typ), export), decls), body)|
                TopLevel::ProcDecl(name, args, typ, export, decls, body)),
    ))
}


fn program<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Program<'tokens>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    top_level().repeated().collect::<Vec<_>>()
}


// fn parser<'tokens, 'src: 'tokens, I>(
// ) -> impl Parser<'tokens, I, Program, extra::Err<Rich<'tokens, Token<'src>>>>
// where
//     I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
// {
//     recursive(||)
// }

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic_expr() {
        assert_eq!(parse_str_expr("42"), Ok(Expr::Lit(Literal::Int(42))));
    }

    #[test]
    fn basic_unop() {
        assert_eq!(parse_str_expr("NOT --+-42"),
        Ok(Expr::Unop(Unop::Not,
                Box::new(Expr::Unop(Unop::Neg,
                    Box::new(Expr::Unop(Unop::Neg,
                        Box::new(Expr::Unop(Unop::Neg,
                            Box::new(Expr::Lit(Literal::Int(42))))))))))))
    }

    #[test]
    fn basic_binop() {
        assert_eq!(parse_str_expr("-2 + (6 - 9) * +7 + 9"),
        Ok(Expr::Binop(Binop::Add,
                Box::new(Expr::Binop(Binop::Add,
                        Box::new(Expr::Unop(Unop::Neg,
                                Box::new(Expr::Lit(Literal::Int(2))))),
                        Box::new(Expr::Binop(Binop::Mul,
                                Box::new(Expr::Binop(Binop::Sub,
                                        Box::new(Expr::Lit(Literal::Int(6))),
                                        Box::new(Expr::Lit(Literal::Int(9))))),
                                Box::new(Expr::Lit(Literal::Int(7))))))),
                Box::new(Expr::Lit(Literal::Int(9))))))
    }

    #[test]
    fn basic_idents() {
        assert_eq!(parse_str_expr("a.b.c + ni"),
        Ok(Expr::Binop(Binop::Add,
                Box::new(Expr::Ident(Ident { name: "a", namespace: vec!["b", "c"] })),
                Box::new(Expr::Ident(Ident { name: "ni", namespace: vec![] })))))
    }

    #[test]
    fn assignment() {
        assert_eq!(parse_str_stmt("foo := 1-2"),
        Ok(Stmt::Assign(Ident {name: "foo", namespace: vec![]},
                Expr::Binop(Binop::Sub,
                    Box::new(Expr::Lit(Literal::Int(1))),
                    Box::new(Expr::Lit(Literal::Int(2)))))))
    }

    #[test]
    fn expr_stmt() {
        assert_eq!(parse_str_expr("3+7"),
        Ok(Expr::Binop(Binop::Add,
                Box::new(Expr::Lit(Literal::Int(3))),
                Box::new(Expr::Lit(Literal::Int(7))))))
    }

    #[test]
    fn import() {
        assert_eq!(parse_str_top_level("IMPORT \"foo\" \"bar\" AS a : b I32"),
        Ok(TopLevel::Import(vec!["foo", "bar"],
                            Ident { name: "a", namespace: vec!["b"] },
                            Type::I32)));

        assert_eq!(parse_str_top_level(
                "IMPORT \"foo\" AS foo (I32 a, b, F32 c): I32"),
            Ok(TopLevel::Import(vec!["foo"],
                                Ident { name: "foo", namespace: vec![] },
                                Type::Proc(vec![Type::I32, Type::I32, Type::F32],
                                           Box::new(Type::I32)))));
    }

    #[test]
    fn var_decl() {
        assert_eq!(parse_str_top_level("VAR END"),
        Ok(TopLevel::VarDecl(vec![], vec![])));

        assert_eq!(parse_str_top_level("VAR : V I32 foo, bar := 3, baz END"),
        Ok(TopLevel::VarDecl(vec!["V"],
                vec![
                    (Type::I32, vec![
                        ("foo", None),
                        ("bar", Some(Expr::Lit(Literal::Int(3)))),
                        ("baz", None),
                    ])
                ])));

        assert_eq!(parse_str_top_level("VAR I32 foo, I64 bar := 7 END"),
        Ok(TopLevel::VarDecl(vec![],
                vec![
                    (Type::I32, vec![("foo", None)]),
                    (Type::I64, vec![("bar", Some(Expr::Lit(Literal::Int(7))))]),
                ])));
    }

    #[test]
    fn const_decl() {
        assert_eq!(parse_str_top_level("CONST END"),
        Ok(TopLevel::ConstDecl(vec![], vec![])));

        assert_eq!(parse_str_top_level("CONST : re.ee foo = 3, bar = 7, END"),
        Ok(TopLevel::ConstDecl(vec!["re", "ee"],
                vec![
                    ("foo", Expr::Lit(Literal::Int(3))),
                    ("bar", Expr::Lit(Literal::Int(7))),
                ])));
    }

    #[test]
    fn proc_call() {
        assert_eq!(parse_str_expr("a.b(c(), 1+2)"),
        Ok(Expr::ProcCall(Ident { name: "a", namespace: vec!["b"] },
                vec![
                    Expr::ProcCall(Ident { name: "c", namespace: vec![] },
                        vec![]),
                    Expr::Binop(Binop::Add,
                        Box::new(Expr::Lit(Literal::Int(1))),
                        Box::new(Expr::Lit(Literal::Int(2))))
                ])))
    }

    #[test]
    fn proc_def() {
        assert_eq!(parse_str_top_level(
                "PROC foo : bar (I32 a, b, F64 c): I32
                EXPORT \"exp\"
                I32 foo
                CONST bar = 1
                VAR I64 baz
                DO
                    ;;
                    foo := 3;;
                    RETURN 7
                END
                "),
        Ok(TopLevel::ProcDecl( Ident {name: "foo", namespace: vec!["bar"]},
                vec![
                    (Type::I32, vec!["a", "b"]),
                    (Type::F64, vec!["c"])],
                Type::I32, Some("exp"),
                vec![
                    ProcDeclBlock::Var(vec![
                        (Type::I32, vec![("foo", None)]),
                    ]),
                    ProcDeclBlock::Const(vec![
                        ("bar", Expr::Lit(Literal::Int(1)))
                    ]),
                    ProcDeclBlock::Var(vec![
                        (Type::I64, vec![("baz", None)]),
                    ]),
                ], vec![
                    Stmt::Assign(Ident { name: "foo", namespace: vec![] },
                        Expr::Lit(Literal::Int(3))),
                    Stmt::Return(Expr::Lit(Literal::Int(7)))
                ])));

        assert_eq!(parse_str_top_level("PROC foo DO i := 1 END"),
        Ok(TopLevel::ProcDecl(Ident {name: "foo", namespace: vec![]},
                vec![], Type::Void, None, vec![], vec![
                    Stmt::Assign(Ident { name: "i", namespace: vec![] },
                        Expr::Lit(Literal::Int(1)))
                ])))
    }
}
