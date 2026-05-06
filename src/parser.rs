use crate::{
    ast::*,
    lexer::Token,
    errors::report_parser_error,
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
        .spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });
    let token_stream = Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    stmt().parse(token_stream).into_result()
}

pub fn parse_str_top_level(input: &str) -> Result<TopLevel<'_>, Vec<Rich<'_, Token<'_>, SimpleSpan<usize, ()>>>>   {
    let token_iter = Token::lexer(input)
        .spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });
    let token_stream = Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    top_level().parse(token_stream).into_result()
}

pub fn parse_str_program<'a>(
    input: &'a str, source_name: &'a String
) -> Program<'a> {
    let token_iter = Token::lexer(input)
        .spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });
    let token_stream = Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

    let (tokens, errors) = program().parse(token_stream).into_output_errors();
    for error in errors {
        report_parser_error(error, source_name, input);
    }

    match tokens {
        Some(tokens) => tokens,
        None => vec![],
    }
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


fn possibly_negative_int<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, i64, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    just(Token::Minus).or_not()
        .then(select! { Token::Int(n) => n })
        .map(|(neg, n)| {
            let n = n.parse().unwrap_or(0);
            if neg.is_some() { -n } else { n }
        })
}


fn accessor<'tokens, 'src: 'tokens, I, P>(
    exp: P,
) -> impl Parser<'tokens, I, Accessor<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
    P: Parser<'tokens, I, Expr<'src>, extra::Err<Rich<'tokens, Token<'src>>>> + Clone,
{
    just(Token::LSquare)
        .ignore_then(
            choice((
                // [type:len:count]
                simple_type()
                    .then_ignore(just(Token::Colon))
                    .then(possibly_negative_int().or_not())
                    .then_ignore(just(Token::Colon))
                    .then(exp.clone())
                    .map(|((typ, len), count)|
                        Accessor {
                            typ: typ,
                            offset_len: len.unwrap_or(1),
                            offset: count,
                        }),

                // [type:offset]
                simple_type()
                    .then_ignore(just(Token::Colon))
                    .then(exp.clone())
                    .map(|(typ, offset)|
                        Accessor {
                            typ: typ.clone(),
                            offset_len: type_len(typ),
                            offset: offset,
                        }),

                // [offset]
                exp.map(|offset|
                    Accessor {
                        typ: Type::I32,
                        offset_len: 4,
                        offset,
                    }),
            ))
        )
        .then_ignore(just(Token::RSquare))
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
                Token::Int(n) => Literal::Int(n.parse().unwrap_or(0)),
                Token::Float(n) => Literal::Float(n.parse().unwrap_or(0.0)),
                Token::True => Literal::Int(1),
                Token::False => Literal::Int(0),
                Token::String(s) => Literal::Str(s),
            }.map_with(|lit, e| Expr::Lit(
                    if !cfg!(test) {e.span()} else {PS},
                    lit)),

            // PARENTHESISED
            exp.clone().delimited_by(just(Token::LRound), just(Token::RRound)),

            // PROC CALL
            ident()
                .then(
                    exp.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LRound), just(Token::RRound)))
                .map_with(|(proc, args), e|
                    Expr::ProcCall(
                        if !cfg!(test) {e.span()} else {PS},
                        proc, args)),

            // IDENT
            ident().map_with(|i, e|
                Expr::Ident(if !cfg!(test) {e.span()} else {PS}, i)),
        )).boxed();

        let raw_access = end_node.clone().foldl_with(
            accessor(exp.clone())
            .repeated(),
            |lhs, accesses, e|
            Expr::Access(if !cfg!(test) {e.span()} else {PS},
                         Box::new(lhs), Box::new(accesses))
        ).boxed();

        let unop = choice((
            just(Token::Minus),
            just(Token::Plus),
            just(Token::Not)
        ))
            .repeated()
            .foldr_with(raw_access, |op, rhs, e| {
                let span = if !cfg!(test) {e.span()} else {PS};
                match op {
                    Token::Minus => Expr::Unop(span, Unop::Neg, Box::new(rhs)),
                    Token::Not   => Expr::Unop(span, Unop::Not, Box::new(rhs)),
                    Token::Plus  => rhs,
                    _ => unreachable!(),
                }
            });

        let binop_product = unop.clone().foldl_with(
            choice((
                just(Token::Star),
                just(Token::Slash),
            ))
            .then(unop)
            .repeated(),
            |lhs, (op, rhs), e|
            Expr::Binop(
                if !cfg!(test) {e.span()} else {PS},
                match op {
                    Token::Star  => Binop::Mul,
                    Token::Slash => Binop::Div,
                    _ => unreachable!(),
                },
                Box::new(lhs), Box::new(rhs))
        );

        let binop_sum = binop_product.clone().foldl_with(
            choice((
                just(Token::Plus),
                just(Token::Minus),
            ))
            .then(binop_product)
            .repeated(),
            |lhs, (op, rhs), e|
            Expr::Binop(
                if !cfg!(test) {e.span()} else {PS},
                match op {
                    Token::Plus  => Binop::Add,
                    Token::Minus => Binop::Sub,
                    _ => unreachable!(),
                },
                Box::new(lhs), Box::new(rhs))
        );

        // TODO: handle types as garbage terminators...
        let expr_garbage =
            none_of([
                Token::Semicolon, Token::End, Token::Comma, Token::Do,
                Token::To, Token::Downto, Token::Until, Token::Step,
                Token::LRound, Token::RRound, Token::RSquare,
                Token::Const, Token::Var, Token::Static, Token::Assign
            ])
            .repeated()
            .at_least(1);

        // Malform expression
        // might start with matching a valid expression
        // for example in `abc def + 42`, `abc` is a valid expression, but it's not
        // as a whole
        choice((
            // valid expression with trailing garbage
            binop_sum.clone()
                .ignore_then(expr_garbage.clone())
                .validate(|_, e, emitter| {
                    emitter.emit(Rich::custom(
                        if !cfg!(test) {e.span()} else {PS},
                        "Malformed expression"));
                    Expr::Malformed
                }),

            // valid expression
            binop_sum,

            // leading garbage
            expr_garbage
                .validate(|_, e, emitter| {
                    emitter.emit(Rich::custom(
                        if !cfg!(test) {e.span()} else {PS},
                        "Malformed expression"));
                    Expr::Malformed
                }),
        ))
    })
}


fn left_value<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, LeftValue<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    expr()
    .validate(|exp, e, emitter| {
        match exp {
            Expr::Ident(span, ident) => LeftValue::Ident(span, ident),
            Expr::Access(span, boxed_exp, boxed_acc)
                => LeftValue::Access(span, *boxed_exp, *boxed_acc),
            Expr::Malformed => LeftValue::Malformed,
            _ => {
                emitter.emit(Rich::custom(
                    if !cfg!(test) {e.span()} else {PS},
                    "Invalid left value"));
                LeftValue::Malformed
            }
        }
    })
}


fn bare_stmt<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Stmt<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        // ASSIGN
        left_value()
            .then_ignore(just(Token::Assign))
            .then(expr())
            .map(|(lv, exp)|
                Stmt::Assign(lv, exp)),

        // RETURN
        just(Token::Return)
            .ignore_then(expr())
            .map_with(|exp, e|
                Stmt::Return(if !cfg!(test) {e.span()} else {PS}, exp)),
        just(Token::Return).map_with(|_, e|
            Stmt::VoidReturn(if !cfg!(test) {e.span()} else {PS})),

        // PROC CALL
        // TODO: migrate to dedicated Stmt::ProcCall
        ident()
            .then(
                expr()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LRound), just(Token::RRound)))
            .map_with(|(proc, args), e|
                Stmt::Expr(Expr::ProcCall(
                    if !cfg!(test) {e.span()} else {PS},
                    proc, args))),
    ))
}


fn stmt<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Stmt<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((
        // Malformed statement
        // might start with matching a valid statement
        // for example in `abc() def := 42`, `abc()` is a valid statement,
        // but it's not as a whole
        bare_stmt().or_not().ignore_then(
            none_of([Token::Semicolon, Token::End])
            .repeated()
            .at_least(1)
        )
        .validate(|_, e, emitter| {
            emitter.emit(Rich::custom(
                if !cfg!(test) {e.span()} else {PS},
                "Malformed statement"));
            Stmt::Malformed
        }),

        bare_stmt()
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


fn access_type<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Type, extra::Err<Rich<'tokens, Token<'src>>>>
where
   I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
 {
    select!{Token::Type(s) => match s {
        "I8" |"i8"  => Type::I8,
        "I16"|"i16" => Type::I16,
        "I32"|"i32" => Type::I32,
        "I64"|"i64" => Type::I64,
        "U8" |"u8"  => Type::U8,
        "U16"|"u16" => Type::U16,
        "U32"|"u32" => Type::U32,
        "U64"|"u64" => Type::I64,
        "F32"|"f32" => Type::F32,
        "F64"|"f64" => Type::F64,
        _ => unreachable!(),
    }}
}


fn simple_type<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Type, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    access_type()
        .validate(|typ, e, emmiter| {
            match typ {
                Type::I32 | Type::I64 | Type::F32 | Type::F64 => typ,
                _ => {
                    emmiter.emit(Rich::custom(
                        if !cfg!(test) {e.span()} else {PS},
                        format!("`{:#?}` is not a valid type in this context",
                               typ)));
                    Type::I32
                }
            }
        })
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
                    .map_with(|(i, exp), e|
                        (if !cfg!(test) {e.span()} else {PS},
                         i, Some(exp))),

                select!{ Token::Ident(s) => s }
                    .map_with(|s, e|
                        (if !cfg!(test) {e.span()} else {PS},
                         s, None))
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
        .map_with(|(i, exp), e|
            (if !cfg!(test) {e.span()} else {PS}, i, exp))
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
                .map_with(|s, e| (if !cfg!(test) {e.span()} else {PS}, s))
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
            .map_with(|((strings, ident), typ), e|
                TopLevel::Import(
                    if !cfg!(test) {e.span()} else {PS},
                    strings, ident, typ)),

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
            .map_with(|ident, e| (if !cfg!(test) {e.span()} else {PS}, ident))
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
        assert_eq!(parse_str_expr("42"), Ok(Expr::Lit(PS, Literal::Int(42))));
    }

    #[test]
    fn basic_unop() {
        assert_eq!(parse_str_expr("NOT --+-42"),
        Ok(Expr::Unop(PS, Unop::Not,
            Box::new(Expr::Unop(PS, Unop::Neg,
                Box::new(Expr::Unop(PS, Unop::Neg,
                    Box::new(Expr::Unop(PS, Unop::Neg,
                        Box::new(Expr::Lit(PS, Literal::Int(42))))))))))))
    }

    #[test]
    fn basic_binop() {
        assert_eq!(parse_str_expr("-2 + (6 - 9) * +7 + 9"),
        Ok(Expr::Binop(PS, Binop::Add,
            Box::new(Expr::Binop(PS, Binop::Add,
                Box::new(Expr::Unop(PS, Unop::Neg,
                    Box::new(Expr::Lit(PS, Literal::Int(2))))),
                Box::new(Expr::Binop(PS, Binop::Mul,
                    Box::new(Expr::Binop(PS, Binop::Sub,
                        Box::new(Expr::Lit(PS, Literal::Int(6))),
                        Box::new(Expr::Lit(PS, Literal::Int(9))))),
                    Box::new(Expr::Lit(PS, Literal::Int(7))))))),
            Box::new(Expr::Lit(PS, Literal::Int(9))))))
    }

    #[test]
        fn basic_idents() {
        assert_eq!(parse_str_expr("a.b.c + ni"),
        Ok(Expr::Binop(PS, Binop::Add,
            Box::new(Expr::Ident(PS, Ident { name: "a",  namespace: vec!["b", "c"] })),
            Box::new(Expr::Ident(PS, Ident { name: "ni", namespace: vec![] })))))
    }

    #[test]
    fn assignment() {
        assert_eq!(parse_str_stmt("foo := 1-2"),
        Ok(Stmt::Assign(LeftValue::Ident(PS, Ident {name: "foo", namespace: vec![]}),
            Expr::Binop(PS, Binop::Sub,
                Box::new(Expr::Lit(PS, Literal::Int(1))),
                Box::new(Expr::Lit(PS, Literal::Int(2)))))))
    }

    #[test]
    fn expr_stmt() {
        assert_eq!(parse_str_expr("3+7"),
        Ok(Expr::Binop(PS, Binop::Add,
            Box::new(Expr::Lit(PS, Literal::Int(3))),
            Box::new(Expr::Lit(PS, Literal::Int(7))))))
    }

    #[test]
    fn raw_accessor() {
        assert_eq!(parse_str_stmt("a[4][I32::2] := 0"),
        Ok(Stmt::Assign(
            LeftValue::Access(PS,
                Expr::Access(PS,
                    Box::new(Expr::Ident(PS, Ident { name: "a", namespace: vec![] })),
                    Box::new(Accessor {
                        typ: Type::I32,
                        offset_len: 4,
                        offset: Expr::Lit(PS, Literal::Int(4))
                    })),
                Accessor {
                    typ: Type::I32,
                    offset_len: 1,
                    offset: Expr::Lit(PS, Literal::Int(2))
                 }),
            Expr::Lit(PS, Literal::Int(0)))));

        assert_eq!(parse_str_expr("a[3] + b[I32:2][F64:2:1]"),
        Ok(Expr::Binop(PS, Binop::Add,
            Box::new(Expr::Access(PS,
                Box::new(Expr::Ident(PS, Ident { name: "a", namespace: vec![] })),
                Box::new(Accessor {
                    typ: Type::I32,
                    offset_len: 4,
                    offset: Expr::Lit(PS, Literal::Int(3))
                }))),
            Box::new(Expr::Access(PS,
                Box::new(Expr::Access(PS,
                    Box::new(Expr::Ident(PS, Ident { name: "b", namespace: vec![] })),
                    Box::new(Accessor {
                        typ: Type::I32,
                        offset_len: 4,
                        offset: Expr::Lit(PS, Literal::Int(2))
                    }))),
                Box::new(Accessor {
                    typ: Type::F64,
                    offset_len: 2,
                    offset: Expr::Lit(PS, Literal::Int(1))
                }))))))
    }

    #[test]
    fn import() {
        assert_eq!(parse_str_top_level("IMPORT \"foo\" \"bar\" AS a : b I32"),
        Ok(TopLevel::Import(PS, vec!["foo", "bar"],
                            Ident { name: "a", namespace: vec!["b"] },
                            Type::I32)));

        assert_eq!(parse_str_top_level(
                "IMPORT \"foo\" AS foo (I32 a, b, F32 c): I32"),
            Ok(TopLevel::Import(PS, vec!["foo"],
                                Ident { name: "foo", namespace: vec![] },
                                Type::Proc(vec![Type::I32, Type::I32, Type::F32],
                                           Box::new(Type::I32)))));
    }

    #[test]
    fn var_decl() {
        assert_eq!(parse_str_top_level("VAR END"),
        Ok(TopLevel::VarDecl(vec![], vec![])));

        assert_eq!(parse_str_top_level("VAR : V I32 foo, bar := 3, baz END"),
        Ok(TopLevel::VarDecl(
            vec!["V"],
            vec![
                (Type::I32, vec![
                    (PS, "foo", None),
                    (PS, "bar", Some(Expr::Lit(PS, Literal::Int(3)))),
                    (PS, "baz", None),
                ])
            ])));

        assert_eq!(parse_str_top_level("VAR I32 foo, I64 bar := 7 END"),
        Ok(TopLevel::VarDecl(
            vec![],
            vec![
                (Type::I32, vec![(PS, "foo", None)]),
                (Type::I64, vec![(PS, "bar", Some(Expr::Lit(PS, Literal::Int(7))))]),
            ])));
    }

    #[test]
    fn const_decl() {
        assert_eq!(parse_str_top_level("CONST END"),
        Ok(TopLevel::ConstDecl(vec![], vec![])));

        assert_eq!(parse_str_top_level("CONST : re.ee foo = 3, bar = 7, END"),
        Ok(TopLevel::ConstDecl(
            vec!["re", "ee"],
            vec![
                (PS, "foo", Expr::Lit(PS, Literal::Int(3))),
                (PS, "bar", Expr::Lit(PS, Literal::Int(7))),
            ])));
    }

    #[test]
    fn proc_call() {
        assert_eq!(parse_str_expr("a.b(c(), 1+2)"),
        Ok(Expr::ProcCall(PS, Ident { name: "a", namespace: vec!["b"] },
            vec![
                Expr::ProcCall(PS, Ident { name: "c", namespace: vec![] },
                    vec![]),
                Expr::Binop(PS, Binop::Add,
                    Box::new(Expr::Lit(PS, Literal::Int(1))),
                    Box::new(Expr::Lit(PS, Literal::Int(2))))
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
        Ok(TopLevel::ProcDecl((PS,
            Ident {name: "foo", namespace: vec!["bar"]}),
            vec![
                (Type::I32, vec![(PS, "a"), (PS, "b")]),
                (Type::F64, vec![(PS, "c")])],
            Type::I32, Some("exp"),
            vec![
                ProcDeclBlock::Var(vec![
                    (Type::I32, vec![(PS, "foo", None)]),
                ]),
                ProcDeclBlock::Const(vec![
                    (PS, "bar", Expr::Lit(PS, Literal::Int(1)))
                ]),
                ProcDeclBlock::Var(vec![
                    (Type::I64, vec![(PS, "baz", None)]),
                ]),
            ], vec![
                Stmt::Assign(LeftValue::Ident(PS,
                                 Ident { name: "foo", namespace: vec![] }),
                    Expr::Lit(PS, Literal::Int(3))),
                Stmt::Return(PS, Expr::Lit(PS, Literal::Int(7)))
            ])));

        assert_eq!(parse_str_top_level("PROC foo DO i := 1 END"),
        Ok(TopLevel::ProcDecl((PS,
            Ident {name: "foo", namespace: vec![]}),
            vec![], Type::Void, None, vec![], vec![
                Stmt::Assign(LeftValue::Ident(PS,
                                 Ident { name: "i", namespace: vec![] }),
                    Expr::Lit(PS, Literal::Int(1)))
            ])))
    }
}
