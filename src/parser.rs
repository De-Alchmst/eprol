use crate::ast::*;
use crate::lexer::Token;
use logos::Logos;
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};

type TokenStream<'src> = Stream<
    std::iter::Map<
        logos::SpannedIter<'src, Token<'src>>,
        fn((Result<Token<'src>, ()>, std::ops::Range<usize>)) -> (Token<'src>, SimpleSpan),
    >,
>;

pub fn make_token_stream(input: &str) -> TokenStream<'_> {
    let token_iter = Token::lexer(input)
        .spanned()
        .map((|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        }) as fn(_) -> _);

    Stream::from_iter(token_iter)
        .map((0..input.len()).into(), |(t, s): (_, _)| (t, s))
}

pub fn parse_expr<'src>(
    token_stream: TokenStream<'src>,
) -> Result<Expr<'src>, Vec<Rich<'src, Token<'src>, SimpleSpan>>> {
    expr().parse(token_stream).into_result()
}

// pub fn parse_str_expr(input: &str) -> Result<Expr<'_>, Vec<Rich<'_, Token<'_>, SimpleSpan<usize, ()>>>>   {
//     // Create a logos lexer over the source code
//     let token_iter = Token::lexer(input)
//         .spanned()
//         // Convert logos errors into tokens. We want parsing to be recoverable
//         // and not fail at the lexing stage, so we have a dedicated
//         // `Token::Error` variant that represents a token error that was
//         // previously encountered
//         .map(|(tok, span)| match tok {
//             // Turn the `Range<usize>` spans logos gives us into chumsky's
//             // `SimpleSpan` via `Into`, because it's easier to work with
//             Ok(tok) => (tok, span.into()),
//             Err(()) => (Token::Error, span.into()),
//         });

//     // Turn the token iterator into a stream that chumsky can use for things
//     // like backtracking
//     let token_stream = Stream::from_iter(token_iter)
//         // Tell chumsky to split the (Token, SimpleSpan) stream into its parts
//         // so that it can handle the spans for us
//         // This involves giving chumsky an 'end of input' span: we just use a
//         // zero-width span at the end of the string
//         .map((0..input.len()).into(), |(t, s): (_, _)| (t, s));

//     expr().parse(token_stream).into_result()
// }


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
            let name = parts.pop().unwrap(); // safe: at_least(1)
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
            select!{
                Token::Int(n) => Expr::Lit(Literal::Int(n.parse().unwrap_or(0))),
                Token::Float(n) => Expr::Lit(Literal::Float(n.parse().unwrap_or(0.0))),
                Token::True => Expr::Lit(Literal::Int(1)),
                Token::False => Expr::Lit(Literal::Int(0)),
                Token::String(s) => Expr::Lit(Literal::Str(s))
            },
            exp.delimited_by(just(Token::LRound), just(Token::RRound)),
            ident().map(Expr::Ident),
        )).boxed(); // needs to be boxed, or cannot unop.clone()

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
) -> impl Parser<'tokens, I, Stmt<'tokens>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let assignment = ident()
        .then_ignore(just(Token::Assign))
        .then(expr())
        .map(|(idnt, exp)|
            Stmt::Assign(idnt, exp));

    assignment
}


// fn top_level<'tokens, 'src: 'tokens, I>(
// ) -> impl Parser<'tokens, I, TopLevel, extra::Err<Rich<'tokens, Token<'src>>>>
// where
//     I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
// {

// }


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
    fn test_basic_expr() {
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
                Box::new(Expr::Ident(Ident { name: "c", namespace: vec!["a", "b"] })),
                Box::new(Expr::Ident(Ident { name: "ni", namespace: vec![] })))))
    }
}
