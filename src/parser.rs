use crate::ast::*;
use crate::lexer::Token;
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

fn expr<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Expr<'src>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|_exp| {
        let end_node = select!{
            Token::Ident(i) => Expr::Ident(i),
            Token::Int(n) => Expr::Lit(Literal::Int(n.parse().unwrap_or(0))),
            Token::Float(n) => Expr::Lit(Literal::Float(n.parse().unwrap_or(0.0))),
            Token::True => Expr::Lit(Literal::Int(1)),
            Token::False => Expr::Lit(Literal::Int(0)),
            Token::String(s) => Expr::Lit(Literal::Str(s))
        };

        end_node
    })
}


// fn stmt<'tokens, 'src: 'tokens, I>(
// ) -> impl Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token<'src>>>>
// where
//     I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
// {

// }


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
}
