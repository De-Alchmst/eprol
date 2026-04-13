use crate::{
    lexer::Token,
};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::{
    Rich,
    SimpleSpan,
};

pub fn report_parser_error<'a>(
    error: Rich<'_, Token<'_>, SimpleSpan>,
    source_name: &'a String,
    source: &'a str,
){
    Report::build(ReportKind::Error, (source_name.clone(), error.span().into_range()))
        .with_config(ariadne::Config::new()
                        .with_index_type(ariadne::IndexType::Byte))
        .with_message(error.to_string())
        .with_label(
            Label::new((source_name.clone(), error.span().into_range()))
                .with_message(error.reason().to_string())
                .with_color(Color::Red),
        )
        .with_labels(error.contexts().map(|(label, span)| {
            Label::new((source_name.clone(), span.into_range()))
                .with_message(format!("while parsing this {label}"))
                .with_color(Color::Yellow)
        }))
        .finish()
        .print(sources([(source_name.clone(), source)]))
        .unwrap();
}
