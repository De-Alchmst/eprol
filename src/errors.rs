use crate::{
    lexer::Token,
};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::{
    Rich,
    SimpleSpan,
};
use::std::sync::Mutex;

static ERROR_APPEARED: Mutex<bool> = Mutex::new(false);
pub fn error_appeared() -> bool { *ERROR_APPEARED.lock().unwrap() }
fn set_error_appeared() {
    let mut error_appeared = ERROR_APPEARED.lock().unwrap();
    *error_appeared = true;
}

pub fn report_parser_error<'a>(
    error: Rich<'_, Token<'_>, SimpleSpan>,
    source_name: &'a String,
    source: &'a str,
){
    set_error_appeared();
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


pub fn report_semantic_error<'a>(
    span: &'a SimpleSpan,
    source_name: &'a String,
    source: &'a str,
    error_title: &'a str,
    error_message: String,
){
    set_error_appeared();
    Report::build(ReportKind::Error, (source_name.clone(), span.into_range()))
        .with_config(ariadne::Config::new()
                        .with_index_type(ariadne::IndexType::Byte))
        .with_message(error_title)
        .with_label(
            Label::new((source_name.clone(), span.into_range()))
                .with_message(error_message)
                .with_color(Color::Red),
        )
        .finish()
        .print(sources([(source_name.clone(), source)]))
        .unwrap();
}
