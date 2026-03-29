use crate::ast::Ident;

pub fn raw_name<'a>(name: &Ident, source: &'a str) -> String {
    let nmsp = if name.namespace.is_empty() {
        ""
    } else {
        &(name.namespace.join(".") + ".")
    };
    format!("${source}:{nmsp}{}", name.name)
}

pub fn raw_arg_name<'a, 'b>(name: &'a str, proc_name: &'b String) -> String {
    format!("{proc_name}:{name}")
}
