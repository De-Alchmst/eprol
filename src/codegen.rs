use crate::{
    ir::*,
    ast::Type,
};
use std::{
    collections::{HashMap},
    sync::{Mutex, OnceLock},
};

pub fn print_wat_header() {
    println!("(module")
}

pub fn print_wat_footer() {
    println!(")")
}

pub fn print_import_ir(lst: &ImportIRList) {
    for ir in lst {
        let (outer_name, raw_name, typ) = ir;
        {
            match typ {
                IRType::Func(arg_types, ret_type) => {
                    println!(" (import {} (func {} (param {}) {}))",
                        outer_name.iter().map(|s| format!("\"{}\"", s))
                                  .collect::<Vec<String>>().join(" "),
                        raw_name,
                        arg_types.iter().map(|t| t.to_string())
                                 .collect::<Vec<String>>().join(" "),
                        irtype_to_return(&*ret_type))
                },

                _ => {
                    println!(" (global {} (import {}) (mut {}))",
                        raw_name,
                        outer_name.iter().map(|s| format!("\"{}\"", s))
                                  .collect::<Vec<String>>().join(" "),
                        typ)
                }
            }
        }
    }
}


pub fn print_memory(pages: i32, import: Vec<&str>) {
    let import_str = if import.len() > 0 {
        format!(" (import {})",
                import.iter().map(|s| format!("\"{}\"", s))
                      .collect::<Vec<String>>().join(" "))
    } else {
        String::from("")
    };
    println!(" (memory {} {})", import_str, pages);
}


static DATA_INFO: OnceLock<Mutex<HashMap<String, u32>>> = OnceLock::new();
fn get_data_info<'a>() -> &'static Mutex<HashMap<String, u32>> {
    DATA_INFO.get_or_init(|| Mutex::new(HashMap::new()))
}

fn u32_to_hex_escapes(value: u32) -> String {
    value.to_le_bytes().iter().map(|b| format!("\\{:02x}", b)).collect()
}

// TODO: escape strings
pub fn print_data(mut data_top: u32, lst: Vec<String>) {
    let mut data_info = get_data_info().lock().unwrap();

    for s in lst {
        let length = s.len() as u32;
        println!(" (data (i32.const {}) \"{}{}\")",
                 data_top, u32_to_hex_escapes(length), s);
        // + 4 for length data
        data_info.insert(s, data_top + 4);
        data_top += length + 4;
    }
}


pub fn print_top_level_ir(lst: &TopLevelIRList) {
    for ir in lst {
        match ir {
            TopLevelIR::GlobalVar(raw_name, val) => {
                let (typ, _) = val;
                println!(" (global {} (mut {}) {})",
                         raw_name, typ, ir_to_str(val))
            },

            TopLevelIR::Proc(raw_name, args, ret_type, export, locals, body) => {
                let export = if let Some(s) = export {
                    format!(" (export \"{}\")", s)
                } else {
                    String::from("")
                };
                println!("\n (func {}{} {} {}\n   {}",
                         raw_name, export,
                         args.iter().map(|(name, typ)|
                             format!("(param {} {})", name, typ))
                             .collect::<Vec<String>>().join(" "),
                         irtype_to_return(ret_type),
                         locals.iter()
                               .map(|(name, typ)| format!("(local {name} {typ})"))
                               .collect::<Vec<String>>().join("\n   "));

                for ir in body {
                    println!("   {}", ir_to_str(ir))
                }
                println!(" )")
            }
        }
    }
}

fn irtype_to_return(typ: &IRType) -> String {
    match typ {
        IRType::Void => String::from(""),
        _ => format!("(result {})", typ)
    }
}

fn ir_to_str(ir: &(IRType, IR)) -> String {
    let (typ, val) = ir;
    match val {
        IR::Drop => format!("drop"),
        IR::LitInt(i) => format!("{typ}.const {i}"),
        IR::LitFloat(f) => format!("{typ}.const {f}"),
        IR::Call(raw_name) => format!("call {raw_name}"),
        IR::GlobalGet(raw_name) => format!("global.get {raw_name}"),
        IR::GlobalSet(raw_name) => format!("global.set {raw_name}"),
        IR::LocalGet(raw_name) => format!("local.get {raw_name}"),
        IR::LocalSet(raw_name) => format!("local.set {raw_name}"),
        IR::Add => format!("{typ}.add"),
        IR::Sub => format!("{typ}.sub"),
        IR::Mul(_signedp) => format!("{typ}.mul"),
        IR::Div(signedp) => format!("{typ}.add_{}", if *signedp {"s"} else {"u"}),
        IR::Return => format!("return"),

        IR::LitStr(s) =>
            format!("i32.const {}",
                    get_data_info().lock().unwrap().get(s).unwrap_or(&0)),

        IR::Neg =>
            match typ {
                IRType::F32 | IRType::F64 => format!("{typ}.neg"),
                _ => unimplemented!()
        }

        IR::Cast(source_type) =>
            match source_type {
                IRType::I32 =>
                    match typ {
                        IRType::I64 => "i64.extend_i32_s",
                        IRType::F32 => "f32.convert_i32_s",
                        IRType::F64 => "f64.convert_i32_s",
                        _ => unimplemented!()
                    }

                IRType::I64 =>
                    match typ {
                        IRType::I32 => "i32.wrap_i64",
                        IRType::F32 => "f32.convert_i64_s",
                        IRType::F64 => "f64.convert_i64_s",
                        _ => unimplemented!()
                    }

                IRType::F32 =>
                    match typ {
                        IRType::I32 => "i32.trunc_f32_s",
                        IRType::I64 => "i64.trunc_f32_s",
                        IRType::F64 => "f64.promote_f32",
                        _ => unimplemented!()
                    }

                IRType::F64 =>
                    match typ {
                        IRType::I32 => "i32.trunc_f64_s",
                        IRType::I64 => "i64.trunc_f64_s",
                        IRType::F32 => "f32.demote_f64",
                        _ => unimplemented!()
                    }

                _ => unimplemented!()
            }.to_string(),

        IR::Load(source_typ) =>
            match typ {
                IRType::I32 =>
                    match source_typ {
                        Type::I8  => "i32.load8_s",
                        Type::U8  => "i32.load8_u",
                        Type::I16 => "i32.load16_s",
                        Type::U16 => "i32.load16_u",
                        Type::I32 | Type::U32 => "i32.load",
                        _ => unimplemented!()
                    }

                IRType::I64 =>
                    match source_typ {
                        Type::I8  => "i64.load8_s",
                        Type::U8  => "i64.load8_u",
                        Type::I16 => "i64.load16_s",
                        Type::U16 => "i64.load16_u",
                        Type::I32 => "i64.load32_s",
                        Type::U32 => "i64.load32_u",
                        Type::I64 | Type::U64 => "i64.load",
                        _ => unimplemented!()
                    }

                IRType::F32 =>
                    match source_typ {
                        Type::F32 => "f32.load",
                        _ => unimplemented!()
                    }

                IRType::F64 =>
                    match source_typ {
                        Type::F64 => "f64.load",
                        _ => unimplemented!()
                    }
                _ => unimplemented!()
            }.to_string(),

        IR::Store(source_typ) =>
            match typ {
                IRType::I32 =>
                    match source_typ {
                        Type::I8  | Type::U8  => "i32.store8",
                        Type::I16 | Type::U16 => "i32.load16",
                        Type::I32 | Type::U32 => "i32.store",
                        _ => unimplemented!()
                    }

                IRType::I64 =>
                    match source_typ {
                        Type::I8  | Type::U8  => "i64.store8",
                        Type::I16 | Type::U16 => "i64.store16",
                        Type::I32 | Type::U32 => "i64.store32",
                        Type::I64 | Type::U64 => "i64.store",
                        _ => unimplemented!()
                    }

                IRType::F32 =>
                    match source_typ {
                        Type::F32 => "f32.store",
                        _ => unimplemented!()
                    }

                IRType::F64 =>
                    match source_typ {
                        Type::F64 => "f64.store",
                        _ => unimplemented!()
                    }
                _ => unimplemented!()
            }.to_string(),

        _ => unimplemented!("{:?}", val)
    }
}
