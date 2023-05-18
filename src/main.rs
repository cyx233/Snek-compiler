use crate::compiler::compile;
use crate::errors::*;
use crate::parser::parse_code;
use std::fs::File;
use std::io::prelude::*;
use std::{env, vec};

mod compiler;
mod errors;
mod instr;
mod parser;
mod syntax;

fn get_err_instrs(label: &String, errcode: i64) -> String {
    let code_instr = format!("\tmov rdi,{}", errcode);
    vec![
        (label.clone() + ":").as_str(),
        code_instr.as_str(),
        "\tand rsp, -16",
        "\tcall snek_error",
    ]
    .join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_contents = String::new();
    let mut in_file = File::open(in_name)?;
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_code(&in_contents);
    let result = compile(&expr);

    let asm_program = [
        "section .text",
        "extern snek_error",
        "extern snek_print",
        "extern snek_eq",
        "global our_code_starts_here",
        "add r15,8",
        "and r15,-8",
        &result,
        ";",
        &get_err_instrs(&ERR_INVALID_ARG_LABEL, *ERR_INVALID_ARG_CODE),
        ";",
        &get_err_instrs(&ERR_OVERFLOW_LABEL, *ERR_OVERFLOW_CODE),
        ";",
        &get_err_instrs(&ERR_OUT_OF_BOUND_LABEL, *ERR_OUT_OF_BOUND_CODE),
    ]
    .join("\n");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
