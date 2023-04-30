use crate::errors::*;
use crate::instr::compile;
use crate::parser::parse_code;
use std::fs::File;
use std::io::prelude::*;
use std::{env, vec};

mod errors;
mod instr;
mod parser;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_contents = String::new();
    let mut in_file = File::open(in_name)?;
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_code(&in_contents);
    let result = compile(&expr);

    let invalid_arg_instr = format!("\tmov rdi,{}\n\tjmp snek_error", *ERR_INVALID_ARG_CODE);

    let overflow_intrs = format!("\tmov rdi,{}\n\tjmp snek_error", *ERR_OVERFLOW_CODE);

    let asm_program = vec![
        "section .text",
        "extern snek_error",
        "global our_code_starts_here",
        &(ERR_OVERFLOW_LABEL.clone() + ":"),
        &overflow_intrs,
        &(ERR_INVALID_ARG_LABEL.clone() + ":"),
        &invalid_arg_instr,
        "our_code_starts_here:",
        &result,
        "\tret",
    ]
    .join("\n");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
