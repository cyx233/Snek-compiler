use std::fs::File;
use std::io::prelude::*;
use std::{env, vec};

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}

fn parse_binding(s: &Sexp) -> (String, Expr) {
    if let Sexp::List(vec) = s {
        if let [Sexp::Atom(S(id)), e] = &vec[..] {
            (id.clone(), parse_expr(e))
        } else {
            panic!("Invalid")
        }
    } else {
        panic!("Invalid")
    }
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(id)) => Expr::Id(id.clone()),
        Sexp::List(vec) => match &vec[..] {
            // (let, <bindings>, <expr>) => Let
            [Sexp::Atom(S(op)), e1, e2] if op == "let" => {
                let mut env = Vec::new();
                if let Sexp::List(bindings) = e1 {
                    for binding in bindings {
                        env.push(parse_binding(binding));
                    }
                    Expr::Let(env, Box::new(parse_expr(e2)))
                } else {
                    panic!("Invalid")
                }
            }
            // (<op>, <expr>) => UnOp
            [Sexp::Atom(S(op)), e] => match op.as_str() {
                "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                _ => panic!("Invalid"),
            },
            // (<op>, <expr>, <expr>) => BinOp
            [Sexp::Atom(S(op)), e1, e2] => {
                let expr_op = match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    _ => panic!("Invalid"),
                };
                Expr::BinOp(expr_op, Box::new(parse_expr(e1)), Box::new(parse_expr(e2)))
            }
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}

fn compile_expr(e: &Expr, si: i32, env: &HashMap<String, i32>) -> String {
    match e {
        Expr::Number(n) => format!("mov rax, {}", *n),
        Expr::Id(id) => match env.get(id) {
            Some(n) => format!("mov rax, [rsp - {n}]"),
            None => panic!("Unbound variable identifier {id}"),
        },
        Expr::Let(bindings, body) => {
            let mut nenv = env.clone();
            let mut cur_si = si;
            let mut binding_instrs: Vec<String> = Vec::new();
            for (id, value_expr) in bindings {
                if nenv.contains_key(id) {
                    panic!("Duplicate binding")
                } else {
                    binding_instrs.push(compile_expr(value_expr, cur_si, &nenv));
                    binding_instrs.push(format!("mov [rsp-{}], rax", cur_si * 8));
                    nenv.insert(id.clone(), cur_si * 8);
                    cur_si += 1;
                }
            }
            let body_instrs = compile_expr(&body, cur_si + 1, &nenv);
            vec![binding_instrs.join("\n"), body_instrs].join("\n")
        }
        Expr::UnOp(op, e) => match op {
            Op1::Add1 => vec![compile_expr(e, si, env), "add rax, 1".to_string()].join("\n"),
            Op1::Sub1 => vec![compile_expr(e, si, env), "sub rax, 1".to_string()].join("\n"),
        },
        Expr::BinOp(op, e1, e2) => {
            let e1_instrs = compile_expr(e1, si, env);
            let concat_instrs = format!("mov [rsp-{}], rax", si * 8);
            let e2_instrs = compile_expr(e2, si + 1, env);
            let op_instrs = match op {
                Op2::Plus => {
                    format!("add rax, [rsp-{}]", si * 8)
                }
                Op2::Minus => {
                    format!("sub [rsp-{}], rax", si * 8)
                }
                Op2::Times => {
                    format!("imul rax, [rsp-{}]", si * 8)
                }
            };
            vec![e1_instrs, concat_instrs, e2_instrs, op_instrs].join("\n")
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    // You will make result hold the result of actually compiling
    let mut in_contents = String::new();
    let mut in_file = File::open(in_name)?;
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let result = compile_expr(&expr, 2, &HashMap::new());

    let asm_program = vec![
        "section .text",
        "global our_code_starts_here",
        "our_code_starts_here:",
        &result,
        "ret",
    ]
    .join("\n");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
