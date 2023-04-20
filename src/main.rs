use std::fs::File;
use std::io::prelude::*;
use std::{env, vec};

use sexp::Atom::*;
use sexp::*;

use im::HashMap;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref ID_REGEX: Regex = Regex::new(r"^[a-zA-Z][a-zA-Z0-9_-]*$").unwrap();
}

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
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

fn parse_bind(s: &Sexp) -> Result<(String, Expr), String> {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(id)), e] => {
                if ID_REGEX.is_match(id) {
                    let e_instrs = parse_expr(e)?;
                    Ok((id.clone(), e_instrs))
                } else {
                    Err(format!("Invalid ID {}", id))
                }
            }
            _ => Err("bad bind".to_string()),
        },
        _ => Err("bindings is not a List".to_string()),
    }
}

fn parse_expr(s: &Sexp) -> Result<Expr, String> {
    match s {
        Sexp::Atom(I(n)) => i32::try_from(*n)
            .map(Expr::Number)
            .map_err(|e| e.to_string()),
        Sexp::Atom(S(id)) => Ok(Expr::Id(id.clone())),
        Sexp::List(vec) => match &vec[..] {
            // (let, <bindings>, <expr>) => Let
            [Sexp::Atom(S(op)), e1, e2] if op == "let" => match e1 {
                Sexp::List(bindings) if !bindings.is_empty() => {
                    let env = bindings
                        .iter()
                        .map(|b| parse_bind(b))
                        .collect::<Result<Vec<(String, Expr)>, String>>()?;
                    let body = parse_expr(e2)?;
                    Ok(Expr::Let(env, Box::new(body)))
                }
                Sexp::List(bindings) if bindings.is_empty() => Err("empty binding".to_string()),
                _ => Err("Bad Sytax: Let".to_string()),
            },
            // (<op>, <expr>) => UnOp
            [Sexp::Atom(S(op)), e] => {
                let e_instrs = parse_expr(e)?;
                match op.as_str() {
                    "add1" => Ok(Expr::UnOp(Op1::Add1, Box::new(e_instrs))),
                    "sub1" => Ok(Expr::UnOp(Op1::Sub1, Box::new(e_instrs))),
                    _ => Err(format!("Unknow operator {}", op)),
                }
            }
            // (<op>, <expr>, <expr>) => BinOp
            [Sexp::Atom(S(op)), e1, e2] => {
                let expr_op = match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    _ => return Err(format!("Unknow operator {}", op)),
                };
                let e1_instrs = parse_expr(e1)?;
                let e2_instrs = parse_expr(e2)?;
                Ok(Expr::BinOp(
                    expr_op,
                    Box::new(e1_instrs),
                    Box::new(e2_instrs),
                ))
            }
            _ => Err("Bad Syntax: Sexp::List".to_string()),
        },
        _ => Err("Bad Syntax: Sexp".to_string()),
    }
}

impl Reg {
    fn to_string(&self) -> String {
        match self {
            Reg::RAX => "rax".to_string(),
            Reg::RBX => "rbx".to_string(),
            Reg::RSP => "rsp".to_string(),
        }
    }
}

impl Val {
    fn to_string(&self) -> String {
        match self {
            Val::Imm(n) => n.to_string(),
            Val::Reg(reg) => reg.to_string(),
            Val::RegOffset(reg, offset) => format!("[{}-{}]", reg.to_string(), offset * 8),
        }
    }
}

impl Instr {
    fn to_string(&self) -> String {
        match self {
            Instr::IMov(v1, v2) => format!("mov {},{}", v1.to_string(), v2.to_string()),
            Instr::IAdd(v1, v2) => format!("add {},{}", v1.to_string(), v2.to_string()),
            Instr::ISub(v1, v2) => format!("sub {},{}", v1.to_string(), v2.to_string()),
            Instr::IMul(v1, v2) => format!("imul {},{}", v1.to_string(), v2.to_string()),
        }
    }
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>) -> Result<Vec<Instr>, String> {
    match e {
        Expr::Number(n) => Ok(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))]),
        Expr::Id(id) => match env.get(id) {
            Some(n) => Ok(vec![Instr::IMov(
                Val::Reg(Reg::RAX),
                Val::RegOffset(Reg::RSP, *n),
            )]),
            None => Err(format!("Unbound variable identifier {id}")),
        },
        Expr::Let(bindings, body) => {
            let mut result: Vec<Instr> = Vec::new();
            let mut nenv: HashMap<String, i32> = env.clone();
            let mut cur_env: HashMap<String, i32> = HashMap::new();
            let mut cur_si = si;
            for (id, value_expr) in bindings {
                if cur_env.contains_key(id) {
                    return Err("Duplicate binding".to_string());
                } else {
                    cur_env.insert(id.clone(), cur_si);
                    nenv.insert(id.clone(), cur_si);
                    let bind_instrs = compile_to_instrs(value_expr, cur_si, &nenv)?;
                    result.extend(bind_instrs);
                    result.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, cur_si),
                        Val::Reg(Reg::RAX),
                    ));
                    cur_si += 1;
                }
            }
            let body_instrs = compile_to_instrs(&body, cur_si, &nenv)?;
            result.extend(body_instrs);
            Ok(result)
        }
        Expr::UnOp(op, e) => {
            let mut result = compile_to_instrs(e, si, env)?;
            match op {
                Op1::Add1 => {
                    result.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                }
                Op1::Sub1 => {
                    result.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
                }
            };
            Ok(result)
        }
        Expr::BinOp(op, e1, e2) => {
            let e1_instrs = compile_to_instrs(e1, si, env)?;
            let concat_instrs = vec![Instr::IMov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            )];
            let e2_instrs = compile_to_instrs(e2, si + 1, env)?;

            let op_instrs = match op {
                Op2::Plus => vec![Instr::IAdd(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, si),
                )],
                Op2::Minus => {
                    vec![
                        Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si)),
                        Instr::ISub(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                        Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                    ]
                }
                Op2::Times => {
                    vec![Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    )]
                }
            };
            let mut result = e1_instrs;
            result.extend(concat_instrs);
            result.extend(e2_instrs);
            result.extend(op_instrs);
            Ok(result)
        }
    }
}

fn generate_expr(s: &String) -> Expr {
    match parse(s) {
        Ok(sexpr) => match parse_expr(&sexpr) {
            Ok(expr) => expr,
            Err(msg) => {
                println!("Parse Error: {}", msg);
                panic!("Invalid")
            }
        },
        Err(msg) => {
            println!("Sexpr Error: {}", msg);
            panic!("Invalid")
        }
    }
}

fn compile(e: &Expr) -> String {
    match compile_to_instrs(e, 2, &HashMap::new()) {
        Ok(instrs) => instrs
            .iter()
            .map(|instr| instr.to_string())
            .collect::<Vec<String>>()
            .join("\n"),
        Err(msg) => panic!("{}", msg),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_contents = String::new();
    let mut in_file = File::open(in_name)?;
    in_file.read_to_string(&mut in_contents)?;

    let expr = generate_expr(&in_contents);
    let result = compile(&expr);

    let asm_program = vec![
        "section .text",
        "extern snek_error",
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
