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

fn parse_bind(s: &Sexp) -> (String, Expr) {
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
        Sexp::Atom(S(id)) => {
            let re = regex::Regex::new(r"^[a-zA-Z][a-zA-Z0-9_-]*$").unwrap();
            if re.is_match(id) {
                Expr::Id(id.clone())
            } else {
                panic!("Invalid")
            }
        }
        Sexp::List(vec) => match &vec[..] {
            // (let, <bindings>, <expr>) => Let
            [Sexp::Atom(S(op)), e1, e2] if op == "let" => {
                let mut env = Vec::new();
                if let Sexp::List(bindings) = e1 {
                    for binding in bindings {
                        env.push(parse_bind(binding));
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

impl Reg {
    fn to_string(&self) -> String {
        match self {
            Reg::RAX => "rax".to_string(),
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

fn compile(e: &Expr) -> String {
    compile_to_instrs(e, 2, &HashMap::new())
        .iter()
        .map(|instr| instr.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))],
        Expr::Id(id) => match env.get(id) {
            Some(n) => {
                vec![Instr::IMov(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, *n),
                )]
            }
            None => {
                panic!("Unbound variable identifier {id}")
            }
        },
        Expr::Let(bindings, body) => {
            let mut result: Vec<Instr> = Vec::new();
            let mut nenv: HashMap<String, i32> = env.clone();
            let mut cur_env: HashMap<String, i32> = HashMap::new();
            let mut cur_si = si;
            for (id, value_expr) in bindings {
                if cur_env.contains_key(id) {
                    panic!("Duplicate binding")
                } else {
                    cur_env.insert(id.clone(), cur_si);
                    nenv.insert(id.clone(), cur_si);
                    result.extend(compile_to_instrs(value_expr, si, &nenv));
                    result.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, cur_si),
                        Val::Reg(Reg::RAX),
                    ));
                    cur_si += 1;
                }
            }
            result.extend(compile_to_instrs(body, cur_si, &nenv));
            result
        }
        Expr::UnOp(op, e) => {
            let mut result: Vec<Instr> = Vec::new();
            match op {
                Op1::Add1 => {
                    result.extend(compile_to_instrs(e, si, env));
                    result.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
                }
                Op1::Sub1 => {
                    result.extend(compile_to_instrs(e, si, env));
                    result.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)));
                }
            };
            result
        }
        Expr::BinOp(op, e1, e2) => {
            let mut result: Vec<Instr> = Vec::new();
            result.extend(compile_to_instrs(e1, si, env));
            result.push(Instr::IMov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            ));
            result.extend(compile_to_instrs(e2, si + 1, env));

            match op {
                Op2::Plus => {
                    result.push(Instr::IAdd(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                }
                Op2::Minus => {
                    result.push(Instr::ISub(
                        Val::RegOffset(Reg::RSP, si),
                        Val::Reg(Reg::RAX),
                    ));
                    result.push(Instr::IMov(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                }
                Op2::Times => {
                    result.push(Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    ));
                }
            };
            result
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
    let result = compile(&expr);

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
