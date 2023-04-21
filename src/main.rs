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
    static ref ERR_INVALID_ARG: i64 = 1;
    static ref ERR_OVERFLOW: i64 = 2;
}

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    Boolean(bool),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RDI,
}

#[derive(Debug)]
enum CondFlag {
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Always,
    Never,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Label(String),
    TypeTest(Val),
    Cmp(Val, Val),
    SetIfElse(Reg, Val, Val, CondFlag),
    JumpIf(String, CondFlag),
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Op2 {
    fn to_condflag(&self) -> CondFlag {
        match self {
            Op2::Equal => CondFlag::Equal,
            Op2::Greater => CondFlag::Greater,
            Op2::Less => CondFlag::Less,
            Op2::GreaterEqual => CondFlag::GreaterEqual,
            Op2::LessEqual => CondFlag::LessEqual,
            _ => CondFlag::Never,
        }
    }
}

#[derive(Debug)]
enum Expr {
    Number(i32),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
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
        Sexp::Atom(S(id)) => match id.as_str() {
            "true" => Ok(Expr::Boolean(true)),
            "false" => Ok(Expr::Boolean(false)),
            _ => Ok(Expr::Id(id.clone())),
        },
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
            // set! <name> <expr> => Set
            [Sexp::Atom(S(op)), Sexp::Atom(S(id)), e] if op == "set!" => {
                let e_instrs = parse_expr(e)?;
                Ok(Expr::Set(id.clone(), Box::new(e_instrs)))
            }
            // if <expr> <expr> <expr> => If
            [Sexp::Atom(S(op)), cond, if_sexp, else_sexp] if op == "if" => {
                let cond_expr = parse_expr(cond)?;
                let if_expr = parse_expr(if_sexp)?;
                let else_expr = parse_expr(else_sexp)?;
                Ok(Expr::If(
                    Box::new(cond_expr),
                    Box::new(if_expr),
                    Box::new(else_expr),
                ))
            }
            // (<op>, <expr>) => Block, Loop, Break, UnOp
            [Sexp::Atom(S(op)), e] => {
                let e_instrs = parse_expr(e)?;
                match op.as_str() {
                    "block" => {
                        if let Sexp::List(vec) = e {
                            let e_instrs = vec
                                .iter()
                                .map(parse_expr)
                                .collect::<Result<Vec<Expr>, String>>()?;
                            Ok(Expr::Block(e_instrs))
                        } else {
                            Err(format!("Block Bad Syntax"))
                        }
                    }
                    "loop" => Ok(Expr::Loop(Box::new(e_instrs))),
                    "break" => Ok(Expr::Break(Box::new(e_instrs))),
                    "add1" => Ok(Expr::UnOp(Op1::Add1, Box::new(e_instrs))),
                    "sub1" => Ok(Expr::UnOp(Op1::Sub1, Box::new(e_instrs))),
                    "isnum" => Ok(Expr::UnOp(Op1::IsNum, Box::new(e_instrs))),
                    "isbool" => Ok(Expr::UnOp(Op1::IsBool, Box::new(e_instrs))),
                    _ => Err(format!("Unknow operator {}", op)),
                }
            }
            // (<op>, <expr>, <expr>) => BinOp
            [Sexp::Atom(S(op)), e1, e2] => {
                let expr_op = match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    ">" => Op2::Greater,
                    "<" => Op2::Less,
                    ">=" => Op2::GreaterEqual,
                    "<=" => Op2::LessEqual,
                    "=" => Op2::Equal,
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
            Reg::RSP => "rsp".to_string(),
            Reg::RDI => "rdi".to_string(),
        }
    }
}

impl Val {
    fn to_string(&self) -> String {
        match self {
            Val::Imm(n) => (n << 1).to_string(),
            Val::Boolean(b) => {
                if *b {
                    "3".to_string()
                } else {
                    "1".to_string()
                }
            }
            Val::Reg(reg) => reg.to_string(),
            Val::RegOffset(reg, offset) => format!("[{}-{}]", reg.to_string(), offset * 8),
        }
    }
}

impl CondFlag {
    fn to_setop(&self) -> String {
        match self {
            CondFlag::Equal => "cmove",
            CondFlag::NotEqual => "cmovne",
            CondFlag::Greater => "cmovg",
            CondFlag::Less => "cmovl",
            CondFlag::GreaterEqual => "cmovge",
            CondFlag::LessEqual => "cmovle",
            CondFlag::Always | CondFlag::Never => "",
        }
        .to_string()
    }
    fn to_jmpop(&self) -> String {
        match self {
            CondFlag::Equal => "je",
            CondFlag::NotEqual => "jne",
            CondFlag::Greater => "jg",
            CondFlag::Less => "jl",
            CondFlag::GreaterEqual => "jge",
            CondFlag::LessEqual => "jle",
            CondFlag::Always => "jmp",
            CondFlag::Never => "",
        }
        .to_string()
    }
}

impl Instr {
    fn to_string(&self) -> String {
        match self {
            Instr::IMov(v1, v2) => format!("\tmov {},{}", v1.to_string(), v2.to_string()),
            Instr::IAdd(v1, v2) => format!("\tadd {},{}", v1.to_string(), v2.to_string()),
            Instr::ISub(v1, v2) => format!("\tsub {},{}", v1.to_string(), v2.to_string()),
            Instr::IMul(v1, v2) => format!(
                "\timul {},{}\n\tsar {},1",
                v1.to_string(),
                v2.to_string(),
                v1.to_string()
            ),
            Instr::Label(name) => format!("{}:", name.clone()),
            Instr::TypeTest(v) => format!("\ttest {},1", v.to_string()),
            Instr::Cmp(v1, v2) => format!("\tcmp {},{}", v1.to_string(), v2.to_string()),
            Instr::SetIfElse(reg, true_v, false_v, cond) => match cond {
                CondFlag::Always => format!("\tmov {}, {}", reg.to_string(), true_v.to_string()),
                CondFlag::Never => format!("\tmov {}, {}", reg.to_string(), false_v.to_string()),
                _ => format!(
                    "\tmov {},{}\n\tmov rbx,{}\n\t{} {},rbx",
                    reg.to_string(),
                    false_v.to_string(),
                    true_v.to_string(),
                    cond.to_setop(),
                    reg.to_string()
                ),
            },
            Instr::JumpIf(v, cond) => match cond {
                CondFlag::Never => "".to_string(),
                _ => format!("\t{} {}", cond.to_jmpop(), *v),
            },
        }
    }
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn compile_to_instrs(
    e: &Expr,
    si: i32,
    env: &HashMap<String, i32>,
    l: &mut i32,
    break_target: &String,
) -> Result<Vec<Instr>, String> {
    match e {
        Expr::Number(n) => Ok(vec![Instr::IMov(
            Val::Reg(Reg::RAX),
            Val::Imm(i64::from(*n)),
        )]),
        Expr::Boolean(b) => Ok(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Boolean(*b))]),
        Expr::Id(id) if id == "input" => {
            Ok(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))])
        }
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
                    let bind_instrs =
                        compile_to_instrs(value_expr, cur_si, &nenv, l, break_target)?;
                    result.extend(bind_instrs);
                    result.push(Instr::IMov(
                        Val::RegOffset(Reg::RSP, cur_si),
                        Val::Reg(Reg::RAX),
                    ));
                    cur_si += 1;
                }
            }
            let body_instrs = compile_to_instrs(&body, cur_si, &nenv, l, break_target)?;
            result.extend(body_instrs);
            Ok(result)
        }
        Expr::UnOp(op, e) => {
            let e_instrs = compile_to_instrs(e, si, env, l, break_target)?;
            let op_instrs = match op {
                Op1::Add1 => {
                    vec![
                        Instr::TypeTest(Val::Reg(Reg::RAX)),
                        Instr::SetIfElse(
                            Reg::RDI,
                            Val::Imm(*ERR_INVALID_ARG),
                            Val::Reg(Reg::RDI),
                            CondFlag::NotEqual,
                        ),
                        Instr::JumpIf("snek_error".to_string(), CondFlag::NotEqual),
                        Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)),
                    ]
                }
                Op1::Sub1 => {
                    vec![
                        Instr::TypeTest(Val::Reg(Reg::RAX)),
                        Instr::SetIfElse(
                            Reg::RDI,
                            Val::Imm(*ERR_INVALID_ARG),
                            Val::Reg(Reg::RDI),
                            CondFlag::NotEqual,
                        ),
                        Instr::JumpIf("snek_error".to_string(), CondFlag::NotEqual),
                        Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)),
                    ]
                }
                Op1::IsNum => vec![
                    Instr::TypeTest(Val::Reg(Reg::RAX)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::NotEqual,
                    ),
                ],
                Op1::IsBool => vec![
                    Instr::TypeTest(Val::Reg(Reg::RAX)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Equal,
                    ),
                ],
            };
            let mut result = e_instrs;
            result.extend(op_instrs);
            Ok(result)
        }
        Expr::BinOp(op, e1, e2) => {
            let mut e2_instrs = compile_to_instrs(e2, si, env, l, break_target)?;
            e2_instrs.extend(vec![
                Instr::TypeTest(Val::Reg(Reg::RAX)),
                Instr::SetIfElse(
                    Reg::RDI,
                    Val::Imm(*ERR_INVALID_ARG),
                    Val::Reg(Reg::RDI),
                    CondFlag::NotEqual,
                ),
                Instr::JumpIf("snek_error".to_string(), CondFlag::NotEqual),
            ]);
            let concat_instrs = vec![Instr::IMov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            )];
            let mut e1_instrs = compile_to_instrs(e1, si + 1, env, l, break_target)?;
            e1_instrs.extend(vec![
                Instr::TypeTest(Val::Reg(Reg::RAX)),
                Instr::SetIfElse(
                    Reg::RDI,
                    Val::Imm(*ERR_INVALID_ARG),
                    Val::Reg(Reg::RDI),
                    CondFlag::NotEqual,
                ),
                Instr::JumpIf("snek_error".to_string(), CondFlag::NotEqual),
            ]);

            let op_instrs = match op {
                Op2::Plus => vec![Instr::IAdd(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, si),
                )],
                Op2::Minus => {
                    vec![Instr::ISub(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    )]
                }
                Op2::Times => {
                    vec![Instr::IMul(
                        Val::Reg(Reg::RAX),
                        Val::RegOffset(Reg::RSP, si),
                    )]
                }
                Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => {
                    vec![
                        Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                        Instr::SetIfElse(
                            Reg::RAX,
                            Val::Boolean(true),
                            Val::Boolean(false),
                            op.to_condflag(),
                        ),
                    ]
                }
            };
            let mut result = e2_instrs;
            result.extend(concat_instrs);
            result.extend(e1_instrs);
            result.extend(op_instrs);
            Ok(result)
        }
        Expr::If(cond, if_expr, else_expr) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            let cond_instrs = compile_to_instrs(cond, si, env, l, break_target)?;
            let if_instrs = compile_to_instrs(if_expr, si, env, l, break_target)?;
            let else_instrs = compile_to_instrs(else_expr, si, env, l, break_target)?;

            let mut result = cond_instrs;
            result.extend(vec![
                Instr::Cmp(Val::Reg(Reg::RAX), Val::Boolean(false)),
                Instr::JumpIf(else_label.clone(), CondFlag::Equal),
            ]);
            result.extend(if_instrs);
            result.extend(vec![
                Instr::JumpIf(end_label.clone(), CondFlag::Always),
                Instr::Label(else_label.clone()),
            ]);
            result.extend(else_instrs);
            result.push(Instr::Label(end_label.clone()));
            Ok(result)
        }
        Expr::Loop(e) => {
            let loop_label = new_label(l, "loop");
            let end_label = new_label(l, "endloop");
            let e_instrs = compile_to_instrs(e, si, env, l, &end_label)?;
            let mut result = vec![Instr::Label(loop_label.clone())];
            result.extend(e_instrs);
            result.push(Instr::JumpIf(loop_label.clone(), CondFlag::Always));
            Ok(result)
        }
        Expr::Break(e) => {
            let mut result = compile_to_instrs(e, si, env, l, break_target)?;
            result.push(Instr::JumpIf(break_target.clone(), CondFlag::Always));
            Ok(result)
        }
        Expr::Set(id, e) => {
            let e_instrs = compile_to_instrs(e, si, env, l, break_target)?;
            let set_instr = match env.get(id) {
                Some(n) => Ok(vec![Instr::IMov(
                    Val::RegOffset(Reg::RSP, *n),
                    Val::Reg(Reg::RAX),
                )]),
                None => Err(format!("Unbound variable identifier {id}")),
            }?;
            let mut result = e_instrs;
            result.extend(set_instr);
            Ok(result)
        }
        Expr::Block(expr_vec) => {
            let b_instrs = expr_vec
                .iter()
                .map(|x| compile_to_instrs(x, si, env, l, break_target))
                .collect::<Result<Vec<Vec<Instr>>, String>>()?;
            let mut result = Vec::new();
            for i in b_instrs {
                result.extend(i);
            }
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
    let mut l = 0;
    match compile_to_instrs(e, 2, &HashMap::new(), &mut l, &"".to_string()) {
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
    dbg!(&expr);
    let result = compile(&expr);

    let asm_program = vec![
        "section .text",
        "extern snek_error",
        "global our_code_starts_here",
        "our_code_starts_here:",
        &result,
        "\tret",
    ]
    .join("\n");

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
