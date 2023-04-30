use crate::errors::*;
use crate::parser::{Expr, Op1, Op2};
use im::{HashMap, HashSet};

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
    RCX,
    RDI,
    RSP,
}

#[derive(Debug)]
enum CondFlag {
    Zero,
    NotZero,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Always,
    Never,
    Overflow,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    Label(String),
    TypeTest(Reg),
    Cmp(Val, Val),
    SetIfElse(Reg, Val, Val, CondFlag),
    JumpIf(String, CondFlag),
}

impl Reg {
    fn to_string(&self) -> String {
        match self {
            Reg::RAX => "rax".to_string(),
            Reg::RCX => "rcx".to_string(),
            Reg::RDI => "rdi".to_string(),
            Reg::RSP => "rsp".to_string(),
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
            CondFlag::Zero => "cmove",
            CondFlag::NotZero => "cmovne",
            CondFlag::Greater => "cmovg",
            CondFlag::Less => "cmovl",
            CondFlag::GreaterEqual => "cmovge",
            CondFlag::LessEqual => "cmovle",
            _ => "",
        }
        .to_string()
    }
    fn to_jmpop(&self) -> String {
        match self {
            CondFlag::Zero => "je",
            CondFlag::NotZero => "jne",
            CondFlag::Greater => "jg",
            CondFlag::Less => "jl",
            CondFlag::GreaterEqual => "jge",
            CondFlag::LessEqual => "jle",
            CondFlag::Always => "jmp",
            CondFlag::Overflow => "jo",
            _ => "",
        }
        .to_string()
    }
}

impl Op2 {
    fn to_condflag(&self) -> CondFlag {
        match self {
            Op2::Equal => CondFlag::Zero,
            Op2::Greater => CondFlag::Greater,
            Op2::Less => CondFlag::Less,
            Op2::GreaterEqual => CondFlag::GreaterEqual,
            Op2::LessEqual => CondFlag::LessEqual,
            _ => CondFlag::Never,
        }
    }
}

impl Instr {
    fn to_string(&self) -> String {
        match self {
            Instr::IMov(v1, v2) => format!("\tmov {},{}", v1.to_string(), v2.to_string()),
            Instr::IAdd(v1, v2) => format!("\tadd {},{}", v1.to_string(), v2.to_string()),
            Instr::ISub(v1, v2) => format!("\tsub {},{}", v1.to_string(), v2.to_string()),
            Instr::IMul(v1, v2) => format!(
                "\tsar {},1\n\timul {},{}",
                v1.to_string(),
                v1.to_string(),
                v2.to_string()
            ),
            Instr::Label(name) => format!("{}:", name.clone()),
            // Bool => 1, Int => 0
            Instr::TypeTest(v) => format!("\ttest {},1", v.to_string()),
            Instr::Cmp(v1, v2) => format!("\tcmp {},{}", v1.to_string(), v2.to_string()),
            // reg = cond ? true_v : false_v
            Instr::SetIfElse(reg, true_v, false_v, cond) => match cond {
                CondFlag::Always => format!("\tmov {}, {}", reg.to_string(), true_v.to_string()),
                CondFlag::Never => "".to_string(),
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
        Expr::Number(n) => Ok(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n))]),
        Expr::Boolean(b) => Ok(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Boolean(*b))]),
        Expr::Input => Ok(vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]),
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
            let mut cur_ids: HashSet<String> = HashSet::new();
            let mut cur_si = si;
            for (id, value_expr) in bindings {
                if cur_ids.contains(id) {
                    return Err("Duplicate binding".to_string());
                } else {
                    let bind_instrs =
                        compile_to_instrs(value_expr, cur_si, &nenv, l, break_target)?;
                    cur_ids.insert(id.clone());
                    nenv.insert(id.clone(), cur_si);
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
                        Instr::TypeTest(Reg::RAX),
                        Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                        Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)),
                        Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                    ]
                }
                Op1::Sub1 => {
                    vec![
                        Instr::TypeTest(Reg::RAX),
                        Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                        Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)),
                        Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                    ]
                }
                Op1::IsNum => vec![
                    Instr::TypeTest(Reg::RAX),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Zero,
                    ),
                ],
                Op1::IsBool => vec![
                    Instr::TypeTest(Reg::RAX),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::NotZero,
                    ),
                ],
            };
            let mut result = e_instrs;
            result.extend(op_instrs);
            Ok(result)
        }
        Expr::BinOp(op, e1, e2) => {
            let mut e2_instrs = compile_to_instrs(e2, si, env, l, break_target)?;
            e2_instrs.extend(vec![Instr::IMov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            )]);
            let e1_instrs = compile_to_instrs(e1, si + 1, env, l, break_target)?;
            let tc_instrs = match op {
                // Op2::Eqaul accepts 2 Bool or 2 Int
                Op2::Equal => vec![
                    Instr::IMov(Val::RegOffset(Reg::RSP, si + 1), Val::Reg(Reg::RAX)),
                    Instr::TypeTest(Reg::RAX),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Zero,
                    ),
                    Instr::IMov(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)),
                    Instr::TypeTest(Reg::RCX),
                    Instr::SetIfElse(
                        Reg::RCX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Zero,
                    ),
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si + 1)),
                ],
                //other ops only accept Int
                _ => vec![
                    Instr::TypeTest(Reg::RAX),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                    Instr::IMov(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RSP, si)),
                    Instr::TypeTest(Reg::RCX),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                ],
            };
            let op_instrs = match op {
                Op2::Plus => vec![
                    Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Minus => vec![
                    Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Times => vec![
                    Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => vec![
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        op.to_condflag(),
                    ),
                ],
            };

            let mut result = e2_instrs;
            result.extend(e1_instrs);
            result.extend(tc_instrs);
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
                Instr::JumpIf(else_label.clone(), CondFlag::Zero),
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
            result.push(Instr::Label(end_label.clone()));
            Ok(result)
        }
        Expr::Break(e) => {
            if break_target == "" {
                Err("Invalid break".to_string())
            } else {
                let mut result = compile_to_instrs(e, si, env, l, break_target)?;
                result.push(Instr::JumpIf(break_target.clone(), CondFlag::Always));
                Ok(result)
            }
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

pub fn compile(e: &Expr) -> String {
    let mut l = 0;
    let env = HashMap::new();
    match compile_to_instrs(e, 2, &env, &mut l, &"".to_string()) {
        Ok(instrs) => instrs
            .iter()
            .map(|instr| instr.to_string())
            .collect::<Vec<String>>()
            .join("\n"),
        Err(msg) => panic!("Compile failed. {}", msg),
    }
}
