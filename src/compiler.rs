use crate::errors::*;
use crate::instr::*;
use crate::syntax::*;

use im::{HashMap, HashSet};
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
                Op1::Print => vec![],
            };
            let mut result = e_instrs;
            result.extend(op_instrs);
            Ok(result)
        }
        Expr::BinOp(op, e1, e2) => {
            let mut e1_instrs = compile_to_instrs(e1, si, env, l, break_target)?;
            e1_instrs.extend(vec![Instr::IMov(
                Val::RegOffset(Reg::RSP, si),
                Val::Reg(Reg::RAX),
            )]);
            let e2_instrs = compile_to_instrs(e2, si + 1, env, l, break_target)?;
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
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                ],
                //other ops only accept Int
                _ => vec![
                    Instr::IMov(Val::RegOffset(Reg::RSP, si + 1), Val::Reg(Reg::RAX)),
                    Instr::TypeTest(Reg::RAX),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si)),
                    Instr::TypeTest(Reg::RAX),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                ],
            };
            let op_instrs = match op {
                Op2::Plus => vec![
                    Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si + 1)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Minus => vec![
                    Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si + 1)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Times => vec![
                    Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si + 1)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => vec![
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si + 1)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        op.to_condflag(),
                    ),
                ],
            };

            let mut result = e1_instrs;
            result.extend(e2_instrs);
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
        Expr::Call(func, exprs) => unimplemented!("coimpile call"),
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
