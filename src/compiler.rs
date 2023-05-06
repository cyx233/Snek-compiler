use crate::errors::*;
use crate::instr::*;
use crate::syntax::*;
use im::{HashMap, HashSet};

struct CompileState<'a> {
    si: i32,
    env: &'a HashMap<String, i32>,
    funcs: &'a HashSet<String>,
    break_target: &'a String,
    result_target: Val,
}

fn new_label(label: &mut u64, s: String) -> String {
    let current = *label;
    *label += 1;
    format!("{s}_{current}")
}

fn depth(e: &Expr) -> i64 {
    match e {
        Expr::Let(e1, e2) => e1
            .iter()
            .map(|x| depth(&(*x).1))
            .max()
            .unwrap()
            .max(depth(e2) + e1.len() as i64),
        Expr::BinOp(_, e1, e2) => depth(e1).max(depth(e2) + 1),
        Expr::If(cond, e1, e2) => depth(cond).max(depth(e1)).max(depth(e2)),
        Expr::Loop(e) => depth(e),
        Expr::Break(e) => depth(e),
        Expr::Set(_, e) => depth(e),
        Expr::Block(exprs) => exprs.iter().map(depth).max().unwrap(),
        Expr::Call(_, exprs) => exprs.iter().map(depth).max().unwrap(),
        _ => 0,
    }
}

fn delivery_result(source: Val, target: Val) -> Vec<Instr> {
    match source {
        Val::Boolean(_) => vec![Instr::IMov(target, source)],
        Val::Imm(_) => vec![Instr::IMov(target, source)],
        Val::Reg(_) => vec![Instr::IMov(target, source)],
        Val::RegOffset(_, _) => {
            if let Val::Reg(_) = target {
                vec![Instr::IMov(target, source)]
            } else {
                vec![
                    Instr::IMov(Val::Reg(Reg::RAX), source),
                    Instr::IMov(target, Val::Reg(Reg::RAX)),
                ]
            }
        }
    }
}

fn compile_expr_to_instrs(
    e: &Expr,
    label: &mut u64,
    state: &CompileState,
) -> Result<Vec<Instr>, String> {
    match e {
        Expr::Number(n) => Ok(vec![Instr::IMov(state.result_target, Val::Imm(*n))]),
        Expr::Boolean(b) => Ok(vec![Instr::IMov(state.result_target, Val::Boolean(*b))]),
        Expr::Input => Ok(vec![Instr::IMov(state.result_target, Val::Reg(Reg::RDI))]),
        Expr::Id(id) => match state.env.get(id) {
            Some(n) => Ok(delivery_result(
                Val::RegOffset(Reg::RSP, *n),
                state.result_target,
            )),
            None => Err(format!("Unbound variable identifier {id}")),
        },
        Expr::Let(bindings, body) => {
            let mut result: Vec<Instr> = Vec::new();
            let mut nenv: HashMap<String, i32> = state.env.clone();
            let mut cur_ids: HashSet<String> = HashSet::new();
            let mut cur_si = state.si;
            for (id, value_expr) in bindings {
                if cur_ids.contains(id) {
                    return Err("Duplicate binding".to_string());
                } else {
                    let bind_instrs = compile_expr_to_instrs(
                        value_expr,
                        label,
                        &CompileState {
                            si: cur_si,
                            env: &nenv,
                            result_target: Val::RegOffset(Reg::RSP, cur_si),
                            ..*state
                        },
                    )?;
                    cur_ids.insert(id.clone());
                    nenv.insert(id.clone(), cur_si);
                    result.extend(bind_instrs);
                    cur_si += 1;
                }
            }
            let body_instrs = compile_expr_to_instrs(
                &body,
                label,
                &CompileState {
                    si: cur_si,
                    env: &nenv,
                    ..*state
                },
            )?;
            result.extend(body_instrs);
            Ok(result)
        }
        Expr::UnOp(op, expr) => {
            let e_instrs = compile_expr_to_instrs(
                expr,
                label,
                &CompileState {
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            let op_instrs = match op {
                Op1::Add1 => {
                    vec![
                        Instr::TypeTest(Val::Reg(Reg::RAX)),
                        Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                        Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)),
                        Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                    ]
                }
                Op1::Sub1 => {
                    vec![
                        Instr::TypeTest(Val::Reg(Reg::RAX)),
                        Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                        Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)),
                        Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                    ]
                }
                Op1::IsNum => vec![
                    Instr::TypeTest(Val::Reg(Reg::RAX)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Zero,
                    ),
                ],
                Op1::IsBool => vec![
                    Instr::TypeTest(Val::Reg(Reg::RAX)),
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
            match state.result_target {
                Val::Reg(Reg::RAX) => {}
                _ => result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX))),
            }
            Ok(result)
        }
        Expr::BinOp(op, e1, e2) => {
            let e1_instrs = compile_expr_to_instrs(
                e1,
                label,
                &CompileState {
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            let e2_instrs = compile_expr_to_instrs(
                e2,
                label,
                &CompileState {
                    si: state.si + 1,
                    result_target: Val::Reg(Reg::RCX),
                    ..*state
                },
            )?;
            let tc_instrs = match op {
                // Op2::Eqaul accepts 2 Bool or 2 Int
                Op2::Equal => vec![
                    Instr::IMov(Val::RegOffset(Reg::RSP, state.si), Val::Reg(Reg::RAX)),
                    Instr::IXor(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::TypeTest(Val::Reg(Reg::RAX)),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, state.si)),
                ],
                //other ops only accept Int
                _ => vec![
                    Instr::IMov(Val::RegOffset(Reg::RSP, state.si), Val::Reg(Reg::RAX)),
                    Instr::IOr(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::TypeTest(Val::Reg(Reg::RAX)),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, state.si)),
                ],
            };
            let op_instrs = match op {
                Op2::Plus => vec![
                    Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Minus => vec![
                    Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Times => vec![
                    Instr::IMul(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow),
                ],
                Op2::Equal | Op2::Greater | Op2::GreaterEqual | Op2::Less | Op2::LessEqual => vec![
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
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
            match state.result_target {
                Val::Reg(Reg::RAX) => {}
                _ => result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX))),
            }
            Ok(result)
        }
        Expr::If(cond, if_expr, else_expr) => {
            let end_label = new_label(label, "ifend".to_string());
            let else_label = new_label(label, "ifelse".to_string());
            let cond_instrs = compile_expr_to_instrs(
                cond,
                label,
                &CompileState {
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            let if_instrs = compile_expr_to_instrs(if_expr, label, state)?;
            let else_instrs = compile_expr_to_instrs(else_expr, label, state)?;

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
        Expr::Loop(expr) => {
            let loop_label = new_label(label, "loop".to_string());
            let end_label = new_label(label, "endloop".to_string());
            let e_instrs = compile_expr_to_instrs(
                expr,
                label,
                &CompileState {
                    break_target: &end_label,
                    ..*state
                },
            )?;
            let mut result = vec![Instr::Label(loop_label.clone())];
            result.extend(e_instrs);
            result.push(Instr::JumpIf(loop_label.clone(), CondFlag::Always));
            result.push(Instr::Label(end_label.clone()));
            Ok(result)
        }
        Expr::Break(expr) => {
            if state.break_target == "" {
                Err("Invalid break".to_string())
            } else {
                let mut result = compile_expr_to_instrs(expr, label, state)?;
                result.push(Instr::JumpIf(state.break_target.clone(), CondFlag::Always));
                Ok(result)
            }
        }
        Expr::Set(id, expr) => {
            let set_target = match state.env.get(id) {
                Some(n) => Ok(Val::RegOffset(Reg::RSP, *n)),
                None => Err(format!("Unbound variable identifier {id}")),
            }?;
            let e_instrs = compile_expr_to_instrs(
                expr,
                label,
                &CompileState {
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            let mut result = e_instrs;
            result.push(Instr::IMov(set_target, Val::Reg(Reg::RAX)));
            match state.result_target {
                Val::Reg(Reg::RAX) => {}
                _ => result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX))),
            }
            Ok(result)
        }
        Expr::Block(expr_vec) => {
            let mut result = Vec::new();
            let default_state = CompileState {
                result_target: Val::Reg(Reg::RAX),
                ..*state
            };
            for expr in &expr_vec[..(expr_vec.len() - 1)] {
                let instrs = compile_expr_to_instrs(expr, label, &default_state)?;
                result.extend(instrs);
            }
            let e_instrs = compile_expr_to_instrs(&expr_vec[expr_vec.len() - 1], label, state)?;
            result.extend(e_instrs);
            Ok(result)
        }
        Expr::Call(name, expr_vec) => {
            if !state.funcs.contains(name) {
                Err(format!("Undefined func {}", name))
            } else {
                let mut result = vec![Instr::ISub(
                    Val::Reg(Reg::RSP),
                    Val::Imm(8 * (expr_vec.len() + 1) as i64),
                )];
                let mut si = 0;
                for expr in expr_vec {
                    let instrs = compile_expr_to_instrs(
                        expr,
                        label,
                        &CompileState {
                            result_target: Val::RegOffset(Reg::RSP, si),
                            ..*state
                        },
                    )?;
                    si += 1;
                }
                result.extend(vec![]);
                Ok(result)
            }
        }
    }
}

pub fn compile_defn_to_instrs(defns: &Vec<Defn>) -> Result<Vec<Instr>, String> {
    let res: Vec<Instr> = vec![];
    for Defn::Func(name, args, expr) in defns {}
    Ok(res)
}

pub fn compile_prog_to_instrs(prog: &Prog) -> Result<Vec<Instr>, String> {
    let Prog::Prog(defns, expr) = prog;
    let funcs = defns
        .iter()
        .map(|Defn::Func(name, ..)| name)
        .collect::<HashSet<String>>();
    if funcs.len() != defns.len() {
        return Err("Functions have the same name".to_string());
    }

    let mut label: u64 = 0;
    let defn_instrs = compile_defn_to_instrs(defns)?;
    let expr_instrs = compile_expr_to_instrs(
        &expr,
        &mut label,
        &CompileState {
            si: 0,
            env: &HashMap::<String, i32>::new(),
            funcs: &funcs,
            break_target: &"".to_string(),
            result_target: Val::Reg(Reg::RAX),
        },
    )?;

    let mut result = vec![];
    let stack_depth = depth(&expr);
    if stack_depth != 0 {
        result.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(8 * stack_depth)));
    };
    result.extend(expr_instrs);
    if stack_depth != 0 {
        result.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(8 * stack_depth)));
    };
    Ok(result)
}

pub fn compile(prog: &Prog) -> String {
    match compile_prog_to_instrs(prog) {
        Ok(instrs) => instrs
            .iter()
            .map(|instr| instr.to_string())
            .collect::<Vec<String>>()
            .join("\n"),
        Err(msg) => panic!("Compile failed. {}", msg),
    }
}
