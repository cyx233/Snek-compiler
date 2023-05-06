use crate::errors::*;
use crate::instr::*;
use crate::syntax::*;
use im::{HashMap, HashSet};

struct CompileState<'a> {
    si: u64,
    env: &'a HashMap<String, u64>,
    funcs: &'a HashMap<String, u64>,
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
            .map(|(_, x)| depth(x))
            .max()
            .unwrap()
            .max(depth(e2) + e1.len() as i64),
        Expr::BinOp(_, e1, e2) => depth(e1).max(depth(e2) + 1),
        Expr::If(cond, e1, e2) => depth(cond).max(depth(e1)).max(depth(e2)),
        Expr::Loop(expr) => depth(expr),
        Expr::Break(expr) => depth(expr),
        Expr::Set(_, expr) => depth(expr),
        Expr::Block(exprs) => exprs.iter().map(depth).max().unwrap(),
        Expr::Call(_, exprs) => exprs.iter().map(depth).max().unwrap(),
        _ => 0,
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
            Some(n) => Ok(vec![Instr::IMov(
                state.result_target,
                Val::RegOffset(Reg::RSP, *n),
            )]),
            None => Err(format!("Unbound variable identifier {id}")),
        },
        Expr::Let(bindings, body) => {
            let mut result: Vec<Instr> = Vec::new();
            let mut nenv: HashMap<String, u64> = state.env.clone();
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
            result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX)));
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
            result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX)));
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
            result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX)));
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
            if !state.funcs.contains_key(name) {
                Err(format!("Undefined func {}", name))
            } else {
                let mut result = vec![Instr::ISub(
                    Val::Reg(Reg::RSP),
                    Val::Imm(4 * (expr_vec.len() + 1) as i64),
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
                result.extend(vec![
                    Instr::IMov(Val::RegOffset(Reg::RSP, si), Val::Reg(Reg::RDI)),
                    Instr::ICall(name.clone()),
                    Instr::IMov(state.result_target, Val::Reg(Reg::RAX)),
                    Instr::IAdd(
                        Val::Reg(Reg::RSP),
                        Val::Imm(4 * (expr_vec.len() + 1) as i64),
                    ),
                ]);
                Ok(result)
            }
        }
    }
}

pub fn compile_defn_to_instrs(
    label: &mut u64,
    defns: &Vec<Defn>,
    funcs: &HashMap<String, u64>,
) -> Result<Vec<Instr>, String> {
    let mut result: Vec<Instr> = vec![];
    for Defn::Func(name, args, expr) in defns {
        result.push(Instr::Label(name.clone()));
        let stack_depth = depth(expr);
        if stack_depth != 0 {
            result.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(4 * stack_depth)));
        };
        let env = args
            .iter()
            .enumerate()
            .map(|(i, name)| (name.clone(), i as u64 + stack_depth as u64))
            .collect::<HashMap<String, u64>>();

        let expr = compile_expr_to_instrs(
            expr,
            label,
            &CompileState {
                si: 0,
                env: &env,
                funcs: funcs,
                break_target: &"".to_string(),
                result_target: Val::Reg(Reg::RAX),
            },
        )?;

        result.extend(expr);

        if stack_depth != 0 {
            result.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(4 * stack_depth)));
        };
        result.push(Instr::Empty());
    }
    Ok(result)
}

pub fn compile_prog_to_instrs(prog: &Prog) -> Result<Vec<Instr>, String> {
    let Prog::Prog(defns, expr) = prog;
    let funcs = defns
        .iter()
        .map(|Defn::Func(name, names, ..)| (name.clone(), names.len() as u64))
        .collect::<HashMap<String, u64>>();
    if funcs.len() != defns.len() {
        return Err("Functions have the same name".to_string());
    }

    let mut label: u64 = 0;
    let mut result = vec![];

    let defn_instrs = compile_defn_to_instrs(&mut label, defns, &funcs)?;
    result.extend(defn_instrs);

    result.push(Instr::Label("our_code_starts_here".to_string()));
    let stack_depth = depth(expr);
    if stack_depth != 0 {
        result.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(4 * stack_depth)));
    };
    let expr_instrs = compile_expr_to_instrs(
        expr,
        &mut label,
        &CompileState {
            si: 0,
            env: &HashMap::<String, u64>::new(),
            funcs: &funcs,
            break_target: &"".to_string(),
            result_target: Val::Reg(Reg::RAX),
        },
    )?;
    result.extend(expr_instrs);
    if stack_depth != 0 {
        result.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(4 * stack_depth)));
    };
    Ok(result)
}

pub fn compile(prog: &Prog) -> String {
    match compile_prog_to_instrs(prog) {
        Ok(instrs) => instrs
            .iter()
            .map(|instr| instr.to_string())
            .filter(|x| !x.is_empty())
            .collect::<Vec<String>>()
            .join("\n"),
        Err(msg) => panic!("Compile failed. {}", msg),
    }
}
