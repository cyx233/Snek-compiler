use crate::errors::*;
use crate::instr::*;
use crate::syntax::*;
use im::{HashMap, HashSet};

struct CompileState<'a> {
    si: i64,
    cur_name: &'a String,
    stack_depth: i64,
    env: &'a HashMap<String, i64>,
    env_offset: i64,
    funcs: &'a HashMap<String, u64>,
    break_target: &'a String,
    result_target: Val,
    in_defn: bool,
    tail: bool,
}

fn new_label(label: &mut u64, s: String) -> String {
    let current = *label;
    *label += 1;
    format!("{s}_{current}")
}

fn depth(e: &Expr, tail: bool, cur_name: &String) -> i64 {
    match e {
        Expr::Let(e1, e2) => e1
            .iter()
            .map(|(_, x)| depth(x, false, cur_name))
            .max()
            .unwrap_or(0)
            .max(depth(e2, tail, cur_name) + e1.len() as i64),
        Expr::UnOp(_, expr) => depth(expr, false, cur_name),
        Expr::BinOp(_, e1, e2) => depth(e1, false, cur_name).max(depth(e2, false, cur_name) + 1),
        Expr::If(cond, e1, e2) => depth(cond, false, cur_name)
            .max(depth(e1, tail, cur_name))
            .max(depth(e2, tail, cur_name)),
        Expr::Loop(expr) => depth(expr, tail, cur_name),
        Expr::Break(expr) => depth(expr, tail, cur_name),
        Expr::Set(_, expr) => depth(expr, tail, cur_name),
        Expr::Block(exprs) => {
            if let Some((last, rest)) = exprs.split_last() {
                rest.iter()
                    .map(|x| depth(x, false, cur_name))
                    .max()
                    .unwrap_or(0)
                    .max(depth(last, tail, cur_name))
            } else {
                0
            }
        }
        Expr::Call(name, exprs) => {
            exprs
                .iter()
                .map(|x| depth(x, false, name))
                .max()
                .unwrap_or(0)
                + if tail && cur_name.eq(name) {
                    exprs.len() as i64
                } else {
                    0
                }
        }
        Expr::Print(expr) => depth(expr, false, cur_name) + 1,
        _ => 0,
    }
}

fn compile_expr_to_instrs(
    e: &Expr,
    label: &mut u64,
    state: &CompileState,
) -> Result<Vec<Instr>, String> {
    match e {
        Expr::Number(n) => Ok(vec![Instr::IMov(state.result_target, Val::Int(*n))]),
        Expr::Boolean(b) => Ok(vec![Instr::IMov(state.result_target, Val::Boolean(*b))]),
        Expr::Input => {
            if state.in_defn {
                Err("input in definition".to_string())
            } else {
                Ok(vec![Instr::IMov(state.result_target, Val::Reg(Reg::RDI))])
            }
        }
        Expr::Id(id) => match state.env.get(id) {
            Some(n) => Ok(vec![Instr::IMov(
                state.result_target,
                Val::RegOffset(Reg::RSP, *n + state.env_offset),
            )]),
            None => Err(format!("Unbound variable identifier {id}")),
        },
        Expr::Let(bindings, body) => {
            let mut result = vec![Instr::Info("===== binding begin =====".to_string())];
            let mut nenv: HashMap<String, i64> = state.env.clone();
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
                            tail: false,
                            ..*state
                        },
                    )?;
                    cur_ids.insert(id.clone());
                    nenv.insert(id.clone(), cur_si - state.env_offset);
                    result.push(Instr::Info(format!(" {}", id)));
                    result.extend(bind_instrs);
                    cur_si += 1;
                }
            }
            result.push(Instr::Info("===== binding end =====".to_string()));
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
        Expr::Print(expr) => {
            let mut result = vec![
                Instr::Info("===== print begin =====".to_string()),
                Instr::IMov(Val::RegOffset(Reg::RSP, state.si), Val::Reg(Reg::RDI)),
            ];
            let e_instrs = compile_expr_to_instrs(
                expr,
                label,
                &CompileState {
                    si: state.si + 1,
                    result_target: Val::Reg(Reg::RDI),
                    tail: false,
                    ..*state
                },
            )?;
            result.extend(e_instrs);
            result.extend([
                Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RSP)),
                Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(8)),
                Instr::IAnd(Val::Reg(Reg::RSP), Val::Imm(-16)),
                Instr::IMov(Val::RegOffset(Reg::RSP, 0), Val::Reg(Reg::RCX)),
                Instr::ICall("snek_print".to_string()),
                Instr::IMov(Val::Reg(Reg::RSP), Val::RegOffset(Reg::RSP, 0)),
                Instr::IMov(state.result_target, Val::Reg(Reg::RAX)),
                Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, state.si)),
                Instr::Info("===== print end =====".to_string()),
            ]);
            Ok(result)
        }
        Expr::UnOp(op, expr) => {
            let e_instrs = compile_expr_to_instrs(
                expr,
                label,
                &CompileState {
                    result_target: Val::Reg(Reg::RAX),
                    tail: false,
                    ..*state
                },
            )?;
            let op_instrs = match op {
                Op1::Add1 => {
                    vec![
                        Instr::IntGuard(Val::Reg(Reg::RAX)),
                        Instr::IAdd(Val::Reg(Reg::RAX), Val::Int(1)),
                    ]
                }
                Op1::Sub1 => {
                    vec![
                        Instr::IntGuard(Val::Reg(Reg::RAX)),
                        Instr::ISub(Val::Reg(Reg::RAX), Val::Int(1)),
                    ]
                }
                //int => 00
                Op1::IsNum => vec![
                    Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(3)),
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(0)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Zero,
                    ),
                ],
                //bool => 01
                Op1::IsBool => vec![
                    Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(3)),
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(1)),
                    Instr::SetIfElse(
                        Reg::RAX,
                        Val::Boolean(true),
                        Val::Boolean(false),
                        CondFlag::Zero,
                    ),
                ],
            };
            let mut result = e_instrs;
            result.extend(op_instrs);
            result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX)));
            Ok(result)
        }
        Expr::BinOp(op, e1, e2) => {
            let mut result = compile_expr_to_instrs(
                e1,
                label,
                &CompileState {
                    result_target: Val::RegOffset(Reg::RSP, state.si),
                    tail: false,
                    ..*state
                },
            )?;
            let e2_instrs = compile_expr_to_instrs(
                e2,
                label,
                &CompileState {
                    si: state.si + 1,
                    result_target: Val::Reg(Reg::RCX),
                    tail: false,
                    ..*state
                },
            )?;
            let tc_instrs = match op {
                // Op2::Eqaul accepts 2 values of the same type
                // xor the type bits
                Op2::Equal => vec![
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, state.si)),
                    Instr::IXor(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(3)),
                    Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(0)),
                    Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, state.si)),
                ],
                //other ops only accept Int
                _ => vec![
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, state.si)),
                    Instr::IOr(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)),
                    Instr::IntGuard(Val::Reg(Reg::RAX)),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, state.si)),
                ],
            };
            let op_instrs = match op {
                Op2::Plus => vec![Instr::IAdd(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))],
                Op2::Minus => vec![Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))],
                Op2::Times => vec![Instr::IMul(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX))],
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
                    tail: false,
                    ..*state
                },
            )?;
            let if_instrs = compile_expr_to_instrs(if_expr, label, state)?;
            let else_instrs = compile_expr_to_instrs(else_expr, label, state)?;

            let mut result = cond_instrs;
            result.extend([
                Instr::Cmp(Val::Reg(Reg::RAX), Val::Boolean(false)),
                Instr::JumpIf(else_label.clone(), CondFlag::Zero),
            ]);
            result.extend(if_instrs);
            result.extend([
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
                Some(n) => Ok(Val::RegOffset(Reg::RSP, *n + state.env_offset)),
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
                tail: false,
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
            if let Some(args_num) = state.funcs.get(name) {
                if *args_num != expr_vec.len() as u64 {
                    return Err(format!(
                        "Expect {} args, but get {}",
                        args_num,
                        expr_vec.len()
                    ));
                }
                let args_offset = *args_num as i64 + 1;
                let mut result = vec![Instr::Info("===== call begin =====".to_string())];

                if state.tail && state.cur_name.eq(name) {
                    // tail recursion
                    // store arguments
                    for (i, expr) in expr_vec.iter().enumerate() {
                        let instrs = compile_expr_to_instrs(
                            expr,
                            label,
                            &CompileState {
                                cur_name: name,
                                si: state.si + i as i64,
                                result_target: Val::RegOffset(Reg::RSP, state.si + i as i64),
                                tail: false,
                                ..*state
                            },
                        )?;
                        result.extend(instrs);
                    }
                    // load arguments
                    for i in 0..expr_vec.len() as i64 {
                        result.push(Instr::IMov(
                            Val::RegOffset(Reg::RSP, state.stack_depth + i + 1),
                            Val::RegOffset(Reg::RSP, state.si + i),
                        ));
                    }
                    // reuse stack
                    // call and restore stack
                    // don't need to save rdi twice here, restore rdi directly from old stack
                    result.extend([
                        Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(state.stack_depth * 8)),
                        Instr::JumpIf(name.clone(), CondFlag::Always),
                        Instr::IMov(
                            Val::Reg(Reg::RDI),
                            Val::RegOffset(Reg::RSP, args_offset - 1),
                        ),
                        Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(state.stack_depth * 8)),
                    ]);
                } else {
                    // allocate stack
                    result.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(args_offset * 8)));
                    // load to destination directly
                    for (i, expr) in expr_vec.iter().enumerate() {
                        let instrs = compile_expr_to_instrs(
                            expr,
                            label,
                            &CompileState {
                                env_offset: state.env_offset + args_offset,
                                cur_name: name,
                                si: state.si + args_offset,
                                result_target: Val::RegOffset(Reg::RSP, i as i64),
                                tail: false,
                                ..*state
                            },
                        )?;
                        result.extend(instrs);
                    }
                    // call and restore stack
                    result.extend([
                        Instr::IMov(
                            Val::RegOffset(Reg::RSP, args_offset - 1),
                            Val::Reg(Reg::RDI),
                        ),
                        Instr::ICall(name.clone()),
                        Instr::IMov(
                            Val::Reg(Reg::RDI),
                            Val::RegOffset(Reg::RSP, args_offset - 1),
                        ),
                        Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(args_offset * 8)),
                    ]);
                }
                // move the result
                result.push(Instr::IMov(state.result_target, Val::Reg(Reg::RAX)));
                result.push(Instr::Info("===== call end =====".to_string()));
                Ok(result)
            } else {
                Err(format!("Undefined func {}", name))
            }
        }
        Expr::Tuple(exprs) => {
            let mut result = vec![Instr::IMov(
                Val::RegOffset(Reg::R15, 0),
                Val::Imm(exprs.len() as i64),
            )];
            let items = exprs
                .iter()
                .enumerate()
                .map(|(i, x)| {
                    compile_expr_to_instrs(
                        x,
                        label,
                        &CompileState {
                            tail: false,
                            result_target: Val::RegOffset(Reg::R15, i as i64 + 1),
                            ..*state
                        },
                    )
                })
                .collect::<Result<Vec<Vec<Instr>>, String>>()?;

            for i in items {
                result.extend(i);
            }
            result.extend([
                Instr::IMov(state.result_target, Val::Reg(Reg::R15)),
                Instr::IAdd(Val::Reg(Reg::R15), Val::Imm(8 * (exprs.len() as i64 + 1))),
            ]);
            Ok(result)
        }
        Expr::Index(tuple_expr, index) => {
            // put tuple ptr in stack
            let tuple_instrs = compile_expr_to_instrs(
                tuple_expr,
                label,
                &CompileState {
                    tail: false,
                    result_target: Val::RegOffset(Reg::RSP, state.si),
                    ..*state
                },
            )?;

            let mut result = tuple_instrs;

            // validate ptr
            result.push(Instr::TupleGuard(Val::RegOffset(Reg::RSP, state.si)));

            // put index in rax
            let index_instrs = compile_expr_to_instrs(
                index,
                label,
                &CompileState {
                    si: state.si + 1,
                    tail: false,
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            result.extend(index_instrs);

            result.extend([
                // validate index
                Instr::IntGuard(Val::Reg(Reg::RAX)),
                // put item addr in rax
                Instr::GetItemAddr(
                    Val::Reg(Reg::RAX),
                    Val::RegOffset(Reg::RSP, state.si),
                    Val::Reg(Reg::RAX),
                ),
                // mov [rax] to result target
                Instr::IMov(state.result_target, Val::RegOffset(Reg::RAX, 0)),
            ]);
            Ok(result)
        }
        Expr::SetIndex(tuple_name, index_expr, value_expr) => {
            let tuple_ptr = match state.env.get(tuple_name) {
                Some(n) => Ok(Val::RegOffset(Reg::RSP, *n + state.env_offset)),
                None => Err(format!("Unbound variable identifier {tuple_name}")),
            }?;
            // validate ptr
            let mut result = vec![Instr::TupleGuard(tuple_ptr)];

            // put index in rax
            let index_instrs = compile_expr_to_instrs(
                index_expr,
                label,
                &CompileState {
                    tail: false,
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            result.extend(index_instrs);

            // put item addr in stack
            result.push(Instr::GetItemAddr(
                Val::RegOffset(Reg::RSP, state.si),
                tuple_ptr,
                Val::Reg(Reg::RAX),
            ));

            // put the new value in rax
            let value_instrs = compile_expr_to_instrs(
                value_expr,
                label,
                &CompileState {
                    si: state.si + 1,
                    result_target: Val::Reg(Reg::RAX),
                    ..*state
                },
            )?;
            result.extend(value_instrs);

            //put the new value in item addr and result_target
            result.extend([
                Instr::IMov(
                    Val::RegOffset(Reg::RAX, 0),
                    Val::RegOffset(Reg::RSP, state.si),
                ),
                Instr::IMov(state.result_target, Val::RegOffset(Reg::RSP, state.si)),
            ]);
            Ok(result)
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
        let env = args
            .iter()
            .enumerate()
            .map(|(i, name)| (name.clone(), i as i64))
            .collect::<HashMap<String, i64>>();
        if env.len() != args.len() {
            return Err("Duplicate names of parameters".to_string());
        }

        let stack_depth = depth(expr, true, name);
        result.push(Instr::Label(name.clone()));
        if stack_depth > 0 {
            result.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(stack_depth * 8)));
        }
        let expr = compile_expr_to_instrs(
            expr,
            label,
            &CompileState {
                si: 0,
                cur_name: name,
                stack_depth: stack_depth,
                env: &env,
                env_offset: stack_depth + 1,
                funcs: funcs,
                break_target: &"".to_string(),
                result_target: Val::Reg(Reg::RAX),
                in_defn: true,
                tail: true,
            },
        )?;
        result.extend(expr);
        if stack_depth > 0 {
            result.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(stack_depth * 8)));
        }
        result.push(Instr::Return());
        result.push(Instr::Info("".to_string()));
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
    let stack_depth = depth(expr, true, &"our_code_starts_here".to_string());
    if stack_depth > 0 {
        result.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(stack_depth * 8)));
    }
    let expr_instrs = compile_expr_to_instrs(
        expr,
        &mut label,
        &CompileState {
            si: 0,
            cur_name: &"our_code_starts_here".to_string(),
            stack_depth: stack_depth,
            env: &HashMap::<String, i64>::new(),
            env_offset: 0,
            funcs: &funcs,
            break_target: &"".to_string(),
            result_target: Val::Reg(Reg::RAX),
            in_defn: false,
            tail: true,
        },
    )?;
    result.extend(expr_instrs);
    if stack_depth > 0 {
        result.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(stack_depth * 8)));
    }
    result.push(Instr::Return());
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
        Err(msg) => panic!("Invalid Compile. {}", msg),
    }
}
