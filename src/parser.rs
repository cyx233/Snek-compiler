use crate::syntax::*;
use lazy_static::lazy_static;
use regex::Regex;
use sexp::Atom::*;
use sexp::*;

lazy_static! {
    static ref ID_REGEX: Regex = Regex::new(r"^[a-zA-Z][a-zA-Z0-9_-]*$").unwrap();
}

fn parse_bind(s: &Sexp) -> Result<(String, Expr), String> {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(id)), e] => {
                if matches!(
                    id.as_str(),
                    "true"
                        | "false"
                        | "input"
                        | "let"
                        | "set!"
                        | "if"
                        | "block"
                        | "loop"
                        | "break"
                        | "add1"
                        | "sub1"
                        | "isnum"
                        | "isbool"
                ) {
                    Err(format!("Invalid Id: can't be a keyword \"{}\"", id))
                } else if ID_REGEX.is_match(id) {
                    let e_instrs = parse_expr(e)?;
                    Ok((id.clone(), e_instrs))
                } else {
                    Err(format!("Invalid ID: {}", id))
                }
            }
            _ => Err("Invalid Syntax: id-expr".to_string()),
        },
        _ => Err("Invalid Syntax: bindings".to_string()),
    }
}

fn parse_expr(s: &Sexp) -> Result<Expr, String> {
    match s {
        Sexp::Atom(I(n)) => {
            if *n < -4611686018427387904 || *n > 4611686018427387903 {
                Err("Invalid number: overflow".to_string())
            } else {
                Ok(Expr::Number(*n))
            }
        }
        Sexp::Atom(S(id)) => match id.as_str() {
            "true" => Ok(Expr::Boolean(true)),
            "false" => Ok(Expr::Boolean(false)),
            "input" => Ok(Expr::Input),
            _ => Ok(Expr::Id(id.clone())),
        },
        Sexp::List(vec) => match &vec[..] {
            // Block
            [Sexp::Atom(S(op)), rest @ ..] if op == "block" => {
                let result = rest
                    .iter()
                    .map(parse_expr)
                    .collect::<Result<Vec<Expr>, String>>()?;
                if !result.is_empty() {
                    Ok(Expr::Block(result))
                } else {
                    Err("Invalid Syntax: empty Block".to_string())
                }
            }
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
                Sexp::List(bindings) if bindings.is_empty() => {
                    Err("Invalid binding: empty".to_string())
                }
                _ => Err("Invalid Sytax: Let".to_string()),
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
            // (<unop>, <expr>) => Loop, Break, UnOp
            [Sexp::Atom(S(op)), e]
                if matches!(
                    op.as_str(),
                    "loop" | "break" | "add1" | "sub1" | "isnum" | "isbool" | "print"
                ) =>
            {
                let e_expr = parse_expr(e)?;
                match op.as_str() {
                    "loop" => Ok(Expr::Loop(Box::new(e_expr))),
                    "break" => Ok(Expr::Break(Box::new(e_expr))),
                    "print" => Ok(Expr::Print(Box::new(e_expr))),
                    "add1" => Ok(Expr::UnOp(Op1::Add1, Box::new(e_expr))),
                    "sub1" => Ok(Expr::UnOp(Op1::Sub1, Box::new(e_expr))),
                    "isnum" => Ok(Expr::UnOp(Op1::IsNum, Box::new(e_expr))),
                    "isbool" => Ok(Expr::UnOp(Op1::IsBool, Box::new(e_expr))),
                    _ => Err(format!("Invalid operator {}", op)),
                }
            }
            // (<binop>, <expr>, <expr>) => BinOp
            [Sexp::Atom(S(op)), e1, e2]
                if matches!(op.as_str(), "+" | "-" | "*" | ">" | "<" | ">=" | "<=" | "=") =>
            {
                let expr_op = match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    ">" => Op2::Greater,
                    "<" => Op2::Less,
                    ">=" => Op2::GreaterEqual,
                    "<=" => Op2::LessEqual,
                    "=" => Op2::Equal,
                    _ => return Err(format!("Invalid operator {}", op)),
                };
                let e1_instrs = parse_expr(e1)?;
                let e2_instrs = parse_expr(e2)?;
                Ok(Expr::BinOp(
                    expr_op,
                    Box::new(e1_instrs),
                    Box::new(e2_instrs),
                ))
            }
            [Sexp::Atom(S(func)), args @ ..] => {
                let exprs = args
                    .iter()
                    .map(parse_expr)
                    .collect::<Result<Vec<Expr>, String>>()?;
                Ok(Expr::Call(func.to_string(), exprs))
            }
            _ => Err("Invalid Syntax: Sexp::List".to_string()),
        },
        _ => Err("Invalid Syntax: Sexp".to_string()),
    }
}

fn parse_defn(s_defns: Vec<Sexp>) -> Result<Vec<Defn>, String> {
    s_defns
        .iter()
        .map(|s| match s {
            Sexp::List(vec) => match &vec[..] {
                [Sexp::Atom(S(op)), Sexp::List(v), s_expr] if op == "fun" => {
                    let e = parse_expr(s_expr)?;
                    let names = v
                        .iter()
                        .map(|n| match n {
                            Sexp::Atom(S(name)) => Ok(name.clone()),
                            _ => Err("Bad Defn Syntax: name must be String".to_string()),
                        })
                        .collect::<Result<Vec<String>, String>>()?;
                    if names.is_empty() {
                        Err("Bad Defn Syntax: names is empty".to_string())
                    } else {
                        Ok(Defn::Func(
                            names[0].clone(),
                            names[1..names.len()].to_vec(),
                            Box::new(e),
                        ))
                    }
                }
                _ => Err("Bad Defn Syntax".to_string()),
            },
            _ => Err("Bad Defn Syntax".to_string()),
        })
        .collect::<Result<Vec<Defn>, String>>()
}

fn split_by_parentheses(s: &str) -> Vec<String> {
    let mut s_expressions = vec![];
    let mut stack = 0;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => {
                if stack == 0 {
                    start = i;
                }
                stack += 1;
            }
            ')' => {
                stack -= 1;
                if stack == 0 {
                    s_expressions.push(s[start..=i].to_string());
                }
            }
            _ => {}
        }
    }
    s_expressions
}

fn split_defn_expr(s: &String) -> Result<(Vec<Sexp>, Sexp), String> {
    let groups = split_by_parentheses(s)
        .iter()
        .filter(|x| !x.is_empty())
        .map(|x| parse(x))
        .collect::<Result<Vec<Sexp>, Box<sexp::Error>>>()
        .map_err(|e| e.to_string())?;

    if groups.is_empty() {
        let sexpr = parse(s).map_err(|e| e.to_string())?;
        Ok((vec![], sexpr))
    } else {
        if let Some((last, rest)) = groups.split_last() {
            Ok((rest.to_vec(), last.clone()))
        } else {
            Err("Bad Syntax: Prog".to_string())
        }
    }
}

fn parse_prog(s_defns: Vec<Sexp>, s_expr: &Sexp) -> Result<Prog, String> {
    let defns = parse_defn(s_defns)?;
    let expr = parse_expr(s_expr)?;
    return Ok(Prog::Prog(defns, Box::new(expr)));
}

pub fn parse_code(s: &String) -> Prog {
    match split_defn_expr(s) {
        Ok((s_defns, s_expr)) => match parse_prog(s_defns, &s_expr) {
            Ok(prog) => prog,
            Err(msg) => {
                panic!("Parse Failed. {}", msg)
            }
        },
        Err(msg) => {
            panic!("Invalid Sexpr. {}", msg)
        }
    }
}
