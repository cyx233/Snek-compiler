use crate::expr::{Expr, Op1, Op2};
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
            [Sexp::Atom(S(op)), ..] if op == "block" => {
                let result = vec[1..]
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
            // (<op>, <expr>) => Block, Loop, Break, UnOp
            [Sexp::Atom(S(op)), e] => {
                let e_expr = parse_expr(e)?;
                match op.as_str() {
                    "loop" => Ok(Expr::Loop(Box::new(e_expr))),
                    "break" => Ok(Expr::Break(Box::new(e_expr))),
                    "add1" => Ok(Expr::UnOp(Op1::Add1, Box::new(e_expr))),
                    "sub1" => Ok(Expr::UnOp(Op1::Sub1, Box::new(e_expr))),
                    "isnum" => Ok(Expr::UnOp(Op1::IsNum, Box::new(e_expr))),
                    "isbool" => Ok(Expr::UnOp(Op1::IsBool, Box::new(e_expr))),
                    _ => Err(format!("Invalid operator {}", op)),
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
            _ => Err("Invalid Syntax: Sexp::List".to_string()),
        },
        _ => Err("Invalid Syntax: Sexp".to_string()),
    }
}

pub fn parse_code(s: &String) -> Expr {
    match parse(s) {
        Ok(sexpr) => match parse_expr(&sexpr) {
            Ok(expr) => expr,
            Err(msg) => {
                panic!("Parse Failed. {}", msg)
            }
        },
        Err(msg) => {
            panic!("Invalid Sexpr. {}", msg)
        }
    }
}
