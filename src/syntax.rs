use crate::instr::CondFlag;

#[derive(Debug)]
pub enum Prog {
    Prog(Vec<Defn>, Box<Expr>),
}

#[derive(Debug)]
pub enum Defn {
    Func(String, Vec<String>, Box<Expr>),
}

#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Print(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
    Input,
    Index(Box<Expr>, Box<Expr>),
    Tuple(Vec<Expr>),
    SetIndex(String, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    DeepEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Op2 {
    pub fn to_condflag(&self) -> CondFlag {
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
