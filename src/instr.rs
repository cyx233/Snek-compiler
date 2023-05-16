use crate::errors::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Val {
    Reg(Reg),
    Int(i64),
    Imm(i64),
    Boolean(bool),
    RegOffset(Reg, i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    RAX,
    RCX,
    RDI,
    RSP,
    R15,
}

#[derive(Debug)]
pub enum CondFlag {
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
pub enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IXor(Val, Val),
    IOr(Val, Val),
    IAnd(Val, Val),
    Label(String),
    IntGuard(Val),
    TupleGuard(Val),
    LenGuard(Val, Val),
    Cmp(Val, Val),
    SetIfElse(Reg, Val, Val, CondFlag),
    JumpIf(String, CondFlag),
    ICall(String),
    Info(String),
    Return(),
    GetItemAddr(Val, Val, Val),
}

impl Reg {
    fn to_string(&self) -> String {
        match self {
            Reg::RAX => "rax".to_string(),
            Reg::RCX => "rcx".to_string(),
            Reg::RDI => "rdi".to_string(),
            Reg::RSP => "rsp".to_string(),
            Reg::R15 => "r15".to_string(),
        }
    }
}

impl Val {
    fn to_string(&self) -> String {
        match self {
            Val::Int(n) => (n << 2).to_string(),
            Val::Imm(n) => n.to_string(),
            Val::Boolean(b) => {
                if *b {
                    "5".to_string()
                } else {
                    "1".to_string()
                }
            }
            Val::Reg(reg) => reg.to_string(),
            Val::RegOffset(reg, offset) => {
                if *offset >= 0 {
                    format!("[{} + {}]", reg.to_string(), offset * 8)
                } else {
                    format!("[{} - {}]", reg.to_string(), -offset * 8)
                }
            }
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

impl Instr {
    pub fn to_string(&self) -> String {
        match self {
            Instr::IMov(v1, v2) if v1 != v2 => match v1 {
                Val::Reg(_) => format!("\tmov {},{}", v1.to_string(), v2.to_string()),
                _ => match v2 {
                    Val::RegOffset(_, _) => {
                        format!("\tmov rbx,{}\n\tmov {},rbx", v2.to_string(), v1.to_string())
                    }
                    Val::Imm(n) if *n > i32::MAX as i64 || *n < i32::MIN as i64 => {
                        format!("\tmov rbx,{}\n\tmov {},rbx", v2.to_string(), v1.to_string())
                    }
                    Val::Int(n) if (n << 2) > i32::MAX as i64 || (n << 2) < i32::MIN as i64 => {
                        format!("\tmov rbx,{}\n\tmov {},rbx", v2.to_string(), v1.to_string())
                    }
                    _ => format!("\tmov qword {},{}", v1.to_string(), v2.to_string()),
                },
            },
            Instr::IAdd(v1, v2) => [
                format!("\tadd {},{}", v1.to_string(), v2.to_string()),
                Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow).to_string(),
            ]
            .join("\n"),
            Instr::ISub(v1, v2) => [
                format!("\tsub {},{}", v1.to_string(), v2.to_string()),
                Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow).to_string(),
            ]
            .join("\n"),
            Instr::IMul(v1, v2) => [
                format!(
                    "\tsar {},2\n\timul {},{}",
                    v1.to_string(),
                    v1.to_string(),
                    v2.to_string()
                ),
                Instr::JumpIf(ERR_OVERFLOW_LABEL.clone(), CondFlag::Overflow).to_string(),
            ]
            .join("\n"),
            Instr::IXor(v1, v2) => format!("\txor {},{}", v1.to_string(), v2.to_string()),
            Instr::IOr(v1, v2) => format!("\tor {},{}", v1.to_string(), v2.to_string()),
            Instr::IAnd(v1, v2) => format!("\tand {},{}", v1.to_string(), v2.to_string()),
            Instr::Label(name) => format!("{}:", name.clone()),
            // Int => 00, Bool => 01, Tuple => 10
            Instr::IntGuard(v) => [
                format!("\tmov rbx,3\n\tand rbx,{}\n\tcmp rbx,0", v.to_string()),
                Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero).to_string(),
            ]
            .join("\n"),
            Instr::TupleGuard(v) => [
                format!("\tmov rbx,3\n\tand rbx,{}\n\tcmp rbx,2", v.to_string()),
                Instr::JumpIf(ERR_INVALID_ARG_LABEL.clone(), CondFlag::NotZero).to_string(),
            ]
            .join("\n"),
            Instr::LenGuard(tuple_ptr, index) => [
                format!("\tmov rbx,{}", tuple_ptr.to_string()),
                // remove type bits
                "\tand rbx,-4".to_string(),
                // [tuple] = len(tuple)
                "\tmov rbx,[rbx]".to_string(),
                format!("\tcmp rbx,{}", index.to_string()),
                Instr::JumpIf(ERR_OUT_OF_BOUND_LABEL.clone(), CondFlag::LessEqual).to_string(),
            ]
            .join("\n"),
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
            Instr::ICall(name) => format!("\tcall {}", name),
            Instr::Info(msg) => ";".to_string() + msg,
            Instr::Return() => "\tret".to_string(),
            // tuple.i = [tuple_ptr + 8*(index+1)]
            Instr::GetItemAddr(target, ptr, index) => [
                // validate bound
                Instr::LenGuard(*ptr, *index).to_string(),
                format!("\tmov rbx,{}", index.to_string()),
                // 3(1 byte offset) - 2(int format) = 1
                "\tshl rbx,1".to_string(),
                // [tuple] = len(tuple), item begin at [tuple+8]
                "\tadd rbx,8".to_string(),
                // tuple_ptr + 8*(index+1)
                format!("\tadd rbx,{}", ptr.to_string()),
                "\tand rbx,-4".to_string(),
                format!("\tmov {},rbx", target.to_string()),
            ]
            .join("\n"),
            _ => "".to_string(),
        }
    }
}
