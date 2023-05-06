#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    Boolean(bool),
    RegOffset(Reg, u64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    RAX,
    RCX,
    RDI,
    RSP,
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
    Label(String),
    TypeTest(Val),
    Cmp(Val, Val),
    SetIfElse(Reg, Val, Val, CondFlag),
    JumpIf(String, CondFlag),
    ICall(String),
    Empty(),
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
            Val::RegOffset(reg, offset) => format!("[{} + {}]", reg.to_string(), offset * 8),
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
                        format!("\tmov rax,{}\n\tmov {},rax", v2.to_string(), v1.to_string())
                    }
                    _ => format!("\tmov qword {},{}", v1.to_string(), v2.to_string()),
                },
            },
            Instr::IAdd(v1, v2) => format!("\tadd {},{}", v1.to_string(), v2.to_string()),
            Instr::ISub(v1, v2) => format!("\tsub {},{}", v1.to_string(), v2.to_string()),
            Instr::IMul(v1, v2) => format!(
                "\tsar {},1\n\timul {},{}",
                v1.to_string(),
                v1.to_string(),
                v2.to_string()
            ),
            Instr::IXor(v1, v2) => format!("\txor {},{}", v1.to_string(), v2.to_string()),
            Instr::IOr(v1, v2) => format!("\tor {},{}", v1.to_string(), v2.to_string()),
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
            Instr::ICall(name) => format!("\tcall {}", name),
            Instr::Empty() => ";".to_string(),
            _ => "".to_string(),
        }
    }
}
