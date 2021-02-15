#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    LongConstant,

    Closure,
    LongClosure,

    DefineGlobal,
    DefineLongGlobal,

    GetGlobal,
    GetLongGlobal,

    SetGlobal,
    SetLongGlobal,

    GetLocal,
    SetLocal,

    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,

    Class,
    LongClass,
    Method,
    LongMethod,

    GetProperty,
    GetLongProperty,
    SetProperty,
    SetLongProperty,

    Invoke,
    LongInvoke,

    JumpIfFalse,
    Jump,
    Loop,

    Call,

    Pop,

    Return,
    Print,

    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Greater,
    Less,

    Negate,
    Not,

    True,
    False,
    Nil,

    Nop,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        if byte > OpCode::Nop as u8 {
            panic!("invalid opcode");
        }
        unsafe { std::mem::transmute(byte) }
    }
}

impl From<OpCode> for u8 {
    fn from(opcode: OpCode) -> Self {
        opcode as u8
    }
}
