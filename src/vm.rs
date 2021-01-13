use crate::InterpretError;

use super::{Chunk, OpCode, Value, InterpretResult};
use super::debug;

pub struct VM {
    chunk: Chunk,
    ip: usize,
    debug: bool,
    stack: Vec<Value>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            ip: 0,
            debug: false,
            stack: Vec::new(),
        }
    }

    pub fn debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    pub fn interpret(&mut self) -> InterpretResult {
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let instruction = self.read_byte();

            if self.debug {
                print!("         ");
                for slot in &self.stack {
                    print!("[ {} ]", slot);
                }
                println!();
                debug::disassemble_instruction(&self.chunk, self.ip-1);
            }

            match OpCode::from(instruction) {
                OpCode::Return => { 
                    return Ok(());
                }
                OpCode::Print => {
                    let constant = self.stack.pop().unwrap();
                    println!("{}", constant);    
                }
                OpCode::Pop => {
                    let _ = self.stack.pop();
                }
                OpCode::Nop => (),
                OpCode::Constant => {
                    let constant = self.read_constant().clone();
                    self.stack.push(constant);
                }
                OpCode::LongConstant => {
                    let constant = self.read_long_constant().clone();
                    self.stack.push(constant);
                }
                OpCode::Negate => {
                    if let Value::Number(n) = self.stack.last_mut().unwrap() {
                        *n = -*n;
                    } else {
                        self.runtime_error("Operand must be a number");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Not => {
                    let v = self.stack.last_mut().unwrap();
                    *v = Value::Bool(v.is_falsey());
                }
                OpCode::Add => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l+r));
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Subtract => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l-r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Multiply => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l*r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Divide => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l/r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Equal => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(lhs == rhs));
                }
                OpCode::Less => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Bool(l<r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Greater => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Bool(l>r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Nil => {
                    self.stack.push(Value::Nil)
                }
                OpCode::True => {
                    self.stack.push(Value::Bool(true))
                }
                OpCode::False => {
                    self.stack.push(Value::Bool(false))
                }
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        self.ip += 1;
        *self.chunk.at(self.ip - 1)
    }

    fn read_constant(&mut self) -> &Value {
        let constant = self.read_byte() as usize;
        self.chunk.get_constant(constant)
    }

    fn read_long_constant(&mut self) -> &Value {
        let constant = self.read_byte() as usize  // first byte
        | (self.read_byte() as usize) << 8 // second byte
        | (self.read_byte() as usize) << 16; // third byte
        self.chunk.get_constant(constant)
    }

    fn peek_stack(&mut self, distance: usize) -> Option<&Value> {
        self.stack.iter().nth_back(distance)
    }

    fn runtime_error(&mut self, err: &str) {
        eprintln!("{}\n[line {}] in script", err, self.chunk.line_at_offset(self.ip));
        self.stack.clear();
    }
}