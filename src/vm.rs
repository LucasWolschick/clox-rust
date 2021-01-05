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

    // remove this attribute when we add other types
    #[allow(irrefutable_let_patterns)]
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
                    let constant = self.stack.pop().unwrap();
                    println!("{}", constant);
                    return Ok(());
                }
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
                    }
                }
                OpCode::Add => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let Value::Number(r) = rhs {
                        if let Value::Number(l) = lhs {
                            self.stack.push(Value::Number(r+l));
                        }
                    }
                }
                OpCode::Subtract => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let Value::Number(r) = rhs {
                        if let Value::Number(l) = lhs {
                            self.stack.push(Value::Number(r-l));
                        }
                    }
                }
                OpCode::Multiply => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let Value::Number(r) = rhs {
                        if let Value::Number(l) = lhs {
                            self.stack.push(Value::Number(r*l));
                        }
                    }
                }
                OpCode::Divide => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let Value::Number(r) = rhs {
                        if let Value::Number(l) = lhs {
                            self.stack.push(Value::Number(r/l));
                        }
                    }
                }
                OpCode::Nop => ()
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
}