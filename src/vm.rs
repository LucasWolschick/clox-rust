use std::rc::Rc;

use super::{HashSet, HashTable};

use super::chunk::{Chunk, Constant};
use super::debug;
use super::opcodes::OpCode;
use super::value::{StringReference, Value, FunctionObject, FunctionReference};
use super::{InterpretError, InterpretResult};

struct CallFrame {
    function: FunctionReference,
    ip: usize,
    slot_offset: usize,
}

pub struct VM {
    debug: bool,
    stack: Vec<Value>,
    strings: HashSet<StringReference>,
    globals: HashTable<StringReference, Value>,
    frames: Vec<CallFrame>,
}

impl VM {
    pub fn new() -> VM {
        let frames = Vec::with_capacity(64 * u8::MAX as usize);

        VM {
            debug: false,
            stack: Vec::new(),
            strings: HashSet::default(),
            globals: HashTable::default(),
            frames,
        }
    }

    pub fn debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    pub fn interpret(&mut self, function: FunctionObject) -> InterpretResult {
        let rc = Rc::new(function);
        self.stack.push(Value::Function(Rc::clone(&rc)));
        let frame = CallFrame {
            function: rc,
            ip: 0,
            slot_offset: 0,
        };
        self.frames.push(frame);
        self.run()
    }

    fn chunk(&self) -> &Chunk {
        let frame = self.frames.last().unwrap();
        frame.function.chunk()
    }

    // fn chunk_mut(&mut self) -> &mut Chunk {
    //     let frame = self.frames.last_mut().unwrap();
    //     frame.function.chunk_mut()
    // }

    fn run(&mut self) -> InterpretResult {
        loop {
            let instruction = self.read_byte();

            if self.debug {
                print!("         ");
                for slot in &self.stack {
                    print!("[ {} ]", slot);
                }
                println!();
                let frame = self.frames.last_mut().unwrap();
                debug::disassemble_instruction(frame.function.chunk(), frame.ip - 1);
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
                OpCode::DefineGlobal => {
                    let name = self.read_constant();
                    if let Constant::String(r) = name {
                        let r = r.clone();
                        let rc = self.allocate_string(r);
                        let val = self.peek_stack(0).unwrap().clone();
                        self.globals.insert(rc, val);
                        self.stack.pop();
                    } else {
                        self.runtime_error("Attempt to define non-string global");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::DefineLongGlobal => {
                    let name = self.read_long_constant();
                    if let Constant::String(r) = name {
                        let r = r.clone();
                        let rc = self.allocate_string(r);
                        let val = self.peek_stack(0).unwrap().clone();
                        self.globals.insert(rc, val);
                        self.stack.pop();
                    } else {
                        self.runtime_error("Attempt to define non-string global");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::GetGlobal => {
                    let name = self.read_constant();
                    if let Constant::String(r) = name {
                        let r = r.clone();
                        let rc = self.allocate_string(r);
                        if self.globals.get(&rc).is_none() {
                            self.runtime_error(format!("Undefined variable '{}'", &rc).as_str());
                            return Err(InterpretError::RuntimeError);
                        }
                        self.stack.push(self.globals.get(&rc).unwrap().clone());
                    } else {
                        self.runtime_error("Attempt to get non-string global");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::GetLongGlobal => {
                    let name = self.read_long_constant();
                    if let Constant::String(r) = name {
                        let r = r.clone();
                        let rc = self.allocate_string(r);
                        if self.globals.get(&rc).is_none() {
                            self.runtime_error(format!("Undefined variable '{}'", &rc).as_str());
                            return Err(InterpretError::RuntimeError);
                        }
                        self.stack.push(self.globals.get(&rc).unwrap().clone());
                    } else {
                        self.runtime_error("Attempt to get non-string global");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::SetGlobal => {
                    let name = self.read_constant();
                    if let Constant::String(r) = name {
                        let r = r.clone();
                        let rc = self.allocate_string(r);
                        if self.globals.get(&rc).is_some() {
                            self.globals.insert(rc, self.peek_stack(0).unwrap().clone());
                        } else {
                            self.runtime_error(format!("Undefined variable '{}'", &rc).as_str());
                            return Err(InterpretError::RuntimeError);
                        }
                    } else {
                        self.runtime_error("Attempt to set non-string global");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::SetLongGlobal => {
                    let name = self.read_long_constant();
                    if let Constant::String(r) = name {
                        let r = r.clone();
                        let rc = self.allocate_string(r);
                        if self.globals.get(&rc).is_some() {
                            self.globals.insert(rc, self.peek_stack(0).unwrap().clone());
                        } else {
                            self.runtime_error(format!("Undefined variable '{}'", &rc).as_str());
                            return Err(InterpretError::RuntimeError);
                        }
                    } else {
                        self.runtime_error("Attempt to set non-string global");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::GetLocal => {
                    let offset = self.frames.last().unwrap().slot_offset;
                    let slot = self.read_byte();
                    let entry = self.stack.get(offset + slot as usize).unwrap().clone();
                    self.stack.push(entry);
                }
                OpCode::SetLocal => {
                    let offset = self.frames.last().unwrap().slot_offset;
                    let slot = self.read_byte();
                    let front = self.stack.last().unwrap().clone();
                    self.stack[offset + slot as usize] = front;
                }
                OpCode::Constant => {
                    let constant = self.read_constant().clone();
                    let val = self.constant_to_value(&constant);
                    self.stack.push(val);
                }
                OpCode::LongConstant => {
                    let constant = self.read_long_constant().clone();
                    let val = self.constant_to_value(&constant);
                    self.stack.push(val);
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
                    match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l + r))
                        }
                        (Value::String(l), Value::String(r)) => {
                            let mut result = String::clone(&l);
                            result.push_str(&r);
                            let string = self.allocate_string(result);
                            self.stack.push(Value::String(string));
                        }
                        _ => {
                            self.runtime_error("Operands must be two numbers or two strings");
                            return Err(InterpretError::RuntimeError);
                        }
                    }
                }
                OpCode::Subtract => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l - r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Multiply => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l * r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Divide => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Number(l / r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Equal => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::String(l), Value::String(r)) = (&rhs, &lhs) {
                        self.stack.push(Value::Bool(Rc::ptr_eq(l, r)));
                    } else {
                        self.stack.push(Value::Bool(lhs == rhs));
                    }
                }
                OpCode::Less => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Bool(l < r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Greater => {
                    let rhs = self.stack.pop().unwrap();
                    let lhs = self.stack.pop().unwrap();
                    if let (Value::Number(l), Value::Number(r)) = (lhs, rhs) {
                        self.stack.push(Value::Bool(l > r));
                    } else {
                        self.runtime_error("Operands must be numbers");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek_stack(0).unwrap().is_falsey() {
                        let frame = self.frames.last_mut().unwrap();
                        frame.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    let frame = self.frames.last_mut().unwrap();
                    frame.ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = self.read_short();
                    let frame = self.frames.last_mut().unwrap();
                    frame.ip -= offset as usize;
                }
            }
        }
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.frames.last_mut().unwrap();
        frame.ip += 1;
        *frame.function.chunk().at(frame.ip - 1)
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.frames.last_mut().unwrap();
        frame.ip += 2;
        (*frame.function.chunk().at(frame.ip - 2) as u16) << 8 | *frame.function.chunk().at(frame.ip - 1) as u16
    }

    fn allocate_string(&mut self, string: String) -> StringReference {
        if let Some(r) = self.strings.get(&string) {
            Rc::clone(r)
        } else {
            let rc = Rc::new(string);
            self.strings.insert(rc.clone());
            rc
        }
    }

    fn read_constant(&mut self) -> &Constant {
        let constant = self.read_byte() as usize;
        self.chunk().get_constant(constant)
    }

    fn read_long_constant(&mut self) -> &Constant {
        let constant = self.read_byte() as usize  // first byte
        | (self.read_byte() as usize) << 8 // second byte
        | (self.read_byte() as usize) << 16; // third byte
        self.chunk().get_constant(constant)
    }

    fn constant_to_value(&mut self, constant: &Constant) -> Value {
        match constant {
            Constant::String(s) => {
                let string = self.allocate_string(s.clone());
                Value::String(string)
            }
            Constant::Number(n) => Value::Number(*n),
            Constant::Nil => Value::Nil,
        }
    }

    fn peek_stack(&self, distance: usize) -> Option<&Value> {
        self.stack.iter().nth_back(distance)
    }

    fn clear_stack(&mut self) {
        self.stack.clear();
        self.frames.clear();
    }

    fn runtime_error(&mut self, err: &str) {
        let frame = self.frames.last_mut().unwrap();
        eprintln!(
            "{}\n[line {}] in script",
            err,
            frame.function.chunk().line_at_offset(frame.ip)
        );
        self.clear_stack();
    }
}
