use std::rc::Rc;

use super::{HashSet, HashTable};

use super::{InterpretResult, InterpretError};
use super::opcodes::OpCode;
use super::value::{Value, ObjectReference, StringReference};
use super::chunk::{Chunk, Constant};
use super::debug;

pub struct VM {
    chunk: Option<Chunk>,
    ip: usize,
    debug: bool,
    stack: Vec<Value>,
    strings: HashSet<Rc<String>>,
    objects: Vec<ObjectReference>,
    globals: HashTable<Rc<String>, Value>,
}

impl VM {
    pub fn new(chunk: Option<Chunk>) -> VM {
        VM {
            chunk,
            ip: 0,
            debug: false,
            stack: Vec::new(),
            objects: Vec::new(),
            strings: HashSet::default(),
            globals: HashTable::default(),
        }
    }

    pub fn debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    pub fn load_chunk(&mut self, chunk: Chunk) {
        self.chunk = Some(chunk);
        self.ip = 0;
    }

    pub fn interpret(&mut self) -> InterpretResult {
        self.run()
    }

    fn chunk(&self) -> &Chunk {
        self.chunk.as_ref().unwrap()
    }

    fn _chunk_mut(&mut self) -> &mut Chunk {
        self.chunk.as_mut().unwrap()
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
                debug::disassemble_instruction(&self.chunk(), self.ip-1);
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
                        (Value::Number(l), Value::Number(r)) => self.stack.push(Value::Number(l+r)),
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
        *self.chunk().at(self.ip - 1)
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
            Constant::Nil => Value::Nil
        }
    }

    fn peek_stack(&self, distance: usize) -> Option<&Value> {
        self.stack.iter().nth_back(distance)
    }

    fn runtime_error(&mut self, err: &str) {
        eprintln!("{}\n[line {}] in script", err, self.chunk().line_at_offset(self.ip));
        self.stack.clear();
    }
}