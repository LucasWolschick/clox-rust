use std::cell::RefCell;
use std::rc::Rc;
use std::time::{Duration, SystemTime};

use crate::value::BoundMethod;

use super::chunk::{Chunk, Constant};
use super::debug;
use super::opcodes::OpCode;
use super::value::{
    ClassObject, ClassReference, ClosureObject, ClosureReference, FunctionObject, InstanceObject,
    NativeFunctionPointer, StringReference, Upvalue, UpvalueReference, Value,
};
use super::{HashSet, HashTable};
use super::{InterpretError, InterpretResult};

struct CallFrame {
    closure: ClosureReference,
    ip: usize,
    slot_offset: usize,
}

pub struct VM {
    debug: bool,
    stack: Vec<Value>,
    strings: HashSet<StringReference>,
    globals: HashTable<StringReference, Value>,
    frames: Vec<CallFrame>,
    open_upvalues: Vec<UpvalueReference>,
    /// Note: will always be Some(string) because of construction semantics
    init_string: Option<StringReference>,
}

impl VM {
    pub fn new() -> VM {
        let frames = Vec::with_capacity(64);

        let mut vm = VM {
            debug: false,
            stack: Vec::with_capacity(64 * u8::MAX as usize),
            strings: HashSet::default(),
            globals: HashTable::default(),
            frames,
            open_upvalues: Vec::new(),
            init_string: None,
        };

        vm.init_string = Some(vm.allocate_string(String::from("init")));

        vm.define_native("clock", NativeFunctionPointer(native_clock, "clock"));
        vm.define_native("wait", NativeFunctionPointer(native_wait, "wait"));
        vm
    }

    pub fn debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    pub fn interpret(&mut self, function: FunctionObject) -> InterpretResult {
        let rc = Rc::new(function);
        let closure = Rc::new(ClosureObject::new(rc));
        self.stack.push(Value::Closure(Rc::clone(&closure)));
        self.call(closure, 0).unwrap();
        self.run()
    }

    fn chunk(&self) -> &Chunk {
        let frame = self.frames.last().unwrap();
        frame.closure.function.chunk()
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
                debug::disassemble_instruction(frame.closure.function.chunk(), frame.ip - 1);
            }

            match OpCode::from(instruction) {
                OpCode::Return => {
                    let result = self.stack.pop().unwrap();
                    self.close_upvalues(self.frames.last().unwrap().slot_offset);
                    let frame = self.frames.pop().unwrap();
                    if self.frames.is_empty() {
                        self.stack.pop();
                        return Ok(());
                    }

                    let frame_offset = frame.slot_offset;
                    self.stack.truncate(frame_offset);
                    self.stack.push(result);
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
                OpCode::Closure => {
                    let function = self.read_constant();
                    if let Constant::Function(f) = function {
                        let function = Rc::new(f.clone());
                        let mut closure = ClosureObject::new(function);

                        for _ in 0..closure.n_upvalues {
                            let is_local = self.read_byte() == 1;
                            let index = self.read_byte() as usize;
                            if is_local {
                                let slot_offset = self.frames.last().unwrap().slot_offset;
                                let upvalue = self.capture_upvalue(slot_offset + index);
                                closure.upvalues.push(upvalue);
                            } else {
                                let upvalue = self.frames.last().unwrap().closure.upvalues
                                    [index as usize]
                                    .clone();
                                closure.upvalues.push(upvalue);
                            }
                        }
                        self.stack.push(Value::Closure(Rc::new(closure)));
                    } else {
                        self.runtime_error("Attempt to make closure out of non-function constant");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::LongClosure => {
                    let function = self.read_long_constant();
                    if let Constant::Function(f) = function {
                        let function = Rc::new(f.clone());
                        let mut closure = ClosureObject::new(function);

                        for _ in 0..closure.n_upvalues {
                            let is_local = self.read_byte() == 1;
                            let index = self.read_byte() as usize;
                            if is_local {
                                let slot_offset = self.frames.last().unwrap().slot_offset;
                                let upvalue = self.capture_upvalue(slot_offset + index);
                                closure.upvalues.push(upvalue);
                            } else {
                                let upvalue = self.frames.last().unwrap().closure.upvalues
                                    [index as usize]
                                    .clone();
                                closure.upvalues.push(upvalue);
                            }
                        }
                        self.stack.push(Value::Closure(Rc::new(closure)));
                    } else {
                        self.runtime_error("Attempt to make closure out of non-function constant");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::GetUpvalue => {
                    let upvalue_i = self.read_byte() as usize;
                    let upvalue = &self.frames.last().unwrap().closure.upvalues[upvalue_i];
                    match &*upvalue.borrow() {
                        // this shouldn't work???????
                        Upvalue::Open { slot: index } => {
                            self.stack.push(self.stack[*index].clone())
                        }
                        Upvalue::Closed { value: val } => self.stack.push(val.clone()),
                    };
                }
                OpCode::SetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let stack_value = self.stack.last().unwrap().clone();
                    let upvalue = &self.frames.last_mut().unwrap().closure.upvalues[slot];
                    match *RefCell::borrow_mut(upvalue) {
                        Upvalue::Open { slot: index } => self.stack[index] = stack_value,
                        Upvalue::Closed { value: ref mut val } => {
                            *val = stack_value;
                        }
                    }
                    // TODO: this might not work because .clone() might duplicate the upvalue and render it useless!
                }
                OpCode::CloseUpvalue => {
                    let idx = self.stack.len() - 1;
                    self.close_upvalues(idx);
                    self.stack.pop();
                }
                OpCode::Invoke => {
                    let name = self.read_constant();
                    if let Constant::String(string) = name {
                        let r = string.clone();
                        let rc = self.allocate_string(r);
                        let args = self.read_byte();
                        self.invoke(rc, args)?;
                    } else {
                        unreachable!("We don't expect non-string constants after Invoke opcodes");
                    }
                }
                OpCode::LongInvoke => {
                    let name = self.read_long_constant();
                    if let Constant::String(string) = name {
                        let r = string.clone();
                        let rc = self.allocate_string(r);
                        let args = self.read_byte();
                        self.invoke(rc, args)?;
                    } else {
                        unreachable!("We don't expect non-string constants after Invoke opcodes");
                    }
                }
                OpCode::Method => {
                    let name = self.read_constant();
                    if let Constant::String(s) = name {
                        let s = s.clone();
                        let rc = self.allocate_string(s);
                        self.define_method(rc)?;
                    } else {
                        self.runtime_error("Attempt to declare method with non-string constant");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::LongMethod => {
                    let name = self.read_long_constant();
                    if let Constant::String(s) = name {
                        let s = s.clone();
                        let rc = self.allocate_string(s);
                        self.define_method(rc)?;
                    } else {
                        self.runtime_error("Attempt to declare method with non-string constant");
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
                OpCode::Class => {
                    let string = match self.read_constant() {
                        Constant::String(s) => {
                            let s = s.clone();
                            self.allocate_string(s)
                        }
                        _ => unreachable!(
                            "After class declarations there should only be string constants"
                        ),
                    };
                    let class = ClassObject::new(string);
                    let rc = Rc::new(RefCell::new(class));
                    self.stack.push(Value::Class(rc));
                }
                OpCode::LongClass => {
                    let string = match self.read_long_constant() {
                        Constant::String(s) => {
                            let s = s.clone();
                            self.allocate_string(s)
                        }
                        _ => unreachable!(
                            "After class declarations there should only be string constants"
                        ),
                    };
                    let class = ClassObject::new(string);
                    let rc = Rc::new(RefCell::new(class));
                    self.stack.push(Value::Class(rc));
                }
                OpCode::GetProperty => {
                    let name = match self.read_constant() {
                        Constant::String(s) => {
                            let s = s.clone();
                            self.allocate_string(s)
                        }
                        _ => unreachable!(
                            "After property accesses there should only be string constants"
                        ),
                    };

                    let instance = if let Value::Instance(instance) = self.peek_stack(0).unwrap() {
                        Rc::clone(instance)
                    } else {
                        self.runtime_error("Attempt to access property of non-instance object");
                        return Err(InterpretError::RuntimeError);
                    };

                    let value = if let Some(value) = instance.borrow().fields.get(&name) {
                        Some(value.clone())
                    } else {
                        None
                    };

                    if let Some(value) = value {
                        self.stack.pop();
                        self.stack.push(value);
                    } else if !self.bind_method(Rc::clone(&instance.borrow().class), &name) {
                        self.runtime_error(format!("Undefined property '{}'", name).as_str());
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::GetLongProperty => {
                    let name = match self.read_long_constant() {
                        Constant::String(s) => {
                            let s = s.clone();
                            self.allocate_string(s)
                        }
                        _ => unreachable!(
                            "After property accesses there should only be string constants"
                        ),
                    };

                    let instance = if let Value::Instance(instance) = self.peek_stack(0).unwrap() {
                        Rc::clone(instance)
                    } else {
                        self.runtime_error("Attempt to access property of non-instance object");
                        return Err(InterpretError::RuntimeError);
                    };

                    let value = if let Some(value) = instance.borrow().fields.get(&name) {
                        Some(value.clone())
                    } else {
                        None
                    };

                    if let Some(value) = value {
                        self.stack.pop();
                        self.stack.push(value);
                    } else if !self.bind_method(Rc::clone(&instance.borrow().class), &name) {
                        self.runtime_error(format!("Undefined property '{}'", name).as_str());
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::SetProperty => {
                    let name = match self.read_constant() {
                        Constant::String(s) => {
                            let s = s.clone();
                            self.allocate_string(s)
                        }
                        _ => unreachable!(
                            "After property setters there should only be string constants"
                        ),
                    };

                    if let Value::Instance(instance) = self.peek_stack(1).unwrap() {
                        let value = self.peek_stack(0).unwrap().clone();
                        RefCell::borrow_mut(&instance).fields.insert(name, value);
                    } else {
                        self.runtime_error("Attempt to set property of non-instance object");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::SetLongProperty => {
                    let name = match self.read_long_constant() {
                        Constant::String(s) => {
                            let s = s.clone();
                            self.allocate_string(s)
                        }
                        _ => unreachable!(
                            "After property setters there should only be string constants"
                        ),
                    };

                    if let Value::Instance(instance) = self.peek_stack(1).unwrap() {
                        let value = self.peek_stack(0).unwrap().clone();
                        RefCell::borrow_mut(&instance).fields.insert(name, value);
                    } else {
                        self.runtime_error("Attempt to set property of non-instance object");
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
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    let value = self.peek_stack(arg_count as usize).unwrap().clone(); //todo: find a better solution
                    if self.call_value(&value, arg_count).is_err() {
                        return Err(InterpretError::RuntimeError);
                    }
                }
            }
        }
    }

    fn capture_upvalue(&mut self, index: usize) -> UpvalueReference {
        for open_upvalue in self.open_upvalues.iter().rev() {
            match *open_upvalue.borrow() {
                Upvalue::Open { slot } if slot <= index => {
                    if slot == index {
                        return Rc::clone(open_upvalue);
                    } else {
                        break;
                    }
                }
                _ => (),
            }
        }

        let new_upvalue = Rc::new(RefCell::new(Upvalue::Open { slot: index }));
        self.open_upvalues.push(Rc::clone(&new_upvalue));

        new_upvalue
    }

    fn close_upvalues(&mut self, last: usize) {
        let mut i = self.open_upvalues.len() as isize;
        loop {
            if i < 1 {
                break;
            }
            let upvalue = &self.open_upvalues[(i - 1) as usize];
            let location = match *upvalue.borrow() {
                Upvalue::Open { slot } => slot,
                _ => unreachable!("Closed upvalues shouldn't be on the open upvalues list"),
            };
            if location < last {
                break;
            }
            upvalue.replace_with(|_| Upvalue::Closed {
                value: self.stack[location].clone(),
            });
            i -= 1;
        }

        self.open_upvalues.truncate(i as usize);
    }

    fn define_method(&mut self, name: StringReference) -> Result<(), InterpretError> {
        let method = self.stack.pop().unwrap();
        let class = self.peek_stack(0).unwrap();
        if let Value::Class(class) = class {
            RefCell::borrow_mut(class).methods.insert(name, method);
            Ok(())
        } else {
            self.runtime_error("Attempt to define method on non-class object");
            Err(InterpretError::RuntimeError)
        }
    }

    fn bind_method(&mut self, class: ClassReference, name: &StringReference) -> bool {
        if let Some(Value::Closure(c)) = class.borrow().methods.get(name) {
            let bound = BoundMethod::new(self.peek_stack(0).unwrap().clone(), c.clone());
            let bound = Rc::new(bound);
            self.stack.pop();
            self.stack.push(Value::BoundMethod(bound));
            true
        } else {
            false
        }
    }

    fn invoke(&mut self, name: StringReference, arg_count: u8) -> Result<(), InterpretError> {
        let receiver = self.peek_stack(arg_count as usize).unwrap().clone();
        let i = if let Value::Instance(i) = receiver {
            i
        } else {
            self.runtime_error("Attempt to call method of non-instance object");
            return Err(InterpretError::RuntimeError);
        };

        let i = i.borrow();
        if let Some(value) = i.fields.get(&name) {
            let value = value.clone();
            let index = self.stack.len() - arg_count as usize - 1;
            self.stack[index] = value.clone();
            self.call_value(&value, arg_count)
                .map_err(|_| InterpretError::RuntimeError)
        } else {
            let class = Rc::clone(&i.class);
            self.invoke_from_class(class, name, arg_count)
        }
    }

    fn invoke_from_class(
        &mut self,
        class: ClassReference,
        name: StringReference,
        arg_count: u8,
    ) -> Result<(), InterpretError> {
        if let Some(Value::Closure(c)) = class.borrow().methods.get(&name) {
            self.call(Rc::clone(c), arg_count)
                .map_err(|_| InterpretError::RuntimeError)
        } else {
            self.runtime_error(format!("Undefined property '{}'", name).as_str());
            Err(InterpretError::RuntimeError)
        }
    }

    fn call_value(&mut self, callable: &Value, arg_count: u8) -> Result<(), ()> {
        match callable {
            Value::BoundMethod(m_obj) => {
                let i = self.stack.len() - arg_count as usize - 1;
                self.stack[i] = m_obj.receiver.clone();
                self.call(Rc::clone(&m_obj.method), arg_count)
            }
            Value::Class(c_obj) => {
                let instance = InstanceObject::new(Rc::clone(c_obj));
                let instance = Rc::new(RefCell::new(instance));
                let i = self.stack.len() - arg_count as usize - 1;
                self.stack[i] = Value::Instance(instance);

                if let Some(closure) = c_obj
                    .borrow()
                    .methods
                    .get(self.init_string.as_ref().unwrap())
                {
                    let closure = match closure {
                        Value::Closure(c) => Rc::clone(c),
                        _ => unreachable!(
                            "There should be no non-closure objects in the methods table"
                        ),
                    };
                    self.call(closure, arg_count)
                } else if arg_count != 0 {
                    self.runtime_error(
                        format!("Expected 0 arguments but got {}", arg_count).as_ref(),
                    );
                    Err(())
                } else {
                    Ok(())
                }
            }
            Value::Closure(c_obj) => self.call(Rc::clone(&c_obj), arg_count),
            Value::NativeFunction(n_f) => {
                let start = self.stack.len() - arg_count as usize;
                let r = n_f.0(arg_count, &mut self.stack[start..]);
                self.stack.pop();
                match r {
                    Ok(r) => {
                        self.stack.push(r);
                        Ok(())
                    }
                    Err(s) => {
                        self.runtime_error(&s);
                        Err(())
                    }
                }
            }
            _ => {
                self.runtime_error("Only functions, methods and classes are callable");
                Err(())
            }
        }
    }

    fn call(&mut self, closure: ClosureReference, arg_count: u8) -> Result<(), ()> {
        if closure.function.arity != arg_count {
            self.runtime_error(
                format!(
                    "Expected {} arguments but got {}",
                    closure.function.arity, arg_count
                )
                .as_str(),
            );
            return Err(());
        }

        if self.frames.len() == isize::MAX as usize {
            self.runtime_error("Stack overflow");
            return Err(());
        }

        let frame = CallFrame {
            closure,
            ip: 0,
            slot_offset: (self.stack.len() as isize - arg_count as isize - 1) as usize,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.frames.last_mut().unwrap();
        frame.ip += 1;
        *frame.closure.function.chunk().at(frame.ip - 1)
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.frames.last_mut().unwrap();
        frame.ip += 2;
        (*frame.closure.function.chunk().at(frame.ip - 2) as u16) << 8
            | *frame.closure.function.chunk().at(frame.ip - 1) as u16
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
            Constant::Function(f) => Value::Function(Rc::new(f.clone())),
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
        eprintln!("{}", err);
        for frame in self.frames.iter().rev() {
            let instruction = frame.ip - 1;
            eprintln!(
                "[line {}] in {}",
                frame.closure.function.chunk().line_at_offset(instruction),
                if let Some(n) = frame.closure.function.name() {
                    n.as_str()
                } else {
                    "script"
                }
            );
        }
        eprintln!("Stack end");
        self.clear_stack();
    }

    fn define_native(&mut self, name: &'static str, function: NativeFunctionPointer) {
        let s = self.allocate_string(name.to_string());
        self.globals.insert(s, Value::NativeFunction(function));
    }
}

fn native_clock(arg_count: u8, _: &[Value]) -> Result<Value, String> {
    if arg_count != 0 {
        return Err(format!("Expected 0 arguments but got {}", arg_count));
    }

    let t: f64 = SystemTime::UNIX_EPOCH
        .elapsed()
        .unwrap_or_else(|_| Duration::default())
        .as_secs_f64();
    Ok(Value::Number(t))
}

fn native_wait(arg_count: u8, args: &[Value]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!("Expected 1 argument but got {}", arg_count));
    }

    let t = match args[0] {
        Value::Number(n) => n,
        _ => {
            return Err("Expected number argument".to_string());
        }
    };
    std::thread::sleep(Duration::from_secs_f64(t));
    Ok(Value::Nil)
}
