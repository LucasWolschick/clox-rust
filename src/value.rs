use std::rc::Rc;
use std::cell::RefCell;

use super::chunk::Chunk;
use super::HashTable;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(StringReference),
    Function(FunctionReference),
    Class(ClassReference),
    Instance(InstanceReference),
    NativeFunction(NativeFunctionPointer),
    Closure(ClosureReference),
    Nil,
}

pub type StringReference = Rc<String>;
pub type FunctionReference = Rc<FunctionObject>;
pub type ClosureReference = Rc<ClosureObject>;
pub type ClassReference = Rc<ClassObject>;
pub type UpvalueReference = Rc<RefCell<Upvalue>>;
pub type InstanceReference = Rc<RefCell<InstanceObject>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Upvalue {
    Open {slot: usize},
    Closed {value: Value},
}

// ugly hack because of https://github.com/rust-lang/rust/issues/54508
#[derive(Clone)]
pub struct NativeFunctionPointer(pub fn(u8, &[Value]) -> Result<Value, String>, pub &'static str);
impl PartialEq for NativeFunctionPointer {
    fn eq(&self, other: &Self) -> bool {
        self.0 as usize == other.0 as usize && self.1 == other.1
    }
}
// i hate this

#[derive(Clone, PartialEq)]
pub struct FunctionObject {
    pub arity: u8,
    pub upvalues: i32,
    chunk: Chunk,
    name: Option<StringReference>,
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }
}

impl Default for Value {
    fn default() -> Value {
        Value::Nil
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::NativeFunction(native_obj) => {
                f.write_str("<native fn ")?;
                f.write_str(native_obj.1)?;
                f.write_str(">")
            }
            Value::Number(n) => f.write_str(n.to_string().as_str()),
            Value::Bool(b) => f.write_str(b.to_string().as_str()),
            Value::String(s) => f.write_str(s.as_str()),
            Value::Function(fun) => match &fun.name {
                Some(s) => f.write_fmt(format_args!("<fn {}>", s)),
                None => f.write_str("<script>")
            }
            Value::Class(c) => f.write_fmt(format_args!("<class {}>", c.name())),
            Value::Instance(i) => f.write_fmt(format_args!("<instance {}>", i.borrow().class.name())),
            Value::Closure(closure) => match &closure.function.name {
                Some(s) => f.write_fmt(format_args!("<fn {}>", s)),
                None => f.write_str("<script>")
            }
            Value::Nil => f.write_str("nil"),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Value as std::fmt::Display>::fmt(self, f)
    }
}

impl FunctionObject {
    pub fn new() -> Self {
        Self {
            arity: 0,
            upvalues: 0,
            name: None,
            chunk: Chunk::new(),
        }
    }

    pub fn set_name(&mut self, name: StringReference) {
        self.name = Some(name)
    }

    pub fn name(&self) -> Option<&StringReference> {
        self.name.as_ref()
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

#[derive(Clone, PartialEq)]
pub struct ClosureObject {
    pub function: FunctionReference,
    pub upvalues: Vec<UpvalueReference>,
    pub n_upvalues: usize,
}

impl ClosureObject {
    pub fn new(function: FunctionReference) -> Self {
        let upvalues = function.upvalues as usize;
        Self { function, upvalues: Vec::with_capacity(upvalues), n_upvalues: upvalues }
    }
}

#[derive(Clone, PartialEq)]
pub struct ClassObject {
    name: StringReference,
}

impl ClassObject {
    pub fn new(name: StringReference) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &StringReference {
        &self.name
    }
}

#[derive(Clone, PartialEq)]
pub struct InstanceObject {
    pub class: ClassReference,
    pub fields: HashTable<StringReference, Value>,
}

impl InstanceObject {
    pub fn new(class: ClassReference) -> Self {
        Self {
            class,
            fields: HashTable::default(),
        }
    }
}