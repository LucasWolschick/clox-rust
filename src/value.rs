use std::rc::Rc;

use super::chunk::Chunk;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(StringReference),
    Function(FunctionReference),
    Nil,
}

pub type StringReference = Rc<String>;
pub type FunctionReference = Rc<FunctionObject>;

#[derive(Clone, PartialEq)]
pub struct FunctionObject {
    arity: u8,
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
            Value::Number(n) => f.write_str(n.to_string().as_str()),
            Value::Bool(b) => f.write_str(b.to_string().as_str()),
            Value::String(s) => f.write_str(s.as_str()),
            Value::Function(fun) => match &fun.name {
                Some(s) => f.write_fmt(format_args!("<fn {}>", s)),
                None => f.write_str("<script>")
            }
            Value::Nil => f.write_str("nil"),
        }
    }
}

impl FunctionObject {
    pub fn new() -> Self {
        Self {
            arity: 0,
            name: None,
            chunk: Chunk::new(),
        }
    }

    pub fn set_name(&mut self, name: StringReference) {
        self.name = Some(name)
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}