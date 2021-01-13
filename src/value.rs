use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Object(ObjectReference),
    Nil,
}

pub type ObjectReference = Rc<ObjectValue>;

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectValue {
    String(String),
}

impl std::fmt::Display for ObjectValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => f.write_str(s.as_str()),
        }
    }
}

impl Default for Value {
    fn default() -> Value {
        Value::Nil
    }
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => f.write_str(n.to_string().as_str()),
            Value::Bool(b) => f.write_str(b.to_string().as_str()),
            Value::Object(o) => f.write_str(o.to_string().as_str()),
            Value::Nil => f.write_str("nil"),
        }
    }
}