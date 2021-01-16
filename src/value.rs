use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(StringReference),
    Nil,
}

pub type StringReference = Rc<String>;

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
            Value::String(s) => f.write_str(s.as_str()),
            Value::Nil => f.write_str("nil"),
        }
    }
}
