#[derive(Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Bool(a), Bool(b)) => a == b,
            (Number(a), Number(b)) => a == b,
            (Nil, Nil) => true,
            _ => false,
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
            Value::Nil => f.write_str("nil"),
        }
    }
}

#[derive(Default)]
pub struct ValueArray {
    values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> ValueArray {
        ValueArray {
            values: Vec::new()
        }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn clear(&mut self) {
        self.values.clear();
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }

    pub fn at(&self, i: usize) -> &Value {
        &self.values[i]
    }
}