#[derive(Clone)]
pub enum Value {
    Number(f64)
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => f.write_str(n.to_string().as_str()),
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