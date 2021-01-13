use super::OpCode;
use super::Value;
use super::value::ValueArray;

#[derive(Default)]
pub struct Chunk {
    constants: ValueArray,
    lines: Vec<(isize, usize)>,
    code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            lines: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write(&mut self, byte: u8, line: isize) {
        self.code.push(byte);
        if !self.lines.is_empty() && self.lines.last().unwrap().0 == line {
            self.lines.last_mut().unwrap().1 += 1;
        } else {
            self.lines.push((line, 1));
        }
    }

    pub fn write_usize(&mut self, bytes: usize, line: isize) {
        self.write(bytes as u8, line); // first byte
        self.write((bytes >> 8) as u8, line); // second byte
        self.write((bytes >> 16) as u8, line); // third byte
    }

    pub fn write_op(&mut self, opcode: OpCode, line: isize) {
        self.write(opcode.into(), line);
    }

    pub fn write_constant(&mut self, value: Value, line: isize) -> Result<usize, ()> {
        if self.constants.count() >= isize::MAX as usize {
            return Err(())
        }

        let i = self.add_constant(value);
        if i > u8::MAX as usize {
            self.write_op(OpCode::LongConstant, line);
            self.write_usize(i, line);
        } else {
            self.write_op(OpCode::Constant, line);
            self.write(i as u8, line);
        }
        Ok(i)
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write(value);
        self.constants.count() - 1
    }

    pub fn get_constant(&self, offset: usize) -> &Value {
        self.constants.at(offset)
    }

    pub fn line_at_offset(&self, offset: usize) -> isize {
        let mut accum = offset;
        for (line, count) in &self.lines {
            if &accum <= count {
                return *line;
            } else {
                accum -= offset;
            }
        }
        panic!("line index out of bounds: index {}", offset)
    }

    pub fn _free(&mut self) {
        self.code.clear();
        self.constants.clear();
        self.lines.clear();
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn at(&self, i: usize) -> &u8 {
        &self.code[i]
    }
}