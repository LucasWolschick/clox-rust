use super::{Chunk, Value, OpCode};
use super::debug;

pub fn test() -> Chunk {
    let mut chunk = Chunk::new();
    chunk.write_constant(Value::Number(1.2), 123);
    chunk.write_constant(Value::Number(3.4), 123);
    chunk.write_o(OpCode::Add, 123);
    chunk.write_constant(Value::Number(5.6), 123);
    chunk.write_o(OpCode::Divide, 123);
    chunk.write_o(OpCode::Negate, 123);
    chunk.write_o(OpCode::Return, 123);

    debug::disassemble(&chunk, "test chunk");
    println!("== CHUNK ABOVE ==");
    chunk
}