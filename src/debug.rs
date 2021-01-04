use super::chunk::Chunk;
use super::opcodes::OpCode;

pub fn disassemble(chunk: &Chunk, title: &str) {
    println!("== {} ==", title);
    
    let mut offset = 0;
    while offset < chunk.count() {
        offset = disassemble_instruction(&chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    let this_line = chunk.line_at_offset(offset);
    if offset > 0 && this_line == chunk.line_at_offset(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", this_line);
    }

    let instruction = chunk.at(offset);
    match OpCode::from(*instruction) {
        OpCode::Return => {
            simple_instruction("OP_RETURN", offset)
        }
        OpCode::Constant => {
            constant_instruction("OP_CONSTANT", &chunk, offset)
        }
        OpCode::LongConstant => {
            long_constant_instruction("OP_CONSTANT_LONG", &chunk, offset)
        }
        OpCode::Negate => {
            simple_instruction("OP_NEGATE", offset)
        }
        OpCode::Add => {
            simple_instruction("OP_ADD", offset)
        }
        OpCode::Subtract => {
            simple_instruction("OP_SUBTRACT", offset)
        }
        OpCode::Multiply => {
            simple_instruction("OP_MULTIPLY", offset)
        }
        OpCode::Divide => {
            simple_instruction("OP_DIVIDE", offset)
        }
        OpCode::Nop => {
            simple_instruction("OP_NOP", offset)
        }
        _ => {
            println!("Unknown opcode {:?}", instruction);
            offset + 1
        }
    }
}

fn simple_instruction(title: &str, offset: usize) -> usize {
    println!("{}", title);
    offset + 1
}

fn constant_instruction(title: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = *chunk.at(offset+1);
    println!("{:16} {:4} '{}'", title, constant, chunk.get_constant(constant as usize));
    offset + 2
}

fn long_constant_instruction(title: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = *chunk.at(offset+1) as usize  // first byte
        | (*chunk.at(offset+2) as usize) << 8 // second byte
        | (*chunk.at(offset+3) as usize) << 16; // third byte
    println!("{:16} {:4} '{}'", title, constant, chunk.get_constant(constant as usize));
    offset + 4
}