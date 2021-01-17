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
        OpCode::Return => simple_instruction("OP_RETURN", offset),
        OpCode::Constant => constant_instruction("OP_CONSTANT", &chunk, offset),
        OpCode::LongConstant => long_constant_instruction("OP_CONSTANT_LONG", &chunk, offset),
        OpCode::DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", &chunk, offset),
        OpCode::DefineLongGlobal => {
            long_constant_instruction("OP_DEFINE_GLOBAL_LONG", &chunk, offset)
        }
        OpCode::GetGlobal => constant_instruction("OP_GET_GLOBAL", &chunk, offset),
        OpCode::GetLongGlobal => long_constant_instruction("OP_GET_GLOBAL_LONG", &chunk, offset),
        OpCode::SetGlobal => constant_instruction("OP_SET_GLOBAL", &chunk, offset),
        OpCode::SetLongGlobal => long_constant_instruction("OP_SET_GLOBAL_LONG", &chunk, offset),
        OpCode::GetLocal => byte_instruction("OP_GET_LOCAL", &chunk, offset),
        OpCode::SetLocal => byte_instruction("OP_SET_LOCAL", &chunk, offset),
        OpCode::Negate => simple_instruction("OP_NEGATE", offset),
        OpCode::Add => simple_instruction("OP_ADD", offset),
        OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
        OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
        OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
        OpCode::Nop => simple_instruction("OP_NOP", offset),
        OpCode::Not => simple_instruction("OP_NOT", offset),
        OpCode::True => simple_instruction("OP_TRUE", offset),
        OpCode::False => simple_instruction("OP_FALSE", offset),
        OpCode::Nil => simple_instruction("OP_NIL", offset),
        OpCode::Greater => simple_instruction("OP_GREATER", offset),
        OpCode::Less => simple_instruction("OP_LESS", offset),
        OpCode::Equal => simple_instruction("OP_EQUAL", offset),
        OpCode::Print => simple_instruction("OP_PRINT", offset),
        OpCode::Pop => simple_instruction("OP_POP", offset),
        OpCode::JumpIfFalse => jump_instruction("OP_JUMP", 1, &chunk, offset),
        OpCode::Jump => jump_instruction("OP_JUMP_IF_FALSE", 1, &chunk, offset),
        OpCode::Loop => jump_instruction("OP_LOOP", -1, &chunk, offset),
        OpCode::Call => byte_instruction("OP_CALL", &chunk, offset),
    }
}

fn simple_instruction(title: &str, offset: usize) -> usize {
    println!("{}", title);
    offset + 1
}

fn byte_instruction(title: &str, chunk: &Chunk, offset: usize) -> usize {
    println!("{:16} {:4}", title, *chunk.at(offset + 1));
    offset + 2
}

fn constant_instruction(title: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = *chunk.at(offset + 1);
    println!(
        "{:16} {:4} '{}'",
        title,
        constant,
        chunk.get_constant(constant as usize)
    );
    offset + 2
}

fn jump_instruction(title: &str, sign: i32, chunk: &Chunk, offset: usize) -> usize {
    let jump = (*chunk.at(offset + 1) as i16) << 8;
    let jump = jump | *chunk.at(offset + 2) as i16;
    println!(
        "{:16} {:4} -> {}",
        title,
        offset,
        (offset + 3) as i32 + sign * jump as i32
    );
    offset + 3
}

fn long_constant_instruction(title: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = *chunk.at(offset+1) as usize  // first byte
        | (*chunk.at(offset+2) as usize) << 8 // second byte
        | (*chunk.at(offset+3) as usize) << 16; // third byte
    println!(
        "{:16} {:4} '{}'",
        title,
        constant,
        chunk.get_constant(constant as usize)
    );
    offset + 4
}
