mod chunk;
mod compiler;
mod debug;
mod opcodes;
mod scanner;
mod value;
mod vm;

use std::{fs::File, io::{Read, Write}};
use chunk::Chunk;
use opcodes::OpCode;
use value::Value;
use scanner::{Scanner, TokenType};
use vm::VM;

// pub fn test() {
//     let chunk = compiler::test();
//     let mut vm = VM::new(chunk);
//     vm.debug(true);
//     vm.interpret();
// }

pub type InterpretResult = Result<(), InterpretError>;
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub fn repl() {
    let stdin = std::io::stdin();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        if stdin.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }

        let _ = interpret(line.as_str());
    }
}

pub fn file(mut file: File) {
    let mut src = String::new();
    file.read_to_string(&mut src).unwrap();
    let result = interpret(src.as_str());
}

fn interpret(source: &str) -> InterpretResult {
    let mut scanner = Scanner::new(source);
    let mut line = 0;
    loop {
        let token = scanner.scan();
        if token.line != line {
            line = token.line;
            print!("{:4} ", line);
        } else {
            print!("   | ");
        }
        println!("{:2} '{}'", token.token_type as isize, token.lexeme);
        if token.token_type == TokenType::Eof {
            break;
        }
    }
    Ok(())
}