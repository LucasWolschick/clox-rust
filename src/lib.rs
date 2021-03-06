#![deny(clippy::all)]

mod chunk;
mod compiler;
mod debug;
mod opcodes;
mod scanner;
mod value;
mod vm;

use fnv::FnvHashMap as HashTable;
use fnv::FnvHashSet as HashSet;

use std::{
    fs::File,
    io::{Read, Write},
};

pub type InterpretResult = Result<(), InterpretError>;
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub fn repl() {
    let stdin = std::io::stdin();
    let mut vm = vm::VM::new();
    println!("Lox - REPL");
    println!("(press Ctrl+Z on an empty line to quit)");

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        if stdin.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }

        let function = compiler::compile(&line);
        if let Ok(function) = function {
            vm.debug(std::env::var("CLOX_DEBUG").is_ok());
            let _ = vm.interpret(function);
        }
    }
}

pub fn file(mut file: File) {
    let mut src = String::new();
    file.read_to_string(&mut src).unwrap();
    let _result = interpret(src.as_str());
}

fn interpret(source: &str) -> InterpretResult {
    let function = compiler::compile(source)?;
    let mut vm = vm::VM::new(); //todo: persist state
    vm.debug(std::env::var("CLOX_DEBUG").is_ok());
    vm.interpret(function)
}
