mod compiler;
mod value;
mod chunk;
mod opcodes;
mod debug;
mod vm;

use chunk::Chunk;
use opcodes::OpCode;
use value::Value;
use vm::VM;

pub fn test() {
    let chunk = compiler::test();
    let mut vm = VM::new(chunk);
    vm.debug(true);
    vm.interpret();
}