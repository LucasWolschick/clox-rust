# clox-rust

An implementation of the [Lox programming language](https://www.craftinginterpreters.com) ([Github](https://www.github.com/munificent/craftinginterpreters)) written in Rust.

## About

This was written over the course of approximately two months, after I finished another implementation (which is not published) for the language similar to the book's `jlox`, also in Rust.

This implementation mimics Crafting Interpreters's `clox`, a single-pass one-token-lookahead compiler + virtual machine for the Lox language. It has worse performance characteristics than the reference C implementation and lacks a garbage collector, mainly because of the gymnastics that Rust requires to ensure its ownership rules and also because it offers reference counting out of the box as an alternative, through the types Rc and Weak.

Some patterns in this repository draw 
inspiration from [rulox](https://www.github.com/mariosangiorgio/rulox), which (at the time of writing) did not have a complete VM implementation.

To output debug information, run clox-rust with `CLOX_DEBUG=1` set in your environment variables.

Some changes from `clox`:

- This has no garbage collector. Memory is managed by `Rc`s scattered around the code, which can potentially form cycles (for which I have not tested this yet).
- Some stack-residing structures in `clox` (such as, well... the stack) are stored in the heap through `Vec`s in this implementation, because of the restrictions around array types caused by the lack of const generics in Rust. This may cause a performance penalty as memory is further away from other memory (but I haven't profiled it yet).
- Early on, I decided to take on one of the book's challenges and add support for constants larger than 255, through the implementation of an extra set of instructions prefixed with `Long` which take `usize`s instead of `u8`s in the VM. This with time turned out quite incovenient, as many instructions need to access constants and that means that every single one of them need their own pair of short/long addressing opcodes and supporting functions. This might be removed in the future as it implied lots and lots of duplicate code and extra complexity (but at least I know what's the difference between big-endianness and little-endianness in practice now).
- This implementation is significantly slower than its C counterpart, mainly because there's less pointer sharing, less stuff on the stack and more cloning (which I added to comply with Rust's borrowing rules). Even optimized builds (`--release`) run slower than `clox` on benchmarks like `fib.lox`.
- There is an extra built-in function, `wait(n)`, which halts execution of the VM for n seconds and then resumes it.
- Among other things...

## Building

Run `cargo build --release` on a terminal and then yank the executable generated inside the target folder. This requires you to have Rust installed in your machine.

clox-rust doesn't have any platform-specific code, so it should run fine in any platform supported by the Rust compiler.

## Usage

Running the executable with no arguments will place you in a basic REPL session. There is no multiline input; remove any newlines from your code before pasting it in. To exit the REPL, press Ctrl-Z on a blank line on Windows (or the equivalent shortcut which emits an EOF signal on other platforms).

If you supply the executable with a path to a Lox script, it will compile it and run it, printing any program output to stdout.

If `CLOX_DEBUG=1` is set in the environment, the interpreter will log the disassembled compiled code and the instructions executed, along with the stack state at any given point.

