use std::rc::Rc;

use super::chunk::{Chunk, Constant};
use super::opcodes::OpCode;
use super::scanner::{Scanner, Token, TokenType};
use super::value::{FunctionObject, StringReference};
use super::InterpretError;

#[derive(Debug)]
struct Local {
    name: String,
    depth: isize,
    captured: bool,
}

#[derive(Debug)]
struct Resolver {
    locals: Vec<Local>,
    depth: isize,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            locals: Vec::new(),
            depth: 0,
        }
    }

    pub fn begin_scope(&mut self) {
        self.depth += 1;
    }

    #[must_use]
    /// Returns the number of pop instructions which must be emitted.
    pub fn end_scope(&mut self) -> isize {
        self.depth -= 1;

        let mut count = 0;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.depth {
            count += 1;
            self.locals.pop();
        }

        count
    }

    pub fn depth(&self) -> isize {
        self.depth
    }

    pub fn n_locals(&self) -> usize {
        self.locals.len()
    }

    pub fn add_local(&mut self, name: String) -> Result<(), ()> {
        if self.locals.len() > u8::MAX as usize {
            return Err(());
        }

        let local = Local {
            name,
            depth: -1,
            captured: false,
        };
        self.locals.push(local);
        Ok(())
    }

    pub fn mark_initialized(&mut self) {
        let depth = self.depth();
        if depth == 0 {
            return;
        }
        self.locals.last_mut().unwrap().depth = depth;
    }

    pub fn local(&self, index: usize) -> &Local {
        &self.locals[index]
    }

    pub fn local_mut(&mut self, index: usize) -> &mut Local {
        &mut self.locals[index]
    }

    /// Returns the index of the local plus whether it was initialized.
    pub fn resolve_local(&self, token: &str) -> (Option<isize>, bool) {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == token {
                return (Some(i as isize), local.depth != -1);
            }
        }
        (None, true)
    }
}

struct Upvalue {
    is_local: bool,
    index: isize,
}

enum CompilerWhere {
    Script,
    Function(StringReference),
    Method,
    Initializer,
}

struct CompilerState {
    function: FunctionObject,
    upvalues: Vec<Upvalue>,
    resolver: Resolver,
    place: CompilerWhere,
}

impl CompilerState {
    fn new(place: CompilerWhere) -> Self {
        let mut resolver = Resolver::new();
        let mut function = FunctionObject::new();

        match place {
            CompilerWhere::Script => {
                resolver.add_local(String::new()).unwrap();
            }
            CompilerWhere::Function(ref name) => {
                let name = name.clone();
                resolver.add_local((*name).clone()).unwrap();
                function.set_name(name);
            }
            CompilerWhere::Method | CompilerWhere::Initializer => {
                resolver.add_local(String::from("this")).unwrap();
            }
        }

        let zeroth_local = resolver.local_mut(resolver.n_locals() - 1);
        zeroth_local.depth = 0;
        zeroth_local.captured = false;

        CompilerState {
            function,
            resolver,
            upvalues: Vec::new(),
            place,
        }
    }
}

struct ClassCompilerState {
    has_superclass: bool,
}

struct Compiler {
    scanner: Scanner,
    previous: Token,
    current: Token,
    failed: bool,
    panic_mode: bool,
    debug: bool,

    state: Vec<CompilerState>,
    class_stack: Vec<ClassCompilerState>,
}

impl Compiler {
    fn new(src: &str) -> Compiler {
        let mut resolver = Resolver::new();
        resolver.add_local(String::new()).unwrap();

        Compiler {
            state: vec![CompilerState::new(CompilerWhere::Script)],
            class_stack: Vec::new(),

            scanner: Scanner::new(src),

            previous: Token::default(),
            current: Token::default(),

            failed: false,
            panic_mode: false,
            debug: false,
        }
    }

    fn debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    // access
    fn chunk(&self) -> &Chunk {
        &self.state.last().unwrap().function.chunk()
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        self.state.last_mut().unwrap().function.chunk_mut()
    }

    // controls
    pub fn is_at_end(&mut self) -> bool {
        self.check(TokenType::Eof)
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.previous, &mut self.current);
        loop {
            self.current = self.scanner.scan();
            if !matches!(self.current.token_type, TokenType::Error) {
                break;
            }
            self.error_at_current(self.current.lexeme.clone().as_str());
        }
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) {
        if token_type == self.current.token_type {
            self.advance();
            return;
        }
        self.error_at_current(msg);
    }

    fn match_advance(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        token_type == self.current.token_type
    }

    // codegen
    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous.line;
        self.chunk_mut().write(byte, line);
    }

    fn emit_usize(&mut self, i: usize) {
        let line = self.previous.line;
        self.chunk_mut().write_usize(i, line);
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        let line = self.previous.line;
        self.chunk_mut().write_op(opcode, line);
    }

    fn emit_jump(&mut self, jump_op: OpCode) -> usize {
        self.emit_opcode(jump_op);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.chunk().count() - 2 // returns the address of the jump offset
    }

    fn patch_jump(&mut self, offset: usize) {
        let jmp = self.chunk().count() - offset - 2; // the distance to jump over
        if jmp > u16::MAX as usize {
            self.error("Too much code to jump over");
        }

        self.chunk_mut().patch(offset, (jmp >> 8) as u8);
        self.chunk_mut().patch(offset + 1, jmp as u8);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_opcode(OpCode::Loop);
        let offset = self.chunk().count() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body too large");
        }

        self.emit_byte((offset >> 8) as u8);
        self.emit_byte(offset as u8);
    }

    fn emit_return(&mut self) {
        if matches!(self.state.last().unwrap().place, CompilerWhere::Initializer) {
            self.emit_opcode(OpCode::GetLocal);
            self.emit_byte(0);
        } else {
            self.emit_opcode(OpCode::Nil);
        }
        self.emit_opcode(OpCode::Return);
    }

    fn make_constant(&mut self, constant: Constant) -> usize {
        let i = self.chunk_mut().register_constant(constant);
        if let Ok(i) = i {
            i
        } else {
            self.error("Too many constants in one chunk");
            0
        }
    }

    fn emit_constant(&mut self, constant: Constant) -> usize {
        let line = self.previous.line;
        self.chunk_mut().write_constant(constant, line).unwrap()
    }

    fn emit_closure(&mut self, constant: Constant) -> usize {
        let line = self.previous.line;
        self.chunk_mut().write_closure(constant, line).unwrap()
    }

    // parser
    fn declaration(&mut self) {
        if self.match_advance(TokenType::Class) {
            self.class_declaration();
        } else if self.match_advance(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_advance(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expected class name");
        let name = self.previous.clone();
        let name_constant = self.identifier_constant(&name);
        self.declare_variable();

        if name_constant > u8::MAX as usize {
            self.emit_opcode(OpCode::LongClass);
            self.emit_usize(name_constant);
        } else {
            self.emit_opcode(OpCode::Class);
            self.emit_byte(name_constant as u8);
        }

        self.define_variable(name_constant);
        let class_state = ClassCompilerState {
            has_superclass: false,
        };
        self.class_stack.push(class_state);

        if self.match_advance(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expected superclass name after '<'");
            self.variable(false);
            if self.previous.lexeme == name.lexeme {
                self.error("A class cannot inherit from itself");
            }

            self.begin_scope();
            self.state
                .last_mut()
                .unwrap()
                .resolver
                .add_local("super".to_string())
                .unwrap();
            self.define_variable(0);

            self.named_variable(&name, false);
            self.emit_opcode(OpCode::Inherit);
            self.class_stack.last_mut().unwrap().has_superclass = true;
        }

        self.named_variable(&name, false);

        self.consume(TokenType::LeftBrace, "Expected '{' before class body");

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }

        self.consume(TokenType::RightBrace, "Expected '}' after class body");
        self.emit_opcode(OpCode::Pop);

        if self.class_stack.last().unwrap().has_superclass {
            self.end_scope();
        }

        self.class_stack.pop();
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expected function name");
        self.state.last_mut().unwrap().resolver.mark_initialized();
        let name = Rc::new(self.previous.lexeme.clone());
        self.function(CompilerWhere::Function(name));
        self.define_variable(global);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name");

        if self.match_advance(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_opcode(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        );
        self.define_variable(global);
    }

    fn parse_variable(&mut self, err: &str) -> usize {
        self.consume(TokenType::Identifier, err);

        self.declare_variable();
        if self.state.last().unwrap().resolver.depth() > 0 {
            0
        } else {
            self.identifier_constant(&self.previous.clone())
        }
    }

    fn declare_variable(&mut self) {
        if self.state.last().unwrap().resolver.depth() == 0 {
            return;
        }

        let name = self.previous.lexeme.clone();

        // handles conflicts
        if self.state.last().unwrap().resolver.n_locals() > 0 {
            for i in (self.state.last().unwrap().resolver.n_locals() - 1)..=0 {
                let local = self.state.last().unwrap().resolver.local(i as usize);
                if local.depth != -1 && local.depth < self.state.last().unwrap().resolver.depth() {
                    break;
                }

                if name == local.name {
                    self.error("Redefinition of variable already in scope");
                }
            }
        }

        self.state
            .last_mut()
            .unwrap()
            .resolver
            .add_local(name)
            .unwrap_or_else(|_| {
                self.error("Too many local variables in function");
            });
    }

    fn identifier_constant(&mut self, token: &Token) -> usize {
        self.make_constant(Constant::String(token.lexeme.clone()))
    }

    fn define_variable(&mut self, global: usize) {
        if self.state.last().unwrap().resolver.depth() > 0 {
            self.state.last_mut().unwrap().resolver.mark_initialized();
            return;
        }

        if global > u8::MAX as usize {
            self.emit_opcode(OpCode::DefineLongGlobal);
            self.emit_usize(global);
        } else {
            self.emit_opcode(OpCode::DefineGlobal);
            self.emit_byte(global as u8);
        }
    }

    fn begin_scope(&mut self) {
        self.state.last_mut().unwrap().resolver.begin_scope();
    }

    fn end_scope(&mut self) {
        let n = self.state.last_mut().unwrap().resolver.end_scope();
        for _ in 0..n {
            let resolver = &self.state.last().unwrap().resolver;
            if resolver.locals[resolver.n_locals() - 1].captured {
                self.emit_opcode(OpCode::CloseUpvalue)
            } else {
                self.emit_opcode(OpCode::Pop);
            }
        }
    }

    fn statement(&mut self) {
        if self.match_advance(TokenType::Print) {
            self.print_statement();
        } else if self.match_advance(TokenType::If) {
            self.if_statement();
        } else if self.match_advance(TokenType::Return) {
            self.return_statement();
        } else if self.match_advance(TokenType::While) {
            self.while_statement();
        } else if self.match_advance(TokenType::For) {
            self.for_statement();
        } else if self.match_advance(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block");
    }

    fn function(&mut self, place: CompilerWhere) {
        self.state.push(CompilerState::new(place));
        self.begin_scope();

        // parameter list
        self.consume(TokenType::LeftParen, "Expected '(' after function name");
        if !self.check(TokenType::RightParen) {
            loop {
                if self.state.last().unwrap().function.arity == 255 {
                    self.error_at_current("Cannot have more than 255 parameters");
                } else {
                    self.state.last_mut().unwrap().function.arity += 1;
                }

                let param_cons = self.parse_variable("Expected parameter name");
                self.define_variable(param_cons);

                if !self.match_advance(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expected ')' after parameters");

        // body
        self.consume(TokenType::LeftBrace, "Expected '{' before function body");
        self.block();

        // return
        self.emit_return();

        // function object
        let new_state = self.state.pop().unwrap();
        let function = new_state.function;
        if self.debug && !self.failed {
            super::debug::disassemble(
                function.chunk(),
                function.name().map_or("script", |x| x.as_str()),
            );
        }

        let func_upvalues = function.upvalues;
        self.emit_closure(Constant::Function(function));

        // process locals
        for i in 0..func_upvalues {
            let upvalue = &new_state.upvalues[i as usize];
            let is_local = upvalue.is_local as u8;
            let index = upvalue.index as u8; // should be good?

            self.emit_byte(is_local);
            self.emit_byte(index);
        }
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expected method name");
        let constant = self.identifier_constant(&self.previous.clone());

        if &self.previous.lexeme == "init" {
            self.function(CompilerWhere::Initializer);
        } else {
            self.function(CompilerWhere::Method);
        }

        if constant > u8::MAX as usize {
            self.emit_opcode(OpCode::LongMethod);
            self.emit_usize(constant);
        } else {
            self.emit_opcode(OpCode::Method);
            self.emit_byte(constant as u8);
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value");
        self.emit_opcode(OpCode::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expected ';' after value");
        self.emit_opcode(OpCode::Pop);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'");
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after condition");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.statement();
        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);
        self.emit_opcode(OpCode::Pop);
        if self.match_advance(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn return_statement(&mut self) {
        if self.state.last().unwrap().function.name().is_none() {
            self.error("Cannot return from top-level code");
        }

        if self.match_advance(TokenType::Semicolon) {
            self.emit_return();
        } else {
            if matches!(self.state.last().unwrap().place, CompilerWhere::Initializer) {
                self.error("Attempt to return value from initializer");
            }
            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after return value");
            self.emit_opcode(OpCode::Return);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.chunk().count();
        self.consume(TokenType::LeftParen, "Expected '(' after while");
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after condition");
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_opcode(OpCode::Pop);
        self.statement();
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
        self.emit_opcode(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'");

        // INITIALIZER
        if self.match_advance(TokenType::Semicolon) {
            // no initializer
        } else if self.match_advance(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.chunk().count();

        // CONDITION
        let exit_jump = if !self.match_advance(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expected ';' after loop condition");

            let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
            self.emit_opcode(OpCode::Pop);
            Some(exit_jump)
        } else {
            None
        };

        // INCREMENT
        if !self.match_advance(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.chunk().count();
            self.expression();
            self.emit_opcode(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expected ')' after for clauses");
            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        // BODY
        self.statement();

        self.emit_loop(loop_start);

        // CONDITION PATCHING
        if let Some(offset) = exit_jump {
            self.patch_jump(offset);
            self.emit_opcode(OpCode::Pop); // for the condition
        }

        self.end_scope();
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn and(&mut self, _: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self, _: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(else_jump);
        self.emit_opcode(OpCode::Pop);
        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn binary(&mut self, _: bool) {
        let token_type = self.previous.token_type;

        let rule = Self::get_rule(token_type);
        self.parse_precedence(rule.2.higher());

        match token_type {
            TokenType::Plus => self.emit_opcode(OpCode::Add),
            TokenType::Minus => self.emit_opcode(OpCode::Subtract),
            TokenType::Star => self.emit_opcode(OpCode::Multiply),
            TokenType::Slash => self.emit_opcode(OpCode::Divide),
            TokenType::Greater => self.emit_opcode(OpCode::Greater),
            TokenType::Less => self.emit_opcode(OpCode::Less),
            TokenType::EqualEqual => self.emit_opcode(OpCode::Equal),
            TokenType::BangEqual => {
                // a != b -> !(a == b)
                self.emit_opcode(OpCode::Equal);
                self.emit_opcode(OpCode::Not);
            }
            TokenType::LessEqual => {
                // a <= b -> !(a > b) ** NOTE: NaN IS BROKEN
                self.emit_opcode(OpCode::Greater);
                self.emit_opcode(OpCode::Not);
            }
            TokenType::GreaterEqual => {
                // a >= b -> !(a < b) ** NOTE: NaN IS BROKEN
                self.emit_opcode(OpCode::Less);
                self.emit_opcode(OpCode::Not);
            }
            _ => unreachable!(),
        }
    }

    fn call(&mut self, _: bool) {
        let arg_count = self.argument_list();
        self.emit_opcode(OpCode::Call);
        self.emit_byte(arg_count);
    }

    fn argument_list(&mut self) -> u8 {
        let mut count = 0;

        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if count == 255 {
                    self.error("Cannot have more than 255 arguments");
                } else {
                    count += 1;
                }

                if !self.match_advance(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expected ')' after arguments");

        count
    }

    fn grouping(&mut self, _: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after expression");
    }

    fn unary(&mut self, _: bool) {
        let token_type = self.previous.token_type;
        self.parse_precedence(Precedence::Unary); // operand
        match token_type {
            TokenType::Minus => self.emit_opcode(OpCode::Negate),
            TokenType::Bang => self.emit_opcode(OpCode::Not),
            _ => unreachable!(),
        }
    }

    fn number(&mut self, _: bool) {
        let value = self.previous.lexeme.parse::<f64>().unwrap_or_default();
        let value = Constant::Number(value);
        self.emit_constant(value);
    }

    fn string(&mut self, _: bool) {
        let len = self.previous.lexeme.len();
        let value = self.previous.lexeme.chars().skip(1).take(len - 2).collect();
        let value = Constant::String(value);
        self.emit_constant(value);
    }

    fn literal(&mut self, _: bool) {
        match self.previous.token_type {
            TokenType::False => self.emit_opcode(OpCode::False),
            TokenType::True => self.emit_opcode(OpCode::True),
            TokenType::Nil => self.emit_opcode(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(&self.previous.clone(), can_assign);
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expected property name after '.'");
        let name = self.identifier_constant(&self.previous.clone());
        let is_long = name > u8::MAX as usize;

        if can_assign && self.match_advance(TokenType::Equal) {
            self.expression();
            if is_long {
                self.emit_opcode(OpCode::SetLongProperty);
                self.emit_usize(name);
            } else {
                self.emit_opcode(OpCode::SetProperty);
                self.emit_byte(name as u8);
            }
        } else if self.match_advance(TokenType::LeftParen) {
            // immediate method call
            let arg_count = self.argument_list();
            if is_long {
                self.emit_opcode(OpCode::LongInvoke);
                self.emit_usize(name);
            } else {
                self.emit_opcode(OpCode::Invoke);
                self.emit_byte(name as u8);
            }
            self.emit_byte(arg_count);
        } else {
            // we do property accesses
            if is_long {
                self.emit_opcode(OpCode::GetLongProperty);
                self.emit_usize(name);
            } else {
                self.emit_opcode(OpCode::GetProperty);
                self.emit_byte(name as u8);
            }
        }
    }

    fn this(&mut self, _: bool) {
        if self.class_stack.is_empty() {
            self.error("Attempt to use 'this' outside of a class");
            return;
        }
        self.variable(false);
    }

    fn super_(&mut self, _: bool) {
        if self.class_stack.is_empty() {
            self.error("Cannot use 'super' outside of class");
        } else if !self.class_stack.last().unwrap().has_superclass {
            self.error("Cannot use 'super' in a class with no superclass");
        }
        self.consume(TokenType::Dot, "Expected '.' after 'super'");
        self.consume(TokenType::Identifier, "Expected method name after 'super'");
        let name = self.identifier_constant(&self.previous.clone());

        self.named_variable(
            &Token {
                token_type: TokenType::This,
                line: 0,
                lexeme: "this".to_string(),
            },
            false,
        );

        if self.match_advance(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(
                &Token {
                    token_type: TokenType::Super,
                    line: 0,
                    lexeme: "super".to_string(),
                },
                false,
            );
            if name > u8::MAX as usize {
                self.emit_opcode(OpCode::LongSuperInvoke);
                self.emit_usize(name);
            } else {
                self.emit_opcode(OpCode::SuperInvoke);
                self.emit_byte(name as u8);
            }
            self.emit_byte(arg_count);
        } else {
            self.named_variable(
                &Token {
                    token_type: TokenType::Super,
                    line: 0,
                    lexeme: "super".to_string(),
                },
                false,
            );
            if name > u8::MAX as usize {
                self.emit_opcode(OpCode::GetLongSuper);
                self.emit_usize(name);
            } else {
                self.emit_opcode(OpCode::GetSuper);
                self.emit_byte(name as u8);
            }
        }
    }

    fn resolve_upvalue(&mut self, name: &str, depth: usize) -> Option<isize> {
        if depth == 0 {
            // root level. we're looking at a global
            return None;
        }

        if let (Some(arg), bool) = self.state[depth - 1].resolver.resolve_local(name) {
            // we got an upvalue
            if !bool {
                self.error("Can't read local variable in own initializer");
            }
            self.state[depth - 1].resolver.locals[arg as usize].captured = true;
            return Some(self.add_upvalue(depth, arg, true));
        }

        if let Some(arg) = self.resolve_upvalue(name, depth - 1) {
            // look in other scopes
            return Some(self.add_upvalue(depth, arg, false));
        }

        None
    }

    fn add_upvalue(&mut self, depth: usize, index: isize, is_local: bool) -> isize {
        let state = &self.state[depth];
        if let Some((i, _)) = state
            .upvalues
            .iter()
            .enumerate()
            .find(|(_, u)| u.index == index && u.is_local == is_local)
        {
            return i as isize;
        }

        if state.upvalues.len() >= u8::MAX as usize {
            self.error("Too many upvalues in function");
            return 0;
        }

        self.state[depth].upvalues.push(Upvalue { is_local, index });
        self.state[depth].function.upvalues += 1;
        self.state[depth].upvalues.len() as isize - 1
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let (arg, get_op, set_op, long) = if let (Some(arg), initialized) = self
            .state
            .last()
            .unwrap()
            .resolver
            .resolve_local(&name.lexeme)
        {
            // we're dealing with locals.
            if !initialized {
                self.error("Can't read local variable in own initializer");
            }
            (arg as usize, OpCode::GetLocal, OpCode::SetLocal, false)
        } else if let Some(arg) = self.resolve_upvalue(&name.lexeme, self.state.len() - 1) {
            // we're dealing with up values
            (arg as usize, OpCode::GetUpvalue, OpCode::SetUpvalue, false)
        } else {
            // we're dealing with globals.
            let arg = self.identifier_constant(name) as usize;
            if arg > u8::MAX as usize {
                (arg, OpCode::GetLongGlobal, OpCode::SetLongGlobal, true)
            } else {
                (arg, OpCode::GetGlobal, OpCode::SetGlobal, false)
            }
        };

        if can_assign && self.match_advance(TokenType::Equal) {
            self.expression();
            self.emit_opcode(set_op);
            if long {
                self.emit_usize(arg);
            } else {
                self.emit_byte(arg as u8);
            }
        } else {
            self.emit_opcode(get_op);
            if long {
                self.emit_usize(arg);
            } else {
                self.emit_byte(arg as u8);
            }
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let can_assign = precedence as isize <= Precedence::Assignment as isize;

        let prefix = Self::get_rule(self.previous.token_type).0;
        if let Some(prefix) = prefix {
            prefix(self, can_assign);
        } else {
            self.error("Expected expression");
            return;
        }

        while precedence as isize <= Self::get_rule(self.current.token_type).2 as isize {
            self.advance();
            let infix = Self::get_rule(self.previous.token_type).1;
            if let Some(infix) = infix {
                infix(self, can_assign);
            }
        }

        if can_assign && self.match_advance(TokenType::Equal) {
            self.error("Invalid assignment target");
        }
    }

    fn get_rule(token_type: TokenType) -> ParseRule {
        match token_type {
            // ParseRule(prefix, infix, precedence)
            TokenType::LeftParen => {
                ParseRule(Some(Self::grouping), Some(Self::call), Precedence::Call)
            }
            TokenType::Minus => ParseRule(Some(Self::unary), Some(Self::binary), Precedence::Term),
            TokenType::Plus => ParseRule(None, Some(Self::binary), Precedence::Term),
            TokenType::Slash => ParseRule(None, Some(Self::binary), Precedence::Factor),
            TokenType::Star => ParseRule(None, Some(Self::binary), Precedence::Factor),
            TokenType::Number => ParseRule(Some(Self::number), None, Precedence::None),
            TokenType::Nil => ParseRule(Some(Self::literal), None, Precedence::None),
            TokenType::False => ParseRule(Some(Self::literal), None, Precedence::None),
            TokenType::True => ParseRule(Some(Self::literal), None, Precedence::None),
            TokenType::Bang => ParseRule(Some(Self::unary), None, Precedence::None),
            TokenType::EqualEqual => ParseRule(None, Some(Self::binary), Precedence::Equality),
            TokenType::BangEqual => ParseRule(None, Some(Self::binary), Precedence::Equality),
            TokenType::LessEqual => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::GreaterEqual => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::Greater => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::Less => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::String => ParseRule(Some(Self::string), None, Precedence::None),
            TokenType::Identifier => ParseRule(Some(Self::variable), None, Precedence::None),
            TokenType::And => ParseRule(None, Some(Self::and), Precedence::And),
            TokenType::Or => ParseRule(None, Some(Self::or), Precedence::Or),
            TokenType::Dot => ParseRule(None, Some(Self::dot), Precedence::Call),
            TokenType::This => ParseRule(Some(Self::this), None, Precedence::None),
            TokenType::Super => ParseRule(Some(Self::super_), None, Precedence::None),
            _ => ParseRule(None, None, Precedence::None),
        }
    }

    // error handling
    fn synchronize(&mut self) {
        self.panic_mode = false;
        while !self.is_at_end() {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }

            match self.current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            };

            self.advance();
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.failed = true;
        self.error_at(&self.current.clone(), msg);
    }

    fn error(&mut self, msg: &str) {
        self.failed = true;
        self.error_at(&self.previous.clone(), msg);
    }

    fn error_at(&mut self, token: &Token, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);
        match token.token_type {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", token.lexeme),
        };
        eprintln!(": {}", msg);
    }

    fn failed(&self) -> bool {
        self.failed
    }
}

pub fn compile(src: &str) -> Result<FunctionObject, InterpretError> {
    let mut compiler = Compiler::new(src);
    compiler.debug(std::env::var("CLOX_DEBUG").is_ok());

    compiler.advance();
    while !compiler.is_at_end() {
        compiler.declaration();
    }
    compiler.emit_return();

    if compiler.failed() {
        Err(InterpretError::CompileError)
    } else {
        if compiler.debug {
            super::debug::disassemble(&compiler.chunk(), "script");
        }
        Ok(compiler.state.pop().unwrap().function)
    }
}

#[derive(Clone, Copy, Debug)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn higher(self) -> Precedence {
        use Precedence::*;
        match self {
            None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call | Primary => Primary,
        }
    }
}

type ParseFn = fn(&mut Compiler, bool) -> ();
struct ParseRule(Option<ParseFn>, Option<ParseFn>, Precedence);
