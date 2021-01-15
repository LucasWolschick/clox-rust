use super::chunk::{Chunk, Constant};
use super::opcodes::OpCode;
use super::scanner::{Scanner, Token, TokenType};
use super::InterpretError;

struct Local {
    name: Token,
    depth: isize,
}

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

    pub fn add_local(&mut self, name: Token) -> Result<(), ()> {
        if self.locals.len() > u8::MAX as usize {
            return Err(());
        }

        let local = Local { name, depth: -1 };
        self.locals.push(local);
        Ok(())
    }

    pub fn mark_initialized(&mut self) {
        let depth = self.depth();
        self.locals.last_mut().unwrap().depth = depth;
    }

    pub fn local(&self, index: usize) -> &Local {
        &self.locals[index]
    }

    /// Returns the index of the local plus whether it was initialized.
    pub fn resolve(&self, token: &Token) -> (Option<isize>, bool) {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name.lexeme == token.lexeme {
                return (Some(i as isize), local.depth != -1);
            }
        }
        (None, true)
    }
}

struct Compiler {
    scanner: Scanner,
    previous: Token,
    current: Token,
    failed: bool,
    panic_mode: bool,
    chunk: Chunk,

    resolver: Resolver,
}

impl Compiler {
    fn new(src: &str) -> Compiler {
        Compiler {
            scanner: Scanner::new(src),
            chunk: Chunk::new(),
            previous: Token {
                token_type: TokenType::Error,
                lexeme: String::new(),
                line: 0,
            },
            current: Token {
                token_type: TokenType::Error,
                lexeme: String::new(),
                line: 0,
            },
            failed: false,
            panic_mode: false,

            resolver: Resolver::new(),
        }
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
        self.chunk.write(byte, self.previous.line);
    }

    fn emit_usize(&mut self, i: usize) {
        self.chunk.write_usize(i, self.previous.line);
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        self.chunk.write_op(opcode, self.previous.line);
    }

    fn emit_jump(&mut self, jump_op: OpCode) -> usize {
        self.emit_opcode(jump_op);
        self.emit_byte(0xFF);
        self.emit_byte(0xFF);
        self.chunk.count() - 2 // returns the address of the jump offset
    }

    fn patch_jump(&mut self, offset: usize) {
        let jmp = self.chunk.count() - offset - 2; // the distance to jump over
        if jmp > u16::MAX as usize {
            self.error("Too much code to jump over");
        }

        self.chunk.patch(offset, (jmp >> 8) as u8);
        self.chunk.patch(offset + 1, jmp as u8);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_opcode(OpCode::Loop);
        let offset = self.chunk.count() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error("Loop body too large");
        }

        self.emit_byte((offset >> 8) as u8);
        self.emit_byte(offset as u8);
    }

    fn make_constant(&mut self, constant: Constant) -> usize {
        let i = self.chunk.register_constant(constant);
        if let Ok(i) = i {
            i
        } else {
            self.error("Too many constants in one chunk");
            0
        }
    }

    fn emit_constant(&mut self, constant: Constant) -> usize {
        let i = self.make_constant(constant);
        if i > u8::MAX as usize {
            self.emit_opcode(OpCode::LongConstant);
            self.emit_usize(i);
        } else {
            self.emit_opcode(OpCode::Constant);
            self.emit_byte(i as u8);
        }
        i
    }

    // parser
    fn declaration(&mut self) {
        if self.match_advance(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
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
        if self.resolver.depth() > 0 {
            0
        } else {
            self.identifier_constant(&self.previous.clone())
        }
    }

    fn declare_variable(&mut self) {
        if self.resolver.depth() == 0 {
            return;
        }

        let name = self.previous.clone();

        // handles conflicts
        if self.resolver.n_locals() > 0 {
            for i in (self.resolver.n_locals() - 1)..=0 {
                let local = self.resolver.local(i as usize);
                if local.depth != -1 && local.depth < self.resolver.depth() {
                    break;
                }

                if name.lexeme == local.name.lexeme {
                    self.error("Redefinition of variable already in scope");
                }
            }
        }

        self.resolver.add_local(name).unwrap_or_else(|_| {
            self.error("Too many local variables in function");
        });
    }

    fn identifier_constant(&mut self, token: &Token) -> usize {
        self.make_constant(Constant::String(token.lexeme.clone()))
    }

    fn define_variable(&mut self, global: usize) {
        if self.resolver.depth() > 0 {
            self.resolver.mark_initialized();
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
        self.resolver.begin_scope();
    }

    fn end_scope(&mut self) {
        let n = self.resolver.end_scope();
        for _ in 0..n {
            self.emit_opcode(OpCode::Pop);
        }
    }

    fn statement(&mut self) {
        if self.match_advance(TokenType::Print) {
            self.print_statement();
        } else if self.match_advance(TokenType::If) {
            self.if_statement();
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

    fn while_statement(&mut self) {
        let loop_start = self.chunk.count();
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

        let mut loop_start = self.chunk.count();

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
            let increment_start = self.chunk.count();
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

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let (arg, get_op, set_op, long) =
            if let (Some(arg), initialized) = self.resolver.resolve(name) {
                // we're dealing with locals.
                if !initialized {
                    self.error("Can't read local variable in own initializer");
                }
                (arg as usize, OpCode::GetLocal, OpCode::SetLocal, false)
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
            TokenType::LeftParen => ParseRule(Some(Self::grouping), None, Precedence::None),
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

pub fn compile(src: &str) -> Result<Chunk, InterpretError> {
    let mut compiler = Compiler::new(src);

    compiler.advance();
    while !compiler.is_at_end() {
        compiler.declaration();
    }
    compiler.emit_opcode(OpCode::Return);

    if compiler.failed() {
        Err(InterpretError::CompileError)
    } else {
        super::debug::disassemble(&compiler.chunk, "code");
        Ok(compiler.chunk)
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
