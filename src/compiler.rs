use super::InterpretError;
use super::opcodes::OpCode;
use super::scanner::{Scanner, Token, TokenType};
use super::chunk::{Chunk, Constant};

struct Compiler {
    scanner: Scanner,
    previous: Token,
    current: Token,
    failed: bool,
    panic_mode: bool,
    chunk: Chunk,
}

impl Compiler {
    fn new(src: &str) -> Compiler {
        Compiler {
            scanner: Scanner::new(src),
            chunk: Chunk::new(),
            previous: Token {token_type: TokenType::Error, lexeme: String::new(), line: 0},
            current: Token {token_type: TokenType::Error, lexeme: String::new(), line: 0},
            failed: false,
            panic_mode: false,
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

    fn emit_constant(&mut self, constant: Constant) -> usize {
        let i = self.chunk.write_constant(constant, self.previous.line);
        if let Ok(i) = i {
            i
        } else {
            self.error("Too many constants in one chunk");
            0
        }
    }

    // parser
    fn declaration(&mut self) {
        self.statement();

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_advance(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
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

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }
    
    fn binary(&mut self) {
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
            _ => unreachable!()
        }
    }
    
    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expected ')' after expression");
    }
    
    fn unary(&mut self) {
        let token_type = self.previous.token_type;
        self.parse_precedence(Precedence::Unary); // operand
        match token_type {
            TokenType::Minus => self.emit_opcode(OpCode::Negate),
            TokenType::Bang => self.emit_opcode(OpCode::Not),
            _ => unreachable!()
        }
    }
    
    fn number(&mut self) {
        let value = self.previous.lexeme.parse::<f64>().unwrap_or_default();
        let value = Constant::Number(value);
        self.emit_constant(value);
    }

    fn string(&mut self) {
        let len = self.previous.lexeme.len();
        let value = self.previous.lexeme.chars().skip(1).take(len-2).collect();
        let value = Constant::String(value);
        self.emit_constant(value);
    }

    fn literal(&mut self) {
        match self.previous.token_type {
            TokenType::False => self.emit_opcode(OpCode::False),
            TokenType::True => self.emit_opcode(OpCode::True),
            TokenType::Nil => self.emit_opcode(OpCode::Nil),
            _ => unreachable!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        
        let prefix = Self::get_rule(self.previous.token_type).0;
        if let Some(prefix) = prefix {
            prefix(self);
        } else {
            self.error("Expected expression");
            return;
        }

        while precedence as isize <= Self::get_rule(self.current.token_type).2 as isize {
            self.advance();
            let infix = Self::get_rule(self.previous.token_type).1;
            if let Some(infix) = infix {
                infix(self);
            }
        }
    }
    
    fn get_rule(token_type: TokenType) -> ParseRule {
        match token_type {
            // ParseRule(prefix, infix, precedence)
            TokenType::LeftParen    => ParseRule(Some(Self::grouping), None, Precedence::None),
            TokenType::Minus        => ParseRule(Some(Self::unary), Some(Self::binary), Precedence::Term),
            TokenType::Plus         => ParseRule(None, Some(Self::binary), Precedence::Term),
            TokenType::Slash        => ParseRule(None, Some(Self::binary), Precedence::Factor),
            TokenType::Star         => ParseRule(None, Some(Self::binary), Precedence::Factor),
            TokenType::Number       => ParseRule(Some(Self::number), None, Precedence::None),
            TokenType::Nil          => ParseRule(Some(Self::literal), None, Precedence::None),
            TokenType::False        => ParseRule(Some(Self::literal), None, Precedence::None),
            TokenType::True         => ParseRule(Some(Self::literal), None, Precedence::None),
            TokenType::Bang         => ParseRule(Some(Self::unary), None, Precedence::None),
            TokenType::EqualEqual   => ParseRule(None, Some(Self::binary), Precedence::Equality),
            TokenType::BangEqual    => ParseRule(None, Some(Self::binary), Precedence::Equality),
            TokenType::LessEqual    => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::GreaterEqual => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::Greater      => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::Less         => ParseRule(None, Some(Self::binary), Precedence::Comparison),
            TokenType::String       => ParseRule(Some(Self::string), None, Precedence::None),
            _                       => ParseRule(None, None, Precedence::None),
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
                TokenType::Class | TokenType::Fun | TokenType::Var
                | TokenType::For | TokenType::If | TokenType::While
                | TokenType::Print | TokenType::Return => return,
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

type ParseFn = fn(&mut Compiler) -> ();
struct ParseRule(Option<ParseFn>, Option<ParseFn>, Precedence);