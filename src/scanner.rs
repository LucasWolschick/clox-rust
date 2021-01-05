pub struct Scanner {
    src: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(src: &str) -> Self {
        Scanner {
            src: src.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;
        if self.is_at_end() {
            return self.token(TokenType::Eof);
        }

        let char = self.advance();
        match char {
            '(' => self.token(TokenType::LeftParen),
            ')' => self.token(TokenType::RightParen),
            '{' => self.token(TokenType::LeftBrace),
            '}' => self.token(TokenType::RightBrace),
            ';' => self.token(TokenType::Semicolon),
            ',' => self.token(TokenType::Comma),
            '.' => self.token(TokenType::Dot),
            '-' => self.token(TokenType::Minus),
            '+' => self.token(TokenType::Plus),
            '/' => self.token(TokenType::Slash),
            '*' => self.token(TokenType::Star),
            '!' => if self.match_advance('=') {
                self.token(TokenType::BangEqual)
            } else {
                self.token(TokenType::Bang)
            }
            '=' => if self.match_advance('=') {
                self.token(TokenType::EqualEqual)
            } else {
                self.token(TokenType::Equal)
            }
            '>' => if self.match_advance('=') {
                self.token(TokenType::GreaterEqual)
            } else {
                self.token(TokenType::Greater)
            }
            '<' => if self.match_advance('=') {
                self.token(TokenType::LessEqual)
            } else {
                self.token(TokenType::Less)
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => self.error_token("Unexpected character")
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn peek(&self) -> char {
        if self.current > self.src.len() - 1 {
            '\0'
        } else {
            self.src[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.src[self.current + 1]
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.src[self.current-1]
    }

    fn match_advance(&mut self, char: char) -> bool {
        if self.is_at_end() || self.peek() != char {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            if c == '\n' {
                self.line += 1;
                self.advance();
            } else if c == '/' {
                if self.peek_next() == '/' {
                    // ignore comments until the end of the line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    // meaningful token
                    return;
                }
            } else if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error_token("Unterminated string")
        } else {
            self.advance();
            self.token(TokenType::String)
        }
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_alpha(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn identifier(&mut self) -> Token {
        while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
            self.advance();
        }

        self.token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        let s: String = self.src[self.start..self.current].iter().collect();
        match s.as_str() {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        }
    }
    
    fn number(&mut self) -> Token {
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && Self::is_digit(self.peek_next()) {
            self.advance();
            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.token(TokenType::Number)
    }

    fn token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: self.src[self.start..self.current].iter().collect(),
            line: self.line,
        }
    }

    fn error_token(&self, msg: &str) -> Token {
        Token {
            token_type: TokenType::Error,
            lexeme: msg.into(),
            line: self.line,
        }
    }
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

#[derive(Clone, Copy, PartialEq)]
pub enum TokenType {
    // 1 char
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // 1-2 chars
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual, Less, LessEqual,

    // literals
    Identifier, String, Number,

    // keywords
    And, Class, Else, False, For, Fun, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Error, Eof,
}