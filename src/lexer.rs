use crate::tokens::Token;

pub fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

fn is_numeric(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_alphanumeric(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || is_numeric(c) || c == '_'
}

#[derive(Debug)]
pub struct Lexer {
    utf8: Vec<char>,
    curr: usize,
    peek: usize,
    pub ch: char,
}

impl Lexer {
    pub fn new(buffer: &str) -> Lexer {
        let mut lexer = Lexer {
            utf8: buffer.chars().collect(),
            curr: 0,
            peek: 0,
            ch: '\0',
        };

        lexer.read();
        lexer
    }

    /// Read the next character, or break if we have reached end of input.
    pub fn read(&mut self) {
        if self.peek >= self.utf8.len() {
            self.ch = '\0'
        } else {
            self.ch = self.utf8[self.peek]
        }

        self.curr = self.peek;
        self.peek += 1;
    }

    pub fn read_peek_char(&mut self) -> char {
        self.utf8[self.peek]
    }

    pub fn read_number(&mut self) -> Token {
        let current = self.curr;
        while is_numeric(self.ch) {
            self.read();
        }

        if self.ch == '.' && is_numeric(self.read_peek_char()) {
            self.read();
            while is_numeric(self.ch) {
                self.read();
            }
            let literal = self.utf8[current..self.curr].iter().collect::<String>();
            return Token::Float(literal);
        }

        Token::Int(self.utf8[current..self.curr].iter().collect::<String>())
    }

    pub fn read_identifier(&mut self) -> Token {
        let current = self.curr;
        loop {
            if is_alphanumeric(self.ch) {
                self.read();
            } else {
                break;
            }
        }
        let ident = self.utf8[current..self.curr].iter().collect::<String>();
        match ident.as_str() {
            "fun" => Token::Fun,
            "let" => Token::Let,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "type" => Token::Type,
            "match" => Token::Match,
            "with" => Token::With,
            "of" => Token::Of,
            "return" => Token::Return,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Identifier(ident),
        }
    }

    pub fn read_type_variable(&mut self) -> Token {
        self.read();
        let current = self.curr;
        loop {
            if self.ch.is_alphabetic() {
                self.read();
            } else {
                break;
            }
        }

        Token::TypeVariable(self.utf8[current..self.curr].iter().collect::<String>())
    }

    pub fn read_string(&mut self) -> Token {
        self.read();
        let mut result = String::new();
        while self.ch != '\0' && self.ch != '"' {
            if self.ch == '\\' {
                self.read();
                match self.ch {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    _ => result.push(self.ch),
                }
            } else {
                result.push(self.ch);
            }
            self.read();
        }

        if self.ch == '"' {
            self.read();
            Token::String(result)
        } else {
            Token::Illegal
        }
    }

    pub fn read_comment(&mut self) -> Token {
        self.read();
        let mut result = String::new();
        while self.ch != '\0' {
            if self.ch == '\\' {
                self.read();
                match self.ch {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    _ => result.push(self.ch),
                }
            } else if self.ch == '*' && self.read_peek_char() == ')' {
                self.read();
                self.read();
                return Token::Comment(String::from(result.trim_start().trim_end()));
            } else {
                result.push(self.ch);
            }
            self.read();
        }
        Token::Illegal
    }

    pub fn advance(&mut self) -> Token {
        loop {
            if is_whitespace(self.ch) {
                self.read();
            } else {
                break;
            }
        }

        let token: Token = match self.ch {
            '=' => {
                if self.read_peek_char() == '=' {
                    self.read();
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            '!' => {
                if self.read_peek_char() == '=' {
                    self.read();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '+' => {
                if self.read_peek_char() == '+' {
                    self.read();
                    Token::PlusPlus
                } else {
                    Token::Plus
                }
            }
            ';' => {
                if self.read_peek_char() == ';' {
                    self.read();
                    Token::SemiColonSemiColon
                } else {
                    Token::SemiColon
                }
            }
            ':' => {
                if self.read_peek_char() == ':' {
                    self.read();
                    Token::ColonColon
                } else {
                    Token::Colon
                }
            }
            '.' => {
                if self.read_peek_char() == '.' {
                    self.read();
                    Token::PeriodPeriod
                } else {
                    Token::Period
                }
            }
            '>' => {
                if self.read_peek_char() == '=' {
                    self.read();
                    Token::GreaterThanOrEqual
                } else {
                    Token::GreaterThan
                }
            }
            '-' => {
                if self.read_peek_char() == '>' {
                    self.read();
                    Token::RightArrow
                } else {
                    Token::Minus
                }
            }
            '<' => {
                if self.read_peek_char() == '=' {
                    self.read();
                    Token::LessThanOrEqual
                } else if self.read_peek_char() == '-' {
                    self.read();
                    Token::LeftArrow
                } else {
                    Token::LessThan
                }
            }
            '(' => {
                if self.read_peek_char() == ')' {
                    self.read();
                    Token::Unit
                } else if self.read_peek_char() == '*' {
                    self.read();
                    return self.read_comment();
                } else {
                    Token::LeftParen
                }
            }
            '*' => {
                if self.read_peek_char() == '*' {
                    self.read();
                    Token::AsteriskAsterisk
                } else {
                    Token::Asterisk
                }
            }
            '~' => Token::Tilde,
            '%' => Token::Percent,
            '?' => Token::QuestionMark,
            '&' => Token::Ampersand,
            '^' => Token::Caret,
            '|' => Token::VerticalBar,
            '/' => Token::BackwardSlash,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            '"' => return self.read_string(),
            '0'..='9' => return self.read_number(),
            'a'..='z' | 'A'..='Z' => return self.read_identifier(),
            '\'' => return self.read_type_variable(),
            '\\' => Token::ForwardSlash,
            '\0' => Token::EOF,
            _ => Token::Illegal,
        };

        self.read();
        token
    }
}
