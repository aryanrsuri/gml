use crate::{
    ast::{Identifier, Program, Statement},
    lexer::Lexer,
    tokens::Token,
};

pub enum Error {
    TokenMismatch(String),
    TokenDoesNotExist(String),
}
pub struct Parser {
    pub lexer: Lexer,
    pub curr: Token,
    peek: Token,
    errors: Vec<Error>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer,
            curr: Token::Illegal,
            peek: Token::Illegal,
            errors: vec![],
        };

        p.advance();
        p.advance();
        p
    }

    pub fn advance(&mut self) {
        self.curr = self.peek.clone();
        self.peek = self.lexer.advance();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = vec![];
        while self.curr != Token::EOF {
            if let Some(statement) = self.parse_statement() {
                program.push(statement);
            } else if !self.errors.is_empty() {
            }
            self.advance();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Comment(s) => Some(Statement::Comment(Identifier(s))),
            _ => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        None
    }
}
