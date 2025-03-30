use crate::{
    ast::{Expression, Identifier, Literal, Program, Statement, TypeAnnotation},
    lexer::Lexer,
    tokens::Token,
};

#[derive(Debug)]
pub enum Precendence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Cons,
    Prefix,
    BitwiseOperation,
    Apply,
}

fn token_to_precendence(token: &Token) -> Precendence {
    match token {
        Token::Equal | Token::NotEqual => Precendence::Equals,
        Token::LessThan
        | Token::GreaterThan
        | Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => Precendence::LessGreater,
        Token::Plus | Token::Minus => Precendence::Sum,
        Token::Caret | Token::Percent | Token::Asterisk | Token::ForwardSlash => {
            Precendence::Product
        }
        Token::Ampersand => Precendence::BitwiseOperation,
        Token::Identifier(_) => Precendence::Apply,
        _ => Precendence::Lowest,
    }
}

#[derive(Debug)]
pub enum Error {
    Unimplemented(String),
    InvalidTypeAnnotation(String),
    PrefixDoesNotExistFor(Token),
    TokenCurrMismatch { want: Token, got: Token },
    TokenPeekMismatch { want: Token, got: Token },
    TokenDoesNotExist(String),
}

pub struct Parser {
    lexer: Lexer,
    curr: Token,
    peek: Token,
    pub errors: Vec<Error>,
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

    pub fn if_peek_advance(&mut self, expected: Token) -> bool {
        if self.peek == expected {
            self.advance();
            true
        } else {
            self.errors.push(Error::TokenPeekMismatch {
                want: expected,
                got: self.peek.clone(),
            });
            false
        }
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
            }
            self.advance();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Comment(s) => Some(Statement::Comment(Identifier(s))),
            _ => {
                self.errors.push(Error::Unimplemented(format!(
                    "Unimplemented Statement, likely a syntax error. Curr: {:?} Peek: {:?}",
                    self.curr, self.peek
                )));
                None
            }
        }
    }

    pub fn parse_identifier(&mut self) -> Option<Identifier> {
        match self.curr.clone() {
            Token::Identifier(s) => Some(Identifier(s)),
            _ => {
                self.errors.push(Error::TokenCurrMismatch {
                    want: Token::Identifier(String::from("any")),
                    got: self.curr.clone(),
                });
                None
            }
        }
    }

    pub fn parse_type_annotation(&mut self) -> Option<TypeAnnotation> {
        match self.curr.clone() {
            Token::Identifier(s) => {
                let annotation = match s.as_str() {
                    "int" => TypeAnnotation::Int,
                    "float" => TypeAnnotation::Float,
                    "string" => TypeAnnotation::String,
                    "bool" => TypeAnnotation::Bool,
                    "unit" => TypeAnnotation::Unit,
                    _ => {
                        // FIXME: Need to implement Named, Product, Fun, and Tuple annotations
                        self.errors.push(Error::InvalidTypeAnnotation(s));
                        return None;
                    }
                };
                Some(annotation)
            }
            _ => {
                self.errors.push(Error::TokenCurrMismatch {
                    want: Token::Identifier(String::from("Type Annotation")),
                    got: self.curr.clone(),
                });
                None
            }
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        match self.peek {
            Token::Identifier(_) => self.advance(),
            _ => return None,
        }
        let identifier = match self.parse_identifier() {
            Some(identifier) => identifier,
            None => return None,
        };

        if !self.if_peek_advance(Token::Colon) {
            return None;
        }
        self.advance();
        let annotation = match self.parse_type_annotation() {
            Some(annotation) => annotation,
            None => return None,
        };
        if !self.if_peek_advance(Token::Equal) {
            return None;
        }
        self.advance();
        // FIXME: Obviously, None should return None, this is just to ignore parse_expression while
        // its under development
        let expression = match self.parse_expression(Precendence::Lowest) {
            Some(expression) => expression,
            None => return None,
        };
        if !self.if_peek_advance(Token::SemiColonSemiColon) {
            return None;
        }
        // println!(
        //     "curr {:?} peek {:?} identifier {:?} annotation {:?}",
        //     self.curr, self.peek, identifier, annotation
        // );
        //
        Some(Statement::Let(identifier, annotation, expression))
    }
    fn parse_literal_expression(&mut self) -> Option<Expression> {
        match &self.curr {
            Token::Int(s) => match s.parse::<i64>() {
                Ok(d) => Some(Expression::Literal(Literal::Int(d))),
                Err(_) => None,
            },
            Token::Float(s) => match s.parse::<f64>() {
                Ok(d) => Some(Expression::Literal(Literal::Float(d))),
                Err(_) => None,
            },
            Token::String(s) => Some(Expression::Literal(Literal::String(s.clone()))),
            Token::Bool(b) => Some(Expression::Literal(Literal::Bool(*b))),
            Token::Unit => Some(Expression::Literal(Literal::Unit)),
            _ => None,
        }
    }

    pub fn parse_expression(&mut self, precendence: Precendence) -> Option<Expression> {
        let mut first = match self.curr.clone() {
            Token::Identifier(s) => Expression::Identifier(Identifier(s)),
            Token::Int(_) | Token::String(_) | Token::Float(_) | Token::Bool(_) | Token::Unit => {
                match self.parse_literal_expression() {
                    Some(literal) => literal,
                    None => return None,
                }
            }
            _ => {
                self.errors
                    .push(Error::PrefixDoesNotExistFor(self.curr.clone()));
                return None;
            }
        };

        Some(first)
    }
}
