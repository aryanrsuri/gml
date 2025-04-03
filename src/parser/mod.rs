use crate::{
    ast::{
        Expression, FunParam, Identifier, Infix, Literal, Prefix, Program, Statement,
        TypeAnnotation, TypeDefinition, UnionVariant,
    },
    lexer::Lexer,
    tokens::Token,
};

pub fn is_capitalized(s: &str) -> bool {
    s.chars().next().unwrap().is_uppercase()
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Sequence, // ;
    Equals,
    And,
    LessGreater,
    Sum,
    Product,
    Cons,
    Prefix,
    Apply, // f x
    Index, // a.[i] or record.field
}

fn token_to_precendence(token: &Token) -> Precedence {
    match token {
        Token::SemiColon => Precedence::Sequence,
        Token::Equal | Token::NotEqual => Precedence::Equals,
        Token::LessThan
        | Token::GreaterThan
        | Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => Precedence::LessGreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::AsteriskAsterisk
        | Token::Caret
        | Token::Percent
        | Token::Asterisk
        | Token::ForwardSlash => Precedence::Product,
        Token::ColonColon | Token::PlusPlus => Precedence::Cons,
        Token::LeftParen
        | Token::Identifier(_)
        | Token::Int(_)
        | Token::Float(_)
        | Token::String(_)
        | Token::Bool(_)
        | Token::Unit
        | Token::LeftBracket
        | Token::LeftBrace
        | Token::If
        | Token::Fun => Precedence::Apply,
        _ => Precedence::Lowest,
    }
}

#[derive(Debug)]
pub enum Error {
    Unimplemented(String),
    UnexpectedToken(Token),
    InvalidTypeAnnotation(String),
    PrefixDoesNotExistFor(Token),
    InfixDoesNotExistFor(Token),
    TokenCurrMismatch { want: Token, got: Token },
    TokenPeekMismatch { want: Token, got: Token },
    TokenExpected { want: Token, got: Token },
    CouldNotParseLiteral(String, String),
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

    pub fn is_current(&mut self, expected: Token) -> bool {
        if self.curr == expected {
            true
        } else {
            self.errors.push(Error::TokenCurrMismatch {
                want: expected,
                got: self.curr.clone(),
            });
            false
        }
    }

    pub fn advance(&mut self) {
        self.curr = self.peek.clone();
        self.peek = self.lexer.advance();
        while matches!(self.curr, Token::Comment(_)) {
            self.curr = self.peek.clone();
            self.peek = self.lexer.advance();
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program: Program = vec![];
        while self.curr != Token::EOF {
            match self.parse_statement() {
                Some(statement) => program.push(statement),
                None => {
                    if self.curr != Token::EOF {
                        self.advance();
                    }
                }
            }

            if self.curr == Token::SemiColonSemiColon {
                self.advance();
            } else if self.curr != Token::EOF {
            }
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Type => self.parse_type_statement(),
            Token::Return => self.parse_return_statement(),
            _ => match self.parse_expression(Precedence::Lowest) {
                Some(expression) => {
                    self.advance();
                    Some(Statement::Expression(expression))
                }
                None => {
                    if self.errors.is_empty()
                        || !matches!(self.errors.last(), Some(Error::PrefixDoesNotExistFor(_)))
                    {
                        self.errors.push(Error::UnexpectedToken(self.curr.clone()));
                    }
                    self.advance();
                    None
                }
            },
        }
    }

    pub fn parse_return_statement(&mut self) -> Option<Statement> {
        self.advance();
        let expression = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };
        Some(Statement::Return(expression))
    }

    pub fn parse_identifier(&mut self) -> Option<Identifier> {
        match &self.curr {
            Token::Identifier(s) => Some(Identifier(s.clone())),
            _ => {
                self.errors.push(Error::TokenCurrMismatch {
                    want: Token::Identifier(String::from("Identifier")),
                    got: self.curr.clone(),
                });
                None
            }
        }
    }

    pub fn parse_type_annotation(&mut self) -> Option<TypeAnnotation> {
        let first_component = match self.parse_primitive_type_annotation() {
            Some(t) => t,
            None => return None,
        };

        // Handle n-ary tuples with asterisk
        if self.peek == Token::Asterisk {
            let mut components = vec![first_component];
            while self.peek == Token::Asterisk {
                self.advance(); // Consume '*'
                self.advance(); // Move to next type
                let next_component = match self.parse_type_annotation() {
                    Some(t) => t,
                    None => return None,
                };
                components.push(next_component);
            }
            return Some(TypeAnnotation::Tuple(components));
        }

        // Handle function types
        if self.peek == Token::RightArrow {
            let mut params: Vec<TypeAnnotation> = vec![first_component];

            while self.peek == Token::RightArrow {
                self.advance();
                self.advance();
                let next_component = match self.parse_type_annotation() {
                    Some(t) => t,
                    None => return None,
                };
                params.push(next_component);
            }

            if params.len() < 2 {
                self.errors.push(Error::InvalidTypeAnnotation(
                    "Function type requires at least two parts separated by ->".to_string(),
                ));
                return None;
            }
            let return_type = params.pop().unwrap();
            return Some(TypeAnnotation::Fun(params, Box::new(return_type)));
        }

        Some(first_component)
    }

    pub fn parse_primitive_type_annotation(&mut self) -> Option<TypeAnnotation> {
        match self.curr.clone() {
            Token::Identifier(s) => {
                let annotation = match s.as_str() {
                    "int" => TypeAnnotation::Int,
                    "float" => TypeAnnotation::Float,
                    "string" => TypeAnnotation::String,
                    "bool" => TypeAnnotation::Bool,
                    "unit" => TypeAnnotation::Unit,
                    "list" => {
                        if self.peek == Token::LessThan {
                            self.advance(); // Consume '<'
                            self.advance(); // Move to next type
                            let element_type = match self.parse_type_annotation() {
                                Some(t) => t,
                                None => return None,
                            };

                            self.advance();
                            if !self.is_current(Token::GreaterThan) {
                                return None;
                            }
                            return Some(TypeAnnotation::Product(
                                Box::new(TypeAnnotation::List),
                                Box::new(element_type),
                            ));
                        } else {
                            self.errors.push(Error::TokenExpected {
                                want: Token::TypeVariable(String::from("Type Variable")),
                                got: self.curr.clone(),
                            });
                            return None;
                        }
                    }
                    _ => {
                        // First check if its a Product type, e.g. linked_list<int>
                        // If not, then it must be a Named type, e.g. linked_list
                        if self.peek == Token::LessThan {
                            self.advance();
                            self.advance();
                            let element_type = self.parse_type_annotation()?;
                            self.advance();
                            TypeAnnotation::Product(
                                Box::new(TypeAnnotation::Named(Identifier(s))),
                                Box::new(element_type),
                            )
                        } else {
                            TypeAnnotation::Named(Identifier(s))
                        }
                    }
                };
                Some(annotation)
            }
            Token::TypeVariable(v) => Some(TypeAnnotation::Var(Identifier(v))),
            Token::LeftParen => {
                self.advance();
                let annotation = self.parse_type_annotation()?;
                self.advance();
                if !self.is_current(Token::RightParen) {
                    return None;
                }
                Some(annotation)
            }
            _ => {
                self.errors.push(Error::TokenCurrMismatch {
                    want: Token::Identifier(String::from("Type Annotation (e.g int, string)")),
                    got: self.curr.clone(),
                });
                None
            }
        }
    }

    fn parse_type_statement(&mut self) -> Option<Statement> {
        self.advance();
        let params = self.parse_optional_type_parameters()?; // Handles 'a or ('a, 'b)
        let name = match self.parse_identifier() {
            Some(id) => id,
            None => return None, // Error already logged
        };
        self.advance();

        if !self.is_current(Token::Equal) {
            return None;
        }

        self.advance(); // Consume '='
        let type_definition = match &self.curr {
            Token::LeftBrace => self.parse_record_definition(name, params)?,
            Token::VerticalBar => self.parse_union_definition(name, params)?,
            Token::Identifier(s) if is_capitalized(s) => {
                self.parse_union_definition(name, params)?
            }
            _ => self.parse_alias_definition(name, params)?,
        };

        if !self.is_current(Token::SemiColonSemiColon) {
            return None;
        }
        Some(Statement::Type(type_definition))
    }

    fn parse_optional_type_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut params = vec![];
        match self.curr.clone() {
            Token::TypeVariable(i) => {
                self.advance();
                params.push(Identifier(i));
            }
            Token::LeftParen => {
                self.advance(); // Consume '('
                loop {
                    match self.curr.clone() {
                        Token::TypeVariable(i) => {
                            self.advance(); // Consume type variable
                            params.push(Identifier(i));
                            match self.curr {
                                Token::Comma => {
                                    self.advance(); // Consume ',' and continue loop
                                }
                                Token::RightParen => {
                                    self.advance(); // Consume ')' and break
                                    break;
                                }
                                _ => {
                                    self.errors.push(Error::TokenExpected {
                                        want: Token::Comma, // Or RParen implicitly
                                        got: self.curr.clone(),
                                    });
                                    return None;
                                }
                            }
                        }
                        Token::RightParen => {
                            self.advance(); // Consume ')' and break
                            break;
                        }
                        _ => {
                            self.errors.push(Error::TokenExpected {
                                want: Token::TypeVariable("type variable".to_string()),
                                got: self.curr.clone(),
                            });
                            return None;
                        }
                    }
                }
            }
            _ => {}
        }
        Some(params)
    }

    fn parse_record_definition(
        &mut self,
        name: Identifier,
        params: Vec<Identifier>,
    ) -> Option<TypeDefinition> {
        // Current token is '{'
        self.advance(); // Consume '{'

        let mut fields = vec![];

        // Handle empty record {}
        if self.curr == Token::RightBrace {
            self.advance(); // Consume '}'
            return Some(TypeDefinition::Record {
                name,
                params,
                fields,
            });
        }

        // Parse fields loop
        loop {
            let field_name = self.parse_identifier()?;
            self.advance(); // Consume field name

            if !self.is_current(Token::Colon) {
                return None;
            }
            self.advance(); // Consume ':'

            let field_type = self.parse_type_annotation()?;
            self.advance(); // Consume last token of type annotation

            fields.push((field_name, field_type));

            match self.curr {
                Token::SemiColon => {
                    self.advance(); // Consume ';'
                    // Check if the next token is '}', allows trailing semicolon
                    if self.curr == Token::RightBrace {
                        self.advance(); // Consume '}'
                        break;
                    }
                    // Otherwise, continue loop for next field
                }
                Token::RightBrace => {
                    self.advance(); // Consume '}'
                    break;
                }
                _ => {
                    self.errors.push(Error::TokenExpected {
                        want: Token::SemiColon, // or RBrace implicitly
                        got: self.curr.clone(),
                    });
                    return None;
                }
            }
        }

        Some(TypeDefinition::Record {
            name,
            params,
            fields,
        })
    }

    /// Parses a union definition: [|] Variant1 [of Type1] | Variant2 [of Type2] ...
    /// Assumes current token is '|' or an Uppercase Identifier.
    fn parse_union_definition(
        &mut self,
        name: Identifier,
        params: Vec<Identifier>,
    ) -> Option<TypeDefinition> {
        let mut variants = vec![];
        loop {
            // First VerticalBar should be options
            if self.curr == Token::VerticalBar {
                self.advance();
            }

            let tag = match &self.curr {
                Token::Identifier(s) if is_capitalized(s) => Identifier(s.clone()),
                _ => {
                    self.errors.push(Error::TokenExpected {
                        want: Token::Identifier("Uppercase Variant Tag".to_string()),
                        got: self.curr.clone(),
                    });
                    return None;
                }
            };
            self.advance(); // Consume variant tag identifier
            let mut types = vec![];
            if self.curr == Token::Of {
                self.advance(); // Consume 'of'
                let associated_type = self.parse_type_annotation()?;
                self.advance(); // Consume last token of type annotation

                types.push(associated_type);
                // --- End Simplification ---

                // --- Code for parsing multiple types separated by '*' ---
                /*
                loop {
                    let associated_type = self.parse_type_annotation()?; // Or a specific function that parses until '*' or non-type
                    self.advance(); // Consume last token of type
                    types.push(associated_type);

                    if self.curr == Token::Asterisk { // Assuming '*' separates types
                        self.advance(); // Consume '*' and parse next type
                    } else {
                        break; // No more types for this variant
                    }
                }
                */
            }

            variants.push(UnionVariant { tag, types });

            if self.curr != Token::VerticalBar {
                break;
            }
        }

        Some(TypeDefinition::Union {
            name,
            params,
            variants,
        })
    }

    /// Parses an alias definition: existing_type_expression
    /// Assumes current token is the start of the type expression.
    fn parse_alias_definition(
        &mut self,
        name: Identifier,
        params: Vec<Identifier>,
    ) -> Option<TypeDefinition> {
        let target = self.parse_type_annotation()?;
        // Advance past the type annotation tokens
        // parse_type_annotation should ideally leave self.curr on the *last* token of the type.
        self.advance(); // Consume last token of type annotation

        Some(TypeDefinition::Alias {
            name,
            params,
            target,
        })
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        match self.peek {
            Token::Identifier(_) => self.advance(),
            _ => return None,
        }
        let name = match self.parse_identifier() {
            Some(identifier) => identifier,
            None => return None,
        };

        self.advance();
        match self.curr {
            Token::Identifier(_) | Token::LeftParen => self.parse_function_let_binding(name),
            Token::Colon | Token::Equal => self.parse_simple_let_binding(name),
            _ => {
                self.errors.push(Error::TokenExpected {
                    want: Token::Colon,
                    got: self.curr.clone(),
                });
                None
            }
        }
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

    pub fn parse_function_parameter(&mut self) -> Option<FunParam> {
        // x | (x: int)

        match self.curr.clone() {
            Token::Identifier(id) => Some(FunParam {
                name: Identifier(id),
                annotation: None,
            }),

            Token::LeftParen => {
                self.advance();
                let name = match self.parse_identifier() {
                    Some(id) => id,
                    None => return None,
                };
                self.advance();
                if !self.is_current(Token::Colon) {
                    return None;
                }
                self.advance();
                let annotation = match self.parse_type_annotation() {
                    Some(a) => a,
                    None => return None,
                };
                self.advance();
                if !self.is_current(Token::RightParen) {
                    return None;
                }
                Some(FunParam {
                    name,
                    annotation: Some(annotation),
                })
            }

            _ => {
                self.errors.push(Error::TokenExpected {
                    want: Token::Identifier(String::from("Expected parameter")),
                    got: self.curr.clone(),
                });
                None
            }
        }
    }

    pub fn parse_function_let_binding(&mut self, name: Identifier) -> Option<Statement> {
        let mut params: Vec<FunParam> = vec![];
        while matches!(self.curr, Token::Identifier(_) | Token::LeftParen) {
            match self.parse_function_parameter() {
                Some(param) => params.push(param),
                None => return None,
            }
            self.advance();
        }
        let return_annotation = if self.curr == Token::Colon {
            self.advance();
            let annotation = self.parse_type_annotation()?;
            self.advance();
            Some(annotation)
        } else {
            None
        };

        if !self.is_current(Token::Equal) {
            return None;
        }
        self.advance();
        let body = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => Box::new(expression),
            None => return None,
        };

        if !self.if_peek_advance(Token::SemiColonSemiColon) {
            return None;
        }

        let value = Expression::Fun { params, body };
        Some(Statement::Let {
            name,
            annotation: return_annotation,
            value,
        })
    }

    pub fn parse_simple_let_binding(&mut self, name: Identifier) -> Option<Statement> {
        let annotation = if self.curr == Token::Colon {
            self.advance();
            let a = self.parse_type_annotation()?;
            self.advance();
            Some(a)
        } else {
            None
        };

        if !self.is_current(Token::Equal) {
            return None;
        }
        self.advance();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return None,
        };
        if !self.if_peek_advance(Token::SemiColonSemiColon) {
            return None;
        }
        Some(Statement::Let {
            name,
            annotation,
            value,
        })
    }

    pub fn peek_precedence(&mut self) -> Precedence {
        token_to_precendence(&self.peek)
    }

    pub fn curr_precedence(&mut self) -> Precedence {
        token_to_precendence(&self.curr)
    }

    pub fn parse_expression(&mut self, precendence: Precedence) -> Option<Expression> {
        let mut left_expr = match self.curr.clone() {
            Token::Identifier(s) => Expression::Identifier(Identifier(s)),
            Token::Int(_) | Token::String(_) | Token::Float(_) | Token::Bool(_) | Token::Unit => {
                match self.parse_literal_expression() {
                    Some(literal) => literal,
                    None => return None,
                }
            }
            Token::If => self.parse_if_then_expression()?,
            Token::Fun => self.parse_function_expression()?,
            Token::LeftBracket => self.parse_list_expression()?,
            Token::LeftParen => self.parse_grouped_expression()?,
            Token::LeftBrace => self.parse_record_expression()?,
            Token::Bang | Token::Minus | Token::Tilde => self.parse_prefix_expression()?,
            _ => {
                self.errors
                    .push(Error::PrefixDoesNotExistFor(self.curr.clone()));
                return None;
            }
        };

        while self.peek != Token::SemiColon && precendence < self.peek_precedence() {
            match self.peek {
                // Standard binary operators
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::AsteriskAsterisk
                | Token::ForwardSlash
                | Token::Caret
                | Token::Percent
                | Token::EqualEqual
                | Token::NotEqual
                | Token::LessThan
                | Token::GreaterThan
                | Token::LessThanOrEqual
                | Token::GreaterThanOrEqual
                | Token::PlusPlus
                | Token::ColonColon
                | Token::SemiColon => {
                    self.advance(); // Consume the operator
                    left_expr = match self.parse_infix_expression(left_expr) {
                        Some(expr) => expr,
                        None => return None,
                    };
                }

                Token::Identifier(_)
                | Token::Int(_)
                | Token::Float(_)
                | Token::String(_)
                | Token::Bool(_)
                | Token::Unit
                | Token::LeftParen
                | Token::LeftBracket
                | Token::LeftBrace
                | Token::If
                | Token::Fun => {
                    left_expr = match self.parse_apply_expression(left_expr) {
                        Some(expr) => expr,
                        None => return None,
                    };
                }
                _ => {
                    return Some(left_expr);
                }
            }
        }

        Some(left_expr)
    }

    fn parse_record_expression(&mut self) -> Option<Expression> {
        self.advance();
        let mut fields = vec![];
        if self.curr == Token::RightBrace {
            return Some(Expression::Record(fields));

        }
        loop {
            let field_name = match self.parse_identifier() {
                Some(id) => id,
                None => return None,
            };
            self.advance();
            if !self.is_current(Token::Equal) { return None}
            self.advance();

            let field_value = match self.parse_expression(Precedence::Lowest) {
                Some(expr) => expr,
                None => return None,
            };
            fields.push((field_name, field_value));
            self.advance();
            match self.curr {
                Token::SemiColon => {
                    self.advance();
                    if self.curr == Token::RightBrace {
                        break;
                    }
                }
                Token::RightBrace => {
                    break;
                }
                _ => {
                    self.errors.push(Error::TokenExpected {
                        want: Token::SemiColon,
                        got: self.curr.clone(),
                    });
                    return None;
                }
            }
        }
        Some(Expression::Record(fields))
    }

    fn parse_list_expression(&mut self) -> Option<Expression> {
        self.advance();
        if self.curr == Token::RightBracket {
            return Some(Expression::List(vec![]));
        }
        let first = match self.parse_expression(Precedence::Lowest) {
            Some(expression) => expression,
            None => return None,
        };
        let mut elements = vec![first];
        self.advance();
        while self.curr == Token::SemiColon {
            self.advance();
            let next = match self.parse_expression(Precedence::Sequence) {
                Some(expression) => expression,
                None => return None,
            };
            elements.push(next);
            self.advance();
        }
        if !self.is_current(Token::RightBracket) {
            return None;
        }
        if self.peek != Token::SemiColonSemiColon {
            return None;
        }
        Some(Expression::List(elements))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.advance();
        let expr = self.parse_expression(Precedence::Lowest)?;
        match self.peek {
            Token::RightParen | Token::Comma | Token::SemiColon => self.advance(),
            _ => {
                self.errors.push(Error::TokenPeekMismatch {
                    want: Token::RightParen,
                    got: self.curr.clone(),
                });
                return None;
            }
        }

        if self.curr == Token::Comma {
            return self.parse_tuple_expression(expr);
        }

        if self.curr == Token::SemiColon {
            todo!("Sequence");
        }

        self.advance();

        Some(expr)
    }

    fn parse_tuple_expression(&mut self, first: Expression) -> Option<Expression> {
        let mut elements = vec![first];
        while self.curr == Token::Comma {
            self.advance();
            let next = match self.parse_expression(Precedence::Sequence) {
                Some(expression) => expression,
                None => return None,
            };
            elements.push(next);
            self.advance();
        }
        if !self.is_current(Token::RightParen) {
            return None;
        }
        if self.peek != Token::SemiColonSemiColon {
            return None;
        }
        Some(Expression::Tuple(elements))
    }

    fn parse_if_then_expression(&mut self) -> Option<Expression> {
        self.advance();
        let cond = Box::new(self.parse_expression(Precedence::Lowest)?);
        if !self.if_peek_advance(Token::Then) {
            return None;
        }
        self.advance();

        let then_branch = Box::new(self.parse_expression(Precedence::Lowest)?);
        if !self.if_peek_advance(Token::Else) {
            return None;
        }
        self.advance();
        let else_branch = Box::new(self.parse_expression(Precedence::Lowest)?);

        Some(Expression::If {
            cond,
            then_branch,
            else_branch,
        })
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        self.advance();
        let mut params: Vec<FunParam> = vec![];
        while matches!(self.curr, Token::Identifier(_) | Token::LeftParen) {
            match self.parse_function_parameter() {
                Some(p) => params.push(p),
                None => return None,
            }
            self.advance();
        }

        if !self.is_current(Token::RightArrow) {
            return None;
        }
        self.advance();
        let body = Box::new(self.parse_expression(Precedence::Lowest)?);
        Some(Expression::Fun { params, body })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = match self.curr {
            Token::Plus => Infix::Plus,
            Token::PlusPlus => Infix::PlusPlus,
            Token::Minus => Infix::Minus,
            Token::Asterisk => Infix::Asterisk,
            Token::AsteriskAsterisk => Infix::AsteriskAsterisk,
            Token::ForwardSlash => Infix::ForwardSlash,
            Token::Caret => Infix::Caret,
            Token::Percent => Infix::Percent,
            Token::EqualEqual => Infix::EqualEqual,
            Token::NotEqual => Infix::NotEqual,
            Token::LessThan => Infix::LessThan,
            Token::GreaterThan => Infix::GreaterThan,
            Token::LessThanOrEqual => Infix::LessThanOrEqual,
            Token::GreaterThanOrEqual => Infix::GreaterThanOrEqual,
            Token::ColonColon => Infix::ColonColon,
            Token::SemiColon => {
                let current_precedence = self.curr_precedence();
                self.advance(); // Consume ';'
                let right = self.parse_expression(current_precedence)?; // Parse right side
                return Some(Expression::Sequence(Box::new(left), Box::new(right)));
            }
            _ => {
                self.errors
                    .push(Error::InfixDoesNotExistFor(self.curr.clone()));
                return None;
            }
        };

        let precedence = self.curr_precedence();
        self.advance(); // Consume the infix operator

        // Parse the right-hand side expression with the precedence of the operator
        match self.parse_expression(precedence) {
            Some(right) => Some(Expression::Infix(operator, Box::new(left), Box::new(right))),
            None => None, // Error parsing right operand
        }
    }

    fn parse_apply_expression(&mut self, function: Expression) -> Option<Expression> {
        self.advance();
        let argument = match self.parse_expression(Precedence::Apply) {
            Some(arg) => Box::new(arg),
            None => return None,
        };
        Some(Expression::Apply {
            func: Box::new(function),
            argument,
        })
    }

    pub fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = match self.curr {
            Token::Bang => Prefix::Bang,
            Token::Minus => Prefix::Minus,
            Token::Tilde => Prefix::Tilde,
            _ => return None,
        };
        self.advance();
        match self.parse_expression(Precedence::Prefix) {
            Some(expression) => Some(Expression::Prefix(operator, Box::new(expression))),
            None => None,
        }
    }
}
