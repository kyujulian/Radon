use crate::token::{Token, TokenType};

use std::any::Any;

// Traits
pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement: Node + Any {
    fn statement_node(&self);

    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self);
}

// PROGRAM
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>, // Dynamic dispatch (?)
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements[0].token_literal();
        } else {
            return "".to_string();
        }
    }
}

// IDENTIFIER

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Identifier { token, value }
    }
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}

// LET STATEMENT
#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
}

impl LetStatement {
    ///Creates a new name
    ///Initializes name to TokenType::ILLEGAL
    pub fn new() -> Self {
        LetStatement {
            token: Token::new(TokenType::LET, "let".to_string()),
            name: Identifier {
                token: Token::new(TokenType::ILLEGAL, "".to_string()),
                value: "".to_string(),
            },
        }
    }
}
impl Node for LetStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}
impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        return self;
    }
}
impl LetStatement {
    pub fn try_from(s: &dyn Statement) -> Option<&LetStatement> {
        s.as_any().downcast_ref::<LetStatement>()
    }
}
