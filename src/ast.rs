use crate::token::Token;

use std::any::Any;

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

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>, // Dynamic dispatch (?)
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

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
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