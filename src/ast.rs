use crate::token::{Token, TokenType};
use std::fmt::{Debug, Display};

use std::any::Any;

// Traits
pub trait Node: Debug + Display {
    /// returns Cloned value of the inner token literal
    fn token_literal(&self) -> String;
}

pub trait Statement: Node + Any {
    fn statement_node(&self);

    fn as_any(&self) -> &dyn Any;
}

pub trait Expression: Node {
    fn expression_node(&self);
    fn as_any(&self) -> &dyn Any;
}

// PROGRAM
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>, // Dynamic dispatch (?)
}

impl Program {
    pub fn new(statements: Vec<Box<dyn Statement>>) -> Self {
        Program { statements }
    }
}

impl Default for Program {
    fn default() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::new();
        for statement in &self.statements {
            s.push_str(&statement.to_string());
            s.push_str(" ");
        }
        write!(f, "{}", s.trim_end())
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

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}
impl Expression for Identifier {
    fn expression_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// LET STATEMENT
#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl LetStatement {
    ///Creates a new name
    ///Initializes name to TokenType::ILLEGAL
    pub fn new(ident: Identifier, value: Option<Box<dyn Expression>>) -> Self {
        LetStatement {
            token: Token::new(TokenType::LET, "let".to_string()),
            name: ident,
            value,
        }
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {} = ", self.token.literal, self.name.value)?;

        match &self.value {
            None => write!(f, ";"),
            Some(value) => write!(f, "{};", value),
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

#[derive(Debug)]
pub struct ReturnStatement {
    token: Token, // return token

    // need to be wrapped in a box because the compiler isn't
    // capable of figuring out the size of a `dyn Expression` in compile time
    return_value: Option<Box<dyn Expression>>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} ", self.token.literal)?;
        match &self.return_value {
            Some(value) => write!(f, "{};", value),
            None => write!(f, ";"),
        }
    }
}

impl ReturnStatement {
    pub fn new() -> Self {
        Self {
            token: Token::from("return"),
            return_value: None,
        }
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        return self.token.literal.clone();
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        return self;
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Box<dyn Expression>) -> Self {
        Self { token, expression }
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        return Self { token, value };
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Option<Box<dyn Expression>>) -> Self {
        Self {
            token,
            operator,
            right,
        }
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let right_write = match &self.right {
            None => "".to_string(),
            Some(s) => format!("{s}"),
        };
        write!(f, "({},{})", self.operator, right_write)
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::{Identifier, LetStatement, Program, Statement};

    #[test]
    fn test_strings() {
        let statements: Vec<Box<dyn Statement>> = vec![Box::new(LetStatement::new(
            Identifier::new(Token::from("myVar"), "myVar".to_string()),
            Some(Box::new(Identifier::new(
                Token::from("anotherVar"),
                "anotherVar".to_string(),
            ))),
        ))];

        let program = Program::new(statements);

        if format!("{}", program) != "let myVar = anotherVar;" {
            panic!("Display implemented wrong, got={}", format!("{}", program))
        }
    }
}
