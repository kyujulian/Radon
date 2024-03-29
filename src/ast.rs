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
#[derive(Debug, PartialEq, Clone)]
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
    pub token: Token, // return token

    // need to be wrapped in a box because the compiler isn't
    // capable of figuring out the size of a `dyn Expression` in compile time
    pub return_value: Option<Box<dyn Expression>>,
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
    pub fn new(return_value: Option<Box<dyn Expression>>) -> Self {
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
        write!(f, "({}{})", self.operator, right_write)
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

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Box<dyn Expression>,
        operator: String,
        right: Option<Box<dyn Expression>>,
    ) -> Self {
        Self {
            token,
            left,
            operator,
            right,
        }
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let right_write = match &self.right {
            None => "".to_string(),
            Some(s) => format!("{s}"),
        };
        write!(f, "({} {} {})", self.left, self.operator, right_write)
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Boolean {
    pub fn new(token: Token, value: bool) -> Self {
        Self { token, value }
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Boolean {
    fn expression_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Box<dyn Statement>>) -> Self {
        Self { token, statements }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut temp = String::new();
        for stmt in &self.statements {
            temp.push_str(format!("{}", stmt).as_str())
        }

        write!(f, "{}", temp)
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(
        token: Token,
        condition: Box<dyn Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "if")?;
        write!(f, "{}", self.condition)?;
        write!(f, " ")?;
        write!(f, "{}", self.consequence)?;
        write!(f, "{}", self.consequence)?;

        match &self.alternative {
            Some(alt) => {
                write!(f, "else")?;
                write!(f, "{}", alt)
            }
            None => Ok(()),
        }
    }
}
impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>,
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Identifier>, body: Option<BlockStatement>) -> Self {
        Self {
            token,
            parameters,
            body,
        }
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut params = Vec::new();
        for p in &self.parameters {
            params.push(p.to_string());
        }

        if let Some(b) = &self.body {
            return write!(f, "{}({}) {}", self.token_literal(), params.join(", "), b);
        }

        return write!(f, "{}({}) {}", self.token_literal(), params.join(", "), "");
    }
}
impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Option<Box<dyn Expression>>>,
}

impl CallExpression {
    pub fn new(
        token: Token,
        function: Box<dyn Expression>,
        arguments: Vec<Option<Box<dyn Expression>>>,
    ) -> Self {
        Self {
            token,
            function,
            arguments,
        }
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.function)?;
        write!(f, "(")?;
        let mut args: Vec<String> = Vec::new();
        for arg in &self.arguments {
            if let Some(arg) = arg {
                args.push(format!("{}", arg));
            };
        }

        write!(f, "{}", args.join(", "))?;
        write!(f, ")")
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
impl Expression for CallExpression {
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
