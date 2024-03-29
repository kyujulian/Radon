use crate::ast::{self};
use crate::lexer;
use crate::token::{self, TokenType};
use lazy_static::lazy_static;
use std::collections::HashMap;

//Type Aliases
type PrefixParseFn = fn(parser: &mut Parser) -> Option<Box<dyn ast::Expression>>;
type InfixParseFn =
    fn(parser: &mut Parser, left: Box<dyn ast::Expression>) -> Option<Box<dyn ast::Expression>>;

#[derive(Debug)]
pub struct Parser {
    lex: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
}

#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub enum ExpressionPriorities {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
}

lazy_static! {
    static ref PRECEDENCES: HashMap<token::TokenType, ExpressionPriorities> = {
        let mut m = HashMap::new();
        m.insert(token::TokenType::EQ, ExpressionPriorities::EQUALS);
        m.insert(token::TokenType::NEQ, ExpressionPriorities::EQUALS);
        m.insert(token::TokenType::LT, ExpressionPriorities::LESSGREATER);
        m.insert(token::TokenType::GT, ExpressionPriorities::LESSGREATER);
        m.insert(token::TokenType::PLUS, ExpressionPriorities::SUM);
        m.insert(token::TokenType::MINUS, ExpressionPriorities::SUM);
        m.insert(token::TokenType::SLASH, ExpressionPriorities::PRODUCT);
        m.insert(token::TokenType::ASTERISK, ExpressionPriorities::PRODUCT);
        m.insert(token::TokenType::LPAREN, ExpressionPriorities::CALL);
        m
    };
}

pub fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();

    if errors.len() == 0 {
        return;
    }

    println!("Parser has {} Errors: ", errors.len());

    for msg in errors {
        println!("Parser error: {}", msg);
    }
}
// Should I reach for thiserrror ?
// Think not yet, will try do it by hand for now, to get a good
// grasp on raw error handling and appreicate the value of thiserror
pub enum ParserError {
    NoPrefixParseFn,
    ExpectPeekTokenError(String),
    ParseIntError(std::num::ParseIntError),
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Self {
        let mut p = Self {
            lex,
            cur_token: token::Token::from('\0'),
            peek_token: token::Token::from('\0'),
            errors: Vec::new(),
            infix_parse_fns: HashMap::new(),
            prefix_parse_fns: HashMap::new(),
        };

        let parse_identifier =
            |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> { parser.parse_identifier() };

        let parse_integer_literal = |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> {
            parser.parse_integer_literal()
        };

        let parse_prefix_expression = |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> {
            parser.parse_prefix_expression()
        };

        let parse_infix_expression = |parser: &mut Parser,
                                      left: Box<dyn ast::Expression>|
         -> Option<Box<dyn ast::Expression>> {
            parser.parse_infix_expression(left)
        };

        let parse_if_expression = |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> {
            parser.parse_if_expression()
        };

        let parse_function_literal = |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> {
            parser.parse_function_literal()
        };

        let parse_boolean =
            |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> { parser.parse_boolean() };

        let parse_grouped_expression = |parser: &mut Parser| -> Option<Box<dyn ast::Expression>> {
            parser.parse_grouped_expression()
        };

        let parse_call_expression =
            |parser: &mut Parser,
             left: Box<dyn ast::Expression>|
             -> Option<Box<dyn ast::Expression>> { parser.parse_call_expression(left) };

        p.register_prefix(TokenType::IDENT, parse_identifier);
        p.register_prefix(TokenType::INT, parse_integer_literal);
        p.register_prefix(TokenType::BANG, parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, parse_prefix_expression);
        p.register_prefix(TokenType::TRUE, parse_boolean);
        p.register_prefix(TokenType::FALSE, parse_boolean);
        p.register_prefix(TokenType::IF, parse_if_expression);
        p.register_prefix(TokenType::LPAREN, parse_grouped_expression);

        p.register_prefix(TokenType::FUNCTION, parse_function_literal);
        //Registring infix
        p.register_infix(TokenType::PLUS, parse_infix_expression);
        p.register_infix(TokenType::MINUS, parse_infix_expression);
        p.register_infix(TokenType::SLASH, parse_infix_expression);
        p.register_infix(TokenType::ASTERISK, parse_infix_expression);
        p.register_infix(TokenType::EQ, parse_infix_expression);
        p.register_infix(TokenType::NEQ, parse_infix_expression);
        p.register_infix(TokenType::LT, parse_infix_expression);
        p.register_infix(TokenType::GT, parse_infix_expression);
        p.register_infix(TokenType::LPAREN, parse_call_expression);

        p.next_token();
        p.next_token();
        p
    }

    fn parse_call_expression(
        &mut self,
        function: Box<dyn ast::Expression>,
    ) -> Option<Box<dyn ast::Expression>> {
        let start_token = self.cur_token.clone();
        let args = self.parse_call_arguments()?;
        let exp = ast::CallExpression::new(start_token, function, args);
        return Some(Box::new(exp));
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Option<Box<dyn ast::Expression>>>> {
        let mut args: Vec<Option<Box<dyn ast::Expression>>> = Vec::new();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(args);
        }

        self.next_token();

        args.push(self.parse_expression(ExpressionPriorities::LOWEST));

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(ExpressionPriorities::LOWEST));
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        Some(args)
    }
    fn parse_function_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let start_token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statement();

        let lit = ast::FunctionLiteral::new(start_token, parameters, body);

        Some(Box::new(lit))
    }

    fn parse_function_parameters(&mut self) -> Vec<ast::Identifier> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let ident = ast::Identifier::new(self.cur_token.clone(), self.cur_token.clone().literal);

        identifiers.push(ident);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();

            let ident =
                ast::Identifier::new(self.cur_token.clone(), self.cur_token.clone().literal);
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return vec![];
        }

        identifiers
    }
    fn parse_if_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let start_token = self.cur_token.clone();
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(ExpressionPriorities::LOWEST)?;
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement()?;

        let mut alternative = None;

        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }
            alternative = self.parse_block_statement();
        }

        let expression = ast::IfExpression::new(start_token, condition, consequence, alternative);

        Some(Box::new(expression))
    }

    fn parse_block_statement(&mut self) -> Option<ast::BlockStatement> {
        let start_token = self.cur_token.clone();
        // let mut block = ast::BlockStatement::new(self.cur_token.clone());
        let mut statements = Vec::new();
        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        let block = ast::BlockStatement::new(start_token, statements);
        Some(block)
    }
    fn parse_grouped_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        self.next_token();
        let exp = self.parse_expression(ExpressionPriorities::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        exp
    }
    fn parse_boolean(&self) -> Option<Box<dyn ast::Expression>> {
        let token = self.cur_token.clone();
        let value = self.cur_token_is(TokenType::TRUE);
        Some(Box::new(ast::Boolean::new(token, value)))
    }
    fn parse_prefix_expression(&mut self) -> Option<Box<dyn ast::Expression>> {
        let cur_token = self.cur_token.clone();
        let cur_token_literal = self.cur_token.literal.clone();

        // if this order changes
        // this runs in infinite loop
        self.next_token();
        let right = self.parse_expression(ExpressionPriorities::PREFIX);

        let expression = ast::PrefixExpression::new(cur_token, cur_token_literal, right);

        return Some(Box::new(expression));
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn ast::Expression>> {
        let value = self.cur_token.literal.parse::<i64>();

        match value {
            Err(val) => {
                self.errors
                    .push(format!("Could not parse {} as integer", val));
                None
            }
            Ok(val) => {
                let literal = ast::IntegerLiteral::new(self.cur_token.clone(), val);
                Some(Box::new(literal))
            }
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn ast::Expression>,
    ) -> Option<Box<dyn ast::Expression>> {
        let mut expression = ast::InfixExpression::new(
            self.cur_token.clone(),
            left,
            self.cur_token.literal.clone(),
            None,
        );
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence);

        expression.right = right;

        // return Some(Box::new(expression));

        return Some(Box::new(expression));
    }

    fn parse_identifier(&self) -> Option<Box<dyn ast::Expression>> {
        Some(Box::new(ast::Identifier::new(
            self.cur_token.clone(),
            self.cur_token.literal.clone(),
        )))
    }
    /// Returns a read-only view (slice) of the errors
    pub fn errors(&self) -> &[String] {
        self.errors.as_slice()
    }

    pub fn peek_error(&mut self, token: token::TokenType) {
        let msg = format!(
            "Expected next token to be of type {:?}, got {:?} instead",
            token, self.peek_token.token_type
        );

        self.errors.push(msg);
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lex.next_token();
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        match self.cur_token.token_type {
            token::TokenType::LET => self.parse_let_statement(),
            token::TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression(
        &mut self,
        precedence: ExpressionPriorities,
    ) -> Option<Box<dyn ast::Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);

        match prefix {
            None => {
                self.errors.push(format!(
                    "No prefix parse function for '{}' found",
                    self.cur_token.token_type
                ));
                None
            }
            Some(prefix_exp) => {
                let mut left_exp = prefix_exp(self)?;
                while !self.peek_token_is(token::TokenType::SEMICOLON)
                    && precedence < self.peek_precedence()
                {
                    let peek_token_type = self.peek_token.token_type.clone();
                    // let thing = self.infix_parse_fns.clone();
                    let infix_fn = self.get_infix_fn(peek_token_type);
                    match infix_fn {
                        None => return Some(left_exp),
                        Some(infix) => {
                            self.next_token();
                            left_exp = infix(self, left_exp)?;
                        }
                    }
                }
                return Some(left_exp);
            }
        }
    }

    fn get_infix_fn(&self, token_type: token::TokenType) -> Option<InfixParseFn> {
        self.infix_parse_fns.get(&token_type).copied()
    }
    pub fn parse_expression_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let expression = self.parse_expression(ExpressionPriorities::LOWEST)?;

        let stmt = ast::ExpressionStatement::new(self.cur_token.clone(), expression);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token()
        }

        return Some(Box::new(stmt));
    }
    fn peek_token_is(&self, ttype: token::TokenType) -> bool {
        self.peek_token.token_type == ttype
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        self.next_token();

        let return_value = self.parse_expression(ExpressionPriorities::LOWEST);

        let stmt = ast::ReturnStatement::new(return_value);

        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        return Some(Box::new(stmt));
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        if !self.expect_peek(token::TokenType::IDENT) {
            //self.peek_error(token::TokenType::ASSIGN);
            return None;
        }

        let name = ast::Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());

        if !self.expect_peek(token::TokenType::ASSIGN) {
            //self.peek_error(token::TokenType::ASSIGN);
            return None;
        }
        self.next_token();

        let value = self.parse_expression(ExpressionPriorities::LOWEST);
        let stmt = ast::LetStatement::new(name, value);

        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }

        return Some(Box::new(stmt));
    }

    pub fn cur_token_is(&self, t: token::TokenType) -> bool {
        self.cur_token.token_type == t
    }

    pub fn expect_peek(&mut self, t: token::TokenType) -> bool {
        if self.peek_token.token_type == t {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    pub fn parse_program(&mut self) -> Option<Box<ast::Program>> {
        let mut program = ast::Program::default();

        while self.cur_token.token_type != token::TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                // all the parsing errors die here, so does
                // it make sense to return a Result here ?
                // Think not, Option should be better, we'll see
                program.statements.push(stmt);
            }
            self.next_token()
        }

        return Some(Box::new(program));
    }

    pub fn register_prefix(&mut self, ttype: token::TokenType, function: PrefixParseFn) {
        self.prefix_parse_fns.insert(ttype, function);
    }

    pub fn register_infix(&mut self, ttype: token::TokenType, function: InfixParseFn) {
        self.infix_parse_fns.insert(ttype, function);
    }

    pub fn peek_precedence(&self) -> ExpressionPriorities {
        let precedence = PRECEDENCES.get(&self.peek_token.token_type);

        match precedence {
            None => ExpressionPriorities::LOWEST,
            Some(p) => *p,
        }
    }

    pub fn cur_precedence(&self) -> ExpressionPriorities {
        let precedence = PRECEDENCES.get(&self.cur_token.token_type);

        match precedence {
            None => ExpressionPriorities::LOWEST,
            Some(p) => *p,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::ast::{Expression, Node, Statement};
    use crate::ast::{LetStatement, ReturnStatement};
    use crate::lexer::Lexer;

    use super::*;

    struct TestStatement<'a> {
        input: &'a str,
        expected_identifier: &'a str,
        expected_value: Expected,
    }

    impl<'a> TestStatement<'a> {
        pub fn new(input: &'a str, expected_identifier: &'a str, expected_value: Expected) -> Self {
            Self {
                input,
                expected_identifier,
                expected_value,
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            TestStatement::new("let x = 5;", "x", Expected::Integer(5)),
            TestStatement::new("let y = true;", "y", Expected::Bool(true)),
            TestStatement::new(
                "let foobar = y;",
                "foobar",
                Expected::Identifier("y".to_string()),
            ),
        ];

        for test in tests {
            let input = test.input;
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let program = match parser.parse_program() {
                Some(p) => {
                    check_parser_errors(&parser);
                    p
                }
                None => panic!("Failed to call `parser.parse_program()`"),
            };

            assert_eq!(program.statements.len(), 1);

            println!("{:#?}", program.statements[0]);

            let stmt = program.statements[0]
                .as_any()
                .downcast_ref::<ast::LetStatement>()
                .expect(" Failed to downcast_ref on stmt ");

            assert!(test_let_statement(
                &Box::new(stmt),
                test.expected_identifier
            ));

            println!("{:#?}", stmt);
            let ident = stmt.value.as_ref().expect("Value is None");

            // println!("{:#?}", value);

            assert!(test_literal_expression(&ident, test.expected_value))
        }
    }

    fn test_let_statement(stmt: &Box<&ast::LetStatement>, name: &str) -> bool {
        if stmt.token_literal() != "let" {
            println!("Token literal not `let`, got={}", stmt.token_literal());
            return false;
        }

        match stmt.as_any().downcast_ref::<LetStatement>() {
            None => {
                println!("stmt not LetStatement");
                return false;
            }
            Some(let_stmt) => {
                if let_stmt.name.value != name {
                    println!(
                        "let_stmt.name.value not `{}`, got={}",
                        name, let_stmt.name.value
                    );
                    return false;
                }

                if let_stmt.name.token_literal() != name {
                    println!(
                        "let_stmt.name.token_literal not `{}`, got={}",
                        name,
                        let_stmt.name.token_literal()
                    );
                    return false;
                }
            }
        };

        true
    }

    pub fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return;
        }

        println!("Parser has {} Errors: ", errors.len());

        for msg in errors {
            println!("Parser error: {}", msg);
        }

        panic!();
    }

    // RETURN STATEMENTS
    #[test]
    fn test_return_statements() {
        use TestStatement as TS;
        let tests = vec![
            TS::new("return 5;", "return", Expected::Integer(5)),
            TS::new("return 10;", "return", Expected::Integer(10)),
            TS::new("return 993322;", "return", Expected::Integer(993322)),
        ];
        // let input = "
        //     return 5;
        //     return 10;
        //     return 993322;
        // ";

        for test in tests {
            let input = test.input;

            let lex = lexer::Lexer::new(input.to_string());

            let mut parser = Parser::new(lex);

            let program = match parser.parse_program() {
                None => panic!("None from parse_program()"),
                Some(p) => {
                    check_parser_errors(&parser);
                    p
                }
            };

            if program.statements.len() != 1 {
                panic!(
                    "Program statements does not contain 3 statements, got {}",
                    program.statements.len()
                );
            }

            let stmt = program.statements[0]
                .as_any()
                .downcast_ref::<ast::ReturnStatement>()
                .expect("Failed to downcast_ref stmt to ReturnStatement");

            assert!(test_return_statement(&Box::new(stmt), test.expected_value));
            // assert!(test_return_statement());
        }
    }

    fn test_return_statement(stmt: &Box<&ast::ReturnStatement>, expected: Expected) -> bool {
        if stmt.token_literal() != "return" {
            println!("Token literal not `let`, got={}", stmt.token_literal());
            return false;
        }

        match stmt.as_any().downcast_ref::<ReturnStatement>() {
            None => {
                println!("stmt not ReturnStatement");
                return false;
            }
            Some(return_stmt) => {
                if let Some(exp_right_ref) = return_stmt.return_value.as_ref() {
                    match expected {
                        Expected::Integer(val) => {
                            assert!(test_integer_literal(exp_right_ref, val as i64));
                        }
                        Expected::Integer64(val) => {
                            assert!(test_integer_literal(exp_right_ref, val));
                        }
                        Expected::Bool(val) => {
                            assert!(test_boolean_literal(exp_right_ref, val));
                        }
                        _ => panic!("Invalid test value"),
                    }
                }
            }
        }
        true
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";

        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            None => panic!("None from parse_program()"),
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
        };

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement, got {}",
            program.statements.len()
        );

        let stmt = &program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(format!("downcast_ref failed for {:?}", program.statements[0]).as_ref());

        let ident = stmt.expression.as_any().downcast_ref::<ast::Boolean>();

        match ident {
            None => panic!("Got none from downcast_ref from  -> {:?}", ident),
            Some(ident) => {
                assert_eq!(
                    ident.token_literal(),
                    "true",
                    "Token Literal not corresponding to a return statement, got {:?}",
                    ident.token_literal()
                );
                assert_eq!(
                    ident.value, true,
                    "Ident Value not corresponding to statement, got {:?}",
                    ident.value
                );
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            None => panic!("None from parse_program()"),
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
        };

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement, got {}",
            program.statements.len()
        );

        let stmt = &program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>();

        assert!(stmt.is_some());

        println!("{:?}", stmt);
        let ident = stmt
            .unwrap()
            .expression
            .as_any()
            .downcast_ref::<ast::Identifier>();

        match ident {
            None => {
                panic!("Got none from downcast_ref from  -> {:?}", ident);
            }
            Some(ident) => {
                assert_eq!(
                    ident.token_literal(),
                    "foobar",
                    "Token Literal not corresponding to a return statement, got {:?}",
                    ident.token_literal()
                );
                assert_eq!(
                    ident.value, "foobar",
                    "Ident Value not corresponding to statement, got {:?}",
                    ident.value
                );
                assert_eq!(
                    ident.token_literal(),
                    "foobar",
                    "Token Literal not corresponding to a return statement, got {:?}",
                    ident.token_literal()
                );
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() -> Result<(), std::io::Error> {
        let input = "5;";

        let mut lex = Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);
        let program = match parser.parse_program() {
            None => panic!("Failed to parse program"),
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
        };

        let statement = program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>();

        let stmt = match statement {
            None => {
                panic!(
                    "program.Statements[0] is not ast.ExpressionStatement , got = {} ",
                    program.statements[0]
                )
            }
            Some(stmt) => stmt,
        };

        let literal = stmt
            .expression
            .as_any()
            .downcast_ref::<ast::IntegerLiteral>();

        let literal = match literal {
            None => {
                panic!(
                    "Literal is not ast.IntegerLiteral , got = {} ",
                    stmt.expression.token_literal()
                )
            }
            Some(stmt) => stmt,
        };

        assert_eq!(
            literal.value, 5,
            "Literal value is not {}, got {}",
            5, literal.value
        );

        assert_eq!(
            literal.token_literal(),
            "5",
            "token_literal() is not {}, got {}",
            5,
            literal.token_literal()
        );

        Ok(())
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        pub struct PrefixTest {
            pub input: String,
            pub operator: String,
            pub value: Expected,
        }
        let prefix_tests = vec![
            PrefixTest {
                input: "!5".to_string(),
                operator: "!".to_string(),
                value: Expected::Integer64(5),
            },
            PrefixTest {
                input: "-15".to_string(),
                operator: "-".to_string(),
                value: Expected::Integer(15),
            },
            PrefixTest {
                input: "!true".to_string(),
                operator: "!".to_string(),
                value: Expected::Bool(true),
            },
            PrefixTest {
                input: "!false".to_string(),
                operator: "!".to_string(),
                value: Expected::Bool(false),
            },
        ];

        for test in prefix_tests {
            let lex = Lexer::new(test.input);
            let mut parser = Parser::new(lex);

            //
            let program = match parser.parse_program() {
                None => panic!("Not able to parse the Program"),
                Some(p) => {
                    check_parser_errors(&parser);
                    p
                }
            };
            //
            assert_eq!(program.statements.len(), 1);

            let statement = program.statements[0]
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .expect("Downcaset_ref failed");

            let exp = statement
                .expression
                .as_any()
                .downcast_ref::<ast::PrefixExpression>()
                .expect("Downcast_ref failed");

            assert_eq!(exp.operator, test.operator);

            if let Some(exp_right_ref) = exp.right.as_ref() {
                match test.value {
                    Expected::Integer(val) => {
                        assert!(test_integer_literal(exp_right_ref, val as i64));
                    }
                    Expected::Integer64(val) => {
                        assert!(test_integer_literal(exp_right_ref, val));
                    }
                    Expected::Bool(val) => {
                        assert!(test_boolean_literal(exp_right_ref, val));
                    }
                    _ => panic!("Invalid test value"),
                }
            } else {
                panic!("Got none in {:?}", exp.right);
            }
        }
    }

    fn test_identifier(ident: &Box<dyn ast::Expression>, value: &str) -> bool {
        let ident = ident
            .as_any()
            .downcast_ref::<ast::Identifier>()
            .expect(format!("Downcast_ref failed for {:?}", ident).as_str());

        assert_eq!(ident.value, value);

        assert_eq!(ident.token_literal(), format!("{value}"));

        true
    }

    enum Expected {
        Integer(i32),
        Integer64(i64),
        Identifier(String),
        Bool(bool),
    }

    fn test_literal_expression(exp: &Box<dyn ast::Expression>, expected: Expected) -> bool {
        match expected {
            Expected::Integer(val) => test_integer_literal(exp, val as i64),
            Expected::Integer64(val) => test_integer_literal(exp, val),
            Expected::Identifier(val) => test_identifier(exp, &val),
            Expected::Bool(val) => test_boolean_literal(exp, val),
        }
    }

    fn test_boolean_literal(exp: &Box<dyn ast::Expression>, expected: bool) -> bool {
        let bool_lit = exp
            .as_any()
            .downcast_ref::<ast::Boolean>()
            .expect(format!("Downcast_ref failed for {:?}", exp).as_str());

        assert_eq!(bool_lit.value, expected);

        assert_eq!(bool_lit.token_literal(), format!("{expected}"));

        true
    }

    fn test_integer_literal(il: &Box<dyn ast::Expression>, value: i64) -> bool {
        let int_lit = il
            .as_any()
            .downcast_ref::<ast::IntegerLiteral>()
            .expect(format!("Downcast_ref failed for {:?} test_integer_literal", il).as_str());

        assert_eq!(int_lit.value, value);

        assert_eq!(int_lit.token_literal(), format!("{value}"));

        true
    }

    pub struct InfixTestExpression {
        pub input: String,
        pub left_value: Expected,
        pub operator: String,
        pub right_value: Expected,
    }

    impl InfixTestExpression {
        pub fn new(
            input: String,
            left_value: Expected,
            operator: String,
            right_value: Expected,
        ) -> InfixTestExpression {
            InfixTestExpression {
                input,
                left_value,
                operator,
                right_value,
            }
        }
    }
    #[test]
    fn test_parse_infix_expressions() {
        let infix_tests = vec![
            InfixTestExpression::new(
                "5 + 5".to_string(),
                Expected::Integer64(5),
                "+".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 - 5".to_string(),
                Expected::Integer64(5),
                "-".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 * 5".to_string(),
                Expected::Integer64(5),
                "*".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 / 5".to_string(),
                Expected::Integer64(5),
                "/".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 > 5".to_string(),
                Expected::Integer64(5),
                ">".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 < 5".to_string(),
                Expected::Integer64(5),
                "<".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 == 5".to_string(),
                Expected::Integer64(5),
                "==".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "5 != 5".to_string(),
                Expected::Integer64(5),
                "!=".to_string(),
                Expected::Integer64(5),
            ),
            InfixTestExpression::new(
                "true != false".to_string(),
                Expected::Bool(true),
                "!=".to_string(),
                Expected::Bool(false),
            ),
            InfixTestExpression::new(
                "true == true".to_string(),
                Expected::Bool(true),
                "==".to_string(),
                Expected::Bool(true),
            ),
            InfixTestExpression::new(
                "false == false".to_string(),
                Expected::Bool(false),
                "==".to_string(),
                Expected::Bool(false),
            ),
        ];

        for test in infix_tests {
            let lex = Lexer::new(test.input);
            let mut parser = Parser::new(lex);

            let program = match parser.parse_program() {
                None => panic!("Failed to parse program"),
                Some(p) => {
                    check_parser_errors(&parser);
                    p
                }
            };

            let statement = program.statements[0]
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .expect("Failed to cast stmt to ExpressionStatement");

            test_infix_expression(
                &statement.expression,
                test.left_value,
                test.operator.as_str(),
                test.right_value,
            );

            // let expression = statement
            //     .expression
            //     .as_any()
            //     .downcast_ref::<ast::InfixExpression>()
            //     .expect(
            //         format!(
            //             "Failed to cast expression as Infix Operation {:#?}",
            //             statement
            //         )
            //         .as_str(),
            //     );
            //
            // assert_eq!(expression.operator, test.operator);
            //
            // assert!(test_integer_literal(
            //     &Box::new(expression.right.as_ref().unwrap()),
            //     test.right_value
            // ));
        }
    }

    fn test_infix_expression(
        exp: &Box<dyn ast::Expression>,
        left: Expected,
        operators: &str,
        right: Expected,
    ) -> bool {
        let infix = exp
            .as_any()
            .downcast_ref::<ast::InfixExpression>()
            .expect("Failed to cast to InfixExpression");

        assert!(test_literal_expression(&infix.left, left));
        assert_eq!(infix.operator, operators);
        assert!(test_literal_expression(
            &infix.right.as_ref().unwrap(),
            right
        ));

        true
    }
    #[test]
    fn test_operator_precedence_parsing() {
        struct OperatorPrecedenceTest {
            input: String,
            expected: String,
        };
        impl OperatorPrecedenceTest {
            pub fn new(input: String, expected: String) -> OperatorPrecedenceTest {
                OperatorPrecedenceTest { input, expected }
            }
        }

        let tests = vec![
            OperatorPrecedenceTest::new("-a * b".to_string(), "((-a) * b)".to_string()),
            OperatorPrecedenceTest::new("!-a".to_string(), "(!(-a))".to_string()),
            OperatorPrecedenceTest::new("a + b + c".to_string(), "((a + b) + c)".to_string()),
            OperatorPrecedenceTest::new("a + b - c".to_string(), "((a + b) - c)".to_string()),
            OperatorPrecedenceTest::new("a * b * c".to_string(), "((a * b) * c)".to_string()),
            OperatorPrecedenceTest::new("a * b / c".to_string(), "((a * b) / c)".to_string()),
            OperatorPrecedenceTest::new("a + b / c".to_string(), "(a + (b / c))".to_string()),
            OperatorPrecedenceTest::new(
                "a + b * c + d / e - f".to_string(),
                "(((a + (b * c)) + (d / e)) - f)".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "3 + 4; -5 * 5".to_string(),
                "(3 + 4) ((-5) * 5)".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "5 > 4 == 3 < 4".to_string(),
                "((5 > 4) == (3 < 4))".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "5 < 4 != 3 > 4".to_string(),
                "((5 < 4) != (3 > 4))".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            ),
            OperatorPrecedenceTest::new("true".to_string(), "true".to_string()),
            OperatorPrecedenceTest::new("false".to_string(), "false".to_string()),
            OperatorPrecedenceTest::new(
                "3 < 5 == true".to_string(),
                "((3 < 5) == true)".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "3 > 5 == false".to_string(),
                "((3 > 5) == false)".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "1 + (2 + 3) + 4".to_string(),
                "((1 + (2 + 3)) + 4)".to_string(),
            ),
            OperatorPrecedenceTest::new("(5 + 5) * 2".to_string(), "((5 + 5) * 2)".to_string()),
            OperatorPrecedenceTest::new("-(5 + 5)".to_string(), "(-(5 + 5))".to_string()),
            OperatorPrecedenceTest::new(
                "!(true == true)".to_string(),
                "(!(true == true))".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "a + add(b * c) + d".to_string(),
                "((a + add((b * c))) + d)".to_string(),
            ),
            OperatorPrecedenceTest::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(),
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string(),
            ),
        ];

        for test in tests {
            let lex = Lexer::new(test.input);
            let mut parser = Parser::new(lex);

            let program = match parser.parse_program() {
                None => panic!("Failed to parse program"),
                Some(p) => {
                    check_parser_errors(&parser);
                    p
                }
            };

            let actual = program.to_string();
            assert_eq!(actual, test.expected);
        }
    }

    #[test]
    fn test_if_expressions() {
        let input = "if (x < y) { x }";
        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
            None => {
                panic!("Failed to call `parse_program`");
            }
        };

        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "Failed to downcast_ref {:#?} to ExpressionStatement",
                    program.statements[0]
                )
                .as_str(),
            );

        let expr = stmt
            .expression
            .as_any()
            .downcast_ref::<ast::IfExpression>()
            .expect(format!("Failed to downcast ref {:#?} to IfExpression", stmt).as_str());

        if !test_infix_expression(
            &expr.condition,
            Expected::Identifier("x".to_string()),
            "<",
            Expected::Identifier("y".to_string()),
        ) {
            panic!("infix test failed");
        };

        let consequence = expr.consequence.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "failed to downcast_ref from {:#?} to ExpressionStatement",
                    &expr.consequence.statements[0]
                )
                .as_str(),
            );

        if !test_identifier(&Box::new(&consequence.expression), "x") {
            return;
        }

        if expr.alternative.is_some() {
            panic!(
                "exp.alternative.Statement was supposed to be None, got : {:#?}",
                expr.alternative
            );
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
            None => {
                panic!("Failed to call `parse_program`");
            }
        };

        assert_eq!(program.statements.len(), 1);
        let stmt = program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "Failed to downcast_ref {:#?} to ExpressionStatement",
                    program.statements[0]
                )
                .as_str(),
            );

        let expr = stmt
            .expression
            .as_any()
            .downcast_ref::<ast::IfExpression>()
            .expect(format!("Failed to downcast ref {:#?} to IfExpression", stmt).as_str());

        if !test_infix_expression(
            &expr.condition,
            Expected::Identifier("x".to_string()),
            "<",
            Expected::Identifier("y".to_string()),
        ) {
            panic!("infix test failed");
        };

        let consequence = expr.consequence.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "failed to downcast_ref from {:#?} to ExpressionStatement",
                    &expr.consequence.statements[0]
                )
                .as_str(),
            );

        if !test_identifier(&Box::new(&consequence.expression), "x") {
            return;
        }

        match &expr.alternative {
            None => {
                panic!(
                    "exp.alternative.Statement was supposed to be Some, expr is : {:#?}",
                    expr
                )
            }
            Some(alt) => {
                let alternative = alt.statements[0]
                    .as_any()
                    .downcast_ref::<ast::ExpressionStatement>()
                    .expect(
                        format!(
                            "failed to downcast_ref from {:#?} to ExpressionStatement",
                            &expr.consequence.statements[0]
                        )
                        .as_str(),
                    );

                if !test_identifier(&Box::new(&alternative.expression), "y") {
                    return;
                }
            }
        }
    }

    #[test]
    fn test_function_iteral_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lex = lexer::Lexer::new(input.to_string());

        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
            None => {
                panic!("Failed to call `parser.parse_program() {:?}`", parser)
            }
        };

        assert_eq!(program.statements.len(), 1);

        let stmt = program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "program.statements[0] is not ast::ExpressionStatements, got {:#?}",
                    program.statements[0]
                )
                .as_str(),
            );

        let function = stmt
            .expression
            .as_any()
            .downcast_ref::<ast::FunctionLiteral>()
            .expect(
                format!(
                    "stmt.expression is not ast::FunctionLiteral, got {:#?}",
                    stmt.expression
                )
                .as_str(),
            );

        assert_eq!(function.parameters.len(), 2);

        let expression_0: Box<dyn Expression> = Box::new(function.parameters[0].clone());
        let expression_1: Box<dyn Expression> = Box::new(function.parameters[1].clone());
        test_literal_expression(&expression_0, Expected::Identifier("x".to_string()));
        test_literal_expression(&expression_1, Expected::Identifier("y".to_string()));

        assert_eq!(
            function
                .body
                .as_ref()
                .expect("BODY IS NONE")
                .statements
                .len(),
            1
        );

        let body_statement = function.body.as_ref().expect("BODY IS NONE").statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "Function body not ast::ExpressionStatement, got= {:#?}",
                    function.body.as_ref().expect("BODY IS NONE").statements[0]
                )
                .as_str(),
            );

        test_infix_expression(
            &body_statement.expression,
            Expected::Identifier("x".to_string()),
            "+",
            Expected::Identifier("y".to_string()),
        );
    }
    #[test]
    fn test_function_parameter_parsing() {
        struct Test {
            input: String,
            expected_params: Vec<String>,
        };
        impl Test {
            fn new(input: &str, expected_params: Vec<String>) -> Self {
                Self {
                    input: input.to_string(),
                    expected_params,
                }
            }
        }

        let tests: Vec<Test> = vec![
            Test::new("fn() {};", vec![]),
            Test::new("fn(x) {};", vec!["x".to_string()]),
            Test::new(
                "fn(x, y, z) {};",
                vec!["x".to_string(), "y".to_string(), "z".to_string()],
            ),
        ];

        for test in tests {
            let lex = lexer::Lexer::new(test.input);
            let mut parser = Parser::new(lex);

            let program = match parser.parse_program() {
                Some(p) => {
                    check_parser_errors(&parser);

                    p
                }
                None => panic!("Failed to call `self.parse_program()`"),
            };

            let stmt = program.statements[0]
                .as_any()
                .downcast_ref::<ast::ExpressionStatement>()
                .expect(
                    format!(
                        "Failed to downcast_ref to ExpressionStatement from {:#?}",
                        program.statements[0]
                    )
                    .as_str(),
                );

            let function = stmt
                .expression
                .as_any()
                .downcast_ref::<ast::FunctionLiteral>()
                .expect(
                    format!(
                        "Failed to downcast_ref to FunctionLiteral from {:#?}",
                        stmt.expression
                    )
                    .as_str(),
                );

            assert_eq!(function.parameters.len(), test.expected_params.len());

            for (i, ident) in test.expected_params.iter().enumerate() {
                let expression: Box<dyn Expression> = Box::new(function.parameters[i].clone());
                test_literal_expression(&expression, Expected::Identifier(ident.to_string()));
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lex = lexer::Lexer::new(input.to_string());

        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
            None => panic!("call to `self.parse_program()` failed"),
        };

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements has len != than 1"
        );

        let stmt = program.statements[0]
            .as_any()
            .downcast_ref::<ast::ExpressionStatement>()
            .expect(
                format!(
                    "downcast_ref failed from {:#?} to ExpressionStatement",
                    program.statements[0]
                )
                .as_str(),
            );

        let expr = stmt
            .expression
            .as_any()
            .downcast_ref::<ast::CallExpression>()
            .expect(format!("downcast_ref failed from {:#?} to CallExpression", stmt).as_str());

        assert!(test_identifier(&expr.function, "add"));
        assert_eq!(expr.arguments.len(), 3);

        test_literal_expression(&expr.arguments[0].as_ref().unwrap(), Expected::Integer(1));
        test_infix_expression(
            &expr.arguments[1].as_ref().unwrap(),
            Expected::Integer(2),
            "*",
            Expected::Integer(3),
        );
        test_infix_expression(
            &expr.arguments[2].as_ref().unwrap(),
            Expected::Integer(4),
            "+",
            Expected::Integer(5),
        );
    }
}
