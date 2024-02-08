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

#[derive(Copy, Clone)]
pub enum ExpressionPriorities {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
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

        p.register_prefix(TokenType::IDENT, parse_identifier);
        p.register_prefix(TokenType::INT, parse_integer_literal);

        p.register_prefix(TokenType::BANG, parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, parse_prefix_expression);

        //Registring infix
        p.register_infix(TokenType::PLUS, parse_infix_expression);
        p.register_infix(TokenType::MINUS, parse_infix_expression);
        p.register_infix(TokenType::SLASH, parse_infix_expression);
        p.register_infix(TokenType::ASTERISK, parse_infix_expression);
        p.register_infix(TokenType::EQ, parse_infix_expression);
        p.register_infix(TokenType::NEQ, parse_infix_expression);
        p.register_infix(TokenType::LT, parse_infix_expression);
        p.register_infix(TokenType::GT, parse_infix_expression);

        p.next_token();
        p.next_token();
        p
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
                let mut left_exp = prefix_exp(self);
                while !self.peek_token_is(token::TokenType::SEMICOLON) {
                    let peek_token_type = self.peek_token.token_type.clone();
                    let thing = self.infix_parse_fns.clone();
                    let infix_fn = thing.get(&peek_token_type);
                    match infix_fn {
                        None => return left_exp,
                        Some(infix) => {
                            self.next_token();
                            left_exp = infix(self, left_exp.unwrap());
                        }
                    }
                }
                return left_exp;
            }
        }
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
        let stmt = ast::ReturnStatement::new();
        self.next_token();
        // TODO: We're skipping the expression, looping until we encounter a semicolon
        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        return Some(Box::new(stmt));
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        if !self.expect_peek(token::TokenType::IDENT) {
            self.errors.push(format!(
                "Expected next token to be of type IDENT, got {:?} instead",
                self.peek_token.token_type
            ));
            return None;
        }

        let name = ast::Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
        let stmt = ast::LetStatement::new(name, None);

        if !self.expect_peek(token::TokenType::ASSIGN) {
            self.peek_error(token::TokenType::ASSIGN);
            return None;
        }

        //TODO: We're skipping the expressions until we encounter a semicolon
        while !self.cur_token_is(token::TokenType::SEMICOLON) {
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
    use crate::ast::{LetStatement, ReturnStatement};
    use crate::ast::{Node, Statement};
    use crate::lexer::Lexer;

    use super::*;

    #[derive(Clone, Debug)]
    struct TestIdentifier {
        expected_identifier: String,
    }
    impl From<&str> for TestIdentifier {
        fn from(s: &str) -> TestIdentifier {
            TestIdentifier {
                expected_identifier: s.to_string(),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            None => panic!("None from parse_program()"),
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
        };

        let tests = vec![
            TestIdentifier::from("x"),
            TestIdentifier::from("y"),
            TestIdentifier::from("foobar"),
        ];

        let has_invalid_tests = tests.iter().enumerate().any(|(i, test)| {
            let stmt = &program.statements[i];
            println!("stmt type {:?}", stmt.token_literal());
            !test_let_statement(stmt, &test.expected_identifier)
        });

        assert!(!has_invalid_tests, "Invalid test found");
    }

    fn test_let_statement(stmt: &Box<dyn ast::Statement>, name: &str) -> bool {
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

    fn check_parser_errors(parser: &Parser) {
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
    fn test_return_statement() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        let lex = lexer::Lexer::new(input.to_string());

        let mut parser = Parser::new(lex);

        let program = match parser.parse_program() {
            None => panic!("None from parse_program()"),
            Some(p) => {
                check_parser_errors(&parser);
                p
            }
        };

        if program.statements.len() != 3 {
            panic!(
                "Program statements does not contain 3 statements, got {}",
                program.statements.len()
            );
        }

        for stmt in program.statements {
            match stmt.as_any().downcast_ref::<ReturnStatement>() {
                None => {
                    panic!(
                        "Got none from downcast_ref from  -> {:?}",
                        stmt.token_literal()
                    );
                }
                Some(st) => {
                    if st.token_literal() != "return" {
                        panic!(
                            "Token Literal not corresponding to a return statement, got {:?}",
                            stmt.token_literal()
                        );
                    }
                }
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

    pub struct PrefixTest {
        pub input: String,
        pub operator: String,
        pub integer_value: i64,
    }
    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            PrefixTest {
                input: "!5".to_string(),
                operator: "!".to_string(),
                integer_value: 5,
            },
            PrefixTest {
                input: "-15".to_string(),
                operator: "-".to_string(),
                integer_value: 15,
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

            //some solid nice syntax here
            if let Some(exp_right_ref) = exp.right.as_ref() {
                assert!(test_integer_literal(exp_right_ref, test.integer_value));
            } else {
                panic!("Got none in {:?}", exp.right);
            }
        }
    }

    fn test_integer_literal(il: &Box<dyn ast::Expression>, value: i64) -> bool {
        let int_lit = il
            .as_any()
            .downcast_ref::<ast::IntegerLiteral>()
            .expect(format!("Downcast_ref failed for {:?}", il).as_str());

        assert_eq!(int_lit.value, value);

        assert_eq!(int_lit.token_literal(), format!("{value}"));

        true
    }

    pub struct InfixTestExpression {
        pub input: String,
        pub left_value: i64,
        pub operator: String,
        pub right_value: i64,
    }

    impl InfixTestExpression {
        pub fn new(
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
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
            InfixTestExpression::new("5 + 5".to_string(), 5, "+".to_string(), 5),
            InfixTestExpression::new("5 - 5".to_string(), 5, "-".to_string(), 5),
            InfixTestExpression::new("5 * 5".to_string(), 5, "*".to_string(), 5),
            InfixTestExpression::new("5 / 5".to_string(), 5, "/".to_string(), 5),
            InfixTestExpression::new("5 > 5".to_string(), 5, ">".to_string(), 5),
            InfixTestExpression::new("5 < 5".to_string(), 5, "<".to_string(), 5),
            InfixTestExpression::new("5 == 5".to_string(), 5, "==".to_string(), 5),
            InfixTestExpression::new("5 != 5".to_string(), 5, "!=".to_string(), 5),
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

            let expression = statement
                .expression
                .as_any()
                .downcast_ref::<ast::InfixExpression>()
                .expect(
                    format!(
                        "Failed to cast expression as Infix Operation {:#?}",
                        statement
                    )
                    .as_str(),
                );

            assert_eq!(expression.operator, test.operator);

            assert!(test_integer_literal(
                &Box::new(expression.right.as_ref().unwrap()),
                test.right_value
            ));
        }
    }
}
