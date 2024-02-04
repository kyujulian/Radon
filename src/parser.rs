use crate::ast::{self, Node, Statement};
use crate::lexer;
use crate::token;

pub struct Parser {
    lex: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Self {
        let mut p = Self {
            lex,
            cur_token: token::Token::from('\0'),
            peek_token: token::Token::from('\0'),
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();
        p
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
            _ => None,
        }
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let mut stmt = ast::ReturnStatement::new();
        self.next_token();
        // TODO: We're skipping the expression, looping until we encounter a semicolon
        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        return Some(Box::new(stmt));
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let mut stmt = ast::LetStatement::new();

        if !self.expect_peek(token::TokenType::IDENT) {
            return None;
        }

        stmt.name = ast::Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());

        if !self.expect_peek(token::TokenType::ASSIGN) {
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
        let mut program = ast::Program::new();
        program.statements = vec![];

        while self.cur_token.token_type != token::TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token()
        }

        return Some(Box::new(program));
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{LetStatement, ReturnStatement};

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

        let mut lex = lexer::Lexer::new(input.to_string());

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
}
