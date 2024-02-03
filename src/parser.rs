use crate::ast::{self, Node, Statement};
use crate::lexer;
use crate::token;

pub struct Parser {
    lex: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Self {
        let mut p = Self {
            lex,
            cur_token: token::Token::from('\0'),
            peek_token: token::Token::from('\0'),
        };

        p.next_token();
        p.next_token();
        p
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lex.next_token();
    }

    pub fn parse_program(&self) -> Option<Box<ast::Program>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::LetStatement;

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
        let parser = Parser::new(lex);

        let program = match parser.parse_program() {
            None => panic!("None from parse_program()"),
            Some(p) => {
                if p.statements.len() != 3 {
                    panic!(
                        "Program.statements does not contain 3 statements, got={}",
                        p.statements.len()
                    );
                } else {
                    p
                }
            }
        };

        let tests = vec![
            TestIdentifier::from("x"),
            TestIdentifier::from("y"),
            TestIdentifier::from("foobar"),
        ];

        let has_invalid_tests = tests.iter().enumerate().any(|(i, test)| {
            let stmt = &program.statements[i];
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
}
