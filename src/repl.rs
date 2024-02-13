use crate::lexer::Lexer;
use crate::parser;
use crate::token::TokenType;
use std::io::{self, Write};
const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let lex = Lexer::new(input.clone());
        let mut parser = parser::Parser::new(lex);

        let program = match parser.parse_program() {
            Some(p) => {
                parser::check_parser_errors(&parser);

                p
            }
            None => panic!("parser.parse_program() failed"),
        };

        if parser.errors().len() == 0 {
            print_parser_errors(parser.errors());
        }
        println!("{}", program);
        // loop {
        //     let tok = lex.next_token();
        //     if tok.token_type == TokenType::EOF {
        //         break;
        //     }
        //     println!("{:?}", tok);
        // }

        if input.trim() == "exit" {
            break;
        }
    }
}

fn print_parser_errors(errors: &[String]) {
    for error in errors {
        println!("      {}\n", error);
    }
}
