use crate::lexer::Lexer;
use crate::parser;
use std::io::{self, Write};
const PROMPT: &str = ">> ";

pub fn start() {
    println!("Radon Programming Language");
    println!("Version 0.1.0");
    println!("__________________________");
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
