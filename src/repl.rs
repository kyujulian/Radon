use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{self, Write};
const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let mut lex = Lexer::new(input.clone());

        loop {
            let tok = lex.next_token();
            if tok.token_type == TokenType::EOF {
                break;
            }
            println!("{:?}", tok);
        }

        if input.trim() == "exit" {
            break;
        }
    }
}
