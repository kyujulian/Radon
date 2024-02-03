use super::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0'
        } else {
            self.ch = self.input.as_bytes()[self.read_position] as char;
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while Lexer::is_letter(self.ch) {
            self.read_char();
        }

        return match &self.input[position..self.position] {
            "let" => Token::new(TokenType::LET, "let".to_string()),
            "fn" => Token::new(TokenType::FUNCTION, "fn".to_string()),
            _ => Token::new(
                TokenType::IDENT,
                String::from(&self.input[position..self.position]),
            ),
        };
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        let number_string = String::from(&self.input[position..self.position]);

        Token::new(TokenType::INT, number_string)
    }

    pub fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            ';' => Token::new(TokenType::SEMICOLON, ";".to_string()),
            '=' => Token::new(TokenType::ASSIGN, "=".to_string()),
            '(' => Token::new(TokenType::LPAREN, "(".to_string()),
            ')' => Token::new(TokenType::RPAREN, ")".to_string()),
            '+' => Token::new(TokenType::PLUS, "+".to_string()),
            '-' => Token::new(TokenType::MINUS, "-".to_string()),
            ',' => Token::new(TokenType::COMMA, ",".to_string()),
            '}' => Token::new(TokenType::RBRACE, "}".to_string()),
            '{' => Token::new(TokenType::LBRACE, "{".to_string()),
            '\0' => Token::new(TokenType::EOF, "".to_string()),
            _ => {
                if Lexer::is_letter(self.ch) {
                    self.read_identifier()
                } else if self.ch.is_ascii_digit() {
                    self.read_number()
                } else {
                    Token::new(TokenType::ILLEGAL, "".to_string())
                }
            }
        };

        self.read_char();

        return tok;
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }
}
