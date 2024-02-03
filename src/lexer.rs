use crate::token::{Token, TokenType};

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

        let ident = &self.input[position..self.position];
        return Token::from(ident);
    }

    fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            _ => TokenType::IDENT,
        }
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
        self.skip_whitespace();
        let tok = match self.ch {
            ';' => Token::from(';'),
            '=' => Token::from('='),
            '(' => Token::from('('),
            ')' => Token::from(')'),
            ',' => Token::from(','),
            '}' => Token::from('}'),
            '{' => Token::from('{'),

            '+' => Token::from('+'),
            '-' => Token::from('-'),
            '!' => Token::from('!'),
            '*' => Token::from('*'),
            '/' => Token::from('/'),
            '<' => Token::from('<'),
            '>' => Token::from('>'),

            '\0' => Token::new(TokenType::EOF, "".to_string()),
            _ => {
                if Lexer::is_letter(self.ch) {
                    return self.read_identifier(); // return to  avoid double read_char
                } else if self.ch.is_ascii_digit() {
                    return self.read_number();
                } else {
                    Token::new(TokenType::ILLEGAL, "".to_string())
                }
            }
        };

        self.read_char();

        return tok;
    }
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_next_token() {
        let input = "let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        }   else {
            return false;
        }
        "
        .to_string();

        let tests: Vec<Token> = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::from('!'),
            Token::from('-'),
            Token::from('/'),
            Token::from('*'),
            Token::from('5'),
            Token::from(';'),
            Token::from('5'),
            Token::from('<'),
            Token::new(TokenType::INT, "10".to_string()),
            Token::from('>'),
            Token::from('5'),
            Token::from(';'),
            Token::from("if"),
            Token::from('('),
            Token::from('5'),
            Token::from('<'),
            Token::new(TokenType::INT, "10".to_string()),
            Token::from(')'),
            Token::from('{'),
            Token::from("return"),
            Token::from("true"),
            Token::from(';'),
            Token::from('}'),
            Token::from("else"),
            Token::from('{'),
            Token::from("return"),
            Token::from("false"),
            Token::from(';'),
            Token::from('}'),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l = Lexer::new(input.to_string());

        for tt in tests {
            let tok = l.next_token();
            println!("{:?}", tok);
            println!("{:?}", tt);
            assert_eq!(tt.token_type, tok.token_type);
            assert_eq!(tt.literal, tok.literal);
        }
    }
}
