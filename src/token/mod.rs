pub mod lexer;

#[derive(Debug)]
enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    MINUS,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenType::ILLEGAL, TokenType::ILLEGAL) => true,
            (TokenType::EOF, TokenType::EOF) => true,
            (TokenType::IDENT, TokenType::IDENT) => true,
            (TokenType::INT, TokenType::INT) => true,
            (TokenType::ASSIGN, TokenType::ASSIGN) => true,
            (TokenType::MINUS, TokenType::MINUS) => true,
            (TokenType::PLUS, TokenType::PLUS) => true,
            (TokenType::COMMA, TokenType::COMMA) => true,
            (TokenType::SEMICOLON, TokenType::SEMICOLON) => true,
            (TokenType::LPAREN, TokenType::LPAREN) => true,
            (TokenType::RPAREN, TokenType::RPAREN) => true,
            (TokenType::LBRACE, TokenType::LBRACE) => true,
            (TokenType::RBRACE, TokenType::RBRACE) => true,
            (TokenType::FUNCTION, TokenType::FUNCTION) => true,
            (TokenType::LET, TokenType::LET) => true,
            _ => false,
        }
    }
}

pub struct Token {
    token_type: TokenType,
}

impl From<char> for Token {
    fn from(ch: char) -> Token {
        match ch {
            ';' => Token::new(TokenType::SEMICOLON),
            '=' => Token::new(TokenType::ASSIGN),
            '(' => Token::new(TokenType::LPAREN),
            ')' => Token::new(TokenType::RPAREN),
            '+' => Token::new(TokenType::PLUS),
            '-' => Token::new(TokenType::MINUS),
            ',' => Token::new(TokenType::COMMA),
            '}' => Token::new(TokenType::RBRACE),
            '{' => Token::new(TokenType::LBRACE),
            '\0' => Token::new(TokenType::EOF),
            _ => Token::new(TokenType::ILLEGAL),
        }
    }
}

impl Token {
    fn new(token_type: TokenType) -> Token {
        Token { token_type }
    }

    fn as_char(&self) -> char {
        match self.token_type {
            TokenType::ASSIGN => '=',
            TokenType::PLUS => '+',
            TokenType::COMMA => ',',
            TokenType::SEMICOLON => ';',
            TokenType::LPAREN => '(',
            TokenType::RPAREN => ')',
            TokenType::LBRACE => '{',
            TokenType::RBRACE => '}',
            TokenType::EOF => '\0',
            _ => '\0',
        }
    }

    fn as_str(&self) -> &'static str {
        match self.token_type {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",
            TokenType::ASSIGN => "=",
            TokenType::MINUS => "-",
            TokenType::PLUS => "+",
            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",
            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",
            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests: Vec<Token> = vec![
            Token::new(TokenType::ASSIGN),
            Token::new(TokenType::PLUS),
            Token::new(TokenType::LPAREN),
            Token::new(TokenType::RPAREN),
            Token::new(TokenType::LBRACE),
            Token::new(TokenType::RBRACE),
            Token::new(TokenType::COMMA),
            Token::new(TokenType::SEMICOLON),
            Token::new(TokenType::EOF),
        ];

        let mut l = lexer::Lexer::new(input.to_string());

        for tt in tests {
            let tok = l.next_token();

            assert_eq!(
                tok.token_type, tt.token_type,
                "expected token type {:?}, got {:?}",
                tt.token_type, tok.token_type
            );
        }
    }
}
