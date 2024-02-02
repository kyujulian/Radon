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
    literal: String,
}

impl From<char> for Token {
    fn from(ch: char) -> Token {
        match ch {
            ';' => Token::new(TokenType::SEMICOLON, ';'.to_string()),
            '=' => Token::new(TokenType::ASSIGN, '='.to_string()),
            '(' => Token::new(TokenType::LPAREN, '('.to_string()),
            ')' => Token::new(TokenType::RPAREN, ')'.to_string()),
            '+' => Token::new(TokenType::PLUS, '+'.to_string()),
            '-' => Token::new(TokenType::MINUS, '-'.to_string()),
            ',' => Token::new(TokenType::COMMA, ','.to_string()),
            '}' => Token::new(TokenType::RBRACE, '}'.to_string()),
            '{' => Token::new(TokenType::LBRACE, '{'.to_string()),
            '\0' => Token::new(TokenType::EOF, "".to_string()),
            _ => Token::new(TokenType::ILLEGAL, "".to_string()),
        }
    }
}

impl Token {
    fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
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
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l = lexer::Lexer::new(input.to_string());

        for tt in tests {
            let tok = l.next_token();

            assert_eq!(
                tok.token_type, tt.token_type,
                "expected token type {:?}, got {:?}",
                tt.token_type, tok.token_type
            );

            assert_eq!(
                tok.literal, tt.literal,
                "expected literal {:?}, got {:?}",
                tt.literal, tok.literal
            );
        }
    }
}
