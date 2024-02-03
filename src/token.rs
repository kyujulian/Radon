#[derive(Debug)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    MINUS,
    PLUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

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
            (TokenType::BANG, TokenType::BANG) => true,
            (TokenType::ASTERISK, TokenType::ASTERISK) => true,
            (TokenType::SLASH, TokenType::SLASH) => true,
            (TokenType::LT, TokenType::LT) => true,
            (TokenType::GT, TokenType::GT) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl From<char> for Token {
    fn from(ch: char) -> Token {
        match ch {
            ';' => Token::new(TokenType::SEMICOLON, ";".to_string()),
            '=' => Token::new(TokenType::ASSIGN, "=".to_string()),
            '(' => Token::new(TokenType::LPAREN, "(".to_string()),
            ')' => Token::new(TokenType::RPAREN, ")".to_string()),
            ',' => Token::new(TokenType::COMMA, ",".to_string()),
            '}' => Token::new(TokenType::RBRACE, "}".to_string()),
            '{' => Token::new(TokenType::LBRACE, "{".to_string()),
            '\0' => Token::new(TokenType::EOF, "".to_string()),

            '+' => Token::new(TokenType::PLUS, "+".to_string()),
            '-' => Token::new(TokenType::MINUS, "-".to_string()),
            '*' => Token::new(TokenType::ASTERISK, "*".to_string()),
            '!' => Token::new(TokenType::BANG, "!".to_string()),
            '/' => Token::new(TokenType::SLASH, "/".to_string()),
            '<' => Token::new(TokenType::LT, "<".to_string()),
            '>' => Token::new(TokenType::GT, ">".to_string()),

            c => {
                if c.is_digit(10) {
                    Token::new(TokenType::INT, c.to_string())
                } else {
                    Token::new(TokenType::ILLEGAL, c.to_string())
                }
            }
        }
    }
}

impl From<&str> for Token {
    fn from(ident: &str) -> Token {
        match ident {
            "fn" => Token::new(TokenType::FUNCTION, "fn".to_string()),
            "let" => Token::new(TokenType::LET, "let".to_string()),
            _ => Token::new(TokenType::IDENT, ident.to_string()),
        }
    }
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }

    pub fn as_char(&self) -> char {
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
}

#[cfg(test)]
mod tests {}
