#[derive(Debug, PartialEq, Clone, Eq, Hash)]
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

    NEQ,
    EQ,

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
    IF,
    ELSE,
    TRUE,
    FALSE,
    RETURN,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut write_buf = |s: String| -> std::fmt::Result { write!(f, "{s}") };
        match self {
            TokenType::SEMICOLON => write_buf(';'.to_string()),
            TokenType::ASSIGN => write_buf('='.to_string()),
            TokenType::LPAREN => write_buf('('.to_string()),
            TokenType::RPAREN => write_buf(')'.to_string()),
            TokenType::COMMA => write_buf(','.to_string()),
            TokenType::RBRACE => write_buf('}'.to_string()),
            TokenType::LBRACE => write_buf('{'.to_string()),
            TokenType::EOF => write_buf('\0'.to_string()),
            TokenType::PLUS => write_buf('+'.to_string()),
            TokenType::MINUS => write_buf('-'.to_string()),
            TokenType::ASTERISK => write_buf('*'.to_string()),
            TokenType::BANG => write_buf('!'.to_string()),
            TokenType::SLASH => write_buf('/'.to_string()),
            TokenType::LT => write_buf('<'.to_string()),
            TokenType::GT => write_buf('>'.to_string()),

            _ => write_buf(format!("{:?}", self)),
        }
    }
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
            "true" => Token::new(TokenType::TRUE, "true".to_string()),
            "false" => Token::new(TokenType::FALSE, "false".to_string()),
            "if" => Token::new(TokenType::IF, "if".to_string()),
            "else" => Token::new(TokenType::ELSE, "else".to_string()),
            "return" => Token::new(TokenType::RETURN, "return".to_string()),

            "!=" => Token::new(TokenType::NEQ, "!=".to_string()),
            "==" => Token::new(TokenType::EQ, "==".to_string()),
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
