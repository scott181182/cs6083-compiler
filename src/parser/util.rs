use std::collections::VecDeque;

use thiserror::Error;

use crate::lexer::Token;



#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Expected {0} token, but found {1:?}")]
    UnexpectedToken(String, Token),
    #[error("Expected {0} token, but found EOF")]
    UnexpectedEndOfFile(String),
    #[error("Expected end of file, but found {0:?}")]
    ExpectedEndOfFile(Token)
}

pub struct TokenStream {
    tokens: VecDeque<Token>
}
impl TokenStream {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        TokenStream { tokens }
    }

    pub fn pop_front(&mut self) -> Option<Token> { self.pop_front() }
    pub fn front(&self) -> Option<&Token> { self.front() }
    pub fn is_empty(&self) -> bool { self.is_empty() }

    pub fn consume_identifier(&mut self) -> Result<String, ParserError> {
        match self.pop_front() {
            Some(Token::Identifier(name)) => Ok(name),
            Some(tok) => Err(ParserError::UnexpectedToken("identifier".to_owned(), tok)),
            _ => Err(ParserError::UnexpectedEndOfFile("identifier".to_owned()))
        }
    }
    pub fn consume_expected(&mut self, expected: Token) -> Result<(), ParserError> {
        if let Some(tok) = self.pop_front() {
            if tok != expected {
                Err(ParserError::UnexpectedToken(format!("{:?}", expected), tok))
            } else {
                Ok(())
            }
        } else {
            Err(ParserError::UnexpectedEndOfFile(format!("{:?}", expected)))
        }
    }
    pub fn consume_if(&mut self, expected: &Token) -> bool {
        match self.front() {
            Some(tok) if tok == expected => {
                self.pop_front();
                true
            },
            _ => false
        }
    }
}



pub trait ParseTokens: Sized {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError>;
}
