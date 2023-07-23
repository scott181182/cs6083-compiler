use thiserror::Error;

use crate::lexer::{Token, TokenStream};



#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Expected {0} token, but found {1:?}")]
    UnexpectedToken(String, Token),
    #[error("Expected {0} token, but found EOF")]
    UnexpectedEndOfFile(String),
    #[error("Expected end of file, but found {0:?}")]
    ExpectedEndOfFile(Token)
}



pub trait ParseTokens: Sized {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError>;
}

pub fn expect_token(toks: &mut TokenStream, expected: Token) -> Result<(), ParserError> {
    if let Some(tok) = toks.pop_front() {
        if tok != expected {
            Err(ParserError::UnexpectedToken(format!("{:?}", expected), tok))
        } else {
            Ok(())
        }
    } else {
        Err(ParserError::UnexpectedEndOfFile(format!("{:?}", expected)))
    }
}