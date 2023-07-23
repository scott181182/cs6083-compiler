


use crate::lexer::{Token, TokenStream};
use crate::parser::util::{ParserError, expect_token};
use crate::parser::nodes::*;



pub trait ParseTokens: Sized {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError>;
}


impl ParseTokens for ProgramNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let header = ProgramHeaderNode::parse(toks)?;
        let body = ProgramBodyNode::parse(toks)?;
        expect_token(toks, Token::Period)?;

        if let Some(next) = toks.pop_front() {
            return Err(ParserError::ExpectedEndOfFile(next))
        }

        Ok(ProgramNode{ header, body })
    }
}
impl ParseTokens for ProgramHeaderNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        expect_token(toks, Token::Program)?;
        let ident = match toks.pop_front() {
            Some(Token::Identifier(name)) => name,
            Some(tok) => return Err(ParserError::UnexpectedToken("identifier".to_owned(), tok)),
            _ => return Err(ParserError::UnexpectedEndOfFile("identifier".to_owned()))
        };
        expect_token(toks, Token::Is)?;
        
        Ok(ProgramHeaderNode{ ident })
    }
}
impl ParseTokens for ProgramBodyNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let mut declarations = Vec::new();
        let mut statements = Vec::new();

        loop {
            let next = toks.front();
            if let Some(Token::Begin) = next {
                break;
            } else if let None = next {
                return Err(ParserError::UnexpectedEndOfFile("begin or declaration".to_owned()));
            } else {
                // declarations.push(DeclarationNode::parse(toks));
            }
        }
        expect_token(toks, Token::Begin)?;
        loop {
            let next = toks.front();
            if let Some(Token::End) = next {
                break;
            } else if let None = next {
                return Err(ParserError::UnexpectedEndOfFile("begin or declaration".to_owned()));
            } else {
                // declarations.push(StatementNode::parse(toks));
            }
        }
        expect_token(toks, Token::End)?;
        expect_token(toks, Token::Program)?;

        Ok(ProgramBodyNode{ declarations, statements })
    }
}