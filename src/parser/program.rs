use crate::lexer::{TokenStream, Token};

use super::declaration::DeclarationNode;
use super::statement::StatementNode;
use super::util::{ParseTokens, ParserError, expect_token};



#[derive(Debug)]
pub struct ProgramNode {
    pub header: ProgramHeaderNode,
    pub body: ProgramBodyNode
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



#[derive(Debug)]
pub struct ProgramHeaderNode {
    pub ident: String
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



#[derive(Debug)]
pub struct ProgramBodyNode {
    pub declarations: Vec<DeclarationNode>,
    pub statements: Vec<StatementNode>
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