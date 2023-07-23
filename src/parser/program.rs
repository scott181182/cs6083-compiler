use crate::lexer::Token;

use super::declaration::DeclarationNode;
use super::statement::StatementNode;
use super::util::{ParseTokens, ParserError, TokenStream};



#[derive(Debug)]
pub struct ProgramNode {
    pub header: ProgramHeaderNode,
    pub body: ProgramBodyNode
}
impl ParseTokens for ProgramNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let header = ProgramHeaderNode::parse(toks)?;
        let body = ProgramBodyNode::parse(toks)?;
        toks.consume_expected(Token::Period)?;

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
        toks.consume_expected(Token::Program)?;
        let ident = toks.consume_identifier()?;
        toks.consume_expected(Token::Is)?;
        
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
                declarations.push(DeclarationNode::parse(toks)?);
            }
        }
        toks.consume_expected(Token::Begin)?;
        loop {
            let next = toks.front();
            if let Some(Token::End) = next {
                break;
            } else if let None = next {
                return Err(ParserError::UnexpectedEndOfFile("begin or declaration".to_owned()));
            } else {
                declarations.push(StatementNode::parse(toks)?);
            }
        }
        toks.consume_expected(Token::End)?;
        toks.consume_expected(Token::Program)?;

        Ok(ProgramBodyNode{ declarations, statements })
    }
}