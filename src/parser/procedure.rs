use crate::lexer::Token;

use super::declaration::{DeclarationNode, VariableDeclarationNode};
use super::expression::ExpressionNode;
use super::misc::TypeMarkNode;
use super::statement::StatementNode;
use super::util::{ParserError, ParseTokens, TokenStream};



#[derive(Debug)]
pub struct ProcedureHeaderNode {
    pub ident: String,
    pub typ: TypeMarkNode,
    pub parameters: Option<ParameterListNode>
}
impl ParseTokens for ProcedureHeaderNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        toks.consume_expected(Token::Procedure)?;
        let ident = toks.consume_identifier()?;
        toks.consume_expected(Token::Colon)?;
        let typ = TypeMarkNode::parse(toks)?;
        toks.consume_expected(Token::LeftParen)?;

        match toks.front() {
            Some(Token::RightParen) => {
                toks.pop_front();
                Ok(ProcedureHeaderNode{ ident, typ, parameters: None })
            },
            Some(Token::Variable) => {
                let parameters = ParameterListNode::parse(toks)?;
                toks.consume_expected(Token::RightParen)?;
                Ok(ProcedureHeaderNode { ident, typ, parameters: Some(parameters) })
            },
            Some(tok) => Err(ParserError::UnexpectedToken("right paren or variable declaration".to_owned(), tok.clone())),
            None => Err(ParserError::UnexpectedEndOfFile("right paren or variable declaration".to_owned()))
        }
    }
}



#[derive(Debug)]
pub struct ParameterListNode(pub Vec<ParameterNode>);

impl ParseTokens for ParameterListNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let mut parameters = Vec::new();

        parameters.push(ParameterNode::parse(toks)?);

        while let Some(&Token::Comma) = toks.front() {
            toks.consume_expected(Token::Comma)?;
            parameters.push(ParameterNode::parse(toks)?);
        }

        Ok(ParameterListNode(parameters))
    }
}




#[derive(Debug)]
pub struct ParameterNode(pub VariableDeclarationNode);

impl ParseTokens for ParameterNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        VariableDeclarationNode::parse(toks).map(ParameterNode)
    }
}



#[derive(Debug)]
pub struct ProcedureBodyNode {
    pub declarations: Vec<DeclarationNode>,
    pub statements: Vec<StatementNode>
}
impl ParseTokens for ProcedureBodyNode {
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
                toks.consume_expected(Token::Semicolon)?;
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
                statements.push(StatementNode::parse(toks)?);
                toks.consume_expected(Token::Semicolon)?;
            }
        }
        toks.consume_expected(Token::End)?;
        toks.consume_expected(Token::Procedure)?;

        Ok(ProcedureBodyNode{ declarations, statements })
    }
}



#[derive(Debug)]
pub struct ProcedureCallNode {
    pub ident: String,
    pub arguments: Option<ArgumentListNode>
}
impl ParseTokens for ProcedureCallNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let ident = toks.consume_identifier()?;
        toks.consume_expected(Token::LeftParen)?;

        match toks.front() {
            Some(Token::RightParen) => {
                toks.pop_front();
                Ok(ProcedureCallNode{ ident, arguments: None })
            },
            Some(Token::Variable) => {
                let arguments = ArgumentListNode::parse(toks)?;
                toks.consume_expected(Token::RightParen)?;
                Ok(ProcedureCallNode { ident, arguments: Some(arguments) })
            },
            Some(tok) => Err(ParserError::UnexpectedToken("right paren or argument list".to_owned(), tok.clone())),
            None => Err(ParserError::UnexpectedEndOfFile("right paren or argument list".to_owned()))
        }
    }
}



#[derive(Debug)]
pub struct ArgumentListNode(pub Vec<ExpressionNode>);

impl ParseTokens for ArgumentListNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let mut arguments = Vec::new();

        arguments.push(ExpressionNode::parse(toks)?);

        while let Some(&Token::Comma) = toks.front() {
            toks.consume_expected(Token::Comma)?;
            arguments.push(ExpressionNode::parse(toks)?);
        }

        Ok(ArgumentListNode(arguments))
    }
}
