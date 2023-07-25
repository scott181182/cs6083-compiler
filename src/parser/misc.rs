use crate::lexer::Token;
use super::expression::ExpressionNode;
use super::util::{ParserError, ParseTokens, TokenStream};



#[derive(Debug)]
pub enum TypeMarkNode {
    Integer,
    Float,
    String,
    Bool
}
impl ParseTokens for TypeMarkNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        match toks.pop_front() {
            Some(Token::Integer) => Ok(TypeMarkNode::Integer),
            Some(Token::Float) => Ok(TypeMarkNode::Float),
            Some(Token::String) => Ok(TypeMarkNode::String),
            Some(Token::Boolean) => Ok(TypeMarkNode::Bool),
            Some(tok) => Err(ParserError::UnexpectedToken("type mark".to_owned(), tok)),
            None => Err(ParserError::UnexpectedEndOfFile("type mark".to_owned()))
        }
    }
}



#[derive(Debug)]
pub struct BoundNode(pub NumberNode);

impl ParseTokens for BoundNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        NumberNode::parse(toks).map(BoundNode)
    }
}




#[derive(Debug)]
pub struct NumberNode(pub String);

impl ParseTokens for NumberNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        match toks.pop_front() {
            Some(Token::NumberLiteral(value)) => Ok(NumberNode(value)),
            Some(tok) => Err(ParserError::UnexpectedToken("number literal".to_owned(), tok)),
            None => Err(ParserError::UnexpectedEndOfFile("number literal".to_owned()))
        }
    }
}



#[derive(Debug)]
pub struct NameNode {
    pub ident: String,
    pub expr: Option<Box<ExpressionNode>>
}
impl ParseTokens for NameNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let ident = toks.consume_identifier()?;
        if toks.consume_if(&Token::LeftBracket) {
            let expr = ExpressionNode::parse(toks)?;
            toks.consume_expected(Token::RightBracket)?;
            Ok(NameNode{ ident, expr: Some(Box::new(expr)) })
        } else {
            Ok(NameNode{ ident, expr: None })
        }
    }
}
