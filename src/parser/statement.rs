use crate::lexer::Token;

use super::expression::ExpressionNode;
use super::util::{ParserError, ParseTokens, TokenStream, CanParseTokens};



#[derive(Debug)]
pub enum StatementNode {
    Assignment(AssignmentStatementNode),
    If(IfStatementNode),
    Loop(LoopStatementNode),
    Return(ReturnStatementNode)
}
impl ParseTokens for StatementNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        match toks.front() {
            Some(Token::Identifier(_)) => Ok(StatementNode::Assignment(AssignmentStatementNode::parse(toks)?)),
            Some(Token::If) => Ok(StatementNode::If(IfStatementNode::parse(toks)?)),
            Some(Token::For) => Ok(StatementNode::Loop(LoopStatementNode::parse(toks)?)),
            Some(Token::Return) => Ok(StatementNode::Return(ReturnStatementNode::parse(toks)?)),
            Some(tok) => Err(ParserError::UnexpectedToken("statement".to_owned(), tok.clone())),
            None => Err(ParserError::UnexpectedEndOfFile("statement".to_owned()))
        }
    }
}
impl CanParseTokens for StatementNode {
    fn can_parse(toks: &TokenStream) -> bool {
        match toks.front() {
            Some(Token::If) | Some(Token::For) | Some(Token::Return) | Some(Token::Identifier(_)) => true,
            _ => false
        }
    }
}



#[derive(Debug)]
pub struct AssignmentStatementNode {
    pub dest: DestinationNode,
    pub expr: ExpressionNode
}
impl ParseTokens for AssignmentStatementNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let dest = DestinationNode::parse(toks)?;
        toks.consume_expected(Token::Assign)?;
        let expr = ExpressionNode::parse(toks)?;
        Ok(AssignmentStatementNode { dest, expr })
    }
}



#[derive(Debug)]
pub struct DestinationNode {
    pub ident: String,
    pub expr: Option<ExpressionNode>
}
impl ParseTokens for DestinationNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let ident = toks.consume_identifier()?;

        if toks.consume_if(&Token::LeftBracket) {
            let expr = ExpressionNode::parse(toks)?;
            toks.consume_expected(Token::RightBracket)?;
            Ok(DestinationNode{ ident, expr: Some(expr) })
        } else {
            Ok(DestinationNode{ ident, expr: None })
        }
    }
}



#[derive(Debug)]
pub struct IfStatementNode {
    pub cond: ExpressionNode,
    pub then_block: Vec<StatementNode>,
    pub else_block: Option<Vec<StatementNode>>
}
impl ParseTokens for IfStatementNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let mut then_block = Vec::new();
        let mut else_block = Vec::new();

        toks.consume_expected(Token::If)?;
        toks.consume_expected(Token::LeftParen)?;
        let cond = ExpressionNode::parse(toks)?;
        toks.consume_expected(Token::RightParen)?;
        toks.consume_expected(Token::Then)?;

        // Parse then body
        while StatementNode::can_parse(toks) {
            then_block.push(StatementNode::parse(toks)?);
            toks.consume_expected(Token::Semicolon)?;
        }

        let has_else = toks.consume_if(&Token::Else);
        // Parse else body
        if has_else {
            while StatementNode::can_parse(toks) {
                else_block.push(StatementNode::parse(toks)?);
                toks.consume_expected(Token::Semicolon)?;
            }
        }

        toks.consume_expected(Token::End)?;
        toks.consume_expected(Token::If)?;

        Ok(IfStatementNode {
            cond,
            then_block,
            else_block: if has_else { Some(else_block) } else { None }
        })
    }
}



#[derive(Debug)]
pub struct LoopStatementNode {
    pub assign: AssignmentStatementNode,
    pub cond: ExpressionNode,
    pub block: Vec<StatementNode>
}
impl ParseTokens for LoopStatementNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let mut block = Vec::new();

        toks.consume_expected(Token::For)?;
        toks.consume_expected(Token::LeftParen)?;
        let assign = AssignmentStatementNode::parse(toks)?;
        toks.consume_expected(Token::Semicolon)?;
        let cond = ExpressionNode::parse(toks)?;
        toks.consume_expected(Token::RightParen)?;

        // Parse the body
        while StatementNode::can_parse(toks) {
            block.push(StatementNode::parse(toks)?);
            toks.consume_expected(Token::Semicolon)?;
        }

        toks.consume_expected(Token::End)?;
        toks.consume_expected(Token::For)?;

        Ok(LoopStatementNode { assign, cond, block })
    }
}



#[derive(Debug)]
pub struct ReturnStatementNode(pub ExpressionNode);

impl ParseTokens for ReturnStatementNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        toks.consume_expected(Token::Return)?;
        let expr = ExpressionNode::parse(toks)?;

        Ok(ReturnStatementNode(expr))
    }
}