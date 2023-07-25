use crate::lexer::Token;

use super::procedure::ProcedureCallNode;
use super::misc::{NameNode, NumberNode};
use super::util::{ParserError, ParseTokens, TokenStream, CanParseTokens};



#[derive(Debug)]
pub enum ExpressionNode {
    And(Box<ExpressionNode>, ArithmeticNode),
    Or(Box<ExpressionNode>, ArithmeticNode),
    Not(ArithmeticNode),
    Nop(ArithmeticNode)
}
impl ParseTokens for ExpressionNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        // match toks.front() {
        //     Some(Token::Identifier(_)) => Ok(StatementNode::Assignment(AssignmentStatementNode::parse(toks)?)),
        //     Some(Token::If) => Ok(StatementNode::If(IfStatementNode::parse(toks)?)),
        //     Some(Token::For) => Ok(StatementNode::Loop(LoopStatementNode::parse(toks)?)),
        //     Some(Token::Return) => Ok(StatementNode::Return(ReturnStatementNode::parse(toks)?)),
        //     Some(tok) => Err(ParserError::UnexpectedToken("statement".to_owned(), tok.clone())),
        //     None => Err(ParserError::UnexpectedEndOfFile("statement".to_owned()))
        // }
        Err(ParserError::ExpectedEndOfFile(Token::Period))
    }
}



#[derive(Debug)]
pub enum ArithmeticNode {
    Plus(Box<ArithmeticNode>, RelationNode),
    Minus(Box<ArithmeticNode>, RelationNode),
    Nop(RelationNode)
}
#[derive(Debug)]
pub enum RelationNode {
    LessThan(Box<RelationNode>, TermNode),
    LessThanEq(Box<RelationNode>, TermNode),
    GreaterThan(Box<RelationNode>, TermNode),
    GreaterThanEq(Box<RelationNode>, TermNode),
    Equal(Box<RelationNode>, TermNode),
    NotEqual(Box<RelationNode>, TermNode),
    Nop(TermNode)
}

#[derive(Debug)]
pub enum TermNode {
    Multiply(Box<TermNode>, FactorNode),
    Divide(Box<TermNode>, FactorNode),
    Nop(FactorNode)
}

#[derive(Debug)]
pub enum FactorNode {
    Paren(Box<ExpressionNode>),
    Call(ProcedureCallNode),
    NegateName(NameNode),
    NegateNumber(NumberNode),
    String(String),
    True,
    False
}