use crate::parser::expression::ExpressionNode;

use super::util::{Analyze, Context, SemanticError, Scope, ValueType};



#[derive(Debug)]
pub struct AnalyzedExpression {
    pub typ: ValueType,
    pub expr: ExpressionNode
}
impl Analyze<AnalyzedExpression> for ExpressionNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedExpression, SemanticError> {
        todo!()
    }
}

// #[derive(Debug)]
// pub enum ExpressionNode {
//     And(Box<ExpressionNode>, ArithmeticNode),
//     Or(Box<ExpressionNode>, ArithmeticNode),
//     Not(ArithmeticNode),
//     Nop(ArithmeticNode)
// }
// impl ParseTokens for ExpressionNode {
//     fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
//         // Interestingly, `not` is only allowed at the beginning of an expression, and not inside its individual terms.
//         let mut expr = if toks.consume_if(&Token::Not) {
//             ExpressionNode::Not(ArithmeticNode::parse(toks)?)
//         } else {
//             ExpressionNode::Nop(ArithmeticNode::parse(toks)?)
//         };

//         loop {
//             expr = match toks.front() {
//                 Some(Token::Ampersand) => {
//                     toks.pop_front();
//                     let next = ArithmeticNode::parse(toks)?;
//                     ExpressionNode::And(Box::new(expr), next)
//                 },
//                 Some(Token::Bar) => {
//                     toks.pop_front();
//                     let next = ArithmeticNode::parse(toks)?;
//                     ExpressionNode::Or(Box::new(expr), next)
//                 },
//                 _ => return Ok(expr)
//             }
//         }
//     }
// }



// #[derive(Debug)]
// pub enum ArithmeticNode {
//     Plus(Box<ArithmeticNode>, RelationNode),
//     Minus(Box<ArithmeticNode>, RelationNode),
//     Nop(RelationNode)
// }
// impl ParseTokens for ArithmeticNode {
//     fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
//         let mut arith_op = ArithmeticNode::Nop(RelationNode::parse(toks)?);

//         loop {
//             arith_op = match toks.front() {
//                 Some(Token::Plus) => {
//                     toks.pop_front();
//                     let next = RelationNode::parse(toks)?;
//                     ArithmeticNode::Plus(Box::new(arith_op), next)
//                 },
//                 Some(Token::Minus) => {
//                     toks.pop_front();
//                     let next = RelationNode::parse(toks)?;
//                     ArithmeticNode::Minus(Box::new(arith_op), next)
//                 },
//                 _ => return Ok(arith_op)
//             }
//         }
//     }
// }



// #[derive(Debug)]
// pub enum RelationNode {
//     LessThan(Box<RelationNode>, TermNode),
//     LessThanEq(Box<RelationNode>, TermNode),
//     GreaterThan(Box<RelationNode>, TermNode),
//     GreaterThanEq(Box<RelationNode>, TermNode),
//     Equal(Box<RelationNode>, TermNode),
//     NotEqual(Box<RelationNode>, TermNode),
//     Nop(TermNode)
// }
// impl ParseTokens for RelationNode {
//     fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
//         let mut relation = RelationNode::Nop(TermNode::parse(toks)?);

//         loop {
//             relation = match toks.front() {
//                 Some(Token::LessThan) => {
//                     toks.pop_front();
//                     let next = TermNode::parse(toks)?;
//                     RelationNode::LessThan(Box::new(relation), next)
//                 },
//                 Some(Token::LessThanEq) => {
//                     toks.pop_front();
//                     let next = TermNode::parse(toks)?;
//                     RelationNode::LessThanEq(Box::new(relation), next)
//                 },
//                 Some(Token::GreaterThan) => {
//                     toks.pop_front();
//                     let next = TermNode::parse(toks)?;
//                     RelationNode::GreaterThan(Box::new(relation), next)
//                 },
//                 Some(Token::GreaterThanEq) => {
//                     toks.pop_front();
//                     let next = TermNode::parse(toks)?;
//                     RelationNode::GreaterThanEq(Box::new(relation), next)
//                 },
//                 Some(Token::DoubleEqual) => {
//                     toks.pop_front();
//                     let next = TermNode::parse(toks)?;
//                     RelationNode::Equal(Box::new(relation), next)
//                 },
//                 Some(Token::BangEqual) => {
//                     toks.pop_front();
//                     let next = TermNode::parse(toks)?;
//                     RelationNode::NotEqual(Box::new(relation), next)
//                 },
//                 _ => return Ok(relation)
//             }
//         }
//     }
// }



// #[derive(Debug)]
// pub enum TermNode {
//     Multiply(Box<TermNode>, FactorNode),
//     Divide(Box<TermNode>, FactorNode),
//     Nop(FactorNode)
// }
// impl ParseTokens for TermNode {
//     fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
//         let mut term = TermNode::Nop(FactorNode::parse(toks)?);

//         loop {
//             term = match toks.front() {
//                 Some(Token::Asterisk) => {
//                     toks.pop_front();
//                     let next = FactorNode::parse(toks)?;
//                     TermNode::Multiply(Box::new(term), next)
//                 },
//                 Some(Token::Slash) => {
//                     toks.pop_front();
//                     let next = FactorNode::parse(toks)?;
//                     TermNode::Divide(Box::new(term), next)
//                 },
//                 _ => return Ok(term)
//             }
//         }
//     }
// }



// #[derive(Debug)]
// pub enum FactorNode {
//     Paren(Box<ExpressionNode>),
//     Call(ProcedureCallNode),
//     Name(NameNode),
//     NegateName(NameNode),
//     Number(NumberNode),
//     NegateNumber(NumberNode),
//     String(String),
//     True,
//     False
// }
// impl ParseTokens for FactorNode {
//     fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
//         match toks.pop_front() {
//             Some(Token::LeftParen) => {
//                 let expr = ExpressionNode::parse(toks)?;
//                 toks.consume_expected(Token::RightParen)?;
//                 Ok(FactorNode::Paren(Box::new(expr)))
//             },
//             Some(Token::True) => Ok(FactorNode::True),
//             Some(Token::False) => Ok(FactorNode::False),
//             Some(Token::StringLiteral(content)) => Ok(FactorNode::String(content)),
//             Some(Token::Minus) => {
//                 match toks.front() {
//                     Some(Token::Identifier(_)) => Ok(FactorNode::NegateName(NameNode::parse(toks)?)),
//                     Some(Token::NumberLiteral(_)) => Ok(FactorNode::NegateNumber(NumberNode::parse(toks)?)),
//                     Some(tok) => Err(ParserError::UnexpectedToken("negated factor".to_owned(), tok.clone())),
//                     None => Err(ParserError::UnexpectedEndOfFile("negated factor".to_owned()))
//                 }
//             },
//             Some(Token::Identifier(ident)) => {
//                 // Need to determine if this is a name or a procedure call.
//                 match toks.front() {
//                     Some(Token::LeftParen) => {
//                         toks.push_front(Token::Identifier(ident));
//                         Ok(FactorNode::Call(ProcedureCallNode::parse(toks)?))
//                     },
//                     _ => {
//                         toks.push_front(Token::Identifier(ident));
//                         Ok(FactorNode::Name(NameNode::parse(toks)?))
//                     }
//                 }
//             },
//             Some(Token::NumberLiteral(value)) => {
//                 toks.push_front(Token::NumberLiteral(value));
//                 Ok(FactorNode::Number(NumberNode::parse(toks)?))
//             },
            
//             Some(tok) => Err(ParserError::UnexpectedToken("factor".to_owned(), tok)),
//             None => Err(ParserError::UnexpectedEndOfFile("factor".to_owned()))
//         }
//     }
// }