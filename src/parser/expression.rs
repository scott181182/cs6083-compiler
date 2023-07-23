use super::procedure::ProcedureCallNode;
use super::misc::{NameNode, NumberNode};



#[derive(Debug)]
pub enum ExpressionNode {
    And(Box<ExpressionNode>, ArithmeticNode),
    Or(Box<ExpressionNode>, ArithmeticNode),
    Not(ArithmeticNode),
    Nop(ArithmeticNode)
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