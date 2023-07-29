use crate::parser::expression::{ExpressionNode, ArithmeticNode, RelationNode, TermNode, FactorNode};
use crate::parser::misc::{NameNode, NumberNode};
use crate::parser::procedure::ProcedureCallNode;

use super::error::SemanticError;
use super::util::{AnalyzeExpression, Context, ValueType, TypeHint};



#[derive(Debug)]
pub enum AnalyzedExpression {
    BitwiseAnd(Box<AnalyzedExpression>, AnalyzedArithmetic),
    BitwiseOr(Box<AnalyzedExpression>, AnalyzedArithmetic),
    BitwiseNot(AnalyzedArithmetic),
    LogicalAnd(Box<AnalyzedExpression>, AnalyzedArithmetic),
    LogicalOr(Box<AnalyzedExpression>, AnalyzedArithmetic),
    LogicalNot(AnalyzedArithmetic),
    Cast(Box<AnalyzedExpression>, ValueType),
    Nop(AnalyzedArithmetic)
}
impl AnalyzeExpression<AnalyzedExpression> for ExpressionNode {
    fn analyze_expr(self, ctx: &mut Context, hint: TypeHint) -> Result<AnalyzedExpression, SemanticError> {
        match ctx {
            ExpressionNode::And(expr, node) => {
                let (arith, arith_typ) = node.analyze_expr(ctx, hint)?;
                let new_hint = hint.unify(arith_typ)?;
                
            }
        }
    }
}

#[derive(Debug)]
pub enum AnalyzedArithmetic {
    Plus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    Minus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    Nop(AnalyzedRelation)
}
impl AnalyzeExpression<(AnalyzedArithmetic, ValueType)> for ArithmeticNode {
    fn analyze_expr(self, ctx: &mut Context, expected_typ: TypeHint) -> Result<(AnalyzedArithmetic, ValueType), SemanticError> {
        todo!()
    }
}


#[derive(Debug)]
pub enum AnalyzedRelation {
    LessThan(Box<AnalyzedRelation>, TermNode),
    LessThanEq(Box<AnalyzedRelation>, TermNode),
    GreaterThan(Box<AnalyzedRelation>, TermNode),
    GreaterThanEq(Box<AnalyzedRelation>, TermNode),
    Equal(Box<AnalyzedRelation>, TermNode),
    NotEqual(Box<AnalyzedRelation>, TermNode),
    Nop(TermNode)
}
impl AnalyzeExpression<AnalyzedRelation> for RelationNode {
    fn analyze_expr(self, ctx: &mut Context, expected_typ: TypeHint) -> Result<AnalyzedRelation, SemanticError> {
        todo!()
    }
}



#[derive(Debug)]
pub enum AnalyzedTerm {
    Multiply(Box<TermNode>, AnalyzedFactor),
    Divide(Box<TermNode>, AnalyzedFactor),
    Nop(AnalyzedFactor)
}
impl AnalyzeExpression<AnalyzedTerm> for TermNode {
    fn analyze_expr(self, ctx: &mut Context, expected_typ: TypeHint) -> Result<AnalyzedTerm, SemanticError> {
        todo!()
    }
}


#[derive(Debug)]
pub enum AnalyzedFactor {
    Paren(Box<AnalyzedExpression>),
    Call(ProcedureCallNode),
    Name(NameNode),
    NegateName(NameNode),
    Number(NumberNode),
    NegateNumber(NumberNode),
    String(String),
    True,
    False
}
impl AnalyzeExpression<AnalyzedFactor> for FactorNode {
    fn analyze_expr(self, ctx: &mut Context, expected_typ: TypeHint) -> Result<AnalyzedFactor, SemanticError> {
        todo!()
    }
}
