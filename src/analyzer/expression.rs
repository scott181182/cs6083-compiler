use crate::parser::expression::{ExpressionNode, ArithmeticNode, RelationNode, TermNode, FactorNode};
use crate::parser::misc::{NameNode, NumberNode};
use crate::parser::procedure::ProcedureCallNode;

use super::error::SemanticError;
use super::util::{AnalyzeExpression, Context, ValueType};



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
impl AnalyzedExpression {
    pub fn cast(self, typ: ValueType) -> AnalyzedExpression {
        AnalyzedExpression::Cast(Box::new(self), typ)
    }
    pub fn as_conditional(self, ctx: &Context) -> Result<AnalyzedExpression, SemanticError> {
        match self.get_type(ctx)? {
            ValueType::Boolean => Ok(self),
            ValueType::Integer => Ok(self.cast(ValueType::Boolean)),
            typ => Err(SemanticError::IncorrectType("integer or boolean".to_owned(), typ))
        }
    }
}
impl AnalyzeExpression<ExpressionNode> for AnalyzedExpression {
    fn analyze_expr(value: ExpressionNode, ctx: &mut Context) -> Result<AnalyzedExpression, SemanticError> {
        match value {
            ExpressionNode::Nop(node) => Ok(AnalyzedExpression::Nop(AnalyzedArithmetic::analyze_expr(node, ctx)?)),
            ExpressionNode::Not(node) => {
                let arith = AnalyzedArithmetic::analyze_expr(node, ctx)?;
                match arith.get_type(ctx)? {
                    ValueType::Integer => Ok(AnalyzedExpression::BitwiseNot(arith)),
                    ValueType::Boolean => Ok(AnalyzedExpression::LogicalNot(arith)),
                    typ => Err(SemanticError::IncorrectType("integer or boolean".to_owned(), typ))
                }
            },
            ExpressionNode::And(expr, node) => {
                let arith = AnalyzedArithmetic::analyze_expr(node, ctx)?;
                let expr = AnalyzedExpression::analyze_expr(*expr, ctx)?;

                match (arith.get_type(ctx)?, expr.get_type(ctx)?) {
                    (ValueType::Integer, ValueType::Integer) => Ok(AnalyzedExpression::BitwiseAnd(Box::new(expr), arith)),
                    (ValueType::Boolean, ValueType::Boolean) => Ok(AnalyzedExpression::LogicalAnd(Box::new(expr), arith)),
                    (arith_typ, expr_typ) => Err(SemanticError::MismatchedType(expr_typ, arith_typ))
                }
            },
            ExpressionNode::Or(expr, node) => {
                let arith = AnalyzedArithmetic::analyze_expr(node, ctx)?;
                let expr = AnalyzedExpression::analyze_expr(*expr, ctx)?;

                match (arith.get_type(ctx)?, expr.get_type(ctx)?) {
                    (ValueType::Integer, ValueType::Integer) => Ok(AnalyzedExpression::BitwiseOr(Box::new(expr), arith)),
                    (ValueType::Boolean, ValueType::Boolean) => Ok(AnalyzedExpression::LogicalOr(Box::new(expr), arith)),
                    (arith_typ, expr_typ) => Err(SemanticError::MismatchedType(expr_typ, arith_typ))
                }
            }
        }
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedExpression::BitwiseAnd(_, _) |
            AnalyzedExpression::BitwiseOr(_, _) |
            AnalyzedExpression::BitwiseNot(_) => Ok(ValueType::Integer),

            AnalyzedExpression::LogicalAnd(_, _) |
            AnalyzedExpression::LogicalOr(_, _) |
            AnalyzedExpression::LogicalNot(_) => Ok(ValueType::Boolean),

            AnalyzedExpression::Cast(_, typ) => Ok(typ.clone()),
            AnalyzedExpression::Nop(arith) => arith.get_type(ctx)
        }
    }
    
}

#[derive(Debug)]
pub enum AnalyzedArithmetic {
    Plus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    Minus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    Nop(AnalyzedRelation)
}
impl AnalyzeExpression<ArithmeticNode> for AnalyzedArithmetic {
    fn analyze_expr(value: ArithmeticNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        todo!()
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
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
    fn analyze_expr(value: AnalyzedRelation, ctx: &mut Context) -> Result<Self, SemanticError> {
        todo!()
    }
    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
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
    fn analyze_expr(value: AnalyzedTerm, ctx: &mut Context) -> Result<Self, SemanticError> {
        todo!()
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
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
    fn analyze_expr(value: AnalyzedFactor, ctx: &mut Context) -> Result<Self, SemanticError> {
        todo!()
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        todo!()
    }
}
