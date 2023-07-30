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
    ArrayScalarPlus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    ScalarArrayPlus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    ArrayPlus(Box<AnalyzedArithmetic>, AnalyzedRelation),

    Minus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    ArrayScalarMinus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    ScalarArrayMinus(Box<AnalyzedArithmetic>, AnalyzedRelation),
    ArrayMinus(Box<AnalyzedArithmetic>, AnalyzedRelation),

    Cast(Box<AnalyzedArithmetic>, ValueType),
    Nop(AnalyzedRelation)
}
impl AnalyzedArithmetic {
    pub fn cast(self, typ: ValueType) -> AnalyzedArithmetic {
        AnalyzedArithmetic::Cast(Box::new(self), typ)
    }
}
impl AnalyzeExpression<ArithmeticNode> for AnalyzedArithmetic {
    fn analyze_expr(value: ArithmeticNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        match value {
            ArithmeticNode::Nop(relation) => Ok(AnalyzedArithmetic::Nop(AnalyzedRelation::analyze_expr(relation, ctx)?)),

            ArithmeticNode::Plus(arith, relation) => {
                let arith = AnalyzedArithmetic::analyze_expr(*arith, ctx)?;
                let relation = AnalyzedRelation::analyze_expr(relation, ctx)?;

                let arith_typ = arith.get_type(ctx)?;
                let relation_typ = relation.get_type(ctx)?;
                match (arith_typ, relation_typ) {
                    // Non-casting adds
                    (ValueType::Integer, ValueType::Integer) |
                    (ValueType::Float, ValueType::Float) =>
                        Ok(AnalyzedArithmetic::Plus(Box::new(arith), relation)),
                    (ValueType::Array(box ValueType::Integer, bound1), ValueType::Array(box ValueType::Integer, bound2)) |
                    (ValueType::Array(box ValueType::Float, bound1), ValueType::Array(box ValueType::Float, bound2)) if bound1 == bound2 =>
                        Ok(AnalyzedArithmetic::ArrayPlus(Box::new(arith), relation)),

                    // Scalar + Scalar w/ Casting
                    (ValueType::Integer, ValueType::Float) => Ok(AnalyzedArithmetic::Plus(Box::new(arith.cast(ValueType::Float)), relation)),
                    (ValueType::Float, ValueType::Integer) => Ok(AnalyzedArithmetic::Plus(Box::new(arith), relation.cast(ValueType::Float))),

                    // Array + Scalar
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Integer) =>
                        Ok(AnalyzedArithmetic::ArrayScalarPlus(Box::new(arith), relation)),
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Float) =>
                        Ok(AnalyzedArithmetic::ArrayScalarPlus(Box::new(arith), relation.cast(ValueType::Integer))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Integer) =>
                        Ok(AnalyzedArithmetic::ArrayScalarPlus(Box::new(arith), relation.cast(ValueType::Float))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Float) =>
                        Ok(AnalyzedArithmetic::ArrayScalarPlus(Box::new(arith), relation)),

                    // Array + Scalar
                    (ValueType::Integer, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayPlus(Box::new(arith), relation)),
                    (ValueType::Float, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayPlus(Box::new(arith.cast(ValueType::Integer)), relation)),
                    (ValueType::Integer, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayPlus(Box::new(arith.cast(ValueType::Float)), relation)),
                    (ValueType::Float, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayPlus(Box::new(arith), relation)),

                    (t1, t2) => Err(SemanticError::MismatchedType(t1, t2))
                }
            }
            
            ArithmeticNode::Minus(arith, relation) => {
                let arith = AnalyzedArithmetic::analyze_expr(*arith, ctx)?;
                let relation = AnalyzedRelation::analyze_expr(relation, ctx)?;

                let arith_typ = arith.get_type(ctx)?;
                let relation_typ = relation.get_type(ctx)?;
                match (arith_typ, relation_typ) {
                    // Non-casting subtractions
                    (ValueType::Integer, ValueType::Integer) |
                    (ValueType::Float, ValueType::Float) =>
                        Ok(AnalyzedArithmetic::Minus(Box::new(arith), relation)),
                    (ValueType::Array(box ValueType::Integer, bound1), ValueType::Array(box ValueType::Integer, bound2)) |
                    (ValueType::Array(box ValueType::Float, bound1), ValueType::Array(box ValueType::Float, bound2)) if bound1 == bound2 =>
                        Ok(AnalyzedArithmetic::ArrayMinus(Box::new(arith), relation)),

                    // Scalar - Scalar w/ Casting
                    (ValueType::Integer, ValueType::Float) => Ok(AnalyzedArithmetic::Minus(Box::new(arith.cast(ValueType::Float)), relation)),
                    (ValueType::Float, ValueType::Integer) => Ok(AnalyzedArithmetic::Minus(Box::new(arith), relation.cast(ValueType::Float))),

                    // Array - Scalar
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Integer) =>
                        Ok(AnalyzedArithmetic::ArrayScalarMinus(Box::new(arith), relation)),
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Float) =>
                        Ok(AnalyzedArithmetic::ArrayScalarMinus(Box::new(arith), relation.cast(ValueType::Integer))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Integer) =>
                        Ok(AnalyzedArithmetic::ArrayScalarMinus(Box::new(arith), relation.cast(ValueType::Float))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Float) =>
                        Ok(AnalyzedArithmetic::ArrayScalarMinus(Box::new(arith), relation)),

                    // Array - Scalar
                    (ValueType::Integer, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayMinus(Box::new(arith), relation)),
                    (ValueType::Float, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayMinus(Box::new(arith.cast(ValueType::Integer)), relation)),
                    (ValueType::Integer, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayMinus(Box::new(arith.cast(ValueType::Float)), relation)),
                    (ValueType::Float, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedArithmetic::ScalarArrayMinus(Box::new(arith), relation)),

                    (t1, t2) => Err(SemanticError::MismatchedType(t1, t2))
                }
            }
        }
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedArithmetic::Nop(relation) => relation.get_type(ctx),
            AnalyzedArithmetic::Cast(_, typ) => Ok(typ.clone()),
            
            AnalyzedArithmetic::Plus(_, relation) => relation.get_type(ctx),
            AnalyzedArithmetic::ArrayScalarPlus(arith, _) => arith.get_type(ctx),
            AnalyzedArithmetic::ScalarArrayPlus(_, relation) => relation.get_type(ctx),
            AnalyzedArithmetic::ArrayPlus(_, relation) => relation.get_type(ctx),
            
            AnalyzedArithmetic::Minus(_, relation) => relation.get_type(ctx),
            AnalyzedArithmetic::ArrayScalarMinus(arith, _) => arith.get_type(ctx),
            AnalyzedArithmetic::ScalarArrayMinus(_, relation) => relation.get_type(ctx),
            AnalyzedArithmetic::ArrayMinus(_, relation) => relation.get_type(ctx),
        }
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
    Cast(Box<AnalyzedRelation>, ValueType),
    Nop(TermNode)
}
impl AnalyzedRelation {
    pub fn cast(self, typ: ValueType) -> AnalyzedRelation {
        AnalyzedRelation::Cast(Box::new(self), typ)
    }
}
impl AnalyzeExpression<RelationNode> for AnalyzedRelation {
    fn analyze_expr(value: RelationNode, ctx: &mut Context) -> Result<Self, SemanticError> {
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
impl AnalyzeExpression<TermNode> for AnalyzedTerm {
    fn analyze_expr(value: TermNode, ctx: &mut Context) -> Result<Self, SemanticError> {
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
impl AnalyzeExpression<FactorNode> for AnalyzedFactor {
    fn analyze_expr(value: FactorNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        todo!()
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        todo!()
    }
}
