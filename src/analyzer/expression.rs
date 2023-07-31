use crate::parser::expression::{ExpressionNode, ArithmeticNode, RelationNode, TermNode, FactorNode};
use crate::parser::misc::{NameNode, NumberNode};

use super::error::SemanticError;
use super::procedure::AnalyzedProcedureCall;
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
    LessThan(Box<AnalyzedRelation>, AnalyzedTerm),
    LessThanEq(Box<AnalyzedRelation>, AnalyzedTerm),
    GreaterThan(Box<AnalyzedRelation>, AnalyzedTerm),
    GreaterThanEq(Box<AnalyzedRelation>, AnalyzedTerm),
    Equal(Box<AnalyzedRelation>, AnalyzedTerm),
    NotEqual(Box<AnalyzedRelation>, AnalyzedTerm),
    Cast(Box<AnalyzedRelation>, ValueType),
    Nop(AnalyzedTerm)
}
impl AnalyzedRelation {
    pub fn cast(self, typ: ValueType) -> AnalyzedRelation {
        AnalyzedRelation::Cast(Box::new(self), typ)
    }

    pub fn try_compatible(relation: RelationNode, term: TermNode, equality: bool, ctx: &mut Context) -> Result<(AnalyzedRelation, AnalyzedTerm), SemanticError> {
        let mut relation = AnalyzedRelation::analyze_expr(relation, ctx)?;
        let mut term = AnalyzedTerm::analyze_expr(term, ctx)?;

        relation = match relation.get_type(ctx)? {
            ValueType::Integer | ValueType::Float => relation,
            ValueType::Boolean => relation.cast(ValueType::Integer),
            ValueType::String if equality => relation,
            typ => return Err(SemanticError::IncorrectType("relation".to_owned(), typ))
        };
        term = match term.get_type(ctx)? {
            ValueType::Integer | ValueType::Float => term,
            ValueType::Boolean => term.cast(ValueType::Integer),
            ValueType::String if equality => term,
            typ => return Err(SemanticError::IncorrectType("relation".to_owned(), typ))
        };

        let relation_typ = relation.get_type(ctx)?;
        let term_typ = term.get_type(ctx)?;

        if relation_typ == term_typ {
            Ok((relation, term))
        } else {
            match (relation_typ, term_typ) {
                (ValueType::Integer, ValueType::Float) => Ok((relation.cast(ValueType::Float), term)),
                (ValueType::Float, ValueType::Integer) => Ok((relation, term.cast(ValueType::Float))),
                (t1, t2) => Err(SemanticError::MismatchedType(t1, t2))
            }
        }
    }
}
impl AnalyzeExpression<RelationNode> for AnalyzedRelation {
    fn analyze_expr(value: RelationNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        match value {
            RelationNode::Nop(term) => Ok(AnalyzedRelation::Nop(AnalyzedTerm::analyze_expr(term, ctx)?)),

            RelationNode::Equal(box relation, term) => {
                let (relation, term) = AnalyzedRelation::try_compatible(relation, term, true, ctx)?;
                Ok(AnalyzedRelation::Equal(Box::new(relation), term))
            },
            RelationNode::NotEqual(box relation, term) => {
                let (relation, term) = AnalyzedRelation::try_compatible(relation, term, true, ctx)?;
                Ok(AnalyzedRelation::NotEqual(Box::new(relation), term))
            },
            RelationNode::LessThan(box relation, term) => {
                let (relation, term) = AnalyzedRelation::try_compatible(relation, term, false, ctx)?;
                Ok(AnalyzedRelation::LessThan(Box::new(relation), term))
            },
            RelationNode::LessThanEq(box relation, term) => {
                let (relation, term) = AnalyzedRelation::try_compatible(relation, term, false, ctx)?;
                Ok(AnalyzedRelation::LessThanEq(Box::new(relation), term))
            },
            RelationNode::GreaterThan(box relation, term) => {
                let (relation, term) = AnalyzedRelation::try_compatible(relation, term, false, ctx)?;
                Ok(AnalyzedRelation::GreaterThan(Box::new(relation), term))
            },
            RelationNode::GreaterThanEq(box relation, term) => {
                let (relation, term) = AnalyzedRelation::try_compatible(relation, term, false, ctx)?;
                Ok(AnalyzedRelation::GreaterThanEq(Box::new(relation), term))
            }
        }
    }
    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedRelation::Nop(term) => term.get_type(ctx),
            AnalyzedRelation::Cast(_, typ) => Ok(typ.clone()),
            _ => Ok(ValueType::Boolean)
        }
    }
}



#[derive(Debug)]
pub enum AnalyzedTerm {
    Multiply(Box<AnalyzedTerm>, AnalyzedFactor),
    ArrayScalarMultiply(Box<AnalyzedTerm>, AnalyzedFactor),
    ScalarArrayMultiply(Box<AnalyzedTerm>, AnalyzedFactor),
    ArrayMultiply(Box<AnalyzedTerm>, AnalyzedFactor),

    Divide(Box<AnalyzedTerm>, AnalyzedFactor),
    ArrayScalarDivide(Box<AnalyzedTerm>, AnalyzedFactor),
    ScalarArrayDivide(Box<AnalyzedTerm>, AnalyzedFactor),
    ArrayDivide(Box<AnalyzedTerm>, AnalyzedFactor),

    Cast(Box<AnalyzedTerm>, ValueType),
    Nop(AnalyzedFactor)
}
impl AnalyzedTerm {
    pub fn cast(self, typ: ValueType) -> AnalyzedTerm {
        AnalyzedTerm::Cast(Box::new(self), typ)
    }
}
impl AnalyzeExpression<TermNode> for AnalyzedTerm {
    fn analyze_expr(value: TermNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        match value {
            TermNode::Nop(factor) => Ok(AnalyzedTerm::Nop(AnalyzedFactor::analyze_expr(factor, ctx)?)),

            TermNode::Multiply(term, factor) => {
                let term = AnalyzedTerm::analyze_expr(*term, ctx)?;
                let factor = AnalyzedFactor::analyze_expr(factor, ctx)?;

                let term_typ = term.get_type(ctx)?;
                let factor_typ = factor.get_type(ctx)?;
                match (term_typ, factor_typ) {
                    // Non-casting adds
                    (ValueType::Integer, ValueType::Integer) |
                    (ValueType::Float, ValueType::Float) =>
                        Ok(AnalyzedTerm::Multiply(Box::new(term), factor)),
                    (ValueType::Array(box ValueType::Integer, bound1), ValueType::Array(box ValueType::Integer, bound2)) |
                    (ValueType::Array(box ValueType::Float, bound1), ValueType::Array(box ValueType::Float, bound2)) if bound1 == bound2 =>
                        Ok(AnalyzedTerm::ArrayMultiply(Box::new(term), factor)),

                    // Scalar + Scalar w/ Casting
                    (ValueType::Integer, ValueType::Float) => Ok(AnalyzedTerm::Multiply(Box::new(term.cast(ValueType::Float)), factor)),
                    (ValueType::Float, ValueType::Integer) => Ok(AnalyzedTerm::Multiply(Box::new(term), factor.cast(ValueType::Float))),

                    // Array + Scalar
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Integer) =>
                        Ok(AnalyzedTerm::ArrayScalarMultiply(Box::new(term), factor)),
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Float) =>
                        Ok(AnalyzedTerm::ArrayScalarMultiply(Box::new(term), factor.cast(ValueType::Integer))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Integer) =>
                        Ok(AnalyzedTerm::ArrayScalarMultiply(Box::new(term), factor.cast(ValueType::Float))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Float) =>
                        Ok(AnalyzedTerm::ArrayScalarMultiply(Box::new(term), factor)),

                    // Array + Scalar
                    (ValueType::Integer, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayMultiply(Box::new(term), factor)),
                    (ValueType::Float, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayMultiply(Box::new(term.cast(ValueType::Integer)), factor)),
                    (ValueType::Integer, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayMultiply(Box::new(term.cast(ValueType::Float)), factor)),
                    (ValueType::Float, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayMultiply(Box::new(term), factor)),

                    (t1, t2) => Err(SemanticError::MismatchedType(t1, t2))
                }
            }
            
            TermNode::Divide(term, factor) => {
                let term = AnalyzedTerm::analyze_expr(*term, ctx)?;
                let factor = AnalyzedFactor::analyze_expr(factor, ctx)?;

                let term_typ = term.get_type(ctx)?;
                let factor_typ = factor.get_type(ctx)?;
                match (term_typ, factor_typ) {
                    // Non-casting subtractions
                    (ValueType::Integer, ValueType::Integer) |
                    (ValueType::Float, ValueType::Float) =>
                        Ok(AnalyzedTerm::Divide(Box::new(term), factor)),
                    (ValueType::Array(box ValueType::Integer, bound1), ValueType::Array(box ValueType::Integer, bound2)) |
                    (ValueType::Array(box ValueType::Float, bound1), ValueType::Array(box ValueType::Float, bound2)) if bound1 == bound2 =>
                        Ok(AnalyzedTerm::ArrayDivide(Box::new(term), factor)),

                    // Scalar - Scalar w/ Casting
                    (ValueType::Integer, ValueType::Float) => Ok(AnalyzedTerm::Divide(Box::new(term.cast(ValueType::Float)), factor)),
                    (ValueType::Float, ValueType::Integer) => Ok(AnalyzedTerm::Divide(Box::new(term), factor.cast(ValueType::Float))),

                    // Array - Scalar
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Integer) =>
                        Ok(AnalyzedTerm::ArrayScalarDivide(Box::new(term), factor)),
                    (ValueType::Array(box ValueType::Integer, _), ValueType::Float) =>
                        Ok(AnalyzedTerm::ArrayScalarDivide(Box::new(term), factor.cast(ValueType::Integer))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Integer) =>
                        Ok(AnalyzedTerm::ArrayScalarDivide(Box::new(term), factor.cast(ValueType::Float))),
                    (ValueType::Array(box ValueType::Float, _), ValueType::Float) =>
                        Ok(AnalyzedTerm::ArrayScalarDivide(Box::new(term), factor)),

                    // Array - Scalar
                    (ValueType::Integer, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayDivide(Box::new(term), factor)),
                    (ValueType::Float, ValueType::Array(box ValueType::Integer, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayDivide(Box::new(term.cast(ValueType::Integer)), factor)),
                    (ValueType::Integer, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayDivide(Box::new(term.cast(ValueType::Float)), factor)),
                    (ValueType::Float, ValueType::Array(box ValueType::Float, _)) =>
                        Ok(AnalyzedTerm::ScalarArrayDivide(Box::new(term), factor)),

                    (t1, t2) => Err(SemanticError::MismatchedType(t1, t2))
                }
            }
        }
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedTerm::Nop(factor) => factor.get_type(ctx),
            AnalyzedTerm::Cast(_, typ) => Ok(typ.clone()),
            
            AnalyzedTerm::Multiply(_, factor) => factor.get_type(ctx),
            AnalyzedTerm::ArrayScalarMultiply(term, _) => term.get_type(ctx),
            AnalyzedTerm::ScalarArrayMultiply(_, factor) => factor.get_type(ctx),
            AnalyzedTerm::ArrayMultiply(_, factor) => factor.get_type(ctx),
            
            AnalyzedTerm::Divide(_, factor) => factor.get_type(ctx),
            AnalyzedTerm::ArrayScalarDivide(term, _) => term.get_type(ctx),
            AnalyzedTerm::ScalarArrayDivide(_, factor) => factor.get_type(ctx),
            AnalyzedTerm::ArrayDivide(_, factor) => factor.get_type(ctx),
        }
    }
}


#[derive(Debug)]
pub enum AnalyzedFactor {
    Paren(Box<AnalyzedExpression>),
    Call(AnalyzedProcedureCall),
    Name(AnalyzedName),
    NegateName(AnalyzedName),
    Number(AnalyzedNumber),
    NegateNumber(AnalyzedNumber),
    String(String),
    True,
    False,

    Cast(Box<AnalyzedFactor>, ValueType)
}
impl AnalyzedFactor {
    pub fn cast(self, typ: ValueType) -> AnalyzedFactor {
        AnalyzedFactor::Cast(Box::new(self), typ)
    }
}
impl AnalyzeExpression<FactorNode> for AnalyzedFactor {
    fn analyze_expr(value: FactorNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        match value {
            FactorNode::True => Ok(AnalyzedFactor::True),
            FactorNode::False => Ok(AnalyzedFactor::False),
            FactorNode::String(s) => Ok(AnalyzedFactor::String(s)),
            FactorNode::Number(num) => Ok(AnalyzedFactor::Number(AnalyzedNumber::analyze_expr(num, ctx)?)),
            FactorNode::NegateNumber(num) =>  Ok(AnalyzedFactor::NegateNumber(AnalyzedNumber::analyze_expr(num, ctx)?)),

            FactorNode::Name(name) => Ok(AnalyzedFactor::Name(AnalyzedName::analyze_expr(name, ctx)?)),
            FactorNode::NegateName(name) =>  {
                let name = AnalyzedName::analyze_expr(name, ctx)?;
                let typ = name.get_type(ctx)?.clone();
                if matches!(typ, ValueType::Integer | ValueType::Float) {
                    Ok(AnalyzedFactor::NegateName(name))
                } else {
                    Err(SemanticError::IncorrectType("number".to_owned(), typ))
                }
            },
            FactorNode::Paren(box expr) => Ok(AnalyzedFactor::Paren(Box::new(AnalyzedExpression::analyze_expr(expr, ctx)?))),
            FactorNode::Call(proc) => Ok(AnalyzedFactor::Call(AnalyzedProcedureCall::analyze_expr(proc, ctx)?)),
        }
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedFactor::True | AnalyzedFactor::False => Ok(ValueType::Boolean),
            AnalyzedFactor::String(_) => Ok(ValueType::String),
            AnalyzedFactor::Number(num) |
            AnalyzedFactor::NegateNumber(num) =>
                num.get_type(ctx),
            AnalyzedFactor::Name(name) | 
            AnalyzedFactor::NegateName(name) =>
                name.get_type(ctx),
            AnalyzedFactor::Paren(box expr) => expr.get_type(ctx),
            AnalyzedFactor::Call(proc) => proc.get_type(ctx),

            AnalyzedFactor::Cast(_, typ) => Ok(typ.clone())
        }
    }
}



#[derive(Debug)]
pub enum AnalyzedNumber {
    Integer(i64),
    Float(f64)
}
impl AnalyzeExpression<NumberNode> for AnalyzedNumber {
    fn analyze_expr(value: NumberNode, _ctx: &mut Context) -> Result<Self, SemanticError> {
        if value.0.contains(".") {
            Ok(AnalyzedNumber::Float(value.try_into()?))
        } else {
            Ok(AnalyzedNumber::Integer(value.try_into()?))
        }
    }

    fn get_type(&self, _ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedNumber::Integer(_) => Ok(ValueType::Integer),
            AnalyzedNumber::Float(_) => Ok(ValueType::Float)
        }
    }
}


#[derive(Debug)]
pub enum AnalyzedName {
    Name(String),
    Indexed(String, Box<AnalyzedExpression>)
}
impl AnalyzeExpression<NameNode> for AnalyzedName {
    fn analyze_expr(value: NameNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        if let Some(box expr) = value.expr {
            let expr = AnalyzedExpression::analyze_expr(expr, ctx)?;
            let expr_typ = expr.get_type(ctx)?;
            if expr_typ != ValueType::Integer {
                Err(SemanticError::NonIntegerIndex(value.ident, expr_typ))
            } else {
                Ok(AnalyzedName::Indexed(value.ident, Box::new(expr)))
            }
        } else {
            Ok(AnalyzedName::Name(value.ident))
        }
    }

    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError> {
        match self {
            AnalyzedName::Name(ident) => ctx.get_variable_type(ident).map(ValueType::clone),
            AnalyzedName::Indexed(ident, _) => {
                let arr_typ = ctx.get_variable_type(ident)?;
                if let ValueType::Array(box val_typ, _) = arr_typ {
                    Ok(val_typ.clone())
                } else {
                    Err(SemanticError::IndexOnNonArray(ident.to_owned()))
                }
            }
        }
    }
}
