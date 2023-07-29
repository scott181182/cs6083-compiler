use crate::parser::statement::{StatementNode, AssignmentStatementNode, IfStatementNode, DestinationNode, LoopStatementNode, ReturnStatementNode};

use super::error::SemanticError;
use super::expression::AnalyzedExpression;
use super::util::{Analyze, Context, Scope, ValueType, AnalyzeExpression, TypeHint};



#[derive(Debug)]
pub struct AnalyzedBlock(pub Vec<AnalyzedStatement>);

#[derive(Debug)]
pub enum AnalyzedStatement {
    Assignment(AnalyzedAssignment),
    If(AnalyzedIf),
    Loop(AnalyzedLoop),
    Return(AnalyzedReturn)
}

impl Analyze<AnalyzedBlock> for Vec<StatementNode> {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedBlock, SemanticError> {
        let stmts = self.into_iter()
            .map(|stmt| stmt.analyze(ctx, scope))
            .collect::<Result<Vec<AnalyzedStatement>, SemanticError>>()?;

        Ok(AnalyzedBlock(stmts))
    }
}
impl Analyze<AnalyzedStatement> for StatementNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedStatement, SemanticError> {
        let stmt = match self {
            StatementNode::Assignment(node) => AnalyzedStatement::Assignment(node.analyze(ctx, scope)?),
            StatementNode::If(node) => AnalyzedStatement::If(node.analyze(ctx, scope)?),
            StatementNode::Loop(node) => AnalyzedStatement::Loop(node.analyze(ctx, scope)?),
            StatementNode::Return(node) => AnalyzedStatement::Return(node.analyze(ctx, scope)?)
        };
        Ok(stmt)
    }
}



#[derive(Debug)]
pub struct AnalyzedAssignment{
    pub dest: AnalyzedDestination,
    pub expr: AnalyzedExpression
}
#[derive(Debug)]
pub struct AnalyzedDestination {
    pub ident: String,
    pub expr: Option<AnalyzedExpression>,
    pub typ: ValueType
}
impl Analyze<AnalyzedDestination> for DestinationNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedDestination, SemanticError> {
        let typ = ctx.get_variable_type(&self.ident)
            .ok_or(SemanticError::UndeclaredAssignment(self.ident.clone()))?
            .clone();
        
        if let Some(idx_expr_node) = self.expr {
            if let ValueType::Array(arr_type, _) = typ {
                let idx_expr = idx_expr_node.analyze_expr(ctx, TypeHint::Type(ValueType::Integer))?;
                Ok(AnalyzedDestination { ident: self.ident, expr: Some(idx_expr), typ: *arr_type })
            } else {
                return Err(SemanticError::IndexOnNonArray(self.ident));
            }
        } else {
            Ok(AnalyzedDestination { ident: self.ident, expr: None, typ })
        }
    }
}

impl Analyze<AnalyzedAssignment> for AssignmentStatementNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedAssignment, SemanticError> {
        let dest = self.dest.analyze(ctx, scope)?;
        let expr = self.expr.analyze_expr(ctx, TypeHint::Type(dest.typ.clone()))?;

        Ok(AnalyzedAssignment{ dest, expr })
    }
}



#[derive(Debug)]
pub struct AnalyzedIf {
    pub cond: AnalyzedExpression,
    pub then_block: AnalyzedBlock,
    pub else_block: Option<AnalyzedBlock>
}

impl Analyze<AnalyzedIf> for IfStatementNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedIf, SemanticError> {
        let cond = self.cond.analyze_expr(ctx, TypeHint::Conditional)?;

        let then_block = self.then_block.analyze(ctx, scope)?;
        let else_block = self.else_block.map(move |blk| blk.analyze(ctx, scope)).transpose()?;

        Ok(AnalyzedIf { cond, then_block, else_block })
    }
}



#[derive(Debug)]
pub struct AnalyzedLoop {
    pub init: Box<AnalyzedAssignment>,
    pub cond: AnalyzedExpression,
    pub block: AnalyzedBlock
}
impl Analyze<AnalyzedLoop> for LoopStatementNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedLoop, SemanticError> {
        let init = self.assign.analyze(ctx, scope)?;
        let cond = self.cond.analyze_expr(ctx, TypeHint::Conditional)?;

        let block = self.block.analyze(ctx, scope)?;

        Ok(AnalyzedLoop { init: Box::new(init), cond, block })
    }
}



#[derive(Debug)]
pub struct AnalyzedReturn {
    pub expr: AnalyzedExpression
}
impl Analyze<AnalyzedReturn> for ReturnStatementNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedReturn, SemanticError> {
        let expected_ret = ctx.get_return_type().clone();
        if expected_ret == ValueType::Void {
            
        }
        let expr = self.0.analyze_expr(ctx, TypeHint::Type(expected_ret))?;

        Ok(AnalyzedReturn { expr })
    }
}
