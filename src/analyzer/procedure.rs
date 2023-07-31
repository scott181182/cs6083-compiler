use crate::parser::declaration::ProcedureDeclarationNode;
use crate::parser::procedure::{ParameterListNode, ProcedureCallNode};

use super::error::SemanticError;
use super::expression::AnalyzedExpression;
use super::statement::AnalyzedBlock;
use super::util::{Analyze, Context, ProcedureSignature, Scope, ValueType, NamedValueType, ScopeContext, AnalyzeExpression};



#[derive(Debug)]
pub struct AnalyzedProcedure {
    pub ident: String,
    pub args: Vec<NamedValueType>,
    pub declarations: ScopeContext,
    pub procedures: Vec<Box::<AnalyzedProcedure>>,
    pub block: AnalyzedBlock
}
impl Analyze<AnalyzedProcedure> for ProcedureDeclarationNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<AnalyzedProcedure, SemanticError> {
        // Analyze Signature
        let args = match self.header.parameters {
            Some(ParameterListNode(params)) => params.into_iter()
                .map(|param| param.try_into())
                .collect::<Result<Vec<NamedValueType>, SemanticError>>()?,
            None => Vec::new()
        };
        let ident = self.header.ident;
        let ret: ValueType = self.header.typ.into();
        let sig = ProcedureSignature(args.clone(), ret.clone());
        ctx.set_proc(scope == &Scope::Global, ident.clone(), sig)?;



        // Start on body
        ctx.start_stack(ret);

        for arg in args.iter() {
            ctx.set_type(false, arg.0.clone(), arg.1.clone())?;
        }

        let mut procedures = Vec::new();
        for decl in self.body.declarations {
            if let Some(proc) = decl.analyze(ctx, &Scope::Local)? {
                procedures.push(Box::new(proc));
            }
        }

        let block = self.body.statements.analyze(ctx, &Scope::Local)?;
        
        Ok(AnalyzedProcedure {
            ident,
            args,
            declarations: ctx.end_stack()?,
            procedures,
            block
        })
    }
}



#[derive(Debug)]
pub struct AnalyzedProcedureCall {
    pub ident: String,
    pub args: Vec<AnalyzedExpression>,
    pub ret: ValueType
}
impl AnalyzeExpression<ProcedureCallNode> for AnalyzedProcedureCall {
    fn analyze_expr(value: ProcedureCallNode, ctx: &mut Context) -> Result<Self, SemanticError> {
        let sig = ctx.get_procedure_signature(&value.ident)?.clone();
        let ident = value.ident;

        let passed_args = value.arguments.map_or(Vec::new(), |node| node.0);

        if passed_args.len() != sig.0.len() {
            return Err(SemanticError::IncorrectNumberOfArgument(sig.0.len(), passed_args.len()));
        }
        let args = passed_args.into_iter().zip(sig.0.into_iter())
            .map(move |(parg, sarg)| {
                let expr = AnalyzedExpression::analyze_expr(parg, ctx)?;
                let expr_typ = expr.get_type(ctx)?;
                if expr_typ != sarg.1 {
                    Err(SemanticError::MismatchedType(expr_typ, sarg.1))
                } else {
                    Ok(expr)
                }
            })
            .collect::<Result<Vec<AnalyzedExpression>, SemanticError>>()?;

        Ok(AnalyzedProcedureCall { ident, args, ret: sig.1 })
    }

    fn get_type(&self, _ctx: &Context) -> Result<ValueType, SemanticError> {
        Ok(self.ret.clone())
    }
}
