use crate::parser::declaration::ProcedureDeclarationNode;
use crate::parser::procedure::ParameterListNode;

use super::error::SemanticError;
use super::statement::AnalyzedBlock;
use super::util::{Analyze, Context, ProcedureSignature, Scope, ValueType, NamedValueType, ScopeContext};



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


