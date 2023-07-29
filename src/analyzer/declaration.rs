use crate::parser::procedure::ParameterNode;
use crate::parser::declaration::DeclarationNode;

use super::procedure::AnalyzedProcedure;
use super::util::{Analyze, Context, SemanticError, Scope, ValueType, NamedValueType};



impl Analyze<Option<AnalyzedProcedure>> for DeclarationNode {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<Option<AnalyzedProcedure>, SemanticError> {
        match self {
            DeclarationNode::Variable { global, variable } => {
                ctx.set_type(scope == &Scope::Global || global, variable.ident, variable.typ.into())?;
                Ok(None)
            },
            DeclarationNode::Procedure { global, procedure } => {
                let scop = if global { &Scope::Global } else { scope };
                Ok(Some(procedure.analyze(ctx, scop)?))
            }
        }
    }
}
impl TryFrom<ParameterNode> for NamedValueType {
    type Error = SemanticError;

    fn try_from(value: ParameterNode) -> Result<Self, SemanticError> {
        if let Some(bound) = value.0.bound {
            Ok(NamedValueType(value.0.ident, ValueType::Array(Box::new(value.0.typ.into()), bound.0.try_into()?)))
        } else {
            Ok(NamedValueType(value.0.ident, value.0.typ.into()))
        }
    }
}
