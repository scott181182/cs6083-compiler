use crate::lexer::Token;
use crate::parser::declaration::DeclarationNode;

use super::util::{Analyze, Context, SemanticError, ValueType, NamedValueType};



#[derive(Debug)]
pub enum AnalyzedDeclaration {
    Procedure{ ident: String, args: Vec<NamedValueType>, ret: ValueType },
    Variable{ ident: String, typ: ValueType }
}
impl Analyze<AnalyzedDeclaration> for DeclarationNode {
    fn analyze(self, ctx: &mut Context) -> Result<Self, SemanticError> {
        match self {
            DeclarationNode::Variable { global, variable } => {
                ctx.set_type(true, variable.ident.clone(), (&variable.typ).into())?;
            },
            DeclarationNode::Procedure { global, procedure } => {

            }
        };
        Ok(ValueType::Void)
    }
}

#[derive(Debug)]
pub struct ProcedureDeclarationNode {
    pub header: ProcedureHeaderNode,
    pub body: ProcedureBodyNode,
}
impl ParseTokens for ProcedureDeclarationNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let header = ProcedureHeaderNode::parse(toks)?;
        let body = ProcedureBodyNode::parse(toks)?;
        Ok(ProcedureDeclarationNode{ header, body })
    }
}


#[derive(Debug)]
pub struct VariableDeclarationNode {
    pub ident: String,
    pub typ: TypeMarkNode,
    pub bound: Option<BoundNode>
}
impl ParseTokens for VariableDeclarationNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        toks.consume_expected(Token::Variable)?;
        let ident = toks.consume_identifier()?;
        toks.consume_expected(Token::Colon)?;
        let typ = TypeMarkNode::parse(toks)?;

        let has_bounds = toks.consume_if(&Token::LeftBracket);
        let bound = if has_bounds {
            let bound = BoundNode::parse(toks)?;
            toks.consume_expected(Token::RightBracket)?;
            Some(bound)
        } else { None };

        Ok(VariableDeclarationNode{ ident, typ, bound })
    }
}
