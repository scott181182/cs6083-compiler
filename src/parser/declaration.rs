use crate::lexer::Token;

use super::misc::{TypeMarkNode, BoundNode};
use super::procedure::{ProcedureHeaderNode, ProcedureBodyNode};
use super::util::{ParserError, ParseTokens, TokenStream};




#[derive(Debug)]
pub enum DeclarationNode {
    Procedure{ global: bool, procedure: ProcedureDeclarationNode },
    Variable{ global: bool, variable: VariableDeclarationNode }
}
impl ParseTokens for DeclarationNode {
    fn parse(toks: &mut TokenStream) -> Result<Self, ParserError> {
        let global = toks.consume_if(&Token::Global);
        match toks.front() {
            Some(Token::Procedure) => Ok(DeclarationNode::Procedure { global, procedure: ProcedureDeclarationNode::parse(toks)? }),
            Some(Token::Variable) => Ok(DeclarationNode::Variable { global, variable: VariableDeclarationNode::parse(toks)? }),
            Some(tok) => Err(ParserError::UnexpectedToken("declaration".to_owned(), tok.clone())),
            None => Err(ParserError::UnexpectedEndOfFile("declaration".to_owned()))
        }
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
