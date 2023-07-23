use super::misc::{TypeMarkNode, BoundNode};
use super::procedure::{ProcedureHeaderNode, ProcedureBodyNode};




#[derive(Debug)]
pub enum DeclarationNode {
    Procedure{ global: bool, procedure: ProcedureDeclarationNode },
    Variable{ global: bool, variable: VariableDeclarationNode }
}

#[derive(Debug)]
pub struct VariableDeclarationNode {
    pub ident: String,
    pub typ: TypeMarkNode,
    pub bound: Option<BoundNode>
}

#[derive(Debug)]
pub struct ProcedureDeclarationNode {
    pub header: ProcedureHeaderNode,
    pub body: ProcedureBodyNode,
}