use super::declaration::{DeclarationNode, VariableDeclarationNode};
use super::expression::ExpressionNode;
use super::misc::TypeMarkNode;
use super::statement::StatementNode;




#[derive(Debug)]
pub struct ProcedureHeaderNode {
    pub ident: String,
    pub typ: TypeMarkNode,
    pub parameters: Option<ParameterListNode>
}
#[derive(Debug)]
pub struct ParameterListNode {
    pub parameters: Vec<ParameterNode>
}
#[derive(Debug)]
pub struct ParameterNode(pub VariableDeclarationNode);

#[derive(Debug)]
pub struct ProcedureBodyNode {
    pub declarations: Vec<DeclarationNode>,
    pub statements: Vec<StatementNode>
}

#[derive(Debug)]
pub struct ProcedureCallNode {
    pub ident: String,
    pub arguments: Option<ArgumentListNode>
}

#[derive(Debug)]
pub struct ArgumentListNode(pub Vec<ExpressionNode>);
