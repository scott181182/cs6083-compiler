use super::expression::ExpressionNode;



#[derive(Debug)]
pub enum StatementNode {
    Assignment(AssignmentStatementNode),
    If(IfStatementNode),
    Loop(LoopStatementNode),
    Return(ReturnStatementNode)
}

#[derive(Debug)]
pub struct AssignmentStatementNode {
    pub dest: DestinationNode,
    pub expr: ExpressionNode
}
#[derive(Debug)]
pub struct DestinationNode {
    pub ident: String,
    pub expr: Option<ExpressionNode>
}

#[derive(Debug)]
pub struct IfStatementNode {
    pub cond: ExpressionNode,
    pub then_block: Vec<StatementNode>,
    pub else_block: Option<Vec<StatementNode>>
}

#[derive(Debug)]
pub struct LoopStatementNode {
    pub assign: AssignmentStatementNode,
    pub cond: ExpressionNode,
    pub block: Vec<StatementNode>
}

#[derive(Debug)]
pub struct ReturnStatementNode(pub ExpressionNode);