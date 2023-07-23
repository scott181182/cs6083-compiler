
#[derive(Debug)]
pub struct ProgramNode {
    pub header: ProgramHeaderNode,
    pub body: ProgramBodyNode
}
#[derive(Debug)]
pub struct ProgramHeaderNode {
    pub ident: String
}


#[derive(Debug)]
pub struct ProgramBodyNode {
    pub declarations: Vec<DeclarationNode>,
    pub statements: Vec<StatementNode>
}

#[derive(Debug)]
pub enum DeclarationNode {
    Procedure{ global: bool, procedure: ProcedureDeclarationNode },
    Variable{ global: bool, variable: VariableDeclarationNode }
}
#[derive(Debug)]
pub struct ProcedureDeclarationNode {
    pub header: ProcedureHeaderNode,
    pub body: ProcedureBodyNode,
}
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
pub struct VariableDeclarationNode {
    pub ident: String,
    pub typ: TypeMarkNode,
    pub bound: Option<BoundNode>
}


#[derive(Debug)]
pub enum TypeMarkNode {
    Integer,
    Float,
    String,
    Bool
}

#[derive(Debug)]
pub struct BoundNode(pub NumberNode);

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

#[derive(Debug)]
pub enum ExpressionNode {
    And(Box<ExpressionNode>, ArithmeticNode),
    Or(Box<ExpressionNode>, ArithmeticNode),
    Not(ArithmeticNode),
    Nop(ArithmeticNode)
}

#[derive(Debug)]
pub enum ArithmeticNode {
    Plus(Box<ArithmeticNode>, RelationNode),
    Minus(Box<ArithmeticNode>, RelationNode),
    Nop(RelationNode)
}
#[derive(Debug)]
pub enum RelationNode {
    LessThan(Box<RelationNode>, TermNode),
    LessThanEq(Box<RelationNode>, TermNode),
    GreaterThan(Box<RelationNode>, TermNode),
    GreaterThanEq(Box<RelationNode>, TermNode),
    Equal(Box<RelationNode>, TermNode),
    NotEqual(Box<RelationNode>, TermNode),
    Nop(TermNode)
}

#[derive(Debug)]
pub enum TermNode {
    Multiply(Box<TermNode>, FactorNode),
    Divide(Box<TermNode>, FactorNode),
    Nop(FactorNode)
}

#[derive(Debug)]
pub enum FactorNode {
    Paren(Box<ExpressionNode>),
    Call(ProcedureCallNode),
    NegateName(NameNode),
    NegateNumber(NumberNode),
    String(String),
    True,
    False
}

#[derive(Debug)]
pub struct ProcedureCallNode {
    pub ident: String,
    pub arguments: Option<ArgumentListNode>
}


#[derive(Debug)]
pub struct NumberNode(pub String);
#[derive(Debug)]
pub struct NameNode {
    pub ident: String,
    pub expr: Option<Box<ExpressionNode>>
}

#[derive(Debug)]
pub struct ArgumentListNode(pub Vec<ExpressionNode>);
