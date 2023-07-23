use super::expression::ExpressionNode;



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
pub struct NumberNode(pub String);
#[derive(Debug)]
pub struct NameNode {
    pub ident: String,
    pub expr: Option<Box<ExpressionNode>>
}
