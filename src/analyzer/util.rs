use std::collections::{VecDeque, HashMap};

use thiserror::Error;

use crate::parser::misc::TypeMarkNode;



#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Expected {0:?} type, but found {1:?}")]
    MismatchedType(ValueType, ValueType),
    #[error("Use of undeclared variable {0}")]
    UndeclaredVariable(String),
    #[error("Variable '{0}' was redeclared")]
    VariableRedeclaration(String),
    #[error("Expected additional scope after procedure, but there were none")]
    OutOfScope
}




#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Boolean,
    Integer,
    Float,
    String,
    Void
}
impl ValueType {
    pub fn expect(self, other: ValueType) -> Result<Self, SemanticError> {
        if self != other {
            Err(SemanticError::MismatchedType(other, self))
        } else {
            Ok(self)
        }
    }
}
impl From<&TypeMarkNode> for ValueType {
    fn from(value: &TypeMarkNode) -> Self {
        match value {
            TypeMarkNode::Integer => ValueType::Integer,
            TypeMarkNode::Float => ValueType::Float,
            TypeMarkNode::String => ValueType::String,
            TypeMarkNode::Bool => ValueType::Boolean,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NamedValueType(String, ValueType);



struct ProcedureDefinition {
    args: Vec<ValueType>,
    ret: ValueType
}



pub enum Scope {
    Global,
    Local
}

struct GlobalScopeContext {
    pub variables: HashMap<String, ValueType>,
    pub procedures: HashMap<String, ProcedureDefinition>
}
impl GlobalScopeContext {
    pub fn new() -> Self {
        GlobalScopeContext{ variables: HashMap::new(), procedures: HashMap::new() }
    }
}

struct ScopeContext {
    pub variables: HashMap<String, ValueType>,
}
impl ScopeContext {
    pub fn new() -> Self {
        ScopeContext{ variables: HashMap::new() }
    }
}



pub struct Context {
    global_scope: GlobalScopeContext,
    scope_stack: Vec<ScopeContext>,
    local_scope: ScopeContext
}
impl Context {
    pub fn new() -> Self {
        Context {
            global_scope: GlobalScopeContext::new(),
            local_scope: ScopeContext::new(),
            scope_stack: VecDeque::new()
        }
    }

    pub fn set_type(&mut self, global: bool, ident: String, typ: ValueType) -> Result<(), SemanticError> {
        let mut scope = if global { self.global_scope } else { self.local_scope };
        if scope.variables.contains_key(&ident) {
            Err(SemanticError::VariableRedeclaration(ident))
        } else {
            scope.variables.insert(ident, typ);
            Ok(())
        }
    }
    pub fn set_proc(&mut self, ident: String, def: ProcedureDefinition) -> Result<(), SemanticError> {
        if self.global_scope.procedures.contains_key(&ident) {
            Err(SemanticError::VariableRedeclaration(ident))
        } else {
            // self.global_scope.procedures.insert(ident, typ);
            Ok(())
        }
    }



    pub fn start_stack(&mut self) {
        self.scope_stack.push_back(self.local_scope);
        self.local_scope = ScopeContext::new();
    }
    pub fn end_stack(&mut self) -> Result<(), SemanticError> {
        if let Some(scope) = self.scope_stack.pop() {
            self.local_scope = scope;
            Ok(())
        } else {
            Err(SemanticError::OutOfScope)
        }
    }
}



pub trait Analyze<T> {
    fn analyze(self, ctx: &mut Context, scope: Scope) -> Result<T, SemanticError>;
}