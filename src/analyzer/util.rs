use std::collections::HashMap;

use crate::parser::misc::TypeMarkNode;
use super::error::SemanticError;



#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Boolean,
    Integer,
    Float,
    String,
    Array(Box<ValueType>, usize),
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

    pub fn can_assign_to(&self, other: &ValueType) -> bool {
        if self == other { true }
        else {
            match (self, other) {
                // Boolean/Integer casting
                (ValueType::Integer, ValueType::Boolean) |
                (ValueType::Boolean, ValueType::Integer) => true,
    
                // Integer/Float casting
                (ValueType::Integer, ValueType::Float) |
                (ValueType::Float, ValueType::Integer) => true,
    
                _ => false
            }
        }
    }
}
impl From<TypeMarkNode> for ValueType {
    fn from(value: TypeMarkNode) -> Self {
        match value {
            TypeMarkNode::Integer => ValueType::Integer,
            TypeMarkNode::Float => ValueType::Float,
            TypeMarkNode::String => ValueType::String,
            TypeMarkNode::Bool => ValueType::Boolean,
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
pub struct NamedValueType(pub String, pub ValueType);



#[derive(Debug, Clone)]
pub struct ProcedureSignature(pub Vec<NamedValueType>, pub ValueType);



#[derive(Debug, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local
}

#[derive(Debug)]
pub struct ScopeContext {
    pub variables: HashMap<String, ValueType>,
    pub procedures: HashMap<String, ProcedureSignature>,
    pub ret: ValueType
}
impl ScopeContext {
    pub fn new(ret: ValueType) -> Self {
        ScopeContext{
            variables: HashMap::new(),
            procedures: HashMap::new(),
            ret
        }
    }

    pub fn new_global() -> Self {
        let mut procedures = HashMap::new();
        // Builtin Functions
        procedures.insert("getbool".to_string(), ProcedureSignature(vec![], ValueType::Boolean));
        procedures.insert("getinteger".to_string(), ProcedureSignature(vec![], ValueType::Integer));
        procedures.insert("getfloat".to_string(), ProcedureSignature(vec![], ValueType::Float));
        procedures.insert("getstring".to_string(), ProcedureSignature(vec![], ValueType::String));
        
        procedures.insert("putbool".to_string(), ProcedureSignature(vec![NamedValueType("value".to_string(), ValueType::Boolean)], ValueType::Boolean));
        procedures.insert("putinteger".to_string(), ProcedureSignature(vec![NamedValueType("value".to_string(), ValueType::Integer)], ValueType::Boolean));
        procedures.insert("putfloat".to_string(), ProcedureSignature(vec![NamedValueType("value".to_string(), ValueType::Float)], ValueType::Boolean));
        procedures.insert("putstring".to_string(), ProcedureSignature(vec![NamedValueType("value".to_string(), ValueType::String)], ValueType::Boolean));
        
        procedures.insert("sqrt".to_string(), ProcedureSignature(vec![NamedValueType("value".to_string(), ValueType::Integer)], ValueType::Float));
        
        ScopeContext{
            variables: HashMap::new(),
            procedures,
            ret: ValueType::Void
        }
    }
}



#[derive(Debug)]
pub struct Context {
    global_scope: ScopeContext,
    scope_stack: Vec<ScopeContext>,
    local_scope: ScopeContext
}
impl Context {
    pub fn new() -> Self {
        Context {
            global_scope: ScopeContext::new_global(),
            local_scope: ScopeContext::new(ValueType::Void),
            scope_stack: Vec::new()
        }
    }
    pub fn into_global(self) -> ScopeContext { self.global_scope }

    pub fn set_type(&mut self, global: bool, ident: String, typ: ValueType) -> Result<(), SemanticError> {
        let variables = if global { &mut self.global_scope.variables } else { &mut self.local_scope.variables };

        if variables.contains_key(&ident) {
            Err(SemanticError::Redeclaration(ident))
        } else {
            variables.insert(ident, typ);
            Ok(())
        }
    }
    pub fn set_proc(&mut self, global: bool, ident: String, sig: ProcedureSignature) -> Result<(), SemanticError> {
        let procedures = if global { &mut self.global_scope.procedures } else { &mut self.local_scope.procedures };

        if procedures.contains_key(&ident) {
            Err(SemanticError::Redeclaration(ident))
        } else {
            procedures.insert(ident, sig);
            Ok(())
        }
    }



    pub fn get_variable_type(&self, ident: &str) -> Result<&ValueType, SemanticError> {
        self.local_scope.variables.get(ident)
            .or_else(|| self.global_scope.variables.get(ident))
            .ok_or_else(|| SemanticError::UndeclaredReference(ident.to_string()))
        // if let Some(var) = self.local_scope.variables.get(ident) {
        //     Ok(var)
        // } else if let Some(var) = self.global_scope.variables.get(ident) {
        //     Ok(var)
        // } else {
        //     Err(SemanticError::UndeclaredReference(ident.to_string()))
        // }
    }
    pub fn get_procedure_signature(&self, ident: &str) -> Result<&ProcedureSignature, SemanticError> {
        self.local_scope.procedures.get(ident)
            .or_else(|| self.global_scope.procedures.get(ident))
            .ok_or_else(|| SemanticError::UndeclaredReference(ident.to_string()))
        // if let Some(var) = self.local_scope.procedures.get(ident) {
        //     Ok(var)
        // } else if let Some(var) = self.global_scope.procedures.get(ident) {
        //     Ok(var)
        // } else {
        //     Err(SemanticError::UndeclaredReference(ident.to_string()))
        // }
    }
    pub fn get_return_type(&self) -> &ValueType { &self.local_scope.ret }


    pub fn start_stack(&mut self, ret: ValueType) {
        let prev_stack = std::mem::replace(&mut self.local_scope, ScopeContext::new(ret));
        self.scope_stack.push(prev_stack);
    }
    pub fn end_stack(&mut self) -> Result<ScopeContext, SemanticError> {
        if let Some(scope) = self.scope_stack.pop() {
            let old_scope = std::mem::replace(&mut self.local_scope, scope);
            Ok(old_scope)
        } else {
            Err(SemanticError::OutOfScope)
        }
    }
}



pub trait Analyze<T> {
    fn analyze(self, ctx: &mut Context, scope: &Scope) -> Result<T, SemanticError>;
}

pub trait AnalyzeExpression<T>: Sized {
    fn analyze_expr(value: T, ctx: &mut Context) -> Result<Self, SemanticError>;
    fn get_type(&self, ctx: &Context) -> Result<ValueType, SemanticError>;
}