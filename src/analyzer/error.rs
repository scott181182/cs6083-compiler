use std::num::{ParseIntError, ParseFloatError};

use thiserror::Error;

use super::util::ValueType;



#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Use of undeclared variable {0}")]
    UndeclaredReference(String),
    #[error("Assignment to undeclared variable {0}")]
    UndeclaredAssignment(String),
    #[error("'{0}' was redeclared")]
    Redeclaration(String),
    #[error("Expected additional scope after procedure, but there were none")]
    OutOfScope,

    #[error(transparent)]
    InvalidInteger(#[from] ParseIntError),
    #[error(transparent)]
    InvalidFloat(#[from] ParseFloatError),

    #[error("Found statements after return statement")]
    StatementAfterReturn,
    #[error("Unexpected return statement in procedure with no return type")]
    UnexpectedReturn,

    // Array Errors
    #[error("Attempt to index non-array variable {0}")]
    IndexOnNonArray(String),
    #[error("Attempt to index array {0} with non-integer type {1:?}")]
    NonIntegerIndex(String, ValueType),

    // Type Errors
    #[error("Expected {0:?} type, but found {1:?}")]
    MismatchedType(ValueType, ValueType),
    #[error("Expected Integer or Boolean expression for conditional, but found {0:?}")]
    InvalidConditionalExpression(ValueType),
    #[error("Expected {0} type, but found {1:?}")]
    IncorrectType(String, ValueType),

    #[error("Expected {0} arguments, but found {1}")]
    IncorrectNumberOfArgument(usize, usize)
}
