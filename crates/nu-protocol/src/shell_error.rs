use miette::Diagnostic;
use thiserror::Error;

use crate::{ast::Operator, Span, Type};

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ShellError {
    #[error("Type mismatch during operation.")]
    OperatorMismatch {
        #[label = "type mismatch for operator"]
        op_span: Span,
        lhs_ty: Type,
        #[label("{1}")]
        lhs_span: Span,
        rhs_ty: Type,
        #[label("{3}")]
        rhs_span: Span,
    },

    #[error("Unsupported operator: {0}.")]
    UnsupportedOperator(Operator, #[label = "unsupported operator"] Span),

    #[error("Unsupported operator: {0}.")]
    UnknownOperator(String, #[label = "unsupported operator"] Span),

    #[error("External commands not yet supported")]
    ExternalNotSupported(#[label = "external not supported"] Span),

    #[error("Internal error: {0}.")]
    InternalError(String),

    #[error("Variable not found")]
    VariableNotFoundAtRuntime(#[label = "variable not found"] Span),

    #[error("Can't convert to {0}.")]
    CantConvert(String, #[label("can't convert to {0}")] Span),

    #[error("Division by zero.")]
    DivisionByZero(#[label = "division by zero"] Span),

    #[error("Can't convert range to countable values")]
    CannotCreateRange(#[label = "can't convert to countable values"] Span),

    #[error("Row number too large (max: {0}).")]
    AccessBeyondEnd(usize, #[label = "too large"] Span),

    #[error("Row number too large.")]
    AccessBeyondEndOfStream(#[label = "too large"] Span),

    #[error("Data cannot be accessed with a cell path")]
    IncompatiblePathAccess(String, #[label("{0} doesn't support cell paths")] Span),

    #[error("Cannot find column")]
    CantFindColumn(#[label = "cannot find column"] Span),
}
