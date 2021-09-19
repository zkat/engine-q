use core::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use miette::{LabeledSpan, Severity, SourceCode};
use nu_parser::ParseError;
use nu_protocol::{engine::StateWorkingSet, ShellError, Span};
use thiserror::Error;

fn convert_span_to_diag(
    working_set: &StateWorkingSet,
    span: &Span,
) -> Result<(usize, Range<usize>), Box<dyn std::error::Error>> {
    for (file_id, (_, start, end)) in working_set.files().enumerate() {
        if span.start >= *start && span.end <= *end {
            let new_start = span.start - start;
            let new_end = span.end - start;

            return Ok((file_id, new_start..new_end));
        }
    }

    if span.start == working_set.next_span_start() {
        // We're trying to highlight the space after the end
        if let Some((file_id, (_, _, end))) = working_set.files().enumerate().last() {
            return Ok((file_id, *end..(*end + 1)));
        }
    }

    panic!(
        "internal error: can't find span in parser state: {:?}",
        span
    )
}

#[derive(Debug, Error)]
#[error("{0}")]
struct CliError<'src>(&'src dyn miette::Diagnostic, &'src StateWorkingSet<'src>);

impl<'src> miette::Diagnostic for CliError<'src> {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.code()
    }

    fn severity(&self) -> Option<Severity> {
        self.0.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.0.url()
    }

    fn labels<'a>(&'a self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + 'a>> {
        self.0.labels()
    }

    // Finally, we redirect the source_code method to our own source.
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.1)
    }
}

pub fn report_parsing_error(
    working_set: &StateWorkingSet,
    error: &ParseError,
) -> Result<(), Box<dyn std::error::Error>> {
    eprintln!(
        "{:?}",
        CliError(error, working_set).into::<miette::Report>()
    );
    Ok(())
}

pub fn report_shell_error(
    working_set: &StateWorkingSet,
    error: &ShellError,
) -> Result<(), Box<dyn std::error::Error>> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let diagnostic =
        match error {
            ShellError::OperatorMismatch {
                op_span,
                lhs_ty,
                lhs_span,
                rhs_ty,
                rhs_span,
            } => {
                let (lhs_file_id, lhs_range) = convert_span_to_diag(working_set, lhs_span)?;
                let (rhs_file_id, rhs_range) = convert_span_to_diag(working_set, rhs_span)?;
                let (op_file_id, op_range) = convert_span_to_diag(working_set, op_span)?;
                Diagnostic::error()
                    .with_message("Type mismatch during operation")
                    .with_labels(vec![
                        Label::primary(op_file_id, op_range)
                            .with_message("type mismatch for operator"),
                        Label::secondary(lhs_file_id, lhs_range).with_message(lhs_ty.to_string()),
                        Label::secondary(rhs_file_id, rhs_range).with_message(rhs_ty.to_string()),
                    ])
            }
            ShellError::UnsupportedOperator(op, span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;
                Diagnostic::error()
                    .with_message(format!("Unsupported operator: {}", op))
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message("unsupported operator")])
            }
            ShellError::UnknownOperator(op, span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;
                Diagnostic::error()
                    .with_message(format!("Unsupported operator: {}", op))
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message("unsupported operator")])
            }
            ShellError::ExternalNotSupported(span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;
                Diagnostic::error()
                    .with_message("External commands not yet supported")
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message("external not supported")])
            }
            ShellError::InternalError(s) => {
                Diagnostic::error().with_message(format!("Internal error: {}", s))
            }
            ShellError::VariableNotFoundAtRuntime(span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;
                Diagnostic::error()
                    .with_message("Variable not found")
                    .with_labels(vec![
                        Label::primary(diag_file_id, diag_range).with_message("variable not found")
                    ])
            }
            ShellError::CantConvert(s, span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;
                Diagnostic::error()
                    .with_message(format!("Can't convert to {}", s))
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message(format!("can't convert to {}", s))])
            }
            ShellError::CannotCreateRange(span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;
                Diagnostic::error()
                    .with_message("Can't convert range to countable values")
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message("can't convert to countable values")])
            }
            ShellError::DivisionByZero(span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;

                Diagnostic::error()
                    .with_message("Division by zero")
                    .with_labels(vec![
                        Label::primary(diag_file_id, diag_range).with_message("division by zero")
                    ])
            }
            ShellError::AccessBeyondEnd(len, span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;

                Diagnostic::error()
                    .with_message("Row number too large")
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message(format!("row number too large (max: {})", *len))])
            }
            ShellError::AccessBeyondEndOfStream(span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;

                Diagnostic::error()
                    .with_message("Row number too large")
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message("row number too large")])
            }
            ShellError::IncompatiblePathAccess(name, span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;

                Diagnostic::error()
                    .with_message("Data cannot be accessed with a cell path")
                    .with_labels(vec![Label::primary(diag_file_id, diag_range)
                        .with_message(format!("{} doesn't support cell paths", name))])
            }
            ShellError::CantFindColumn(span) => {
                let (diag_file_id, diag_range) = convert_span_to_diag(working_set, span)?;

                //FIXME: add "did you mean"
                Diagnostic::error()
                    .with_message("Cannot find column")
                    .with_labels(vec![
                        Label::primary(diag_file_id, diag_range).with_message("cannot find column")
                    ])
            }
        };

    // println!("DIAG");
    // println!("{:?}", diagnostic);
    codespan_reporting::term::emit(&mut writer.lock(), &config, working_set, &diagnostic)?;

    Ok(())
}
