use std::{fmt::Display, error::Error};
use crate::{Span, FrozenSpan};

#[derive(Debug, PartialEq)]
pub struct ParseError {
    span: FrozenSpan,
    //grammar_string: String,
    kind: ParseErrorKind,
}

impl ParseError {
    pub fn new(span: Span, kind: ParseErrorKind) -> ParseError {
        ParseError { span: span.frozen(), kind, /*grammar_string: String::from("<unknown>")*/ }
    }
}

impl Error for ParseError {}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    Starving { found: usize, required: usize },
    Unexpected { found: String, expected: String },
    Neither(Vec<ParseError>),
    ConditionFailed,
    Other(String),
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParseErrorKind::Starving { found, required } => write!(f, "Found {} units but expected {}.", found, required),
            ParseErrorKind::Unexpected { found, expected } => write!(f, "Unexpected: '{}', Expected: '{}'", found, expected),
            ParseErrorKind::Neither(errors) => write!(f, "Neither parser succeeded: {:?}", errors),
            ParseErrorKind::ConditionFailed => write!(f, "Condition failed."),
            ParseErrorKind::Other(details) => write!(f, "{}", details),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "(input) -> {}: {}", self.grammar_string, self.kind)
        write!(f, "{}", self.kind)
    }
}

pub type ParseResult<'a, T> = std::result::Result<(Span<'a>, T), ParseError>;