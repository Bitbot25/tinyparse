use std::{fmt::Display, error::Error};
use crate::Span;

#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    span: Span<'a>,
    //grammar_string: String,
    kind: ParseErrorKind<'a>,
}

impl<'a> ParseError<'a> {
    pub fn new(span: Span<'a>, kind: ParseErrorKind<'a>) -> ParseError<'a> {
        ParseError { span, kind, /*grammar_string: String::from("<unknown>")*/ }
    }
}

impl<'a> Error for ParseError<'a> {}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind<'a> {
    Starving { found: usize, required: usize },
    Unexpected { found: &'a str, expected: &'static str },
    Neither(Vec<ParseError<'a>>),
    ConditionFailed,
    Other,
}

impl<'a> Display for ParseErrorKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParseErrorKind::Starving { found, required } => write!(f, "Found {} units but expected {}.", found, required),
            ParseErrorKind::Unexpected { found, expected } => write!(f, "Unexpected: '{}', Expected: '{}'", found, expected),
            ParseErrorKind::Neither(errors) => write!(f, "Neither parser succeeded: {:?}", errors),
            ParseErrorKind::ConditionFailed => write!(f, "Condition failed."),
            ParseErrorKind::Other => write!(f, "Unknown error.")
        }
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "(input) -> {}: {}", self.grammar_string, self.kind)
        write!(f, "{}", self.kind)
    }
}

pub type ParseResult<'a, T> = std::result::Result<(Span<'a>, T), ParseError<'a>>;