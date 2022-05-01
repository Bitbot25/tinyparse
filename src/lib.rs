pub mod error;
pub mod grammar;
pub mod common;

use error::{ParseError, ParseResult, ParseErrorKind};
use std::fmt::Display;

/// Performs a conversion for any type that can be converted into a [Parser].
pub trait IntoParser<'a, R> {
    fn into_parser(self) -> Parser<'a, R>;
}

impl<'a, R, P: Parse<'a, R> + 'a> IntoParser<'a, R> for P {
    fn into_parser(self) -> Parser<'a, R> {
        Parser { parser: Box::new(self) }
    }
}

// TODO: Each Parser should come with a grammar so that we can create parsers from just Grammar.
// TODO: Implement optimizer that will change .or.or.or.or to just be one_of function. Same for and.
/// Anything that is capable of Parsing from a [Span].
pub trait Parse<'a, R> {
    fn parse(&self, span: Span<'a>) -> ParseResult<'a, R>;

    fn map<F, M>(self, map_fn: F) -> Parser<'a, M>
    where
        F: Fn(R) -> M + 'a,
        Self: Sized + 'a,
    {
        (move |span| {
            self.parse(span).map(|(span, res)| (span, map_fn(res)))
        }).into_parser()
    }

    fn map_error<F>(self, map_fn: F) -> Parser<'a, R>
    where
        F: Fn(ParseError) -> ParseError + 'a,
        Self: Sized + 'a,
    {
        (move |span| {
            #[allow(clippy::redundant_closure)] // The closure consumes self if we just replace |err| map_fn(err) with the shorthand.
            self.parse(span).map_err(|err| map_fn(err))
        }).into_parser()
    }

    fn only_if<F>(self, pred: F) -> Parser<'a, R>
    where
        F: Fn(&R) -> bool + 'a,
        Self: Sized + 'a,
    {
        (move |old_span| {
            let (span, parsed) = self.parse(old_span)?;
            if pred(&parsed) {
                Ok((span, parsed))
            } else {
                Err(ParseError::new(old_span, ParseErrorKind::ConditionFailed))
            }
        }).into_parser()
    }

    fn or_value(self, val: R) -> Parser<'a, R>
    where
        Self: Sized + 'a,
        R: Clone + 'a,
    {
        (move |span| {
            Ok(self.parse(span).unwrap_or((span, val.clone())))
        }).into_parser()
    }

    fn or_else_value<F>(self, compute: F) -> Parser<'a, R>
    where
        Self: Sized + 'a,
        F: Fn() -> R + 'a,
    {
        (move |span| {
            Ok(self.parse(span).unwrap_or_else(|_| (span, compute())))
        }).into_parser()
    }

    /// Returns the result of this parser if it succeeded, otherwise it returns the result of `other_parser`.
    /// 
    /// # Errors
    /// [ParseErrorKind::Neither] is returned when neither of the parsers successfully ran.
    fn or<P: Parse<'a, R> + 'a>(self, other_parser: P) -> Parser<'a, R>
    where
        Self: Sized + 'a,
    {
        (move |span| {
            match self.parse(span) {
                ok @ Ok(_) => ok,
                Err(error) => match other_parser.parse(span) {
                    ok @ Ok(_) => ok,
                    Err(second_error) => Err(ParseError::new(span, ParseErrorKind::Neither(vec![error, second_error])))
                }
            }
        }).into_parser()
    }

    /// Combines this parser with another and returns the results in a tuple.
    /// 
    /// # Errors
    /// No new errors are thrown by this function, only by the combining ones.
    fn and<R2, P: Parse<'a, R2> + 'a>(self, other_parser: P) -> Parser<'a, (R, R2)>
    where
        Self: Sized + 'a,
    {
        (move |span| {
            let (span, res0) = self.parse(span)?;
            let (span, res1) = other_parser.parse(span)?;
            Ok((span, (res0, res1)))
        }).into_parser()
    }

    /// Runs this parser n or more times and returns the results in a [Vec].
    /// 
    /// # Errors
    /// A [ParseErrorKind::Starving] error is returned when the parser could not match `n` times.
    fn n_or_more(self, n: usize) -> Parser<'a, Vec<R>>
    where
        Self: Sized + 'a,
    {
        (move |mut span| {
            let initial_span = span;
            let mut results = Vec::with_capacity(n);
            while let Ok((new_span, res)) = self.parse(span) {
                results.push(res);
                span = new_span;
            }

            if results.len() < n {
                Err(ParseError::new(initial_span, ParseErrorKind::Starving { found: results.len(), required: n }))
            } else {
                Ok((span, results))
            }
        }).into_parser()
    }
}

impl<'a, R, F> Parse<'a, R> for F
where 
    F: Fn(Span<'a>) -> ParseResult<'a, R>,
{
    fn parse(&self, span: Span<'a>) -> ParseResult<'a, R> {
        self(span)
    }
}

impl<'a, R> Parse<'a, R> for Parser<'a, R> {
    fn parse(&self, span: Span<'a>) -> ParseResult<'a, R> {
        self.parser.parse(span)
    }
}

/// A container for the actual data we are parsing.
/// 
/// **Note: This struct is only commonly used for calling the [Parse::parse] function. The usage below can be recreated with the provided parsers already.**
/// 
/// # Usage
/// 
/// ```
/// use tinyparse::{Parser, Span, ParseError, ParseErrorKind, IntoParser};
/// 
/// // You can use impl Parse<'a, &'a str> as the return type here if you don't want to do .into_parser().
/// fn my_special_parser<'a>() -> Parser<'a, &'a str> {
///     (move |span: Span<'a>| {
///         let hello = "hello";
///         if span.left.len() < hello.len() {
///             Err(ParseError::new(span, ParseErrorKind::Starving { found: span.left.len(), required: hello.len() }))
///         } else if &span.left[..hello.len()] == hello {
///             Ok((span.incremented(hello.len()), "They really said hello!"))
///         } else {
///             Err(ParseError::new(span.until(hello.len()), ParseErrorKind::Unexpected { found: String::from(&span.left[..hello.len()]), expected: String::from(hello) }))
///         }
///     }).into_parser()
/// }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span<'a> {
    src_idx: usize,
    src: &'a str,
    pub left: &'a str,
}

impl<'a> Span<'a> {
    pub fn new(left: &'a str) -> Span<'a> {
        Span { left, src: left, src_idx: 0 }
    }

    pub fn incremented(&self, n: usize) -> Span<'a> {
        Span {
            left: &self.left[n..],
            src: self.src,
            src_idx: self.src_idx + n,
        }
    }

    pub fn until(&self, n: usize) -> Span<'a> {
        Span { left: &self.left[..n], src: self.src, src_idx: self.src_idx }
    }

    pub fn until_span(&self, other: Span<'a>) -> Span<'a> {
        Span { src_idx: self.src_idx, src: self.src, left: &self.src[self.src_idx..other.src_idx] }
    }

    pub fn empty() -> Span<'a> {
        Span { left: "", src: "", src_idx: 0 }
    }

    pub fn frozen(&self) -> FrozenSpan {
        let src = String::from(self.src);
        let left = String::from(self.left);
        FrozenSpan { src_idx: self.src_idx, src, left }
    } 
} 

impl<'a> Default for Span<'a> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<'a> Display for Span<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.left)
    }
}

// TODO: Make Span a trait that can be implemented instead of this?
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrozenSpan {
    src: String,
    left: String,
    src_idx: usize,
}

pub struct Parser<'a, R> {
    parser: Box<dyn Parse<'a, R> + 'a>,
}

impl<'a, R> Parser<'a, R> {
    pub fn new(parser: Box<dyn Parse<'a, R>>) -> Self {
        Parser { parser }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use crate::common::*;
    use super::*;

    fn assert_parses<'a, R: Debug + PartialEq, P: Parse<'a, R>>(parser: P, test: &'static str, expected: R) {
        let parse_result = parser.parse(Span::new(test));
        assert!(parse_result.is_ok(), "Failed to parse valid string.");
        let success_result = parse_result.unwrap();
        assert!(success_result.0.left == "", "Didn't parse the whole string.");
        assert!(success_result.1 == expected, "Parsed the string but the result is wrong.");
    }

    fn assert_not_parses<'a, P: Parse<'a, R>, R>(parser: P, test: &'static str) {
        assert!(parser.parse(Span::new(test)).is_err(), "Parses invalid string.")
    }

    #[test]
    fn it_parses_numbers() {
        assert_parses(uint(), "200", 200);
        assert_not_parses(uint(), "not a number");
        assert_parses(int(), "-5", -5);
        assert_parses(int(), "5", 5);
        assert_parses(int(), "+5", 5);
        assert_not_parses(int(), "not a number");
        assert_not_parses(int(), "+not a number");
        assert_not_parses(int(), "-not a number");
    }

    #[test]
    fn it_parses_literals() {
        assert_parses(literal("hey"), "hey", "hey");
        assert_not_parses(literal("hey"), "not hey");
    }
}
