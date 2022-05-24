use crate::{Parse, Span, Parser};
use crate::error::{ParseErrorKind, ParseError};
use unicode_xid::UnicodeXID;

// TODO: Remove impl Parse and replace it with Parser

// Note: This is impl Parse because the type can be inferred at compile time. If the type is unknown; you should use the Parser struct. Not Box<dyn Parser>
/// Parses any UTF-8 character.
/// 
/// # Errors
/// This returns a [ParseErrorKind::Starving] if the required amount of characters (1) was not fulfilled.
pub fn any_char<'a>() -> impl Parse<'a, char> {
    move |span: Span<'a>| {
        let c = span.left
            .chars()
            .next()
            .ok_or_else(|| ParseError::new(span, ParseErrorKind::Starving { found: 0, required: 1 }))?;
        Ok((span.incremented(1), c))
    }
}

/// Parses a literal equal to the `expected` parameter.
/// 
/// # Errors
/// A [ParseErrorKind::Starving] is returned if there are not enough characters and a [ParseErrorKind::Unexpected] is returned when the literal did not match.
pub fn literal<'a>(expected: &'static str) -> impl Parse<'a, &'a str> {
    move |span: Span<'a>| {
        if span.left.len() < expected.len() {
            Err(ParseError::new(span, ParseErrorKind::Starving { found: span.left.len(), required: expected.len() }))
        } else {
            let sub = &span.left[..expected.len()];
            if sub == expected {
                Ok((span.incremented(expected.len()), sub))
            } else {
                Err(ParseError::new(span.until(expected.len()), ParseErrorKind::Unexpected { found: String::from(sub), expected: String::from(expected) }))
            }
        }
    }
}

pub fn identifier<'a>() -> impl Parse<'a, &'a str> {
    fn err(span: Span) -> ParseError {
        ParseError::new(span, ParseErrorKind::Other("Expected unicode xid start character.".to_string()))
    }
    move |span: Span<'a>| {
        let mut chars = span.left.chars();
        let first_c = chars.next().ok_or_else(|| err(span))?;
        if UnicodeXID::is_xid_start(first_c) {
            let mut index = 1;
            while {
                let c = chars.next();
                c.is_some() && UnicodeXID::is_xid_continue(c.unwrap())
            } {
                index += 1;
            }

            Ok((span.incremented(index), &span.left[..index]))
        } else {
            Err(err(span))
        }
    }
}

/// An alterative to chaining multiple [Parse::or] calls. One benefit is that all the errors will be returned in one [Vec], instead of each returning two errors.
/// The intention may also be more clear.
/// 
/// # Errors
/// Same as [Parse::or]
pub fn one_of<'a, R: 'a, const N: usize>(parsers: [Parser<'a, R>; N]) -> impl Parse<'a, R> {
    move |span: Span<'a>| {
        let mut errors = Vec::with_capacity(N / 2);
        for parser in parsers.iter() {
            match parser.parse(span) {
                ok @ Ok(_) => return ok,
                Err(error) => errors.push(error),
            }
        }

        Err(ParseError::new(span, ParseErrorKind::Neither(errors)))
    }
}

/// An alternative to chaining multiple [Parse::and] calls. One benefit is that all the results will be returned in one [Vec], instead of each returning two results.
/// The intention may also be more  clear.
/// 
/// # Errors
/// Any of errors from the parameter `parsers`.
pub fn all<'a, R: 'a, const N: usize>(parsers: [Parser<'a, R>; N]) -> impl Parse<'a, Vec<R>> {
    move |mut span: Span<'a>| {
        let mut results = Vec::with_capacity(N);
        for parser in parsers.iter() {
            let (new_span, res) = parser.parse(span)?;
            results.push(res);
            span = new_span;
        }
        Ok((span, results))
    }
}

/// Parses an unsigned integer.
pub fn uint<'a>() -> impl Parse<'a, u32> {
    any_char().only_if(|c| c.is_digit(10)).n_or_more(1).map(|digits| {
        let digits_as_string: String = digits.into_iter().collect();
        digits_as_string.parse().unwrap()
    })
}

/// Parses an integer.
pub fn int<'a>() -> impl Parse<'a, i32> {
    let sign = literal("+").or(literal("-")).or_value("+");
    sign.and(uint()).map(|(sign, uint)| {
        match sign {
            "+" => uint as i32,
            "-" => -(uint as i32),
            _ => unreachable!(), 
        }
    })
}