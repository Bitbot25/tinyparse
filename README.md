# Tinyparse

A tiny library for parsing simple expressions.

## Quickstart
```Rust
use tinyparse::common;
use tinyparse::{Span, Parse};

let hello_or_int = common::literal("hello!").or(common::int());
// The parse functions return a result containing what's left of "10" and the actual result.
assert_eq!(hello_or_int.parse(Span::new("10")), Ok((Span::empty(), 10)));
```

## Limitations

Some expressions need lookahead capability or something similar to be parsed. Unfortunately; this library does not include this.</br>If you want lookahead capability, consider implementing your own parser using the `Parse` trait.

## Note: This library is a work in progress. New things are getting added and breaking changes may occur.