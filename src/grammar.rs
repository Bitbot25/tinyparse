// TODO: Make this an enum / AST with like Grammar::Or, Grammar:And, Grammar:NOrMore
// TODO: Make a Grammar::into_parser that will generate a parser for that grammar.


// TODO: Actually use this and make it so you can construct a parser from just Grammar.
pub enum GrammarNode {
    OneOrMore(Vec<GrammarNode>),
    ZerOrMore(Vec<GrammarNode>),
    And(Vec<GrammarNode>),
    Or(Vec<GrammarNode>),
    Literal(String),
}