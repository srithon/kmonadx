use pest::iterators::Pairs;

use pest_derive::Parser;
use pest::Parser as _;

#[derive(Parser)]
#[grammar = "kbdx.pest"]
pub struct Parser;

pub type ParseError = pest::error::Error<Rule>;

impl Parser {
    pub fn parse_string(input: &str) -> Result<Pairs<Rule>, ParseError> {
        Self::parse(Rule::main, input.as_ref())
    }
}
