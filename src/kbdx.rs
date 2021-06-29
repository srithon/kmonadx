use pest::iterators::Pairs;

use pest_derive::Parser;
use pest::Parser as _;

#[derive(Parser)]
#[grammar = "kbdx.pest"]
pub struct Parser;

pub type ParseError = pest::error::Error<Rule>;

impl Parser {
    fn parse_string(input: &str) -> Result<Pairs<Rule>, ParseError> {
        Self::parse(Rule::main, input.as_ref())
    }
}

pub fn compile_string(input: &str) -> Result<String, ParseError> {
    let pairs = Parser::parse_string(input)?;
    Ok(format!("{:#?}", pairs))
}
