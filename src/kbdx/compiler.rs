use super::parser::{ Parser, ParseError };

pub fn compile_string(input: &str) -> Result<String, ParseError> {
    let pairs = Parser::parse_string_raw(input)?;
    Ok(format!("{:#?}", pairs))
}
