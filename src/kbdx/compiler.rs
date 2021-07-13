use super::parser::Parser;

pub fn compile_string(mut parser: Parser) -> color_eyre::Result<String> {
    let data = parser.parse_string::<()>()?;
    Ok(format!("{:#?}", data))
}
