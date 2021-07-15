use super::diagnostic::FileDiagnostics;
use super::parser::{Data, LazyButton, Pair, Parser, Rule};

pub struct Compiler<'a, 'b> {
    parser_data: Data<'a, ()>,
    file_diagnostics: FileDiagnostics<'a, 'b>,
}

mod parse {
    use super::*;

    fn as_str<'a>(pair: Pair<'a>) -> &'a str {
        pair.as_str()
    }

    macro_rules! create_type_parser {
        ($rule_name:ident, $type:ty, $success_conversion:expr) => {
            pub fn $rule_name<'a>(input: &Pair<'a>) -> Result<$type, String> {
                match input.as_rule() {
                    Rule::$rule_name => Ok($success_conversion(input.clone())),
                    r => Err(format!(
                        "expected {}, found {:?}",
                        stringify!($rule_name),
                        r
                    )),
                }
            }
        };
    }

    create_type_parser!(double_quoted_string, &'a str, as_str);
    create_type_parser!(single_quoted_string, &'a str, as_str);
    create_type_parser!(number, usize, |i: Pair<'a>| i
        .as_str()
        .parse::<usize>()
        .expect("Numbers must be parseable as `usize`s."));
    create_type_parser!(boolean, bool, |i: Pair<'a>| match i
        .into_inner()
        .next()
        .expect("Booleans must have inner rules for their values")
        .as_rule()
    {
        Rule::keyword_true => true,
        Rule::keyword_false => false,
        x => unreachable!("Invalid inner boolean type: {:?}", x),
    });
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(mut parser: Parser<'a, 'b>) -> color_eyre::Result<Compiler<'a, 'b>> {
        Ok(Compiler {
            parser_data: parser.parse_string::<>()?,
            file_diagnostics: parser.file_diagnostics,
        })
    }

    pub fn compile_string(mut self) -> color_eyre::Result<String> {
        Ok(format!("{:#?}", self.parser_data))
    }
}
