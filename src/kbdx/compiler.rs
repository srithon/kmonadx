use color_eyre::eyre::eyre;

use std::borrow::Cow;
use std::cell::UnsafeCell;

use super::diagnostic::{Diagnostic, FileDiagnostics};
use super::parser::{Data, LazyButton, Pair, Parser, Rule};

/// Represents a button that has been converted into its kbd Lisp form
#[derive(Debug)]
struct ProcessedButton {
    string: String,
}

/// Struct containing typed [configuration] values
#[derive(Debug)]
struct Configuration<'a> {
    input: &'a str,
    output_name: &'a str,
    output_pre_command: &'a str,
    cmp_seq: &'a str,
    cmp_seq_delay: usize,
    fallthrough: bool,
    allow_cmd: bool,
    starting_layer: &'a str,
}

pub struct Compiler<'a, 'b> {
    parser_data: Data<'a, ProcessedButton<'a>>,
    file_diagnostics: UnsafeCell<FileDiagnostics<'a, 'b>>,
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

    create_type_parser!(identifier, &'a str, as_str);
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(mut parser: Parser<'a, 'b>) -> color_eyre::Result<Compiler<'a, 'b>> {
        Ok(Compiler {
            parser_data: parser.parse_string::<_>()?,
            file_diagnostics: UnsafeCell::new(parser.file_diagnostics),
        })
    }

    /// Creates an error diagnostic in the current file and returns a handle to it
    fn error(&self, headline: impl Into<String>) -> &mut Diagnostic {
        unsafe { (*self.file_diagnostics.get()).error(headline) }
    }

    /// Returns true if there are errors in the current file, false otherwise
    fn has_errors(&self) -> bool {
        unsafe { (*self.file_diagnostics.get()).error_count() != 0 }
    }

    /// Creates a warning diagnostic in the current file and returns a handle to it
    fn warning(&self, headline: impl Into<String>) -> &mut Diagnostic {
        unsafe { (*self.file_diagnostics.get()).warning(headline) }
    }

    fn try_parse_configuration(&mut self) -> color_eyre::Result<Configuration<'a>> {
        let Compiler {
            ref parser_data, ..
        } = self;

        let mut configuration_is_valid = true;

        macro_rules! try_parse {
            ($field_name:expr, $parse_function:expr) => {{
                match parser_data.configuration.get($field_name) {
                    None => {
                        self.error(format!("{} field missing in configuration!", $field_name))
                            .add_note("See the tutorial for a valid example configuration");

                        configuration_is_valid = false;

                        None
                    }
                    Some(x) => match $parse_function(x) {
                        Err(e) => {
                            self.error("mismatched types")
                                .add_message(Message::from_pest_span(&x.as_span(), e));

                            configuration_is_valid = false;

                            None
                        }
                        Ok(x) => Some(x),
                    },
                }
            }};
        }

        // we make these Option's rather than simply early-returning within a closure so that we
        // can report multiple configuration errors at oncee
        let input = try_parse!("input", parse::double_quoted_string);
        let output_name = try_parse!("output-name", parse::double_quoted_string);
        let output_pre_command = try_parse!("output-pre-command", parse::double_quoted_string);
        let cmp_seq = try_parse!("cmp-seq", parse::single_quoted_string);
        let cmp_seq_delay = try_parse!("cmp-seq-delay", parse::number);
        let fallthrough = try_parse!("fallthrough", parse::boolean);
        let allow_cmd = try_parse!("allow-cmd", parse::boolean);
        let starting_layer = try_parse!("starting-layer", parse::identifier);

        if configuration_is_valid {
            Ok(Configuration {
                input: input.unwrap(),
                output_name: output_name.unwrap(),
                output_pre_command: output_pre_command.unwrap(),
                cmp_seq: cmp_seq.unwrap(),
                cmp_seq_delay: cmp_seq_delay.unwrap(),
                fallthrough: fallthrough.unwrap(),
                allow_cmd: allow_cmd.unwrap(),
                starting_layer: starting_layer.unwrap(),
            })
        } else {
            Err(eyre!("Invalid configuration!"))
        }
    }

    pub fn compile_string(mut self) -> color_eyre::Result<String> {
        // read configuration into Configuration struct
        let configuration = self.try_parse_configuration()?;

        println!("Configuration: {:#?}", configuration);

        Ok(format!("{:#?}", self.parser_data))
    }
}
