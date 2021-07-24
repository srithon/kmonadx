use color_eyre::eyre::{bail, eyre};

use std::borrow::Cow;
use std::cell::UnsafeCell;

use super::diagnostic::{Diagnostic, FileDiagnostics, Message};
use super::parser::{Data, LazyButton, Pair, Parser, Rule};

use std::io::Write;

/// The context in which a button is defined
enum ButtonContext<'a> {
    /// Button is defined within an [aliases] block
    Aliases,
    /// Button is defined within a layer
    Layer(&'a str),
}

/// A String that is either owned or immutably borrowed
type MaybeOwnedString<'a> = Cow<'a, str>;

/// A button that has been converted into its kbd Lisp form
pub type ProcessedButton<'a> = color_eyre::eyre::Result<MaybeOwnedString<'a>>;

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

    /// Searches for a button within a certain context, returning `Some(_)` if it is found and
    /// `None` otherwise
    fn lookup_button<'x>(
        &self,
        button_identifier: &'a str,
        context: &ButtonContext,
        prepend_at: bool,
    ) -> Option<MaybeOwnedString<'a>> {
        // 1: check in public/private of layer
        if let ButtonContext::Layer(layer_name) = context {
            if let Some(layer) = self.parser_data.layers.get(*layer_name) {
                layer.aliases.get(button_identifier).map(|_| {
                    MaybeOwnedString::Owned(format!(
                        "{}{}.{}",
                        if prepend_at { "@" } else { "" },
                        layer_name,
                        button_identifier
                    ))
                })
            } else {
                panic!("Invalid layer: {:?}", layer_name)
            }
        } else {
            self.parser_data
                .global_aliases
                .get(button_identifier)
                .map(|_| {
                    if prepend_at {
                        MaybeOwnedString::Owned(format!("@{}", button_identifier))
                    } else {
                        MaybeOwnedString::Borrowed(button_identifier)
                    }
                })
        }
    }

    /// Given a button identifier `Pair`, processes the `Pair` and returns a `ProcessedButton`,
    /// creating diagnostics if necessary
    fn button_identifier_to_kbdx_alias(
        &self,
        button_identifier: Pair<'a>,
        context: &ButtonContext,
    ) -> ProcessedButton<'a> {
        // replace the button alias with the name of the actual button
        // to do this, we need to do all of the fun lookup rules with precedences
        // 1. look under private and public
        // 2. look under aliases
        match self.lookup_button(button_identifier.as_str(), context, true) {
            Some(button) => Ok(button),
            None => {
                self.error("undefined alias")
                    .add_message(Message::from_pest_span(&button_identifier.as_span(), "could not resolve alias in scope"))
                    .add_note("aliases are first searched for in the current layer's [private] and [public], and then in the global aliases table");

                bail!("undefined alias")
            }
        }
    }

    /// Given a `Pair` WITHIN a `button`, recursively processes the `Pair` and its children and returns a
    /// ProcessedButton.
    fn process_inner_button_pair(
        &self,
        pair: Pair<'a>,
        context: &ButtonContext,
    ) -> ProcessedButton<'a> {
        use Rule as R;

        let processed_string = {
            match pair.as_rule() {
                // tap macros and normal buttons do not yet exhibit special behavior
                r @ (R::tap_macro | R::normal_button) => {
                    // TODO: 15 is just a random number
                    let mut string_buffer = String::with_capacity(pair.as_str().len() + 15);

                    if matches!(r, R::tap_macro) {
                        string_buffer.push('#')
                    }

                    string_buffer.push('(');

                    let mut num_children = 0;

                    for child in pair.into_inner() {
                        num_children += 1;

                        string_buffer.push_str(&self.process_inner_button_pair(child, context)?);
                        string_buffer.push(' ');
                    }

                    if num_children != 0 {
                        // remove trailing space
                        string_buffer.pop();
                    }

                    string_buffer.push(')');

                    MaybeOwnedString::Owned(string_buffer)
                }
                R::button_alias => {
                    let button_identifier = pair
                        .into_inner()
                        .next()
                        .expect("Button aliases must have inner identifiers");

                    self.button_identifier_to_kbdx_alias(button_identifier, context)?
                }
                R::variable_reference => {
                    self.warning("unimplemented feature used").add_message(
                        Message::from_pest_span(
                            &pair.as_span(),
                            "variable references are not yet supported",
                        ),
                    );

                    bail!("unsupported operation")
                }
                R::normal_button_non_keyword => {
                    MaybeOwnedString::Borrowed(pair.as_str().trim_end())
                }
                x => unreachable!("Cannot have {:?} in `button`", x),
            }
        };

        Ok(processed_string)
    }

    /// Given a `button` rule, creates and returns a `ProcessedButton`
    fn process_button_pair(&self, pair: Pair<'a>, context: &ButtonContext) -> ProcessedButton<'a> {
        use Rule as R;

        match pair.as_rule() {
            // if its a regular button
            R::button => {
                let inner_rule = pair
                    .into_inner()
                    .next()
                    .expect("Buttons must contain an inner rule");

                self.process_inner_button_pair(inner_rule, context)
            }
            R::string => unimplemented!(),
            R::reference => unimplemented!(),
            R::identifier => self.button_identifier_to_kbdx_alias(pair, context),
            x => {
                // TODO: handle this at the parser level
                // numbers are only valid during configuration
                self.error("mismatched types")
                    .add_message(Message::from_pest_span(
                        &pair.as_span(),
                        format!("expected button, found {:?}", x),
                    ));

                bail!("mismatched types");
            }
        }
    }

    /// Given a `LazyButton` and a `ButtonContext`, processes the `LazyButton` in-place, inserting
    /// a ProcessedButton
    fn process_lazy_button(
        &self,
        button: &LazyButton<'a, ProcessedButton<'a>>,
        context: &ButtonContext,
    ) -> color_eyre::Result<()> {
        button.process(|pair| self.process_button_pair(pair, context))
    }

    pub fn compile_string(mut self) -> color_eyre::Result<String> {
        // read configuration into Configuration struct
        let configuration = self.try_parse_configuration()?;

        println!("Configuration: {:#?}", configuration);

        Ok(format!("{:#?}", self.parser_data))
    }
}
