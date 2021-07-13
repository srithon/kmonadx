use pest::iterators::Pairs;

use pest::Parser as _;
use pest_derive::Parser;

use crate::util::StringStack;

use super::diagnostic::*;

#[derive(Parser)]
#[grammar = "kbdx/kbdx.pest"]
/// Struct implementing the Pest parser
struct InternalParser;

pub type ParseError = pest::error::Error<Rule>;
pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;

use ahash::AHashMap;

#[derive(Debug)]
pub enum AccessModifier {
    Public,
    Private,
}

/// LazyButton allows the Compiler to mutate the returned data in-place
/// Rather than creating whole new maps for the processed data, instead we make each button
/// into a LazyButton which can be converted into whichever type the compiler wants to use
#[derive(Debug)]
pub enum LazyButton<'a, T> {
    Unprocessed(Pair<'a>),
    Processed(T),
}

pub type Map<'a, T> = AHashMap<&'a str, T>;
pub type PairMap<'a, T> = Map<'a, LazyButton<'a, T>>;
// This has to be String's and not &str's because they must describe the entire layer, including
// the context which will not be present in the input most of the time
pub type LayerMap<'a, T> = AHashMap<String, Layer<'a, T>>;

#[derive(Debug)]
pub struct Layer<'a, T> {
    parent_name: Vec<&'a str>,
    aliases: Map<'a, (LazyButton<'a, T>, AccessModifier)>,
    keys: PairMap<'a, T>,
}

impl<'a, T> Default for Layer<'a, T> {
    fn default() -> Layer<'a, T> {
        Layer {
            parent_name: Vec::new(),
            aliases: AHashMap::default(),
            keys: AHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct Data<'a, T> {
    configuration: PairMap<'a, T>,
    global_aliases: PairMap<'a, T>,
    layers: LayerMap<'a, T>,
}

pub struct Parser<'a, 'b> {
    input_string: &'a str,
    file_diagnostics: FileDiagnostics<'a, 'b>
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(input_string: &'a str, file_diagnostics: FileDiagnostics<'a, 'b>) -> Parser<'a, 'b> {
        Parser {
            input_string,
            file_diagnostics
        }
    }

    fn add_parser_error(&mut self, parser_error: ParseError) {
        use pest::error::*;

        let span = match parser_error.location {
            InputLocation::Pos(byte_offset) => byte_offset..byte_offset + 1,
            InputLocation::Span((byte_start, byte_end)) => byte_start..byte_end
        };

        let headline = "parsing error!".to_owned();

        let error = self.file_diagnostics.error(headline);

        match parser_error.variant {
            ErrorVariant::ParsingError { positives, negatives } => {
                error.add_message(Message::from_byte_range(span.clone(), format!("Positive Attempts: {:?}", positives)));
                error.add_message(Message::from_byte_range(span, format!("Negative Attempts: {:?}", negatives)));
            }
            ErrorVariant::CustomError { message } => {
                error.add_message(Message::from_byte_range(span, message));
            }
        };
    }

    /// Parses the input text and returns the raw Pest output
    pub fn parse_string_raw(&mut self) -> color_eyre::Result<Pairs<'a, Rule>> {
        match InternalParser::parse(Rule::main, self.input_string.as_ref()) {
            Ok(pairs) => Ok(pairs),
            Err(parse_error) => {
                self.add_parser_error(parse_error);
                return Err(color_eyre::eyre::eyre!("Parsing error."))
            }
        }
    }

    /// Parses the input text and returns it in a more structured format
    pub fn parse_string<T>(&mut self) -> color_eyre::Result<Data<'a, T>> {
        let pairs = self.parse_string_raw()?;

        let mut pairs_iter = pairs.into_iter();

        // step 1: parse configuration
        let configuration_pair = pairs_iter.next().unwrap();
        let config_map = create_pair_map(configuration_pair);

        let mut aliases = PairMap::default();
        let mut layers = LayerMap::default();

        let mut layer_stack: StringStack = StringStack::new();
        let mut current_layer: Option<&mut Layer<'a, T>> = None;

        #[derive(Debug)]
        enum LayerContext {
            Private,
            Public,
            Keys,
            Aliases,
        }

        let mut layer_context = None;

        for pair in pairs_iter {
            use Rule as R;

            match pair.as_rule() {
                R::table_header => {
                    let header_text = pair.as_str();
                    // see how many ['s there are
                    // [some_table] has a depth of 1
                    let header_depth = header_text.chars().take_while(|&c| c == '[').count();

                    // we are going deeper in the current layer
                    let deeper_in_layer = header_depth > layer_stack.num_segments()
                        && layer_stack.num_segments() != 0;

                    if !deeper_in_layer {
                        layer_context = None;
                        // keep popping off the stack until we are in the correct depth
                        while header_depth <= layer_stack.num_segments() {
                            if !layer_stack.pop() {
                                panic!("Layer stack should not be empty")
                            }
                        }
                    }
                    // you can only add one layer of nesting at a time
                    // more than that does not make sense
                    // because of the way we are handling layers vs layer contexts, this also
                    // handles the error where a user tries to nest [[[private]]] inside of
                    // [[public]]
                    else if header_depth > layer_stack.num_segments() + 1 {
                        panic!("Header is nested too far!")
                    }

                    let table_name = pair
                        .into_inner()
                        .next()
                        .expect("The first child of a table_header should be a table_name");

                    assert!(matches!(table_name.as_rule(), R::table_name));

                    let _ = layer_context.take();
                    for identifier in table_name.into_inner() {
                        assert!(layer_context.is_none());

                        match identifier.as_rule() {
                            R::keyword_layer => match identifier.as_str() {
                                "public" => {
                                    layer_context.insert(LayerContext::Public);
                                }
                                "private" => {
                                    layer_context.insert(LayerContext::Private);
                                }
                                "keys" => {
                                    layer_context.insert(LayerContext::Keys);
                                }
                                _ => unreachable!(),
                            },
                            R::identifier => {
                                if layer_stack.num_segments() == 0 {
                                    layer_stack.push(identifier.as_str());
                                } else {
                                    layer_stack.push(&format!(".{}", identifier.as_str()))
                                }

                                current_layer = None;
                                // create layer if it doesnt exist
                                let layer_entry =
                                    layers.entry(layer_stack.as_str().to_owned()).or_default();
                                // save as current layer
                                current_layer.insert(layer_entry);
                            }
                            _ => unreachable!(),
                        }
                    }

                    eprintln!(
                        "Header: {}; Stack: {}; Layer Context: {:#?}",
                        header_text,
                        layer_stack.as_str(),
                        layer_context
                    )
                }
                R::header_aliases => {
                    layer_stack.clear();
                    layer_context.insert(LayerContext::Aliases);
                }
                R::table_property => {
                    let inner_property = pair.into_inner().next().expect(
                        "Table properties must contain an internal property to match against",
                    );

                    match inner_property.as_rule() {
                        R::parent_assignment => {
                            let parent_name = inner_property
                                .into_inner()
                                .next()
                                .expect("Parent name must contain a layer_name")
                                .as_str();

                            (&mut current_layer)
                                .as_mut()
                                .expect("Current layer must exist")
                                .parent_name
                                .push(parent_name)
                        }
                        _ => unreachable!(
                            "Invalid property assignment: '{:#?}'",
                            inner_property.as_rule()
                        ),
                    }
                }
                R::assignment => {
                    let assignment =
                        try_parse_assignment(pair).expect("Assignments must be parseable");

                    use LayerContext as L;
                    match layer_context
                        .as_ref()
                        .expect("Assignments must be within a context!")
                    {
                        L::Aliases => {
                            aliases.insert(assignment.0, assignment.1);
                        }
                        L::Public => {
                            (&mut current_layer)
                                .as_mut()
                                .expect("Current layer must exist")
                                .aliases
                                .insert(assignment.0, (assignment.1, AccessModifier::Public));
                        }
                        L::Private => {
                            (&mut current_layer)
                                .as_mut()
                                .expect("Current layer must exist")
                                .aliases
                                .insert(assignment.0, (assignment.1, AccessModifier::Private));
                        }
                        L::Keys => {
                            (&mut current_layer)
                                .as_mut()
                                .expect("Current layer must exist")
                                .keys
                                .insert(assignment.0, assignment.1);
                        }
                    }
                }
                Rule::EOI => break,
                _ => unreachable!("{:#?}", pair.as_rule()),
            }
        }

        Ok(Data {
            configuration: config_map,
            layers,
            global_aliases: aliases,
        })
    }
}

/// Given an `assignment` Pair, parses the assignment into a tuple where the first item is the
/// lvalue identifier and the second item is the rvalue Pair
fn try_parse_assignment<'a, T>(
    maybe_assignment: Pair<'a>,
) -> Option<(&'a str, LazyButton<'a, T>)> {
    let input_rule = maybe_assignment.as_rule();

    if input_rule != Rule::assignment {
        return None;
    }

    let mut pairs = maybe_assignment.into_inner();
    let lvalue = pairs.next().expect("The pair must have an lvalue!");

    assert!(matches!(lvalue.as_rule(), Rule::identifier));

    // TODO support lists as lvalues
    let identifier_name = lvalue.as_str();

    let rvalue = pairs.next().expect("The pair must have an rvalue!");

    Some((identifier_name, LazyButton::Unprocessed(rvalue)))
}

/// Given a Pair that contains `assignment`'s, return a PairMap with the format
/// { "<lvalue>": "<rvalue>" }
fn create_pair_map<'a, T>(input: Pair<'a>) -> PairMap<'a, T> {
    input
        .into_inner()
        .map(try_parse_assignment)
        // effectively take while they are assignments
        .take_while(Option::is_some)
        // remove the Option layer
        .map(Option::unwrap)
        .collect()
}
