use pest::iterators::Pairs;

use pest_derive::Parser;
use pest::Parser as _;

#[derive(Parser)]
#[grammar = "kbdx/kbdx.pest"]
pub struct Parser;

pub type ParseError = pest::error::Error<Rule>;
pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;

use ahash::AHashMap;

#[derive(Debug)]
enum AccessModifier {
    Public,
    Private,
}

pub type Map<'a, T> = AHashMap<&'a str, T>;
pub type PairMap<'a> = Map<'a, Pair<'a>>;
pub type LayerMap<'a> = Map<'a, Layer<'a>>;

#[derive(Debug)]
pub struct Layer<'a> {
    parent_name: Option<&'a str>,
    aliases: Map<'a, (Pair<'a>, AccessModifier)>,
    keys: PairMap<'a>,
}

impl<'a> Layer<'a> {
    fn default() -> Layer<'a> {
        Layer {
            parent_name: None,
            aliases: AHashMap::default(),
            keys: AHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct Data<'a> {
    configuration: PairMap<'a>,
    global_aliases: PairMap<'a>,
    layers: LayerMap<'a>,
}

impl<'a> Data<'a> {
    fn default() -> Data<'a> {
        Data {
            configuration: AHashMap::default(),
            global_aliases: AHashMap::default(),
            layers: AHashMap::default(),
        }
    }
}

impl Parser {
    /// Parses the input text and returns the raw Pest output
    pub fn parse_string_raw<'a>(input: &str) -> Result<Pairs<Rule>, ParseError> {
        Self::parse(Rule::main, input.as_ref())
    }

    /// Parses the input text and returns it in a more structured format
    pub fn parse_string<'a>(input: &'a str) -> Result<Data<'a>, ParseError> {
        let pairs = Self::parse(Rule::main, input.as_ref())?;

        let mut pairs_iter = pairs.into_iter();

        // step 1: parse configuration
        let configuration_pair = pairs_iter.next().unwrap();
        let config_map = create_pair_map(configuration_pair);

        eprintln!("Config: {:#?}", config_map);

        let mut aliases = PairMap::default();
        let mut layers = LayerMap::default();

        let mut layer_stack: Vec<&'a str> = Vec::new();
        let mut current_layer: Option<Layer<'a>> = None;

        #[derive(Debug)]
        enum LayerContext {
            Private,
            Public,
            Keys,
            Aliases
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
                    let deeper_in_layer = header_depth > layer_stack.len() && layer_stack.len() != 0;

                    if !deeper_in_layer {
                        // keep popping off the stack until we are in the correct depth
                        while header_depth <= layer_stack.len() {
                            layer_stack.pop().expect("Layer stack should not be empty");
                        }
                    }
                    // you can only add one layer of nesting at a time
                    // more than that does not make sense
                    // because of the way we are handling layers vs layer contexts, this also
                    // handles the error where a user tries to nest [[[private]]] inside of
                    // [[public]]
                    else if header_depth > layer_stack.len() + 1 {
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
                            R::keyword_layer => {
                                match identifier.as_str() {
                                    "public" => {
                                        layer_context.insert(LayerContext::Public);
                                    },
                                    "private" => {
                                        layer_context.insert(LayerContext::Private);
                                    },
                                    "keys" => {
                                        layer_context.insert(LayerContext::Keys);
                                    },
                                    _ => unreachable!()
                                }
                            },
                            R::identifier => {
                                layer_stack.push(identifier.as_str())
                            },
                            _ => unreachable!()
                        }
                    }

                    eprintln!("Header: {}; Stack: {:#?}; Layer Context: {:#?}", header_text, layer_stack, layer_context)
                },
                R::header_aliases => {
                    layer_stack.clear();
                    layer_context.insert(LayerContext::Aliases);
                }
                _ => (),
            }
        }

        Ok(Data {
            configuration: config_map,
            ..Data::default()
        })
    }
}

/// Given an `assignment` Pair, parses the assignment into a tuple where the first item is the
/// lvalue identifier and the second item is the rvalue Pair
fn try_parse_assignment<'a>(maybe_assignment: Pair<'a>) -> Option<(&'a str, Pair<'a>)> {
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

    Some((identifier_name, rvalue))
}

/// Given a Pair that contains `assignment`'s, return a PairMap with the format
/// { "<lvalue>": "<rvalue>" }
fn create_pair_map<'a>(input: Pair<'a>) -> PairMap<'a> {
    input
        .into_inner()
        .map(try_parse_assignment)
        // effectively take while they are assignments
        .take_while(Option::is_some)
        // remove the Option layer
        .map(Option::unwrap)
        .collect()
}
