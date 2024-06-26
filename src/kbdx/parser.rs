use pest::iterators::Pairs;

use pest::Parser as _;
use pest_derive::Parser;

use crate::util::StringStack;

use std::cell::{Ref, RefCell};

use super::diagnostic::*;
use super::keys::normalize_keycode;

use super::graph::{DependencyGraph, NodeIndex};

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
pub struct LazyButton<'a, T>(RefCell<LazyButtonInternal<'a, T>>);

#[derive(Debug)]
enum LazyButtonInternal<'a, T> {
    Unprocessed(Pair<'a>),
    Processed(T),
}

impl<'a, T> LazyButton<'a, T> {
    /// Creates a new `LazyButton` from a `Pair`
    pub fn new(pair: Pair<'a>) -> LazyButton<'a, T> {
        LazyButton(RefCell::new(LazyButtonInternal::Unprocessed(pair)))
    }

    /// Returns `true` if the LazyButton is Unprocessed, otherwise `false`
    pub fn is_unprocessed(&self) -> bool {
        matches!(*self.0.borrow(), LazyButtonInternal::Unprocessed(_))
    }

    /// Modifies the LazyButton in-place using `f`.
    ///
    /// If the LazyButton is already Processed, returns Err.
    /// Otherwise, returns Ok
    pub fn process(&self, f: impl FnOnce(Pair<'a>) -> T) -> color_eyre::Result<()> {
        let mut internal = self.0.borrow_mut();

        *internal = match *internal {
            LazyButtonInternal::Unprocessed(ref pair) => {
                // create a bitwise copy of `pair` without moving the value
                // we need to do this because self.process_button_pair may return an Err, in which
                // case we would not have a valid value to write to *button, and the `pair` within
                // the LazyButton would be invalid memory
                // MISTAKE: thought that doing a bitwise copy was equivalent to a clone.
                // OPTIMIZE: don't clone all the time since you only need it for errors
                // the reason this didn't work is because pair's internal string was being freed.
                let pair = pair.clone();

                LazyButtonInternal::Processed(f(pair))
            }
            _ => return Err(color_eyre::eyre::eyre!("Trying to process processed item!")),
        };

        Ok(())
    }

    /// Attempts to extract a T out of a LazyButton::Processed, panicking if it is Unprocessed.
    pub fn unwrap_processed(self) -> T {
        match self.0.into_inner() {
            LazyButtonInternal::Processed(t) => t,
            _ => panic!("Tried to unwrap unprocessed LazyButton as processed"),
        }
    }

    /// Attempts to extract an &T out of a LazyButton::Processed, panicking if it is Unprocessed.
    pub fn unwrap_processed_ref(&self) -> std::cell::Ref<T> {
        Ref::map(self.0.borrow(), |button| match button {
            LazyButtonInternal::Processed(ref t) => t,
            _ => panic!("Tried to unwrap unprocessed LazyButton as processed"),
        })
    }

    /// Attempts to extract a Pair out of a LazyButton::Processed, panicking if it is Unprocessed.
    pub fn unwrap_unprocessed(self) -> Pair<'a> {
        match self.0.into_inner() {
            LazyButtonInternal::Unprocessed(pair) => pair,
            _ => panic!("Tried to unwrap processed LazyButton as unprocessed"),
        }
    }

    /// Attempts to extract a Pair of a LazyButton::Processed, panicking if it is Unprocessed.
    pub fn unwrap_unprocessed_ref(&self) -> std::cell::Ref<Pair<'a>> {
        Ref::map(self.0.borrow(), |button| match button {
            LazyButtonInternal::Unprocessed(ref pair) => pair,
            _ => panic!("Tried to unwrap processed LazyButton as unprocessed"),
        })
    }
}

pub type Map<'a, T> = AHashMap<&'a str, T>;
pub type LazyButtonMap<'a, T> = Map<'a, LazyButton<'a, T>>;
pub type PairMap<'a> = Map<'a, Pair<'a>>;
// This has to be String's and not &str's because they must describe the entire layer, including
// the context which will not be present in the input most of the time
pub type LayerMap<'a, T> = AHashMap<String, Layer<'a, T>>;

#[derive(Debug)]
pub struct Layer<'a, T> {
    pub parent_name: Vec<Pair<'a>>,
    pub aliases: Map<'a, (NodeIndex, AccessModifier)>,
    pub keys: LazyButtonMap<'a, T>,
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
    pub configuration: PairMap<'a>,
    pub global_aliases: Map<'a, NodeIndex>,
    pub layers: LayerMap<'a, T>,
}

pub struct Parser<'a, 'b> {
    pub(in crate::kbdx) input_string: &'a str,
    pub(in crate::kbdx) file_diagnostics: &'b mut FileDiagnostics<'a>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(input_string: &'a str, file_diagnostics: &'b mut FileDiagnostics<'a>) -> Parser<'a, 'b> {
        Parser {
            input_string,
            file_diagnostics,
        }
    }

    fn add_parser_error(&mut self, parser_error: ParseError) {
        use pest::error::*;

        let span = match parser_error.location {
            InputLocation::Pos(byte_offset) => byte_offset..byte_offset + 1,
            InputLocation::Span((byte_start, byte_end)) => byte_start..byte_end,
        };

        let headline = "parsing error!".to_owned();

        let error = self.file_diagnostics.error(headline);

        match parser_error.variant {
            ErrorVariant::ParsingError {
                positives,
                negatives,
            } => {
                error.add_message(Message::from_byte_range(
                    span.clone(),
                    format!("Positive Attempts: {:?}", positives),
                ));
                error.add_message(Message::from_byte_range(
                    span,
                    format!("Negative Attempts: {:?}", negatives),
                ));
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
                return Err(color_eyre::eyre::eyre!("Parsing error."));
            }
        }
    }

    /// Parses the input text and returns it in a more structured format
    pub fn parse_string<T>(
        &mut self,
    ) -> color_eyre::Result<(Data<'a, T>, DependencyGraph<LazyButton<'a, T>>)> {
        let pairs = self.parse_string_raw()?;

        let mut pairs_iter = pairs.into_iter();

        // step 1: parse configuration
        let configuration_pair = pairs_iter.next().unwrap();
        let config_map = create_pair_map(configuration_pair);

        let mut aliases = Map::default();
        let mut layers = LayerMap::default();

        let mut layer_stack: StringStack = StringStack::new();
        let mut current_layer: Option<&mut Layer<'a, T>> = None;

        let mut alias_graph = DependencyGraph::new();

        #[derive(Debug)]
        enum LayerContext {
            Private,
            Public,
            Keys,
            Aliases,
        }

        let mut layer_context = None;

        let mut current_header_depth = 0;

        for pair in pairs_iter {
            use Rule as R;

            match pair.as_rule() {
                R::table_header => {
                    let header_text = pair.as_str();
                    // see how many ['s there are
                    // [some_table] has a depth of 1
                    let header_depth = header_text.chars().take_while(|&c| c == '[').count();

                    // we are going deeper in the current layer
                    // MISTAKE: layer_stack.num_segments is not equal to current header depth;
                    // could have [scroll.arrow-keys], for example
                    let deeper_in_layer =
                        header_depth > current_header_depth && current_header_depth > 1;

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
                    else if header_depth > current_header_depth + 1 {
                        panic!("Header is nested too far!")
                    }

                    let table_name = pair
                        .into_inner()
                        .next()
                        .expect("The first child of a table_header should be a table_name");

                    assert!(matches!(table_name.as_rule(), R::table_name));

                    let _ = layer_context.take();
                    for (element_index, identifier) in table_name.into_inner().enumerate() {
                        assert!(layer_context.is_none());

                        match identifier.as_rule() {
                            R::keyword_layer => match identifier.as_str() {
                                "public" => {
                                    let _ = layer_context.insert(LayerContext::Public);
                                }
                                "private" => {
                                    let _ = layer_context.insert(LayerContext::Private);
                                }
                                "keys" => {
                                    let _ = layer_context.insert(LayerContext::Keys);
                                }
                                _ => unreachable!(),
                            },
                            R::identifier => {
                                if layer_stack.num_segments() == 0 {
                                    layer_stack.push(identifier.as_str());
                                } else {
                                    layer_stack.push(&format!(".{}", identifier.as_str()));
                                    if element_index > 0 {
                                        assert!(layer_stack.merge_last());
                                    }
                                }

                                current_layer = None;
                                // create layer if it doesnt exist
                                let layer_entry =
                                    layers.entry(layer_stack.as_str().to_owned()).or_default();
                                // save as current layer
                                let _ = current_layer.insert(layer_entry);
                            }
                            _ => unreachable!(),
                        }
                    }

                    current_header_depth = header_depth;
                }
                R::header_aliases => {
                    layer_stack.clear();
                    let _ = layer_context.insert(LayerContext::Aliases);
                }
                R::table_property => {
                    let inner_property = pair.into_inner().next().expect(
                        "Table properties must contain an internal property to match against",
                    );

                    match inner_property.as_rule() {
                        R::parent_assignment => {
                            // TODO: handle lists as well
                            // see the pest grammar for details
                            let parent_name = inner_property
                                .into_inner()
                                .next()
                                .expect("Parent name must contain a layer_name");

                            macro_rules! add_parent_name {
                                ($parent_name:ident) => {{
                                    (&mut current_layer)
                                        .as_mut()
                                        .expect("Current layer must exist")
                                        .parent_name
                                        .push($parent_name)
                                }};
                            }

                            // TODO: check if layer_name or layer_name_list
                            match parent_name.as_rule() {
                                R::identifier => {
                                    add_parent_name!(parent_name);
                                }
                                R::layer_name_list => {
                                    for parent_name in parent_name.into_inner() {
                                        assert!(matches!(parent_name.as_rule(), R::identifier));
                                        add_parent_name!(parent_name);
                                    }
                                }
                                x => unreachable!("Cannot have {:?} within parent_assignment", x),
                            }
                        }
                        _ => unreachable!(
                            "Invalid property assignment: '{:#?}'",
                            inner_property.as_rule()
                        ),
                    }
                }
                R::assignment => {
                    use LayerContext as L;

                    let layer_context = layer_context
                        .as_ref()
                        .expect("Assignments must be within a context!");

                    let span = pair.as_span();

                    if matches!(layer_context, L::Keys) {
                        let assignment = try_parse_assignment_generic(
                            pair,
                            |keycode_pair| {
                                let res = normalize_keycode(keycode_pair.as_str());

                                if res.is_none() {
                                    self.file_diagnostics
                                        .error(format!(
                                            "invalid keycode `{}`",
                                            &keycode_pair.as_str()
                                        ))
                                        .add_message(Message::from_pest_span_no_text(
                                            &keycode_pair.as_span(),
                                        ));
                                }

                                res
                            },
                            LazyButton::new,
                        );

                        if let Some(assignment) = assignment {
                            if let (Some(lvalue), rvalue) = assignment {
                                let old_value = (&mut current_layer)
                                    .as_mut()
                                    .expect("Current layer must exist")
                                    .keys
                                    .insert(lvalue, rvalue);

                                if let Some(old_value) = old_value {
                                    self.file_diagnostics
                                        .error("key redefined in layer")
                                        .add_message(Message::from_pest_span(
                                            &span,
                                            "redefinition was here",
                                        ))
                                        .add_message(Message::from_pest_span(
                                            &old_value.unwrap_unprocessed().as_span(),
                                            "originally defined here",
                                        ));
                                }
                            }
                        }
                    } else {
                        let assignment = try_parse_assignment_lazy_button_rvalue(pair)
                            .expect("Assignments must be parseable");

                        macro_rules! test_redefined_alias {
                            ($optional_overwritten_index:ident) => {{
                                if let Some(overwritten_index) = $optional_overwritten_index {
                                    let original_definition =
                                        alias_graph.lookup_node_by_index(overwritten_index);

                                    self.file_diagnostics
                                        .error("alias redefined")
                                        .add_message(Message::from_pest_span(
                                            &span,
                                            "redefinition was here",
                                        ))
                                        .add_message(Message::from_pest_span(
                                            &original_definition.unwrap_unprocessed_ref().as_span(),
                                            "originally defined here",
                                        ));
                                }
                            }};
                        }

                        match layer_context {
                            L::Aliases => {
                                let (index, optional_overwritten_index) =
                                    alias_graph.add_node(assignment.0.to_owned(), assignment.1);

                                test_redefined_alias!(optional_overwritten_index);

                                aliases.insert(assignment.0, index);
                            }
                            access @ (L::Public | L::Private) => {
                                let (index, optional_overwritten_index) = alias_graph.add_node(
                                    format!("{}.{}", layer_stack.as_str(), assignment.0),
                                    assignment.1,
                                );

                                test_redefined_alias!(optional_overwritten_index);

                                // MISTAKE: reversed the order of matches! args
                                let access = if matches!(access, L::Public) {
                                    AccessModifier::Public
                                } else {
                                    AccessModifier::Private
                                };

                                (&mut current_layer)
                                    .as_mut()
                                    .expect("Current layer must exist")
                                    .aliases
                                    .insert(assignment.0, (index, access));
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                Rule::EOI => break,
                _ => unreachable!("{:#?}", pair.as_rule()),
            }
        }

        Ok((
            Data {
                configuration: config_map,
                layers,
                global_aliases: aliases,
            },
            alias_graph,
        ))
    }
}

/// Given an `assignment` Pair, parses the assignment into a tuple where the first item is the
/// result of the lvalue Pair passed into `lvalue_mapper` and the second item is the result of the
/// rvalue Pair passed into `rvalue_mapper`
fn try_parse_assignment_generic<'a, T, U>(
    maybe_assignment: Pair<'a>,
    mut lvalue_mapper: impl FnMut(Pair<'a>) -> T,
    mut rvalue_mapper: impl FnMut(Pair<'a>) -> U,
) -> Option<(T, U)> {
    let input_rule = maybe_assignment.as_rule();

    if input_rule != Rule::assignment {
        return None;
    }

    let mut pairs = maybe_assignment.into_inner();
    let lvalue = pairs.next().expect("The pair must have an lvalue!");

    // TODO support lists as lvalues
    assert!(matches!(lvalue.as_rule(), Rule::identifier));

    let rvalue = pairs.next().expect("The pair must have an rvalue!");

    Some((lvalue_mapper(lvalue), rvalue_mapper(rvalue)))
}

/// Given an `assignment` Pair, parses the assignment into a tuple where the first item is the
/// lvalue identifier and the second item is a LazyButton<T>::Unprocessed containing the rvalue
/// Pair
fn try_parse_assignment_lazy_button_rvalue<'a, T>(
    maybe_assignment: Pair<'a>,
) -> Option<(&'a str, LazyButton<T>)> {
    try_parse_assignment_generic(maybe_assignment, |x| x.as_str(), LazyButton::new)
}

/// Given an `assignment` Pair, parses the assignment into a tuple where the first item is the
/// lvalue identifier and the second item is the rvalue Pair
fn try_parse_assignment_pair_rvalue<'a>(maybe_assignment: Pair<'a>) -> Option<(&'a str, Pair<'a>)> {
    try_parse_assignment_generic(maybe_assignment, |x| x.as_str(), |x| x)
}

/// Given a Pair that contains `assignment`'s, return a PairMap with the format
/// { "<lvalue>": "<rvalue>" }
fn create_pair_map<'a>(input: Pair<'a>) -> PairMap<'a> {
    input
        .into_inner()
        .map(try_parse_assignment_pair_rvalue)
        // effectively take while they are assignments
        .take_while(Option::is_some)
        // remove the Option layer
        .map(Option::unwrap)
        .collect()
}
