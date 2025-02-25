use color_eyre::eyre::{bail, eyre};

use std::fmt::Write;
use std::fmt::{self, Display, Formatter};

use std::borrow::Cow;
use std::cell::UnsafeCell;

use super::diagnostic::{Diagnostic, FileDiagnostics, Message};
use super::keys::normalize_keycode;
use super::layer::{Layer as ProcessedLayer, LayerButton};
use super::parser::{AccessModifier, Data, LayerMap, LazyButton, Pair, Parser, Rule};

use super::graph::{DependencyGraph, NodeIndex};
use ahash::AHashMap;

use std::collections::{hash_map::Entry, BTreeSet};

const INDENT_LEVEL: &'static str = "  ";

/// The context in which a button is defined
#[derive(Debug)]
enum ButtonContext<'a> {
    /// Button is defined within an [aliases] block
    Aliases,
    /// Button is defined within a layer
    Layer(&'a str),
}

// NOTE: this is only for use with creating button paths
impl<'a> Display for ButtonContext<'a> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        if let ButtonContext::Layer(string) = self {
            write!(formatter, "{}", string)?;
            write!(formatter, ".")?;
        }

        Ok(())
    }
}

/// A String that is either owned or immutably borrowed
type MaybeOwnedString<'a> = Cow<'a, str>;

/// Classifies ProcessedButtons so the compiler can decide whether to display it during the final
/// stage of compilation
#[derive(Debug, Clone)]
pub enum ValueType {
    KbdOnly,
    ConstantOnly { quoted: bool },
    Either,
}

impl ValueType {
    pub fn is_valid_kbd(&self) -> bool {
        !matches!(self, ValueType::ConstantOnly { .. })
    }

    pub fn is_valid_constant(&self) -> bool {
        !matches!(self, ValueType::KbdOnly)
    }

    pub fn is_quoted(&self) -> bool {
        match self {
            ValueType::ConstantOnly { quoted } => *quoted,
            _ => false,
        }
    }
}

/// A button that has been converted into its kbd Lisp form
// TODO: newtype_derive, add methods onto ProcessedButton
pub type ProcessedButton<'a> = color_eyre::eyre::Result<(MaybeOwnedString<'a>, ValueType)>;

/// Creates a successful ProcessedButton with ValueType::Kbd
fn create_kbd_only_button<'a>(string: MaybeOwnedString<'a>) -> ProcessedButton<'a> {
    Ok((string, ValueType::KbdOnly))
}

/// Creates a successful ProcessedButton with ValueType::Other
fn create_constant_only_button<'a>(
    string: MaybeOwnedString<'a>,
    quoted: bool,
) -> ProcessedButton<'a> {
    Ok((string, ValueType::ConstantOnly { quoted }))
}

/// Creates a successful ProcessedButton with ValueType::Kbd
fn create_either_button<'a>(string: MaybeOwnedString<'a>) -> ProcessedButton<'a> {
    Ok((string, ValueType::Either))
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

impl<'a> Display for Configuration<'a> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write_section(
            "defcfg",
            // OPTIMIZE: do this without allocating a vector and separate strings
            vec![
                format!("input (device-file \"{}\")", self.input),
                format!(
                    "output (uinput-sink \"{}\" \"{}\")",
                    self.output_name, self.output_pre_command
                ),
                format!("cmp-seq {}", self.cmp_seq),
                format!("cmp-seq-delay {}", self.cmp_seq_delay),
                format!("fallthrough true"),
                format!("allow-cmd {}", self.allow_cmd),
            ],
            formatter,
        )
    }
}

struct AliasBlock<'a> {
    // if defined in [aliases], then None
    dependency_graph: DependencyGraph<LazyButton<'a, ProcessedButton<'a>>>,
}

impl<'a> Display for AliasBlock<'a> {
    // TODO: move into this function so that the memory can be freed after use
    // in order to do this, we would need to change the code a bit and not use the Display trait
    // directly
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        // TODO: handle toposort errors properly
        let sorted_aliases = self
            .dependency_graph
            .toposort()
            .expect("There must not be any cycles in the dependency graph")
            .filter(|(_, button)| !button.is_unprocessed())
            .filter_map(|(name, button)| {
                let unwrapped = button.unwrap_processed_ref();

                let as_ref = unwrapped
                    .as_ref()
                    .expect("All aliases must be successfully compiled");

                // need to track which things in the graph are being referred to as variables an
                // which are referred to as aliases
                // NOTE: anything that is a valid constant is not a valid alias; numbers, strings
                if !as_ref.1.is_valid_constant() {
                    Some(format!("{} {}", name, as_ref.0))
                } else {
                    None
                }
            });

        write_section("defalias", sorted_aliases, formatter)
    }
}

/// Struct containing the name of a layer and the layer.
struct LayerTuple<'layer, 'string, T>(&'string str, ProcessedLayer<'layer, T>);

impl<'layer, 'string, T> Display for LayerTuple<'layer, 'string, T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write_section(
            &format!("deflayer {}", self.0),
            self.1.keys.iter().map(|button| button.value()),
            formatter,
        )
    }
}

/// Struct containing the source layer. This is essentially the same as
/// LayerTuple, except it yields a defsrc instead of a deflayer block.
struct SourceLayer<'layer, T>(ProcessedLayer<'layer, T>);

impl<'layer, T> Display for SourceLayer<'layer, T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write_section(
            "defsrc",
            self.0.keys.iter().map(|button| button.value()),
            formatter,
        )
    }
}

/// Contains the intermediate state of the compiler.
pub struct Compiler<'a, 'b> {
    parser_data: Data<'a, ProcessedButton<'a>>,
    alias_dependency_graph: DependencyGraph<LazyButton<'a, ProcessedButton<'a>>>,
    file_diagnostics: UnsafeCell<&'b mut FileDiagnostics<'a>>,
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

    create_type_parser!(double_quoted_string, &'a str, |i: Pair<'a>| {
        let mut inner_elements = i.into_inner();
        let first = inner_elements
            .next()
            .expect("Double quoted strings must contain inner text");

        match first.as_rule() {
            Rule::double_quoted_string_inner_text => {
                assert_eq!(
                    0,
                    inner_elements.count(),
                    "Constant interpolation is not allowed in the configuration block"
                );
                first.as_str()
            }
            _ => unreachable!(),
        }
    });
    create_type_parser!(single_quoted_string, &'a str, |i: Pair<'a>| i
        .into_inner()
        .next()
        .expect("Single quoted strings must contain inner text")
        .as_str());
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

/// Either a NodeIndex or a ProcessedButton. Used as a return type when the
/// caller may want either type returned
#[derive(Debug)]
enum NodeIndexOrButton<'a> {
    Index(NodeIndex),
    Button(ProcessedButton<'a>),
}

/// The requested return type from lookup_button: the index of the node within
/// the graph, or a string containing an alias to the button.
#[derive(Debug, Clone)]
enum LookupButtonType {
    ReferenceAt,
    NodeIndex,
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(mut parser: Parser<'a, 'b>) -> color_eyre::Result<Compiler<'a, 'b>> {
        let (data, graph) = parser.parse_string::<_>()?;

        Ok(Compiler {
            parser_data: data,
            alias_dependency_graph: graph,
            file_diagnostics: UnsafeCell::new(parser.file_diagnostics),
        })
    }

    /// Creates an error diagnostic in the current file and returns a handle to it
    fn error(&self, headline: impl Into<String>) -> &mut Diagnostic {
        unsafe { (*self.file_diagnostics.get()).error(headline) }
    }

    /// Returns true if there are errors in the current file, false otherwise
    fn has_errors(&self) -> bool {
        unsafe { (*self.file_diagnostics.get()).errors().len() != 0 }
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
        // The Pair containing the button's rvalue.
        // This could be of type `reference` or `identifier`
        button_identifier: Pair<'a>,
        // The layer in which the button is being looked up from
        context: &ButtonContext,
        return_type: LookupButtonType,
        expecting_constant: bool,
    ) -> NodeIndexOrButton<'a> {
        use Rule as R;

        macro_rules! prepend_at {
            () => {{
                matches!(return_type, LookupButtonType::ReferenceAt)
            }};
        }

        macro_rules! err {
            ($err:expr) => {{
                return Button(Err(eyre!($err)));
            }};
        }

        use LookupButtonType::*;
        use NodeIndexOrButton::*;

        // Macro for taking a NodeIndexOrButton and plugging the associated ProcessedButton into
        // check_button_type!
        macro_rules! verify_button_type {
            ($node_index_or_button:expr, $pair:expr) => {{
                match &$node_index_or_button {
                    NodeIndexOrButton::Index(index) => {
                        let lazy_button = self.alias_dependency_graph.lookup_node_by_index(*index);
                        let unwrapped = lazy_button.unwrap_processed_ref();

                        _verify_button_type_helper!(unwrapped.as_ref(), $pair)
                    }
                    NodeIndexOrButton::Button(button) => {
                        _verify_button_type_helper!(button, $pair)
                    }
                }
            }};
        }

        // Internal helper macro for verifying that a ProcessedButton is the correct type; constant
        // if we are expecting a constant or a Lisp button if we are expecting that
        macro_rules! _verify_button_type_helper {
            ($processed_button:expr, $pair:expr) => {{
                match $processed_button {
                    Ok((_, value_info)) => {
                        let is_correct = if expecting_constant {
                            value_info.is_valid_constant()
                        } else {
                            value_info.is_valid_kbd()
                        };

                        if !is_correct {
                            let (expected, received) = if expecting_constant {
                                ("constant", "lisp button")
                            } else {
                                ("lisp button", "constant")
                            };

                            self.error("mismatched types")
                                .add_message(Message::from_pest_span(
                                    &$pair.as_span(),
                                    format!("expected {}, found {}", expected, received),
                                ));

                            err!("mismatched types")
                        }
                    }
                    Err(_) => (),
                }
            }};
        }

        match button_identifier.as_rule() {
            R::button_alias => {
                let button_identifier = button_identifier
                    .into_inner()
                    .next()
                    .expect("Button aliases must have inner identifiers");

                let button =
                    self.lookup_button(button_identifier.clone(), context, return_type, false);

                verify_button_type!(button, button_identifier);

                button
            }
            R::reference => {
                // OPTIMIZE: don't clone the entire thing, only preserve the span?
                let mut pairs = button_identifier.clone().into_inner();
                let reference_layer_name = pairs.next().expect("Pair must have >0 children");

                if let Some(layer) = self.parser_data.layers.get(reference_layer_name.as_str()) {
                    let reference_identifier = pairs.next().expect("Pair must have >1 children");

                    if let Some((key_index, access_modifier)) =
                        layer.aliases.get(reference_identifier.as_str())
                    {
                        let button = self.alias_dependency_graph.lookup_node_by_index(*key_index);

                        // process the button
                        //
                        // TODO: unify the occurrences of this line
                        // currently there are 3 separate instances
                        // MISTAKE: didn't change button context to referenced layer when processing layer references
                        let _ = self.process_lazy_button(
                            reference_identifier.as_str(),
                            button,
                            &ButtonContext::Layer(reference_layer_name.as_str()),
                        );

                        // use internal helper macro because we already have the LazyButton
                        _verify_button_type_helper!(
                            button.unwrap_processed_ref().as_ref(),
                            button_identifier
                        );

                        if matches!(access_modifier, AccessModifier::Public) {
                            let cow = MaybeOwnedString::Owned(format!(
                                "{}{}.{}",
                                if prepend_at!() { "@" } else { "" },
                                reference_layer_name.as_str(),
                                reference_identifier.as_str()
                            ));

                            if matches!(return_type, NodeIndex) {
                                Index(*key_index)
                            } else {
                                Button(create_kbd_only_button(cow))
                            }
                        } else {
                            self.error(format!(
                                "button `{}` is private",
                                reference_identifier.as_str()
                            ))
                            .add_message(Message::from_pest_span_no_text(
                                &reference_identifier.as_span(),
                            ))
                            .add_note("consider making it public to use it in this context");

                            err!("illegal access")
                        }
                    } else {
                        self.error("could not resolve layer reference").add_message(
                            Message::from_pest_span_no_text(&reference_identifier.as_span()),
                        );

                        err!("undefined reference")
                    }
                } else {
                    self.error("undefined layer")
                        .add_message(Message::from_pest_span_no_text(
                            &reference_layer_name.as_span(),
                        ));

                    err!("undefined layer")
                }
            }
            R::identifier => {
                if let ButtonContext::Layer(layer_name) = context {
                    // try to lookup in layer
                    if let Some(layer) = self.parser_data.layers.get(*layer_name) {
                        if let Some((key_index, _)) = layer.aliases.get(button_identifier.as_str())
                        {
                            let button =
                                self.alias_dependency_graph.lookup_node_by_index(*key_index);

                            // process the button
                            let _ = self.process_lazy_button(
                                button_identifier.as_str(),
                                button,
                                context,
                            );

                            // use internal helper macro because we already have the LazyButton
                            _verify_button_type_helper!(
                                button.unwrap_processed_ref().as_ref(),
                                button_identifier
                            );

                            if matches!(return_type, NodeIndex) {
                                return Index(*key_index);
                            }

                            return Button(create_kbd_only_button(MaybeOwnedString::Owned(
                                format!(
                                    "{}{}.{}",
                                    if prepend_at!() { "@" } else { "" },
                                    layer_name,
                                    button_identifier.as_str()
                                ),
                            )));
                        }
                        // else, proceed by trying in global
                    } else {
                        unreachable!("Layer context cannot be invalid! {:?}", layer_name)
                    }
                }

                if let Some(key_index) = self
                    .parser_data
                    .global_aliases
                    .get(button_identifier.as_str())
                {
                    let button = self.alias_dependency_graph.lookup_node_by_index(*key_index);

                    // process the button
                    let _ = self.process_lazy_button(
                        button_identifier.as_str(),
                        button,
                        &ButtonContext::Aliases,
                    );

                    // use internal helper macro because we already have the LazyButton
                    // TODO: move this out to the end of the method; return (NodeIndex, Option<Alias>)
                    _verify_button_type_helper!(
                        button.unwrap_processed_ref().as_ref(),
                        button_identifier
                    );

                    if matches!(return_type, NodeIndex) {
                        return Index(*key_index);
                    }

                    Button(create_kbd_only_button(if prepend_at!() {
                        MaybeOwnedString::Owned(format!("@{}", button_identifier.as_str()))
                    } else {
                        MaybeOwnedString::Borrowed(button_identifier.as_str())
                    }))
                } else {
                    let find_target = if expecting_constant {
                        "constant"
                    } else {
                        "alias"
                    };

                    let error_handle = self
                        .error(format!("could not resolve {}", find_target))
                        .add_message(Message::from_pest_span_no_text(
                            &button_identifier.as_span(),
                        ));

                    if !expecting_constant {
                        error_handle.add_note("should you be using a reference instead?");
                    }

                    err!("could not resolve");
                }
            }
            x => unreachable!("Cannot pass in {:?} to lookup_button", x),
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
        match self.lookup_button(
            button_identifier,
            context,
            LookupButtonType::ReferenceAt,
            false,
        ) {
            NodeIndexOrButton::Button(x) => x,
            _ => unreachable!("lookup_button must return ReferenceAt"),
        }
    }

    /// Given a `Pair` WITHIN a `button`, recursively processes the `Pair` and its children and returns a
    /// ProcessedButton.
    fn process_inner_button_pair(
        &self,
        original_button_index: Option<NodeIndex>,
        pair: Pair<'a>,
        context: &ButtonContext,
    ) -> ProcessedButton<'a> {
        use Rule as R;

        let processed_button = {
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

                        string_buffer.push_str(
                            &self
                                .process_inner_button_pair(original_button_index, child, context)?
                                .0,
                        );
                        string_buffer.push(' ');
                    }

                    if num_children != 0 {
                        // remove trailing space
                        string_buffer.pop();
                    }

                    string_buffer.push(')');

                    create_kbd_only_button(MaybeOwnedString::Owned(string_buffer))
                }
                R::button_alias | R::reference => {
                    // have to clone pair so we can use it in case of errors
                    let resolution = self.lookup_button(
                        pair.clone(),
                        context,
                        LookupButtonType::NodeIndex,
                        false,
                    );

                    match resolution {
                        NodeIndexOrButton::Index(other_index) => {
                            // MISTAKE: forgot to process button when looking up
                            let processed_button = self
                                .alias_dependency_graph
                                .lookup_node_by_index(other_index);

                            if processed_button.is_unprocessed() {
                                assert!(pair.as_rule() != R::reference);

                                let _ = processed_button.process(|pair| {
                                    self.process_button_pair(None, pair, context)
                                })?;
                            }

                            if let Some(index) = original_button_index {
                                self.alias_dependency_graph
                                    .add_dep_by_index(index, other_index);
                            };

                            create_kbd_only_button(MaybeOwnedString::Owned(format!(
                                "@{}",
                                self.alias_dependency_graph
                                    .lookup_key_by_index(&other_index)
                                    .expect("Index must be present in graph")
                            )))
                        }
                        NodeIndexOrButton::Button(button) => {
                            // this only happens when lookup_button throws an error
                            // unreachable!("lookup_button must return a Button since we passed in LookupButtonType::ReferenceAt for the last parameter")
                            return Err(button.expect_err("Since we passed in LookupButtonType::Index for the last parameter, lookup_button must only return a Button in case of errors"));
                        }
                    }
                }
                R::constant_reference => {
                    let identifier_or_reference = pair
                        .into_inner()
                        .next()
                        .expect("Constant references must have an inner reference type")
                        .into_inner()
                        .next()
                        .expect("Constant references must have an inner identifier|reference");

                    // TODO: repeated code #constant_reference
                    let lookup =
                        // NOTE: not expecting a constant this time
                        self.lookup_button(identifier_or_reference, context, LookupButtonType::NodeIndex, false);

                    match lookup {
                        NodeIndexOrButton::Index(index) => {
                            self.alias_dependency_graph
                                .lookup_node_by_index(index)
                                .unwrap_processed_ref()
                                .as_ref()
                                .map(|(string, value_type)| (string.clone(), value_type.clone()))
                                // throw away the error because we already reported it
                                .map_err(|_| eyre!("Placeholder error"))
                        }
                        NodeIndexOrButton::Button(button) => {
                            return Err(button.expect_err("lookup_button must only return a Button in case of errors"));
                        }
                    }
                }
                // we can create a constant-only button because the contents of the button will
                // immediately be merged into the outer button.
                // as a result, the actual type of this button is irrelevant.
                R::normal_button_non_keyword => create_constant_only_button(
                    MaybeOwnedString::Borrowed(pair.as_str().trim_end()),
                    false,
                ),
                R::double_quoted_string => self.process_double_quoted_string(pair, context),
                R::single_quoted_string => self.process_single_quoted_string(pair),
                x => unreachable!("Cannot have {:?} in `button`", x),
            }
        };

        processed_button
    }

    fn process_double_quoted_string(
        &self,
        pair: Pair<'a>,
        context: &ButtonContext,
    ) -> ProcessedButton<'a> {
        // subtract 2 for double quotes on both sides
        let original_string_length = pair.as_str().len() - 2;

        let mut inner_elements = pair.clone().into_inner();

        let first_inner = inner_elements
            .next()
            .expect("Double quoted strings must have inner text");

        // only do a borrowed Cow if possible; we have a double quoted string with no interpolation
        // at all
        if matches!(first_inner.as_rule(), Rule::double_quoted_string_inner_text)
            && inner_elements.peek().is_none()
        {
            create_constant_only_button(MaybeOwnedString::Borrowed(pair.as_str()), true)
        } else {
            let starting_capacity = (original_string_length as f32 * 1.5) as usize;
            let mut string_buffer = String::with_capacity(starting_capacity);

            string_buffer.push('"');

            let mut process_inner_string = |pair: Pair<'a>| -> color_eyre::eyre::Result<()> {
                match pair.as_rule() {
                    Rule::double_quoted_string_inner_text => string_buffer.push_str(pair.as_str()),
                    Rule::user_constant_reference => {
                        let identifier = pair
                            .into_inner()
                            .next()
                            .expect("Constant references must have inner identifiers");

                        let result = self.lookup_button(
                            identifier.clone(),
                            &context,
                            LookupButtonType::NodeIndex,
                            true,
                        );

                        match result {
                            NodeIndexOrButton::Index(index) => {
                                let value = self.alias_dependency_graph.lookup_node_by_index(index);
                                if value.is_unprocessed() {
                                    value.process(|pair| {
                                        self.process_button_pair(
                                            Some(identifier.as_str()),
                                            pair,
                                            &context,
                                        )
                                    })?;
                                }

                                let processed_button = value.unwrap_processed_ref();

                                match processed_button.as_ref() {
                                    Ok((string, rvalue_info)) => {
                                        if rvalue_info.is_valid_constant() {
                                            if rvalue_info.is_quoted() {
                                                // skip the outer quotes
                                                string_buffer.push_str(&string[1..string.len() - 1])
                                            } else {
                                                string_buffer.push_str(string.as_ref())
                                            }
                                        } else {
                                            self.error("mismatched types").add_message(
                                                Message::from_pest_span(
                                                    &identifier.as_span(),
                                                    "expected constant, found lisp button",
                                                ),
                                            )
                                            .add_note("only numbers and strings may be interpolated using the $ syntax");

                                            bail!("Expected constant, got button")
                                        }
                                    }
                                    Err(_) => bail!("Variable dependency failed"),
                                }
                            }
                            _ => bail!("Could not resolve variable"),
                        }
                    }
                    Rule::escaped_dollar => {
                        string_buffer.push_str("$")
                    }
                    x => panic!("Invalid inner double quoted string rule: {:?}", x),
                };

                Ok(())
            };

            process_inner_string(first_inner)?;
            for inner in inner_elements {
                process_inner_string(inner)?;
            }

            string_buffer.push('"');

            create_constant_only_button(MaybeOwnedString::Owned(string_buffer), true)
        }
    }

    fn process_single_quoted_string(&self, pair: Pair<'a>) -> ProcessedButton<'a> {
        let inner_rule = pair
            .into_inner()
            .next()
            .expect("Single quoted strings must have inner text");

        if let Some(keycode) = normalize_keycode(inner_rule.as_str()) {
            create_kbd_only_button(MaybeOwnedString::Borrowed(keycode))
        } else {
            self.error("invalid keycode")
                .add_message(Message::from_pest_span(
                    &inner_rule.as_span(),
                    format!("looked up {}", inner_rule.as_str()),
                ));

            bail!("invalid keycode")
        }
    }

    /// Given a `button` rule, creates and returns a `ProcessedButton`
    fn process_button_pair(
        &self,
        button_name: Option<&str>,
        pair: Pair<'a>,
        context: &ButtonContext,
    ) -> ProcessedButton<'a> {
        use Rule as R;

        let get_button_index = || {
            if let Some(button_name) = button_name {
                Some(match context {
                    ButtonContext::Aliases => *self
                        .parser_data
                        .global_aliases
                        .get(button_name)
                        .expect("Alias must be present in list"),
                    ButtonContext::Layer(name) => {
                        let layer = self
                            .parser_data
                            .layers
                            .get(*name)
                            .expect("Button name must be valid");

                        layer
                            .aliases
                            // MISTAKE: got `name` instead of button_name
                            .get(button_name)
                            .expect("Alias must be present in list")
                            .0
                    }
                })
            } else {
                None
            }
        };

        match pair.as_rule() {
            // if its a regular button
            R::button => {
                let inner_rule = pair
                    .into_inner()
                    .next()
                    .expect("Buttons must contain an inner rule");

                let original_button_index = get_button_index();

                self.process_inner_button_pair(original_button_index, inner_rule, context)
            }
            R::single_quoted_string => self.process_single_quoted_string(pair),
            R::double_quoted_string => self.process_double_quoted_string(pair, context),
            R::identifier | R::reference => {
                // OPTIMIZE: do not repeat the work here; abstract so that we can get the
                // alias and the node index at the same time
                let other_index =
                    // OPTIMIZE: related: do not clone the pair here
                    match self.lookup_button(pair.clone(), context, LookupButtonType::NodeIndex, false) {
                        NodeIndexOrButton::Index(index) => index,
                        NodeIndexOrButton::Button(button) => {
                            return Err(button.expect_err("Since we passed in LookupButtonType::Index for the last parameter, lookup_button must only return a Button in case of errors"))
                        },
                    };

                let alias_string = self.button_identifier_to_kbdx_alias(pair, context);

                // MISTAKE: only need to do dependency stuff when the original button is named; do
                // not do for [[keys]]
                let original_button_index = get_button_index();

                if let Some(index) = original_button_index {
                    self.alias_dependency_graph
                        .add_dep_by_index(index, other_index);
                };

                alias_string
            }
            R::number => create_either_button(MaybeOwnedString::Borrowed(pair.as_str())),
            R::constant_reference => {
                let inner = pair
                    .into_inner()
                    .next()
                    .expect("Constant reference must have an internal type");
                match inner.as_rule() {
                    R::user_constant_reference => {
                        // TODO: repeated code #constant_reference
                        let identifier = inner
                            .into_inner()
                            .next()
                            .expect("User constant reference must contain an identifier");

                        assert_eq!(identifier.as_rule(), R::identifier);

                        let lookup = self.lookup_button(
                            identifier,
                            context,
                            LookupButtonType::NodeIndex,
                            true,
                        );

                        if let NodeIndexOrButton::Index(index) = lookup {
                            self.alias_dependency_graph
                                .lookup_node_by_index(index)
                                .unwrap_processed_ref()
                                .as_ref()
                                .map(|(string, value_type)| (string.clone(), value_type.clone()))
                                // throw away the error because we already reported it
                                .map_err(|_| eyre!("Placeholder error"))
                        } else {
                            bail!("")
                        }
                    }
                    _ => unimplemented!(),
                }
            }
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
        button_name: &str,
        button: &LazyButton<'a, ProcessedButton<'a>>,
        context: &ButtonContext,
    ) -> color_eyre::Result<()> {
        button.process(|pair| self.process_button_pair(Some(button_name), pair, context))
    }

    pub fn compile_string(mut self) -> color_eyre::Result<String> {
        // read configuration into Configuration struct
        let configuration = self.try_parse_configuration()?;

        // we use used_keys to derive the source layer
        let mut used_keys = BTreeSet::default();
        let mut processed_layers: AHashMap<String, ProcessedLayer<ProcessedButton>> =
            AHashMap::default();

        for (layer_name, layer) in &self.parser_data.layers {
            let button_context = ButtonContext::Layer(&layer_name);

            for (key_name, key_value) in &layer.keys {
                // MISTAKE: was giving self.process_lazy_button the name of the key being mapped;
                // instead, should treat the keys as unnamed buttons
                if key_value.is_unprocessed() {
                    key_value
                        .process(|pair| self.process_button_pair(None, pair, &button_context))?;
                }

                used_keys.insert(key_name);
            }

            for parent_name in &layer.parent_name {
                match parent_name.as_str() {
                    "default" | "fallthrough" | "block" | "source" => (),
                    x => {
                        if self.parser_data.layers.get(x).is_none() {
                            self.error("undefined parent layer").add_message(
                                Message::from_pest_span_no_text(&parent_name.as_span()),
                            );
                        }
                    }
                }
            }
        }

        if self.has_errors() {
            bail!("errors")
        }

        let file_source = unsafe { (*self.file_diagnostics.get()).file_contents() }.as_bytes();
        for (_, layer) in &self.parser_data.layers {
            for (_, (lazy_button_index, access_modifier)) in &layer.aliases {
                let lazy_button = self
                    .alias_dependency_graph
                    .lookup_node_by_index(*lazy_button_index);

                if lazy_button.is_unprocessed() {
                    let rvalue_span = lazy_button.unwrap_unprocessed_ref().as_span();

                    // HACK: search for the lvalue byte bounds from the rvalue span
                    // we should really be storing the lvalue span
                    let (lvalue_start, lvalue_end) = {
                        let mut current_byte = rvalue_span.start();
                        let mut sep_byte = current_byte;

                        loop {
                            // NOTE: we do not have to check for current_byte bounds because assignments are guaranteed to come after a newline
                            match file_source[current_byte] {
                                b'\n' => break (current_byte + 1, sep_byte),
                                b' ' | b'=' => sep_byte = current_byte,
                                _ => (),
                            };

                            current_byte -= 1;
                        }
                    };

                    self.warning(format!(
                        "unused {} alias",
                        match access_modifier {
                            AccessModifier::Public => "public",
                            AccessModifier::Private => "private",
                        }
                    ))
                    .add_message(Message::from_byte_range_no_text(lvalue_start..lvalue_end));
                }
            }
        }

        let fallthrough_layer = ProcessedLayer::new(
            used_keys
                .iter()
                .map(|_| LayerButton::fallthrough_button())
                .collect::<Vec<_>>(),
            None,
        );
        let block_layer = ProcessedLayer::new(
            used_keys
                .iter()
                .map(|_| LayerButton::block_button())
                .collect::<Vec<_>>(),
            None,
        );
        let source_layer = ProcessedLayer::new(
            used_keys
                .iter()
                .map(|s| LayerButton::new_inherited(s))
                .collect::<Vec<_>>(),
            None,
        );

        let default_parent = {
            if configuration.fallthrough {
                fallthrough_layer.clone_as_parent()
            } else {
                block_layer.clone_as_parent()
            }
        };

        struct ProcessLayerImmutableContext<'a, 'b: 'a> {
            default_parent: ProcessedLayer<'a, ()>,
            block_layer: ProcessedLayer<'a, ()>,
            fallthrough_layer: ProcessedLayer<'a, ()>,
            source_layer: ProcessedLayer<'a, ()>,
            parser_layers: &'a LayerMap<'b, ProcessedButton<'b>>,
        }

        fn process_layer<'mut_ref, 'map, 'source>(
            processed_layers: &'mut_ref mut AHashMap<
                String,
                ProcessedLayer<'map, ProcessedButton<'map>>,
            >,
            state: &'map ProcessLayerImmutableContext<'map, 'source>,
            layer_name: &str,
        ) -> &'mut_ref ProcessedLayer<'map, ProcessedButton<'map>>
        where
            'source: 'map,
        {
            if let Some(layer) = match layer_name {
                "default" => Some(&state.default_parent),
                "fallthrough" => Some(&state.fallthrough_layer),
                "block" => Some(&state.block_layer),
                "source" => Some(&state.source_layer),
                other => {
                    if processed_layers.contains_key(other) {
                        // we cannot just use if-let and return because non-lexical lifetimes are not yet
                        // supported
                        return processed_layers.get(other).unwrap();
                    }

                    None
                }
            } {
                // TODO: make sure that the `()` does not get optimized out in memory?
                // HACK: transmuting &ProcessedLayer<()> to &ProcessedLayer<ProcessedButton>
                // This is safe because we know that state.{layers} are initialized with
                // `None` for `aliases`.
                // Because `aliases` is of type `Option<&T>` (simplified significantly), we can
                // conclude that the `T` parameter has no effect on size or safety and can thus be
                // modified freely
                return unsafe { std::mem::transmute(layer) };
            }

            // TODO: entry insert layer if it doesnt exist
            // TODO: if the layer does not exist, create it...
            // TODO: if the layer exists, proceed...
            //
            // TODO: processing builtin layers should have a separate function?
            // they should not be put into the processed_layers map
            let layer = state.parser_layers.get(layer_name).unwrap();

            let mut parents = layer.parent_name.iter();
            let mut new_layer: ProcessedLayer<ProcessedButton> =
                if let Some(parent) = parents.next() {
                    process_layer(processed_layers, state, parent.as_str()).clone_as_parent()
                } else {
                    // NOTE: this usage of `transmute` is justified earlier in the function
                    unsafe {
                        std::mem::transmute::<&ProcessedLayer<()>, &ProcessedLayer<ProcessedButton>>(
                            &state.default_parent,
                        )
                    }
                    .clone_as_parent()
                };

            for parent_name in parents {
                // do inheritance
                let parent = process_layer(processed_layers, state, parent_name.as_str());
                new_layer.inherit(parent);
            }

            for (key_name, key_value) in &layer.keys {
                let index = state
                    .source_layer
                    .keys
                    .binary_search_by_key(key_name, |k| k.value())
                    .expect("Key name must exist in source layer");

                *new_layer.keys.get_mut(index).unwrap() = LayerButton::new(unsafe {
                    // HACK: (this is horrible) converting a temporary reference into a longer reference with transmute
                    // this is "safe" because the underlying reference has a lifetime of 'source, which is longer than 'map
                    std::mem::transmute(
                        key_value
                            .unwrap_processed_ref()
                            .as_ref()
                            .expect("Keys must be successfully processed")
                            .0
                            .as_ref(),
                    )
                });
            }

            let ref aliases = layer.aliases;
            // HACK: (this is even worse than the other one) blindly converting into the type we want
            // the error was "lifetime mismatch: data from `processed_layers` flows into `state`"
            new_layer.aliases = Some(unsafe { std::mem::transmute(aliases) });

            match processed_layers.entry(layer_name.to_owned()) {
                Entry::Vacant(entry) => entry.insert(new_layer),
                _ => unreachable!("Entry must be vacant"),
            }
        }

        let process_layer_state = ProcessLayerImmutableContext {
            default_parent,
            block_layer,
            fallthrough_layer,
            source_layer,
            parser_layers: &self.parser_data.layers,
        };

        for (layer_name, _) in &self.parser_data.layers {
            let _ = process_layer(&mut processed_layers, &process_layer_state, layer_name);
        }

        if self.has_errors() {
            bail!("errors")
        }

        // 20KB buffer
        let mut string_buffer = String::with_capacity(20 * 1024);

        write!(string_buffer, "{}", configuration)
            .expect("Writing to string buffer must not error");

        // write [aliases] table
        // print out all edges in graph
        // self.alias_dependency_graph.print_all_connections();

        let alias_block = AliasBlock {
            dependency_graph: self.alias_dependency_graph,
        };

        write!(string_buffer, "{}", alias_block).expect("Writing to string buffer must not error");

        let write_layer = |layer_name: &str, layer, string_buffer: &mut String| {
            // println!("layer: {}", layer_name);
            write!(string_buffer, "{}", LayerTuple(&layer_name, layer))
                .expect("Writing to string buffer must not error");
        };

        let default_layer = processed_layers
            .remove(configuration.starting_layer)
            .expect("starting layer must exist");

        write!(
            string_buffer,
            "{}",
            SourceLayer(process_layer_state.source_layer.clone_as_parent())
        )
        .expect("Writing to string buffer must not error");

        write_layer(
            configuration.starting_layer,
            default_layer,
            &mut string_buffer,
        );

        for (layer_name, layer) in processed_layers {
            write_layer(&layer_name, layer, &mut string_buffer)
        }

        Ok(string_buffer)
    }
}

/// Writes a Lisp "section" where all items in `body` are indented.
/// The output is in the following form:
/// ```
/// (<header>
///     <body item 1>
///     <body item 2>
///     ...
///     )
/// ```
fn write_section<T>(
    header: &str,
    body: impl IntoIterator<Item = T>,
    formatter: &mut Formatter<'_>,
) -> fmt::Result
where
    T: Display,
{
    macro_rules! _write {
        ($($inner:tt),+) => {
            writeln!(formatter, $($inner),+)?
        };
    }

    let mut body_iterator = body.into_iter().peekable();

    // if there are no children, do not write the block
    if body_iterator.peek().is_none() {
        // tried returning Err to stop the newline from being written but that made println panic
        // solution was to replace writeln with just write
        // with write, nothing gets written unless this function writes something
        // write_section writes a trailing newline, so it takes care of that on its own
        return Ok(());
    }

    _write!("({}", header);

    for child in body_iterator {
        _write!("{}{}", INDENT_LEVEL, child)
    }

    _write!("{})", INDENT_LEVEL);

    Ok(())
}
