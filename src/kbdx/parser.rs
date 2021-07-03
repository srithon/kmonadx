use pest::iterators::Pairs;

use pest_derive::Parser;
use pest::Parser as _;

#[derive(Parser)]
#[grammar = "kbdx/kbdx.pest"]
pub struct Parser;

pub type ParseError = pest::error::Error<Rule>;
pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;

use ahash::AHashMap;

enum AccessModifier {
    Public,
    Private,
}

pub type Map<'a, T> = AHashMap<&'a str, T>;
pub type PairMap<'a> = Map<'a, Pair<'a>>;
pub type LayerMap<'a> = Map<'a, Layer<'a>>;

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
        unimplemented!()
    }
}
