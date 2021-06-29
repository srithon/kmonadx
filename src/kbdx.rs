use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "kbdx.pest"]
pub struct Parser;
