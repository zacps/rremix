use pest::Parser;
use pest_derive::*;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct RemixParser;
