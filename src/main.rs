use std::fs;

use pest::Parser;
use rremix::{parser::{RemixParser, Rule}, format_pair};

fn main() {
    let file = fs::read_to_string("test.rem").expect("failed to read test.rem");
    let parsed = RemixParser::parse(
        Rule::function_definition,
        &file
    ).expect("failed parse");
    print!("{}", parsed.map(|pair| format_pair(pair, 0, true)).collect::<Vec<_>>().join("\n"));
}
