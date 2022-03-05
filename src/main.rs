use std::{fs, fmt::Display};

use pest::{Parser, iterators::Pair, RuleType};
use rremix::parser::{RemixParser, Rule};

fn main() {
    let file = fs::read_to_string("test.rem").expect("failed to read test.rem");
    let parsed = RemixParser::parse(
        Rule::function_definition,
        &file
    ).expect("failed parse");
    print!("{}", parsed.map(|pair| format_pair(pair, 0, true)).collect::<Vec<_>>().join("\n"));
}

fn format_pair<T: RuleType + std::fmt::Debug>(pair: Pair<T>, indent_level: usize, is_newline: bool) -> String {
    let indent = if is_newline {
        "  ".repeat(indent_level)
    } else {
        "".to_string()
    };

    let children: Vec<_> = pair.clone().into_inner().collect();
    let len = children.len();
    let children: Vec<_> = children.into_iter().map(|pair| {
        format_pair(pair, if len > 1 { indent_level + 1 } else { indent_level }, len > 1)
    }).collect();

    let dash = if is_newline {
        "- "
    } else {
        ""
    };

    match len {
        0 => format!("{}{}{:?}: {:?}", indent, dash, pair.as_rule(), pair.into_span().as_str()),
        1 => format!("{}{}{:?} > {}", indent, dash, pair.as_rule(), children[0]),
        _ => format!("{}{}{:?}\n{}", indent, dash, pair.as_rule(), children.join("\n"))
    }
}
