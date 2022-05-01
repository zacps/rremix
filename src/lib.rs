//! rRemix is an experimental compiler for the Remix programming language.
//!
//! The compiler is implemented as a number of simple sequential passes.
//! Currently, the following parses are used:
//! * [Parser](parser)
//! * [AST](HIR)
//! * [Function resolution](resolver)
//! * [Codegen](codegen)

// We deny a few lints which are almost certainly errors.
// Unreachable patterns in particular is an easy mistake to make in
// HIR if a use of enum variants is missing.
#![forbid(unsafe_code, unreachable_patterns, overlapping_range_endpoints)]
#![deny(rust_2018_idioms)]

use parser::RemixParser;
use pest::{iterators::Pair, Parser, RuleType, Span};

/// High level intermediate representation.
///
/// This represents the compiler's state after name resolution, before optimisation and codegen.
/// We will have rejected the majority of incomplete programs after this is constructed.
#[allow(non_snake_case)]
pub mod HIR;
// Parser
pub mod parser;
// Name resolution
pub mod resolver;
// Codegen
pub mod codegen;

static STANDARD_LIB: &'static str = include_str!("standard-lib.rem");
static MINIMAL_STANDARD_LIB: &'static str = include_str!("standard-lib-min.rem");

static C_LIB: &'static str = include_str!("remix.c");
static C_HEADER: &'static str = include_str!("remix.h");

/// Format a parsed token pair nicely.
pub fn format_pair<T: RuleType + std::fmt::Debug>(
    pair: &Pair<'_, T>,
    indent_level: usize,
    is_newline: bool,
) -> String {
    let indent = if is_newline {
        "  ".repeat(indent_level)
    } else {
        "".to_string()
    };

    let children: Vec<_> = pair.clone().into_inner().collect();
    let len = children.len();
    let children: Vec<_> = children
        .into_iter()
        .map(|pair| {
            format_pair(
                &pair,
                if len > 1 {
                    indent_level + 1
                } else {
                    indent_level
                },
                len > 1,
            )
        })
        .collect();

    let dash = if is_newline { "- " } else { "" };

    match len {
        0 => format!(
            "{}{}{:?}: {:?}",
            indent,
            dash,
            pair.as_rule(),
            pair.as_span().as_str()
        ),
        1 => format!("{}{}{:?} > {}", indent, dash, pair.as_rule(), children[0]),
        _ => format!(
            "{}{}{:?}\n{}",
            indent,
            dash,
            pair.as_rule(),
            children.join("\n")
        ),
    }
}

trait SpanExt {
    fn format(&self) -> String;
}

impl<'s> SpanExt for Span<'s> {
    /// Format a span as a machine/human readable position.
    ///
    /// ```{line}:col..line:col```
    fn format(&self) -> String {
        let (start, end) = self.clone().split();
        format!(
            "{}:{}..{}:{}",
            start.line_col().0,
            start.line_col().1,
            end.line_col().0,
            end.line_col().1
        )
    }
}
