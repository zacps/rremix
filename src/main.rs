use std::{fs, path::PathBuf};

use clap::Parser;
use pest::Parser as PestParser;
use rremix::{
    format_pair,
    parser::{RemixParser, Rule},
    resolver::Resolver,
    HIR::Program,
};

/// RRemix: The experimental Remix compiler
#[derive(Parser, Debug)]
struct Args {
    /// The Remix file to compile
    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(&args.file).expect(&format!("failed to read {:?}", &args.file));
    let mut parsed = RemixParser::parse(Rule::program, &file).expect("failed parse");
    print!(
        "{}",
        parsed
            .clone()
            .map(|ref pair| format_pair(pair, 0, true))
            .collect::<Vec<_>>()
            .join("\n")
    );

    let ast = Program::new(parsed.next().unwrap());
    println!("{:?}", ast);

    let resolved_ast = Resolver::resolve(ast);
    println!("{:?}", resolved_ast);
}
