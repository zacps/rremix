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

    let mut parsed = match RemixParser::parse(Rule::program, &file) {
        Ok(parse) => parse,
        Err(e) => {
            eprintln!("{e}");
            panic!("failed to parse {:?}", &args.file);
        }
    };
    println!(
        "{}",
        parsed
            .clone()
            .map(|ref pair| format_pair(pair, 0, true))
            .collect::<Vec<_>>()
            .join("\n")
    );

    let mut ast = Program::new(parsed.next().unwrap());
    // println!("{:#?}", ast);

    Resolver::resolve(&mut ast);
    println!("{:#?}", ast);
}
