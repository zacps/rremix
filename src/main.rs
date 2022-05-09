/// The compiler entrypoint.
///
/// This sequentially calls a number of 'passes' which do the actual work.
/// * Parsing
/// * AST building (HIR)
/// * Function resolution
/// * Codegen
use std::{
    fs::{self, File},
    io::Write,
    path::PathBuf,
};

use clap::Parser;
use pest::Parser as PestParser;
use rremix::{
    codegen::Codegen,
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

    // Parse the program
    let mut parsed = match RemixParser::parse(Rule::program, &file) {
        Ok(parse) => parse,
        Err(e) => {
            eprintln!("{e}");
            panic!("failed to parse {:?}", &args.file);
        }
    };

    // Build the AST
    // This step also resolves variable names (but not functions) and resolves
    // what ambiguities we can.
    let mut ast = Program::new(&file, parsed.next().unwrap(), false);

    // Resolve functions
    // This is a separate step to avoid needing forward declarations.
    Resolver::resolve(&mut ast);

    // // Codegen
    // // Where the magic happens. Currently we just emit C which is compiled
    // // by clang.
    // File::create("out.c")
    //     .unwrap()
    //     .write(
    //         Codegen::new()
    //             .emit_program(ast)
    //             .expect("codegen failed")
    //             .as_bytes(),
    //     )
    //     .unwrap();
}
