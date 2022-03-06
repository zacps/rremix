use pest::{Parser};
use pest_derive::*;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct RemixParser;


#[cfg(test)]
mod tests {
    use std::fs;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use test_case::test_case;
    use crate::format_pair;

    use super::*;

    static PARSER_TESTS: [&'static str; 50] = [
        "standard-lib.rem",
        "ex/factorial.rem",
        "ex/factorial2.rem",
        "ex/primes.rem",
        "ex/test1.rem",
        "ex/test2.rem",
        "ex/test3.rem",
        "ex/test4.rem",
        "ex/test5.rem",
        "ex/test6.rem",
        "ex/test7.rem",
        "ex/test8.rem",
        "ex/test9.rem",
        "ex/test10.rem",
        "ex/test11.rem",
        "ex/test12.rem",
        "ex/test13.rem",
        "ex/test14.rem",
        "ex/test15.rem",
        "ex/test16.rem",
        "ex/test17.rem",
        "ex/test18.rem",
        "ex/test19.rem",
        "ex/test20.rem",
        "ex/test21.rem",
        "ex/test22.rem",
        "ex/test23.rem",
        "ex/test24.rem",
        "ex/test25.rem",
        "ex/test26.rem",
        "ex/test27.rem",
        "ex/test28.rem",
        "gr/drawing.rem",
        "gr/arrows.rem",
        "gr/squares.rem",
        "gr/bounce.rem",
        "ob/obj-demo.rem",
        "ob/sidewindermaze.rem",
        "ex/test29.rem",
        "ex/test30.rem",
        "ex/test31.rem",
        "ex/test32.rem",
        "ex/test33.rem",
        "ex/test34.rem",
        "ex/test35.rem",
        "ex/test36.rem",
        "ex/test37.rem",
        "ex/test38.rem",
        "ex/test39.rem",
        "temp.rem"
    ];

    #[test_case("standard-lib.rem")]
    #[test_case("ex/factorial.rem")]
    #[test_case("ex/factorial2.rem")]
    #[test_case("ex/primes.rem")]
    #[test_case("ex/test1.rem")]
    #[test_case("ex/test2.rem")]
    #[test_case("ex/test3.rem")]
    #[test_case("ex/test4.rem")]
    #[test_case("ex/test5.rem")]
    #[test_case("ex/test6.rem")]
    #[test_case("ex/test7.rem")]
    #[test_case("ex/test8.rem")]
    #[test_case("ex/test9.rem")]
    #[test_case("ex/test10.rem")]
    #[test_case("ex/test11.rem")]
    #[test_case("ex/test12.rem")]
    #[test_case("ex/test13.rem")]
    #[test_case("ex/test14.rem")]
    #[test_case("ex/test15.rem")]
    #[test_case("ex/test16.rem")]
    #[test_case("ex/test17.rem")]
    #[test_case("ex/test18.rem")]
    #[test_case("ex/test19.rem")]
    #[test_case("ex/test20.rem")]
    #[test_case("ex/test21.rem")]
    #[test_case("ex/test22.rem")]
    #[test_case("ex/test23.rem")]
    #[test_case("ex/test24.rem")]
    #[test_case("ex/test25.rem")]
    #[test_case("ex/test26.rem")]
    #[test_case("ex/test27.rem")]
    #[test_case("ex/test28.rem")]
    #[test_case("gr/drawing.rem")]
    #[test_case("gr/arrows.rem")]
    #[test_case("gr/squares.rem")]
    #[test_case("gr/bounce.rem")]
    #[test_case("ob/obj-demo.rem")]
    #[test_case("ob/sidewindermaze.rem")]
    #[test_case("ex/test29.rem")]
    #[test_case("ex/test30.rem")]
    #[test_case("ex/test31.rem")]
    #[test_case("ex/test32.rem")]
    #[test_case("ex/test33.rem")]
    #[test_case("ex/test34.rem")]
    #[test_case("ex/test35.rem")]
    #[test_case("ex/test36.rem")]
    #[test_case("ex/test37.rem")]
    #[test_case("ex/test38.rem")]
    #[test_case("ex/test39.rem")]
    fn test_parser(path: &'static str) {
        let file = fs::read_to_string("../Remix/".to_owned() + path).expect("could not find test file");
        match RemixParser::parse(
            Rule::program,
            &file
        ) {
            Ok(mut parse) => {
                let pair = parse.next();
                assert_snapshot!(path, format_pair(pair.unwrap(), 0, true))
            }
            Err(e) => {
                eprintln!("{e}");
                panic!("Failed to parse file {path}")
            }
        }
    }
}
