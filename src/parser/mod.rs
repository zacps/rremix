use pest_derive::*;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct RemixParser;

#[cfg(test)]
mod tests {
    use crate::format_pair;
    use insta::assert_snapshot;
    use pest::Parser;
    use std::fs;
    use test_case::test_case;

    use super::*;

    #[test_case("../src/standard-lib.rem")]
    //#[test_case("ex/factorial.rem")]
    //#[test_case("ex/factorial2.rem")]
    //#[test_case("ex/primes.rem")]
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
    // #[test_case("ex/test27.rem")]
    #[test_case("ex/test28.rem")]
    //#[test_case("gr/drawing.rem")]
    //#[test_case("gr/arrows.rem")]
    //#[test_case("gr/squares.rem")]
    //#[test_case("gr/bounce.rem")]
    //#[test_case("ob/obj-demo.rem")]
    //#[test_case("ob/sidewindermaze.rem")]
    //#[test_case("ex/test29.rem")]
    //#[test_case("ex/test30.rem")]
    //#[test_case("ex/test31.rem")]
    //#[test_case("ex/test32.rem")]
    //#[test_case("ex/test33.rem")]
    //#[test_case("ex/test34.rem")]
    //#[test_case("ex/test35.rem")]
    //#[test_case("ex/test36.rem")]
    //#[test_case("ex/test37.rem")]
    //#[test_case("ex/test38.rem")]
    //#[test_case("ex/test39.rem")]
    fn test_parser(path: &'static str) {
        let file =
            fs::read_to_string("tests/".to_owned() + path).expect("could not find test file");
        match RemixParser::parse(Rule::program, &file) {
            Ok(mut parse) => {
                let pair = parse.next();
                assert_snapshot!(path, format_pair(&pair.unwrap(), 0, true))
            }
            Err(e) => {
                eprintln!("{e}");
                panic!("Failed to parse file {path}")
            }
        }
    }
}
