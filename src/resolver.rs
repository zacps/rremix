/// This module is responsible for name resolution, both for variables and functions.
///
use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;
use pest::Parser;

use crate::{parser::RemixParser, parser::Rule, HIR::*};

/// Names and corresponding IDs of builtin functions.
/// These are added to the symbol table during resolution
static BUILTIN_SYMBOLS: Lazy<Mutex<Vec<(FunctionName<'static>, FunctionID)>>> = Lazy::new(|| {
    let mut map = Vec::new();

    let builtins = vec![
        "do (executable)",
        "based on (original)",
        "show (output)",
        "ask (description)",
        "if (condition) (consequence)",
        "if (condition) (consequence) otherwise (alternative)",
        "type of (thing)",
        "convert (string-input) to integer",
        "convert (item) to string",
        "wait (number) sec/secs",
        "length of (list)",
        "length of /the (list)",
        "start (list)",
        "next (list)",
        "end of /the (list)",
        "add/append /the (value) to /the (list)",
        "(start) to (finish)",
        // index/key
        "(list) (index)",
        "(list) (index) (value)",
        "randomize",
        "randomize with (seed)",
        "random (max-value)",
        "sine (degrees)",
        "cosine (degrees)",
        "arctangent (change-y) over (change-x)",
        "sqrt/√ (value)",
    ];

    for (i, builtin) in builtins.iter().enumerate() {
        map.push((
            visit_function_signature(
                RemixParser::parse(Rule::function_signature, builtin)
                    .unwrap()
                    .next()
                    .unwrap(),
                None,
            ),
            i.into(),
        ));
    }

    Mutex::new(map.into())
});

type SymbolTable<'s> = Vec<(FunctionName<'s>, FunctionID)>;

fn symbol_get<'s>(table: &SymbolTable<'s>, function_name: &FunctionName<'s>) -> Option<FunctionID> {
    let mut matches = Vec::new();
    for (def, id) in table {
        if function_name.resolves_to(def) {
            matches.push(*id)
        }
    }
    if matches.len() == 1 {
        Some(matches[0])
    } else {
        None
    }
}

pub struct Resolver<'s> {
    symbol_table: SymbolTable<'s>,
}

impl<'s> Resolver<'s> {
    pub fn resolve(program: &mut Program<'s>) -> SymbolTable<'s> {
        let mut r = Self {
            symbol_table: BUILTIN_SYMBOLS.lock().unwrap().clone(),
        };
        r.symbol_table.extend(program.symbol_table());
        r.resolve_names(program);
        r.symbol_table
    }

    fn resolve_names(&self, program: &mut Program<'s>) {
        use Statement::*;
        for statement in &mut program.main {
            match statement {
                Expression(expr) => self.visit_expr(expr),
                Assignment { variable, value } => {
                    self.visit_var(variable);
                    self.visit_expr(value);
                }
                ListAssignment {
                    variable,
                    index,
                    value,
                } => {
                    self.visit_var(variable);
                    self.visit_expr(index);
                    self.visit_expr(value);
                }
                Return | Redo => (),
            }
        }
    }

    pub(crate) fn visit_expr(&self, expr: &mut Expression<'s>) -> () {
        use Expression::*;
        match expr {
            Binary { lhs, operator, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Unary(unary) => self.visit_unary(unary),
        }
    }

    pub(crate) fn visit_var(&self, variable: &mut Variable<'s>) -> () {
        todo!()
    }

    pub(crate) fn visit_unary(&self, unary: &mut UnaryExpression<'s>) {
        use UnaryExpression::*;
        match unary {
            FunctionCall { function } => {
                if let Some(_id) = symbol_get(&self.symbol_table, function) {
                    // map function call to function table
                    function.id = Some(_id)
                } else {
                    panic!("ICE: Attempted to call unknown function {:?}", function);
                }
            }
            ListElement { variable, index } => {
                self.visit_var(variable);
                self.visit_expr(index);
            }
            Literal(_) => todo!(),
            Variable(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use crate::resolver::Resolver;
    use crate::HIR;
    use crate::HIR::VisitAst;

    use insta::assert_debug_snapshot;
    use pest::Parser;
    use std::collections::{HashMap, HashSet};
    use std::fs;
    use test_case::test_case;

    //#[test_case("standard-lib.rem")]
    //#[test_case("ex/factorial.rem")]
    //#[test_case("ex/factorial2.rem")]
    //#[test_case("ex/primes.rem")]
    #[test_case("ex/test1.rem")]
    //#[test_case("ex/test2.rem")]
    //#[test_case("ex/test3.rem")]
    //#[test_case("ex/test4.rem")]
    //#[test_case("ex/test5.rem")]
    //#[test_case("ex/test6.rem")]
    //#[test_case("ex/test7.rem")]
    //#[test_case("ex/test8.rem")]
    //#[test_case("ex/test9.rem")]
    //#[test_case("ex/test10.rem")]
    //#[test_case("ex/test11.rem")]
    //#[test_case("ex/test12.rem")]
    //#[test_case("ex/test13.rem")]
    //#[test_case("ex/test14.rem")]
    //#[test_case("ex/test15.rem")]
    //#[test_case("ex/test16.rem")]
    //#[test_case("ex/test17.rem")]
    //#[test_case("ex/test18.rem")]
    //#[test_case("ex/test19.rem")]
    //#[test_case("ex/test20.rem")]
    //#[test_case("ex/test21.rem")]
    //#[test_case("ex/test22.rem")]
    //#[test_case("ex/test23.rem")]
    //#[test_case("ex/test24.rem")]
    //#[test_case("ex/test25.rem")]
    //#[test_case("ex/test26.rem")]
    //#[test_case("ex/test27.rem")]
    //#[test_case("ex/test28.rem")]
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
    fn test_build_ast(path: &'static str) {
        let file =
            fs::read_to_string("../Remix/".to_owned() + path).expect("could not find test file");
        match RemixParser::parse(Rule::program, &file) {
            Ok(mut parse) => {
                let pair = parse.next();
                let mut ast = HIR::Program::new(pair.unwrap());

                Resolver::resolve(&mut ast);

                let resolutions = ResolveCollector::collect_resolutions(&ast);

                // TODO: Replace with assert_snapshot when we have nicer display implementations
                assert_debug_snapshot!(resolutions);
            }
            Err(_) => {
                panic!("Failed to parse file {path}")
            }
        }
    }

    /// Struct which collects the result of function/method resolutions so they can be
    /// examined in tests.
    struct ResolveCollector<'s> {
        function_resoltions: Vec<FunctionName<'s>>,
    }

    use HIR::*;

    use super::{symbol_get, BUILTIN_SYMBOLS};

    impl<'s> ResolveCollector<'s> {
        fn collect_resolutions(program: &Program<'s>) -> Vec<FunctionName<'s>> {
            let mut collector = Self {
                function_resoltions: Vec::new(),
            };
            collector.walk_program(&program);
            collector.function_resoltions
        }
    }

    impl<'s> VisitAst<'s> for ResolveCollector<'s> {
        fn visit_function_call(&mut self, function_name: &FunctionName<'s>) {
            self.function_resoltions.push(function_name.clone());
        }
    }

    /// Parse a function signature
    fn str_to_name(str: &'static str) -> FunctionName<'static> {
        visit_function_signature(
            RemixParser::parse(Rule::function_call, str)
                .unwrap()
                .next()
                .unwrap(),
            None,
        )
    }
}
