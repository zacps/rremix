/// This module is responsible for name resolution for functions.
use std::{cell::RefCell, ops::DerefMut, process::exit, sync::Mutex};

use once_cell::sync::Lazy;
use pest::Parser;

use crate::{
    errors::{
        experimental_function_resolution_ambiguous, experimental_function_resolution_fail,
        experimental_function_resolution_var_missing,
    },
    parser::RemixParser,
    parser::Rule,
    SpanExt,
    HIR::*,
};

/// Names and corresponding IDs of builtin functions.
/// These are added to the symbol table during resolution
pub static BUILTIN_SYMBOLS: Lazy<Mutex<Vec<(FunctionSignature<'static>, FunctionID)>>> =
    Lazy::new(|| {
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
        let mut builtin_var_ids = (0..=9999).map(|i| VariableID::new(i));

        for (i, builtin) in builtins.iter().enumerate() {
            map.push((
                visit_function_signature(
                    RemixParser::parse(Rule::function_signature, builtin)
                        .unwrap()
                        .next()
                        .unwrap(),
                    &mut builtin_var_ids,
                    &builtin,
                ),
                i.into(),
            ));
        }

        Mutex::new(map.into())
    });

pub type SymbolTable<'s> = Vec<(FunctionSignature<'s>, FunctionID)>;
pub type InvSymbolTable<'s> = Vec<(FunctionID, FunctionSignature<'s>)>;

fn invert<'s>(mut table: SymbolTable<'s>) -> InvSymbolTable<'s> {
    table.drain(0..).map(|(sig, id)| (id, sig)).collect()
}

fn id_get<'s>(table: &SymbolTable<'s>, function_name: &FunctionCall<'s>) -> Option<FunctionID> {
    let mut matches = Vec::new();
    for (def, id) in table {
        if function_name.resolves_to(def) {
            matches.push((*id, def))
        }
    }
    if matches.len() == 1 {
        Some(matches[0].0)
    } else {
        None
    }
}

fn id_get_experimental<'s>(
    program: &Program<'s>,
    table: &SymbolTable<'s>,
    function_name: &FunctionCall<'s>,
    scope: Scope,
) -> Option<FunctionID> {
    let mut matches = Vec::new();
    for (def, id) in table {
        if function_name.resolves_to_experimental(def) {
            matches.push((*id, def))
        }
    }
    match matches.len() {
        1 => {
            // Resolve any parts which were converted to variables
            for part in &function_name.name {
                match part.borrow_mut().deref_mut() {
                    ref mut name @ FunctionCallPart::Defered(..) => {
                        let span = if let FunctionCallPart::Defered(span) = name {
                            span
                        } else {
                            unreachable!()
                        };
                        match program.find_var_scope(span.as_str(), scope) {
                            Some(id) => *name = &mut FunctionCallPart::var(id),
                            None => experimental_function_resolution_var_missing(
                                span.clone().with_file(program.text),
                                matches[0].1.span.clone(),
                            ),
                        }
                    }
                    _ => (),
                }
            }
            Some(matches[0].0)
        }
        0 => {
            experimental_function_resolution_fail(function_name.span.clone());
            return None;
        }
        _ => {
            // no specialisation
            if true {
                experimental_function_resolution_ambiguous(
                    function_name.span.clone(),
                    matches.iter().map(|(_, sig)| sig.span.clone()),
                );
                return None;
            // specialisation
            } else {
                // try a definition
                for (id, attempt) in &matches {
                    // check that all other matches are a specialisation of that definition
                    if matches
                        .iter()
                        .all(|(_, def)| attempt.is_specialisation(def))
                    {
                        return Some(*id);
                    }
                }
                // no matched definition is a specialisation of all other matches
                experimental_function_resolution_ambiguous(
                    function_name.span.clone(),
                    matches.iter().map(|(_, sig)| sig.span.clone()),
                );
                return None;
            }
        }
    }
}

/// The `Resolver` is responsible for resolving function names.
///
/// This is done as a separate pass to the initial AST construction so that
/// there is no need to forward declare functions.
pub struct Resolver<'s> {
    symbol_table: SymbolTable<'s>,
    scope: Scope,
    error: bool,
}

impl<'s> Resolver<'s> {
    /// Resolves all names in the [`Program`].
    ///
    /// This additionally returns the symbol table of the program.
    pub fn resolve(program: &mut Program<'s>) -> SymbolTable<'s> {
        let mut r = Self {
            symbol_table: BUILTIN_SYMBOLS.lock().unwrap().clone(),
            scope: Scope::Global,
            error: false,
        };
        let symbol_table = program.symbol_table();
        r.symbol_table.extend(symbol_table);
        r.resolve_names(program);
        if r.error {
            exit(-1);
        }
        r.symbol_table
    }

    fn resolve_names(&mut self, program: &mut Program<'s>) {
        for statement in &program.main {
            self.visit_statement(program, statement);
        }
        for function in &program.functions {
            self.scope = Scope::Function(function.id);
            for statement in &function.statements {
                self.visit_statement(program, statement);
            }
            self.scope = Scope::Global;
        }
    }

    fn visit_statement(&mut self, program: &Program<'s>, statement: &Statement<'s>) {
        use Statement::*;
        match statement {
            Expression(expr) => self.visit_expr(program, expr),
            Assignment { variable, value } => {
                self.visit_var(variable);
                self.visit_expr(program, value);
            }
            ListAssignment {
                variable,
                index,
                value,
            } => {
                self.visit_var(variable);
                self.visit_expr(program, index);
                self.visit_expr(program, value);
            }
            Return(_) | Redo => (),
        }
    }

    pub(crate) fn visit_expr(&mut self, program: &Program<'s>, expr: &Expression<'s>) -> () {
        use Expression::*;
        match expr {
            Binary {
                lhs,
                operator: _,
                rhs,
            } => {
                self.visit_expr(program, lhs);
                self.visit_expr(program, rhs);
            }
            Unary(unary) => self.visit_unary(program, unary),
        }
    }

    pub(crate) fn visit_var(&self, _variable: &VariableID) -> () {}

    pub(crate) fn visit_unary(&mut self, program: &Program<'s>, unary: &UnaryExpression<'s>) {
        use UnaryExpression::*;
        match unary {
            FunctionCall { function } => {
                //let id = id_get(&self.symbol_table, function);
                let id = id_get_experimental(program, &self.symbol_table, function, self.scope);
                if let Some(_id) = id {
                    // map function call to function table
                    function.id.replace(Some(_id));
                } else {
                    self.error = true;
                }
            }
            ListElement { variable, index } => {
                self.visit_var(variable);
                self.visit_expr(program, index);
            }
            Literal(_) => {}
            Variable(_) => {}
            Block(statements) => {
                for statement in statements {
                    self.visit_statement(program, statement)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use crate::resolver::{invert, InvSymbolTable, Resolver};
    use crate::HIR;
    use crate::HIR::VisitAst;

    use insta::assert_snapshot;
    use pest::Parser;

    use std::fs;
    use test_case::test_case;

    fn symbol_get<'s>(table: &InvSymbolTable<'s>, id: FunctionID) -> Option<FunctionSignature<'s>> {
        let mut matches = Vec::new();
        for (id1, sig) in table {
            if Into::<u64>::into(id) == Into::<u64>::into(id1.clone()) {
                matches.push((id, sig))
            }
        }
        if matches.len() == 1 {
            Some(matches[0].1.clone())
        } else {
            None
        }
    }

    #[test]
    fn test_build_stdlib_ast() {
        let file = "\n";
        match RemixParser::parse(Rule::program, &file) {
            Ok(mut parse) => {
                let pair = parse.next();
                let mut ast = HIR::Program::new(&file, pair.unwrap(), false);

                let symbol_table = Resolver::resolve(&mut ast);
                let inv_symbol_table = invert(symbol_table);

                let resolutions =
                    ResolveCollector::collect_resolutions(&ast, FunctionLocation::Stdlib);

                assert_snapshot!(&resolutions
                    .iter()
                    .map(|r| format!(
                        "{r} -> {}\n",
                        symbol_get(&inv_symbol_table, r.id.borrow().unwrap()).unwrap()
                    ))
                    .collect::<String>());
            }
            Err(_) => {
                panic!("Failed to parse stdlib")
            }
        }
    }

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
                let mut ast = HIR::Program::new(&file, pair.unwrap(), false);

                let symbol_table = Resolver::resolve(&mut ast);
                let inv_symbol_table = invert(symbol_table);

                let resolutions =
                    ResolveCollector::collect_resolutions(&ast, FunctionLocation::User);

                assert_snapshot!(&resolutions
                    .iter()
                    .map(|r| format!(
                        "{r} -> {}\n",
                        symbol_get(&inv_symbol_table, r.id.borrow().unwrap()).unwrap()
                    ))
                    .collect::<String>());
            }
            Err(_) => {
                panic!("Failed to parse file {path}")
            }
        }
    }

    /// Struct which collects the result of function/method resolutions so they can be
    /// examined in tests.
    struct ResolveCollector<'s> {
        function_resoltions: Vec<FunctionCall<'s>>,
    }

    use HIR::*;

    impl<'s> ResolveCollector<'s> {
        fn collect_resolutions(
            program: &Program<'s>,
            filter: FunctionLocation,
        ) -> Vec<FunctionCall<'s>> {
            let mut collector = Self {
                function_resoltions: Vec::new(),
            };
            collector.walk_program(&program, filter);
            collector.function_resoltions
        }
    }

    impl<'s> VisitAst<'s> for ResolveCollector<'s> {
        fn visit_function_call(&mut self, function_name: &FunctionCall<'s>) {
            self.function_resoltions.push(function_name.clone());
        }
    }
}
