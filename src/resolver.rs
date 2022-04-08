/// This module is responsible for name resolution, both for variables and functions.
///
use std::{collections::HashMap, sync::Mutex};

use once_cell::sync::Lazy;
use pest::Parser;

use crate::{parser::RemixParser, parser::Rule, HIR::*};

/// Names and corresponding IDs of builtin functions.
/// These are added to the symbol table during resolution
static BUILTIN_SYMBOLS: Lazy<Mutex<HashMap<FunctionName<'static>, FunctionID>>> = Lazy::new(|| {
    let mut map = HashMap::new();

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
        map.insert(
            visit_function_signature(
                RemixParser::parse(Rule::function_signature, builtin)
                    .unwrap()
                    .next()
                    .unwrap(),
                None,
            ),
            i.into(),
        );
    }

    Mutex::new(map.into())
});

pub struct Resolver<'s> {
    symbol_table: HashMap<FunctionName<'s>, FunctionID>,
}

impl<'s> Resolver<'s> {
    pub fn resolve(program: &mut Program<'s>) {
        let mut r = Self {
            symbol_table: BUILTIN_SYMBOLS.lock().unwrap().clone(),
        };
        r.symbol_table.extend(program.symbol_table());
        r.resolve_names(program);
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
                if let Some(_id) = self.symbol_table.get(function) {
                    // map function call to function table
                    function.id = Some(*_id)
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
