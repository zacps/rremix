use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

use super::*;
use derivative::Derivative;
use pest::Span;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub struct FunctionID(u64);

impl From<usize> for FunctionID {
    fn from(n: usize) -> Self {
        Self(n as u64)
    }
}

/// A single 'word' in a function name, either a part of the name itself,
/// a 'name', or a parameter.
#[derive(Debug, Clone)]
pub enum FunctionPart<'s> {
    Name { name: Span<'s> },
    Parameter { name: Span<'s>, reference: bool },
    // This should be literal/block in future; both are 'unnamed' literals
    Statements(Vec<Statement<'s>>),
}

impl<'s> FunctionPart<'s> {
    pub fn name(name: &'s str) -> Self {
        Self::Name {
            name: Span::new(name, 0, name.len()).unwrap(),
        }
    }
    pub fn param(param: &'s str) -> Self {
        Self::Parameter {
            name: Span::new(param, 0, param.len()).unwrap(),
            reference: param.chars().next().unwrap() == '#',
        }
    }
}

impl<'s> PartialEq for FunctionPart<'s> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Name { name: l_name }, Self::Name { name: r_name }) => l_name == r_name,
            (
                Self::Parameter {
                    name: l_name,
                    reference: _,
                },
                Self::Parameter {
                    name: r_name,
                    reference: _,
                },
            ) => l_name == r_name,
            _ => false,
        }
    }
}
impl<'s> Eq for FunctionPart<'s> {}

/// TODO: This will probably need a more compliated implementation at some point so disallow some overlapping names.
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct FunctionName<'s> {
    pub name: Vec<FunctionPart<'s>>,
    pub id: Option<FunctionID>,
}

impl<'s> Display for FunctionName<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for name in &self.name {
            match name {
                FunctionPart::Name { name } => f.write_str(name.as_str())?,
                FunctionPart::Parameter { name, reference } if !reference => {
                    f.write_str(name.as_str())?
                }
                FunctionPart::Parameter { name, reference } if *reference => {
                    f.write_str(&format!("#{}", name.as_str()))?
                }
                FunctionPart::Statements { .. } => f.write_str("[statements]")?,
                _ => unreachable!(),
            }
            f.write_str(" ")?
        }
        Ok(())
    }
}

impl<'s> FunctionName<'s> {
    pub fn new(parts: Vec<FunctionPart<'s>>) -> Self {
        Self {
            name: parts,
            id: None,
        }
    }
    pub fn len(&self) -> usize {
        self.name.len()
    }

    /// Whether or not this function name should resolve to the name `def`.
    pub fn resolves_to<'t>(&self, def: &FunctionName<'t>) -> bool {
        if self.len() != def.len() {
            return false;
        }
        // Iterate until we find a reason to reject or traverse the full name...
        for (name1, name2) in self.name.iter().zip(&def.name) {
            match (name1, name2) {
                // Corresponding name parts must match exactly
                (FunctionPart::Name { name: name1 }, FunctionPart::Name { name: name2 }) => {
                    if name1.as_str() != name2.as_str() {
                        return false;
                    }
                }

                // TODO: For now we're rejecting this case but this will have to change when we
                // implement our name resolution expansion.
                (FunctionPart::Name { .. }, FunctionPart::Parameter { .. }) => return false,
                (FunctionPart::Name { .. }, FunctionPart::Statements(..)) => return false,

                // Name in the definition and parameter in the reference always rejects
                (FunctionPart::Parameter { .. }, FunctionPart::Name { .. }) => return false,
                (FunctionPart::Statements(..), FunctionPart::Name { .. }) => return false,

                // Corresponding parameters accept
                (FunctionPart::Parameter { .. }, FunctionPart::Parameter { .. }) => (),
                (FunctionPart::Statements(..), FunctionPart::Statements(..)) => (),
                (FunctionPart::Parameter { .. }, FunctionPart::Statements(..)) => (),
                (FunctionPart::Statements(..), FunctionPart::Parameter { .. }) => (),
            }
        }

        true
    }
}

#[derive(Debug, Clone)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Exponentiation,
    Modulus,
    Equality,
    InEquality,
    GreaterThan,
    LessThan,
    GreaterThanEq,
    LessThanEq,
}

// /// An 'optional' type which is either a reference to a resolved function/variable/object
// /// or a marker 'unknown'. At the end of building the HIR if there are any 'unknown' markers
// /// compilation fails.
// #[derive(Debug)]
// pub enum Resolved<T> {
//     Resolved(T),
//     Unknown(T),
// }

#[derive(Debug, Clone)]
pub struct Variable<'s>(Span<'s>);

#[derive(Debug, Clone)]
pub enum Literal<'s> {
    String(&'s str),
    Integer(
        i64, // TODO: What numeric type should be used?
    ),
    Float(
        f64, // TODO:
    ),
    Boolean(bool),
    List(()),
    Map(()),
}

#[derive(Debug, Clone)]
pub enum UnaryExpression<'s> {
    ListElement {
        variable: Variable<'s>,
        index: Expression<'s>,
    },
    FunctionCall {
        function: FunctionName<'s>,
    },
    Literal(Literal<'s>),
    Variable(Variable<'s>),
}

#[derive(Debug, Clone)]
pub enum Expression<'s> {
    Binary {
        lhs: Box<Expression<'s>>,
        operator: Operator,
        rhs: Box<Expression<'s>>,
    },
    Unary(Box<UnaryExpression<'s>>),
}

#[derive(Debug, Clone)]
pub enum Statement<'s> {
    Assignment {
        variable: Variable<'s>,
        value: Expression<'s>,
    },
    Return,
    Redo,
    Expression(Expression<'s>),
    ListAssignment {
        variable: Variable<'s>,
        index: Expression<'s>,
        value: Expression<'s>,
    },
}

/// A function definition
#[derive(Debug, Clone)]
pub struct Function<'s> {
    pub name: FunctionName<'s>,
    pub statements: Vec<Statement<'s>>,
    pub span: Span<'s>,
    pub id: FunctionID,
}

impl<'s> Hash for Function<'s> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl<'s> PartialEq for Function<'s> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'s> Borrow<FunctionName<'s>> for Function<'s> {
    fn borrow(&self) -> &FunctionName<'s> {
        &self.name
    }
}

impl<'s> Eq for Function<'s> {}

/// A scope containing variables and possibly inner scopes
#[derive(Debug)]
struct VariableScope<'s> {
    variables: HashSet<&'s str>,
    inner: Vec<VariableScope<'s>>,
}

impl<'s> VariableScope<'s> {
    // Check if the variable is in scope
    pub fn search(&self, var: &'s str) -> bool {
        if self.variables.contains(var) {
            return true;
        } else {
            for scope in &self.inner {
                if scope.search(var) {
                    return true;
                }
            }
        }
        false
    }
}

// TODO: Rename, this more accurately represents the state during parse
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Program<'s> {
    pub functions: HashSet<Function<'s>>,
    pub main: Vec<Statement<'s>>,
    #[derivative(Debug = "ignore")]
    pub function_id: Box<dyn Iterator<Item = FunctionID>>,
}

impl<'s> Program<'s> {
    pub fn new(pair: Pair<'s, parser::Rule>) -> Self {
        let mut this = Self {
            functions: HashSet::new(),
            main: Vec::new(),
            // Non-builtin function IDs start at 1000
            function_id: Box::new((1000..).map(|i| FunctionID(i))),
        };
        this.build(pair);
        this
    }

    pub fn symbol_table(&self) -> Vec<(FunctionName<'s>, FunctionID)> {
        let mut map = Vec::new();
        for func in &self.functions {
            map.push((func.name.clone(), func.id));
        }
        map
    }

    fn build(&mut self, pair: Pair<'s, parser::Rule>) {
        use parser::Rule::*;
        match pair.as_rule() {
            program => self.visit_program(pair),
            _ => (),
        }
    }

    fn visit_program(&mut self, pair: Pair<'s, parser::Rule>) {
        use parser::Rule::*;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                function_definition => {
                    let func = self.visit_function(pair);
                    assert!(self.functions.insert(func));
                }
                statement => {
                    let st = self.visit_statement(pair);
                    self.main.push(st)
                }
                _ => (),
            }
        }
    }

    fn visit_function(&mut self, pair: Pair<'s, parser::Rule>) -> Function<'s> {
        use parser::Rule::*;

        let mut name = None;
        let id = self.function_id.next().unwrap();
        let mut statements = Vec::new();
        let span = pair.as_span().clone();

        for pair in pair.into_inner() {
            match pair.as_rule() {
                function_signature => {
                    name = Some(visit_function_signature(
                        pair.into_inner().next().unwrap(),
                        Some(id),
                    ))
                }
                function_statements => {
                    for pair in pair.into_inner() {
                        match pair.as_rule() {
                            statement => statements.push(self.visit_statement(pair)),
                            _ => (),
                        }
                    }
                }
                _ => (),
            }
        }
        Function {
            name: name.unwrap(),
            statements,
            span,
            id: id,
        }
    }

    fn visit_statement(&self, pair: Pair<'s, parser::Rule>) -> Statement<'s> {
        use parser::Rule::*;
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            assignment_statement => unimplemented!(),
            return_statement => Statement::Return,
            redo_statement => Statement::Redo,
            list_element_assignment => unimplemented!(),
            expression => Statement::Expression(self.visit_expression(pair)),
            rule => panic!("ICE: Unexpected rule {rule:?}"),
        }
    }

    fn visit_expression(&self, pair: Pair<'s, parser::Rule>) -> Expression<'s> {
        use parser::Rule::*;
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            binary_expression => {
                // let bin = Expression::Binary {};
                let (mut lhs, mut op, mut rhs) = (None, None, None);
                for pair in pair.into_inner() {
                    match pair.as_rule() {
                        unary_expression => lhs = Some(self.visit_unary_expression(pair)),
                        operator => {
                            use Operator::*;
                            op = Some(match pair.as_str() {
                                "+" => Addition,
                                "-" => Subtraction,
                                "×" => Multiplication,
                                "÷" => Division,
                                "%" => Modulus,
                                "=" => Equality,
                                "≠" => InEquality,
                                "<" => LessThan,
                                "≤" => LessThanEq,
                                ">" => GreaterThan,
                                "≥" => GreaterThanEq,
                                op => panic!("ICE: Unexpected operator {op:?}"),
                            })
                        }
                        expression => rhs = Some(self.visit_expression(pair)),
                        rule => panic!("ICE: Unexpected rule {rule:?}"),
                    }
                }
                Expression::Binary {
                    lhs: Box::new(Expression::Unary(Box::new(lhs.unwrap().unwrap()))),
                    operator: op.unwrap(),
                    rhs: Box::new(rhs.unwrap()),
                }
            }
            unary_expression => {
                Expression::Unary(Box::new(self.visit_unary_expression(pair).unwrap()))
            }
            rule => panic!("ICE: Unexpected rule {rule:?}"),
        }
    }

    fn visit_unary_expression(
        &self,
        pair: Pair<'s, parser::Rule>,
    ) -> Result<UnaryExpression<'s>, Box<dyn std::error::Error>> {
        use parser::Rule::*;

        for pair in pair.into_inner() {
            match pair.as_rule() {
                simple_expression => {
                    let pair = pair.into_inner().next().unwrap();
                    match pair.as_rule() {
                        list_element => {
                            let (mut var, mut expr) = (None, None);
                            for pair in pair.into_inner() {
                                match pair.as_rule() {
                                    single_name_part => var = Some(Variable(pair.as_span())),
                                    expression => expr = Some(self.visit_expression(pair)),
                                    rule => {
                                        panic!("ICE: Unexpected rule {rule:?} in list_element")
                                    }
                                }
                            }
                            return Ok(UnaryExpression::ListElement {
                                variable: var.unwrap(),
                                index: expr.unwrap(),
                            });
                        }
                        create_call => unimplemented!(),
                        function_call => {
                            return Ok(UnaryExpression::FunctionCall {
                                function: self.visit_function_call(pair)?,
                            })
                        }
                        literal => return Ok(UnaryExpression::Literal(self.visit_literal(pair)?)),
                        single_name_part => {
                            return Ok(UnaryExpression::Variable(Variable(pair.as_span())))
                        }
                        _ => (),
                    }
                }
                _ => (),
            }
        }
        panic!("ICE: didn't encounter simple_expression in unary_expression")
    }

    // TODO: The ugliest part yet, please clean this up
    fn visit_function_call(
        &self,
        pair: Pair<'s, parser::Rule>,
    ) -> Result<FunctionName<'s>, Box<dyn std::error::Error>> {
        use parser::Rule::*;
        let mut name = FunctionName {
            name: Vec::new(),
            id: None,
        };
        for pair in pair.into_inner() {
            match pair.as_rule() {
                single_name_part => name.name.push(FunctionPart::Name {
                    name: pair.as_span(),
                }),
                literal => name
                    .name
                    .push(FunctionPart::Statements(vec![Statement::Expression(
                        Expression::Unary(Box::new(UnaryExpression::Literal(
                            self.visit_literal(pair)?,
                        ))),
                    )])),
                expression => {
                    name.name
                        .push(FunctionPart::Statements(vec![Statement::Expression(
                            self.visit_expression(pair),
                        )]))
                }
                statement => name
                    .name
                    .push(FunctionPart::Statements(vec![self.visit_statement(pair)])),
                block => {
                    let mut statements = Vec::new();
                    for pair in pair.into_inner() {
                        match pair.as_rule() {
                            statement => statements.push(self.visit_statement(pair)),
                            _ => (),
                        }
                    }
                    name.name.push(FunctionPart::Statements(statements))
                }
                _ => (),
            }
        }
        Ok(name)
    }

    fn visit_literal(
        &self,
        pair: Pair<'s, parser::Rule>,
    ) -> Result<Literal<'s>, Box<dyn std::error::Error>> {
        use parser::Rule::*;
        let pair = pair.into_inner().next().unwrap();
        Ok(match pair.as_rule() {
            number => {
                let int = pair.as_str().parse::<i64>().map(|i| Literal::Integer(i));
                let float = pair.as_str().parse::<f64>().map(|f| Literal::Float(f));
                int.or(float)?
            }
            boolean => match pair.as_str() {
                "true" => Literal::Boolean(true),
                "false" => Literal::Boolean(false),
                b => panic!("ICE: unknown boolean value {b}"),
            },
            list => unimplemented!(),
            string => Literal::String(&pair.as_str()[1..pair.as_str().len() - 1]),
            _ => panic!("ICE: Didn't get a known literal in literal"),
        })
    }
}

pub fn visit_function_signature<'s>(
    pair: Pair<'s, parser::Rule>,
    id: Option<FunctionID>,
) -> FunctionName<'s> {
    use parser::Rule::*;
    let mut name = FunctionName {
        name: Vec::new(),
        id: id,
    };
    assert!(pair.as_rule() == parser::Rule::function_signature);

    for pair in pair.into_inner() {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            name_part => name.name.push(FunctionPart::Name {
                name: pair.as_span(),
            }),
            paren => name.name.push(FunctionPart::Parameter {
                name: pair.as_span(),
                reference: false,
            }),
            ref_paren => name.name.push(FunctionPart::Parameter {
                name: pair.as_span(),
                reference: true,
            }),
            _ => (),
        }
    }
    name
}

pub trait VisitAst<'s> {
    fn walk_program(&mut self, program: &Program<'s>) {
        for function in &program.functions {
            self.walk_function(function)
        }
        for statement in &program.main {
            self.walk_statement(statement)
        }
    }

    fn walk_function(&mut self, function: &Function<'s>) {
        self.visit_function(function);

        for statement in &function.statements {
            self.walk_statement(statement)
        }
    }
    fn walk_statement(&mut self, statement: &Statement<'s>) {
        use Statement::*;
        self.visit_statement(statement);
        match statement {
            Expression(expr) => self.walk_expression(expr),
            _ => (),
        }
    }
    fn walk_expression(&mut self, expression: &Expression<'s>) {
        use Expression::*;
        self.visit_expression(expression);
        match expression {
            Unary(expr) => self.walk_unary_expression(expr),
            Binary {
                lhs,
                operator: _,
                rhs,
            } => {
                self.walk_expression(lhs);
                self.walk_expression(rhs)
            }
        }
    }
    fn walk_unary_expression(&mut self, unary: &UnaryExpression<'s>) {
        use UnaryExpression::*;
        match unary {
            FunctionCall { function } => self.walk_function_call(function),
            ListElement {
                variable: _var,
                index,
            } => self.walk_expression(index),
            _ => (),
        }
    }
    fn walk_function_call(&mut self, function_name: &FunctionName<'s>) {
        self.visit_function_call(function_name);
    }

    fn visit_function(&mut self, _function: &Function<'s>) {}
    fn visit_statement(&mut self, _statement: &Statement<'s>) {}
    fn visit_expression(&mut self, _expression: &Expression<'s>) {}
    fn visit_function_call(&mut self, _function_name: &FunctionName<'s>) {}
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use crate::HIR;

    use insta::assert_debug_snapshot;
    use pest::Parser;
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
            fs::read_to_string("tests/".to_owned() + path).expect("could not find test file");
        match RemixParser::parse(Rule::program, &file) {
            Ok(mut parse) => {
                let pair = parse.next();
                let ast = HIR::Program::new(pair.unwrap());

                // TODO: Replace with assert_snapshot when we have nicer display implementations
                assert_debug_snapshot!(ast);
            }
            Err(e) => {
                eprintln!("{e}");
                panic!("Failed to parse file {path}")
            }
        }
    }
}
