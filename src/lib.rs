#![forbid(unsafe_code)]

use pest::{iterators::Pair, RuleType};

pub mod parser;

pub fn format_pair<T: RuleType + std::fmt::Debug>(
    pair: Pair<T>,
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
                pair,
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

/// High level intermediate representation.
///
/// This represents the compiler's state after name resolution, before optimisation and codegen.
/// We will have rejected the majority of incomplete programs after this is constructed.
pub mod HIR {
    use std::{collections::HashSet, env::VarError, hash::Hash, ops::Mul};

    use super::*;
    use pest::{iterators::Pairs, Span};

    /// A single 'word' in a function name, either a part of the name itself,
    /// a 'name', or a parameter.
    pub enum FunctionPart<'s> {
        Name { name: Span<'s> },
        Parameter { name: Span<'s>, reference: bool },
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

    impl<'s> Hash for FunctionPart<'s> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            core::mem::discriminant(self).hash(state);
            match self {
                Self::Name { name } => name.hash(state),
                Self::Parameter { name, .. } => name.hash(state),
            }
        }
    }

    /// TODO: This will probably need a more compliated implementation at some point so disallow some overlapping names.
    #[derive(PartialEq, Hash)]
    pub struct FunctionName<'s> {
        pub name: Vec<FunctionPart<'s>>,
    }

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

    /// An 'optional' type which is either a reference to a resolved function/variable/object
    /// or a marker 'unknown'. At the end of building the HIR if there are any 'unknown' markers
    /// compilation fails.
    pub enum Resolved<T> {
        Resolved(T),
        Unknown,
    }

    pub struct Variable<'s> {
        pub name: Span<'s>,
    }

    pub enum Literal<'s> {
        String {
            value: &'s str,
        },
        Integer {
            value: i64, // TODO: What numeric type should be used?
        },
        Float {
            value: f64, // TODO:
        },
        Boolean {
            value: bool,
        },
    }

    pub enum UnaryExpression<'s> {
        ListElement {
            variable: Resolved<Variable<'s>>,
            index: Expression<'s>,
        },
        FunctionCall {
            function: Resolved<Function<'s>>,
        },
        Literal(Literal<'s>),
        Variable(Resolved<Variable<'s>>),
    }

    pub enum Expression<'s> {
        Binary {
            lhs: Box<Expression<'s>>,
            operator: Operator,
            rhs: Box<Expression<'s>>,
        },
        Unary(Box<UnaryExpression<'s>>),
    }

    pub enum Statement<'s> {
        Assignment {
            variable: Resolved<Variable<'s>>,
            value: Expression<'s>,
        },
        Return,
        Redo,
        Expression(Expression<'s>),
        ListAssignment {
            variable: Resolved<Variable<'s>>,
            index: Expression<'s>,
            value: Expression<'s>,
        },
    }

    /// A function definition
    pub struct Function<'s> {
        pub name: FunctionName<'s>,
        pub statements: Vec<Statement<'s>>,
        pub span: Span<'s>,
    }

    impl<'s> PartialEq for Function<'s> {
        fn eq(&self, other: &Self) -> bool {
            self.name == other.name
        }
    }
    impl<'s> Eq for Function<'s> {}

    impl<'s> Hash for Function<'s> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.name.hash(state);
        }
    }

    pub struct Program<'s> {
        functions: HashSet<Function<'s>>,
        main: Vec<Statement<'s>>,
    }

    impl<'s> Program<'s> {
        pub fn new() -> Self {
            Self {
                functions: HashSet::new(),
                main: Vec::new(),
            }
        }

        pub fn build(&mut self, pair: Pair<'s, parser::Rule>) {
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
                        self.functions.insert(func);
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

            let mut name = FunctionName { name: Vec::new() };
            let mut statements = Vec::new();
            let span = pair.as_span().clone();

            for pair in pair.into_inner() {
                match pair.as_rule() {
                    function_signature => {
                        let pair = pair
                            .into_inner()
                            .next()
                            .expect("function signature must contain name_paren");
                        for pair in pair.into_inner() {
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
                name,
                statements,
                span,
            }
        }

        fn visit_statement(&mut self, pair: Pair<'s, parser::Rule>) -> Statement<'s> {
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

        fn visit_expression(&mut self, pair: Pair<'s, parser::Rule>) -> Expression<'s> {
            use parser::Rule::*;
            let pair = pair.into_inner().next().unwrap();
            match pair.as_rule() {
                binary_expression => {
                    // let bin = Expression::Binary {};
                    let (mut lhs, mut op, mut rhs) = (None, None, None);
                    for pair in pair.into_inner() {
                        match pair.as_rule() {
                            unary_expression => {}
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
                        lhs: lhs.unwrap(),
                        operator: op.unwrap(),
                        rhs: Box::new(rhs.unwrap()),
                    }
                }
                unary_expression => unimplemented!(),
                rule => panic!("ICE: Unexpected rule {rule:?}"),
            }
        }

        fn visit_unary_expression(&mut self, pair: Pair<'s, parser::Rule>) -> Expression<'s> {
            use parser::Rule::*;
            let pair = pair.into_inner().next().unwrap();

            unimplemented!()
        }
    }
}
