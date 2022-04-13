use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

use super::*;
use derivative::Derivative;
use pest::Span;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub struct FunctionID(u64);

impl FunctionID {
    pub(crate) fn new(id: u64) -> Self {
        FunctionID(id)
    }
}

impl From<usize> for FunctionID {
    fn from(n: usize) -> Self {
        Self(n as u64)
    }
}

/// A single 'word' in a function name, either a part of the name itself,
/// a 'name', or a parameter.
#[derive(Debug, Clone)]
pub enum FunctionCallPart<'s> {
    Name { name: Span<'s> },
    Parameter { name: Span<'s>, reference: bool },
    // This should be literal/block in future; both are 'unnamed' literals
    Expression(Expression<'s>),
    Block(Vec<Statement<'s>>),
}

impl<'s> Display for FunctionCallPart<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionCallPart::Name { name } => f.write_str(name.as_str()),
            FunctionCallPart::Parameter { name, reference } => {
                write!(
                    f,
                    "({}{})",
                    if *reference { "#" } else { "" },
                    name.as_str()
                )
            }
            FunctionCallPart::Expression(_) => f.write_str("(expression)"),
            FunctionCallPart::Block(_) => f.write_str("[statements]"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Necessity<T> {
    Required(T),
    Optional(T),
}

/// A single 'word' in a function name, either a part of the name itself,
/// a 'name', or a parameter.
#[derive(Debug, Clone)]
pub enum FunctionSignaturePart<'s> {
    // TODO: Need a way to indicate optional names
    Name { names: Necessity<Vec<Span<'s>>> },
    Parameter { name: Span<'s>, reference: bool },
}

impl<'s> Display for FunctionSignaturePart<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionSignaturePart::Name { names } => {
                let str = &match names {
                    Necessity::Required(names) => {
                        let n = names
                            .iter()
                            .map(|n| n.as_str().to_string() + "/")
                            .collect::<String>();
                        n
                    }
                    Necessity::Optional(names) => {
                        let n = names
                            .iter()
                            .map(|n| "/".to_string() + n.as_str() + "/")
                            .collect::<String>();
                        n
                    }
                };
                f.write_str(&str[0..str.len() - 1])
            }
            FunctionSignaturePart::Parameter { name, reference } => {
                write!(
                    f,
                    "({}{})",
                    if *reference { "#" } else { "" },
                    name.as_str()
                )
            }
        }
    }
}

/// TODO: This will probably need a more compliated implementation at some point so disallow some overlapping names.
#[derive(Debug, Clone)]
pub struct FunctionCallName<'s> {
    pub name: Vec<FunctionCallPart<'s>>,
    pub id: RefCell<Option<FunctionID>>,
}

impl<'s> Display for FunctionCallName<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FunctionCallPart::*;
        for name in &self.name {
            match name {
                Name { name } => f.write_str(name.as_str())?,
                Parameter { name, reference } if !reference => f.write_str(name.as_str())?,
                Parameter { name, reference } if *reference => {
                    f.write_str(&format!("#{}", name.as_str()))?
                }
                Expression(..) => f.write_str("(expression)")?,
                Block(..) => f.write_str("[statements]")?,
                _ => unreachable!(),
            }
            f.write_str(" ")?
        }
        Ok(())
    }
}

trait PairExt<R> {
    /// Unwrap the pair and descend the parse tree, panicking if there was no next token.
    fn descend(self) -> Self;
    /// Assert the pair has a particular rule, panicking otherwise
    fn expect(self, rule: R) -> Self;
}

impl<'s> PairExt<parser::Rule> for Pair<'s, parser::Rule> {
    fn descend(self) -> Self {
        let rule = self.as_rule();
        self.into_inner()
            .next()
            .expect(&format!("failed to descend from rule {rule:?}"))
    }

    fn expect(self, rule: parser::Rule) -> Self {
        assert!(
            self.as_rule() == rule,
            "ICE: Expected rule {rule:?} got {:?}",
            self.as_rule()
        );
        self
    }
}

/// TODO: This will probably need a more compliated implementation at some point so disallow some overlapping names.
#[derive(Debug, Clone)]
pub struct FunctionSignature<'s> {
    pub name: Vec<FunctionSignaturePart<'s>>,
    pub id: FunctionID,
}

impl<'s> FunctionSignature<'s> {
    fn len(&self) -> usize {
        self.name.len()
    }
}

impl<'s> Display for FunctionSignature<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FunctionSignaturePart::*;
        for name in &self.name {
            match name {
                Name { names } => {
                    let str = &match names {
                        Necessity::Required(names) => {
                            let n = names
                                .iter()
                                .map(|n| n.as_str().to_string() + "/")
                                .collect::<String>();
                            n
                        }
                        Necessity::Optional(names) => {
                            let n = names
                                .iter()
                                .map(|n| "/".to_string() + n.as_str() + "/")
                                .collect::<String>();
                            n
                        }
                    };
                    f.write_str(&str[0..str.len() - 1])?
                }
                Parameter { name, reference } if !reference => f.write_str(name.as_str())?,
                Parameter { name, reference } if *reference => {
                    f.write_str(&format!("#{}", name.as_str()))?
                }
                _ => unreachable!(),
            }
            f.write_str(" ")?
        }
        Ok(())
    }
}

impl<'s> FunctionCallName<'s> {
    pub fn new(parts: Vec<FunctionCallPart<'s>>) -> Self {
        Self {
            name: parts,
            id: RefCell::new(None),
        }
    }
    pub fn len(&self) -> usize {
        self.name.len()
    }

    /// Whether or not this function name should resolve to the name `def`.
    pub fn resolves_to<'t>(&self, def: &FunctionSignature<'t>) -> bool {
        // Iterate until we find a reason to reject or traverse the full name...
        let (mut i, mut j) = (0, 0);
        while j != def.name.len() && i != def.name.len() {
            // We skip advancing the reference if we fail to match against an optional parameter
            let mut matched = true;

            if let None = self.name.get(i) {
                return false;
            }

            // Debug
            if let (FunctionCallPart::Name { name }, FunctionSignaturePart::Name { names }) =
                (&self.name[0], &def.name[0])
            {
                match names {
                    Necessity::Required(names) => {
                        if name.as_str() == "for" && names[0].as_str() == "for" {
                            println!(
                                "trying to match '{}' against '{}' (required)",
                                &self.name[i], &def.name[j]
                            )
                        }
                    }
                    Necessity::Optional(names) => {
                        if name.as_str() == "for" && names[0].as_str() == "for" {
                            println!(
                                "trying to match '{}' against '{}' (optional)",
                                &self.name[i], &def.name[j]
                            )
                        }
                    }
                }
            }

            match (&self.name[i], &def.name[j]) {
                // Corresponding name parts must match exactly
                (FunctionCallPart::Name { name }, FunctionSignaturePart::Name { names }) => {
                    match names {
                        Necessity::Optional(names) => {
                            if !names.iter().map(|n| n.as_str()).any(|n| n == name.as_str()) {
                                matched = false;
                            }
                        }
                        Necessity::Required(names) => {
                            if !names.iter().map(|n| n.as_str()).any(|n| n == name.as_str()) {
                                return false;
                            }
                        }
                    }
                }

                // TODO: For now we're rejecting this case but this will have to change when we
                // implement our name resolution expansion.
                (FunctionCallPart::Name { .. }, FunctionSignaturePart::Parameter { .. }) => {
                    return false
                }

                // Name in the definition and parameter in the reference always rejects
                (
                    FunctionCallPart::Parameter { .. }
                    | FunctionCallPart::Expression(..)
                    | FunctionCallPart::Block(..),
                    FunctionSignaturePart::Name { .. },
                ) => return false,

                // Corresponding parameters accept
                (
                    FunctionCallPart::Parameter { .. }
                    | FunctionCallPart::Expression(..)
                    | FunctionCallPart::Block(..),
                    FunctionSignaturePart::Parameter { .. },
                ) => (),
            }
            if matched {
                i += 1;
            }
            j += 1;
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
    // FIXME: This should also accept blocks, which should probably be their own type...
    List(Vec<Expression<'s>>),
    Map(HashMap<Variable<'s>, Expression<'s>>),
}

#[derive(Debug, Clone)]
pub enum UnaryExpression<'s> {
    ListElement {
        variable: Variable<'s>,
        index: Expression<'s>,
    },
    FunctionCall {
        function: FunctionCallName<'s>,
    },
    Literal(Literal<'s>),
    Variable(Variable<'s>),
    Block(Vec<Statement<'s>>),
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
    pub name: FunctionSignature<'s>,
    pub statements: Vec<Statement<'s>>,
    pub span: Span<'s>,
    pub id: FunctionID,
}

#[derive(Eq, PartialEq, Debug)]
pub enum FunctionLocation {
    Builtin,
    Stdlib,
    User,
}

impl<'s> Function<'s> {
    fn location(&self) -> FunctionLocation {
        match self.id.0 {
            0..=999 => FunctionLocation::Builtin,
            1000..=1999 => FunctionLocation::Stdlib,
            2000.. => FunctionLocation::User,
        }
    }
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

impl<'s> Borrow<FunctionSignature<'s>> for Function<'s> {
    fn borrow(&self) -> &FunctionSignature<'s> {
        &self.name
    }
}

impl<'s> Eq for Function<'s> {}

// TODO: Rename, this more accurately represents the state during parse
pub struct Program<'s> {
    pub functions: HashSet<Function<'s>>,
    pub main: Vec<Statement<'s>>,
    pub function_id: Box<dyn Iterator<Item = FunctionID>>,
}

impl<'s> Debug for Program<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Program")
            .field(
                "functions",
                &self
                    .functions
                    .iter()
                    // Only print user code, skipping builtins and stdlib functions
                    .filter(|f| f.location() == FunctionLocation::User)
                    .collect::<HashSet<_>>(),
            )
            .field("main", &self.main)
            .finish()
    }
}

impl Program<'static> {
    /// Parse the standard library into HIR
    fn standard_library() -> HashSet<Function<'static>> {
        let tokens = RemixParser::parse(parser::Rule::program, &STANDARD_LIB)
            .expect("Failed to parse standard library")
            .next()
            .unwrap();
        let mut this = Self {
            // TODO: I'm not sure if this is the right place to inject this...
            // It's certainly not the traditional linking mechanism or flexible for imports
            // but it's probably fine for now?
            functions: HashSet::new(),
            main: Vec::new(),
            // Non-builtin function IDs start at 1000
            function_id: Box::new((1000..).map(|i| FunctionID(i))),
        };
        this.build(tokens);
        this.functions
    }
}

impl<'s> Program<'s> {
    pub fn new(pair: Pair<'s, parser::Rule>) -> Self {
        let mut this = Self {
            // TODO: I'm not sure if this is the right place to inject this...
            // It's certainly not the traditional linking mechanism or flexible for imports
            // but it's probably fine for now?
            functions: Program::standard_library(),
            main: Vec::new(),
            // User function IDs start at 2000
            function_id: Box::new((2000..).map(|i| FunctionID(i))),
        };
        this.build(pair);
        this
    }

    pub fn symbol_table(&self) -> Vec<(FunctionSignature<'s>, FunctionID)> {
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
                function_signature => name = Some(visit_function_signature(pair, id)),
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
            assignment_statement => {
                let (mut var, mut expr) = (None, None);
                for pair in pair.into_inner() {
                    match pair.as_rule() {
                        variable => var = Some(Variable(pair.as_span())),
                        expression => expr = Some(self.visit_expression(pair)),
                        colon => (),
                        _ => panic!("ICE: unexpected pair {pair} in assignment_statement"),
                    }
                }
                Statement::Assignment {
                    variable: var.unwrap(),
                    value: expr.unwrap(),
                }
            }
            return_statement => Statement::Return,
            redo_statement => Statement::Redo,
            list_element_assignment => todo!(),
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
                    lhs: Box::new(lhs.unwrap().unwrap()),
                    operator: op.unwrap(),
                    rhs: Box::new(rhs.unwrap()),
                }
            }
            unary_expression => self.visit_unary_expression(pair).unwrap(),
            rule => panic!("ICE: Unexpected rule {rule:?}"),
        }
    }

    fn visit_unary_expression(
        &self,
        pair: Pair<'s, parser::Rule>,
    ) -> Result<Expression<'s>, Box<dyn std::error::Error>> {
        use parser::Rule::*;
        assert!(pair.as_rule() == unary_expression);

        for pair in pair.clone().into_inner() {
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
                            return Ok(Expression::Unary(Box::new(UnaryExpression::ListElement {
                                variable: var.unwrap(),
                                index: expr.unwrap(),
                            })));
                        }
                        create_call => unimplemented!(),
                        function_call => {
                            return Ok(Expression::Unary(Box::new(UnaryExpression::FunctionCall {
                                function: self.visit_function_call(pair)?,
                            })))
                        }
                        literal => {
                            return Ok(Expression::Unary(Box::new(UnaryExpression::Literal(
                                self.visit_literal(pair)?,
                            ))))
                        }
                        variable => {
                            return Ok(Expression::Unary(Box::new(UnaryExpression::Variable(
                                Variable(pair.as_span()),
                            ))))
                        }
                        block => {
                            let mut statements = Vec::new();
                            for pair in pair.into_inner() {
                                match pair.as_rule() {
                                    statement => statements.push(self.visit_statement(pair)),
                                    _ => panic!("ICE: unexpected pair {pair} in block"),
                                }
                            }
                            return Ok(Expression::Unary(Box::new(UnaryExpression::Block(
                                statements,
                            ))));
                        }
                        _ => println!("found unexpected pair in simple_expression {pair}"),
                    }
                }
                expression => return Ok(self.visit_expression(pair)),
                _ => panic!("ICE: found unexpected pair {pair}"),
            }
        }
        panic!(
            "ICE: didn't encounter simple_expression or expression in unary_expression\n{}",
            pair
        )
    }

    // TODO: The ugliest part yet, please clean this up
    fn visit_function_call(
        &self,
        pair: Pair<'s, parser::Rule>,
    ) -> Result<FunctionCallName<'s>, Box<dyn std::error::Error>> {
        use parser::Rule::*;
        let mut name = FunctionCallName {
            name: Vec::new(),
            id: RefCell::new(None),
        };
        for pair in pair.into_inner() {
            match pair.as_rule() {
                single_name_part => name.name.push(FunctionCallPart::Name {
                    name: pair.as_span(),
                }),
                literal => name
                    .name
                    .push(FunctionCallPart::Expression(Expression::Unary(Box::new(
                        UnaryExpression::Literal(self.visit_literal(pair)?),
                    )))),
                expression => name
                    .name
                    .push(FunctionCallPart::Expression(self.visit_expression(pair))),
                block | newindent_block => {
                    let mut statements = Vec::new();
                    for pair in pair.into_inner() {
                        match pair.as_rule() {
                            statement => statements.push(self.visit_statement(pair)),
                            _ => (),
                        }
                    }
                    name.name.push(FunctionCallPart::Block(statements))
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
            list => {
                let mut items = Vec::new();
                for pair in pair.into_inner() {
                    match pair.as_rule() {
                        list_item => {
                            for pair in pair.into_inner() {
                                match pair.as_rule() {
                                    block => unimplemented!(),
                                    // FIXME: Also support maps
                                    key_value => unimplemented!(),
                                    expression => items.push(self.visit_expression(pair)),
                                    _ => panic!("ICE: unexpected pair {} in list_item", pair),
                                }
                            }
                        }
                        _ => (),
                    }
                }
                Literal::List(items)
            }
            string => Literal::String(&pair.as_str()[1..pair.as_str().len() - 1]),
            _ => panic!("ICE: Didn't get a known literal in literal"),
        })
    }
}

pub fn visit_function_signature<'s>(
    pair: Pair<'s, parser::Rule>,
    id: FunctionID,
) -> FunctionSignature<'s> {
    use parser::Rule::*;
    let mut name = FunctionSignature {
        name: Vec::new(),
        id: id,
    };
    let pair = pair.expect(function_signature);

    for pair in pair.into_inner() {
        let pair = pair.descend();
        match pair.as_rule() {
            name_part => {
                let pair = pair.descend();
                match pair.as_rule() {
                    name_list => {
                        let mut parts = Vec::new();
                        for pair in pair.into_inner() {
                            match pair.as_rule() {
                                single_name_part => parts.push(pair.as_span()),
                                _ => panic!("ICE: unexpected pair in name_list {pair}"),
                            }
                        }
                        name.name.push(FunctionSignaturePart::Name {
                            names: Necessity::Required(parts),
                        })
                    }
                    optional_name => {
                        println!("optional name {}", pair.clone().descend().as_str());
                        name.name.push(FunctionSignaturePart::Name {
                            names: Necessity::Optional(vec![pair
                                .descend()
                                .expect(single_name_part)
                                .as_span()]),
                        })
                    }
                    _ => panic!("ICE: unexpected pair in name_part {pair}"),
                }
            }
            paren => name.name.push(FunctionSignaturePart::Parameter {
                name: pair.as_span(),
                reference: false,
            }),
            ref_paren => name.name.push(FunctionSignaturePart::Parameter {
                name: pair.descend().descend().as_span(),
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
    fn walk_function_call(&mut self, function_name: &FunctionCallName<'s>) {
        self.visit_function_call(function_name);
    }

    fn visit_function(&mut self, _function: &Function<'s>) {}
    fn visit_statement(&mut self, _statement: &Statement<'s>) {}
    fn visit_expression(&mut self, _expression: &Expression<'s>) {}
    fn visit_function_call(&mut self, _function_name: &FunctionCallName<'s>) {}
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
