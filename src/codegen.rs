use core::fmt;
use std::collections::HashMap;
use std::fmt::Write as FmtWrite;
use std::ops::Deref;
use std::path::Path;
use std::sync::Mutex;
use std::{fs::File, io::Write};

use indoc::indoc;
use once_cell::sync::Lazy;
use tempfile::tempdir;
use xshell::{cmd, Shell};

use crate::HIR::{
    self, Expression, Function, FunctionID, FunctionSignature, Literal, Program, Statement,
};
use crate::{SpanExt, C_HEADER, C_LIB};

/// Names and corresponding IDs of builtin functions.
pub static BUILTIN_NAMES: Lazy<Mutex<HashMap<FunctionID, &'static str>>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert(FunctionID::new(2), "b_show");
    Mutex::new(map)
});

// static C_LIBARY: &'static str = include_str!("remix.c");

pub fn compile_c(program: &str, out: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let dir = tempdir().unwrap();
    let transpiled = dir.path().join("transpiled.c");
    let stdlib = dir.path().join("remix.c");
    let stdlib_h = dir.path().join("remix.h");

    File::create(transpiled.clone())?.write_all(program.as_bytes())?;

    File::create(stdlib.clone())?.write_all(C_LIB.as_bytes())?;
    File::create(stdlib_h.clone())?.write_all(C_HEADER.as_bytes())?;

    let sh = Shell::new()?;
    cmd!(
        sh,
        "clang {transpiled}
        {stdlib}
        -o {out}"
    )
    .run()?;
    // clang -c remix.c
    // clang hand.c
    Ok(())
}

pub struct Codegen {
    c: String,
    counter: Box<dyn Iterator<Item = u64>>,
}

impl<'s> Codegen {
    pub fn new() -> Self {
        Self {
            c: String::new(),
            counter: Box::new(0..),
        }
    }

    pub fn emit_program(mut self, program: Program<'s>) -> Result<String, std::fmt::Error> {
        // Link the standard library
        write!(self.c, "#include \"remix.h\"\n\n")?;

        for function in &program.functions {
            self.emit_function(function)?;
            write!(self.c, "\n")?;
        }
        self.emit_main(&program.main)?;
        Ok(self.c)
    }

    fn emit_function(&mut self, function: &Function<'s>) -> fmt::Result {
        self.emit_signature(function.id, &function.name)?;
        for statement in &function.statements {
            self.emit_statement(&statement)?;
        }
        match function.statements[function.statements.len() - 1] {
            Statement::Return(_) => (),
            _ => write!(self.c, "return new_object(renone);\n")?,
        }
        write!(self.c, "}}\n")?;
        Ok(())
    }

    fn emit_statement(&mut self, statement: &Statement<'s>) -> fmt::Result {
        match statement {
            Statement::Assignment { variable, value } => {
                let value = self.emit_expression(value);
                write!(self.c, "var_{} = {};\n", variable, value?)?;
            }
            Statement::Return(expr) => {
                let value = self.emit_expression(expr);
                write!(self.c, "return {};\n", value?)?;
            }
            Statement::Redo => todo!(),
            Statement::Expression(expr) => {
                self.emit_expression(expr)?;
                write!(self.c, ";\n")?;
            }
            Statement::ListAssignment { .. } => todo!(),
        }
        Ok(())
    }

    fn emit_main(&mut self, main: &Vec<Statement<'s>>) -> fmt::Result {
        write!(self.c, "int main() {{\n")?;
        for statement in main {
            self.emit_statement(statement)?;
        }
        write!(self.c, "}}")?;
        Ok(())
    }

    fn emit_signature(&mut self, id: FunctionID, name: &FunctionSignature<'s>) -> fmt::Result {
        write!(
            self.c,
            "// Generated from {name} defined at {}\n",
            name.span.span.format()
        )?;
        write!(self.c, "ReObject func_{}(", Into::<u64>::into(id))?;
        for part in &name.name {
            match part {
                HIR::FunctionSignaturePart::Name { .. } => (),
                HIR::FunctionSignaturePart::Parameter { id, .. } => {
                    write!(self.c, "ReObject var_{}", id)?;
                }
            }
        }
        write!(self.c, ") {{\n")?;

        Ok(())
    }

    /// Emit an expression, returning the variable name in which it is stored.
    pub(crate) fn emit_expression(
        &mut self,
        expr: &Expression<'s>,
    ) -> Result<String, std::fmt::Error> {
        match expr {
            Expression::Binary { lhs, operator, rhs } => {
                let lhs = self.emit_expression(lhs)?;
                let rhs = self.emit_expression(rhs)?;
                write!(self.c, "ReObject lhs = {};\n", lhs)?;
                write!(self.c, "ReObject rhs = {};\n", rhs)?;
                match operator {
                    HIR::Operator::Addition => {
                        write!(self.c, "ReObject expr_ret = add(lhs, rhs);\n")?;
                    }
                    HIR::Operator::Subtraction => todo!(),
                    HIR::Operator::Multiplication => todo!(),
                    HIR::Operator::Division => todo!(),
                    HIR::Operator::Exponentiation => todo!(),
                    HIR::Operator::Modulus => todo!(),
                    HIR::Operator::Equality => todo!(),
                    HIR::Operator::InEquality => todo!(),
                    HIR::Operator::GreaterThan => todo!(),
                    HIR::Operator::LessThan => todo!(),
                    HIR::Operator::GreaterThanEq => todo!(),
                    HIR::Operator::LessThanEq => todo!(),
                };
                Ok("expr_ret".to_owned())
            }
            Expression::Unary(unary) => match unary.as_ref() {
                HIR::UnaryExpression::Variable(id) => Ok(format!("var_{}", id)),
                HIR::UnaryExpression::ListElement { .. } => todo!(),
                HIR::UnaryExpression::FunctionCall { function } => {
                    let mut args = Vec::new();
                    for part in &function.name {
                        match part.borrow().deref() {
                            HIR::FunctionCallPart::Name { .. } => (),
                            HIR::FunctionCallPart::Expression(expr) => {
                                args.push(self.emit_expression(&expr)?);
                            }
                            HIR::FunctionCallPart::Defered(..) => todo!(),
                        }
                    }
                    let name = if function.builtin() {
                        BUILTIN_NAMES
                            .lock()
                            .unwrap()
                            .get(&function.id.borrow().unwrap())
                            .unwrap()
                            .to_string()
                    } else {
                        format!("func_{}", Into::<u64>::into(function.id.borrow().unwrap()))
                    };
                    let count = self.counter.next().unwrap();
                    write!(
                        self.c,
                        "ReObject call_ret_{} = {}({})",
                        count,
                        name,
                        args.iter()
                            .map(|name| format!("{name}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                    Ok(format!("call_ret_{}", count))
                }
                HIR::UnaryExpression::Literal(lit) => self.format_literal(lit),
                HIR::UnaryExpression::Block(_) => todo!(),
            },
        }
    }

    /// Format a literal, obtaining a value which can be used as the RHS of some expression
    fn format_literal(&mut self, lit: &Literal<'s>) -> Result<String, fmt::Error> {
        match lit {
            // Simple literals
            Literal::Integer(i) => Ok(format!("{}", i)),
            Literal::Float(f) => Ok(format!("{}", f)),
            Literal::Boolean(b) => Ok(match b {
                true => "true".into(),
                false => "false".into(),
            }),
            Literal::None => Ok("null".into()),

            // Composite literals
            Literal::String(s) => Ok(format!("new_string(\"{}\", {})", escape(s), s.len())),
            Literal::List(list) => {
                let count = self.counter.next().unwrap();
                let items = list
                    .iter()
                    .map(|expr| {
                        Ok(format!(
                            "push(list_{count}, {});\n",
                            self.emit_expression(expr)?
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .join("");
                write!(
                    self.c,
                    indoc!(
                        "
                        ReObject list_{} = new_list_cap({});
                        {}
                        "
                    ),
                    count,
                    list.len(),
                    items
                )?;
                Ok(format!("list_{}", count))
            }
            Literal::Map(_) => todo!(),
        }
    }
}

fn escape(s: &str) -> String {
    s.replace("\n", "\\n").replace("\r", "\\r")
}

#[cfg(test)]
mod tests {
    use std::{fs, process::Command};

    use insta::assert_snapshot;
    use pest::Parser;
    use tempfile::tempdir;
    use test_case::test_case;

    use crate::{
        parser::{RemixParser, Rule},
        resolver::Resolver,
    };

    use super::*;

    #[test]
    fn test_compile_c() {
        let dir = tempdir().unwrap();
        compile_c(
            r#"
            #include <stdio.h>
            int main() {
                printf("Hello world\n");
                return 0;
            }
        "#,
            &dir.path().join("test.exe"),
        )
        .expect("failed to compile c");
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
    fn test_codegen(path: &'static str) {
        let file =
            fs::read_to_string("../Remix/".to_owned() + path).expect("could not find test file");
        match RemixParser::parse(Rule::program, &file) {
            Ok(mut parse) => {
                let dir = tempdir().unwrap();
                let test = dir.path().join("test.exe");
                let pair = parse.next();
                let mut ast = HIR::Program::new(&file, pair.unwrap(), true);

                Resolver::resolve(&mut ast);

                let transpiled = Codegen::new().emit_program(ast).expect("codegen failed");
                compile_c(&transpiled, &test).expect("failed to compile c");

                let out = Command::new(test)
                    .output()
                    .expect("running built test failed");

                assert_snapshot!(String::from_utf8_lossy(&out.stdout));
            }
            Err(_) => {
                panic!("Failed to parse file {path}")
            }
        }

        let _ = fs::remove_file("built.exe");
        let _ = fs::remove_file("transpiled.c");
    }
}
