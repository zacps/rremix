use core::fmt;
use std::fmt::Write as FmtWrite;
use std::{fs::File, io::Write};

use xshell::{cmd, Shell};

use crate::SpanExt;
use crate::HIR::{self, Function, FunctionID, FunctionSignature, Program, Statement};

static C_LIBARY: &'static str = include_str!("remix.c");

pub fn compile_c(program: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::create("transpiled.c")?;
    file.write_all(program.as_bytes())?;

    let sh = Shell::new()?;
    cmd!(sh, "clang transpiled.c -o built.exe").run()?;
    Ok(())
}

pub struct Codegen {
    c: String,
}

impl<'s> Codegen {
    pub fn emit_program(&mut self, program: Program<'s>) -> fmt::Result {
        for function in &program.functions {
            self.emit_function(function)?;
        }
        self.emit_main(&program.main)?;
        Ok(())
    }
    fn emit_function(&mut self, function: &Function<'s>) -> fmt::Result {
        self.emit_signature(function.id, &function.name)?;
        for statement in &function.statements {
            self.emit_statement(&statement)
        }
        Ok(())
    }

    fn emit_statement(&mut self, statement: &Statement<'s>) {
        match statement {
            Statement::Assignment { variable, value } => todo!(),
            Statement::Return => todo!(),
            Statement::Redo => todo!(),
            Statement::Expression(_) => todo!(),
            Statement::ListAssignment {
                variable,
                index,
                value,
            } => todo!(),
        }
    }

    fn emit_main(&mut self, main: &Vec<Statement<'s>>) -> fmt::Result {
        write!(self.c, "void main() {{\n")?;
        // TODO: Emit any runtime setup needed?
        for statement in main {
            self.emit_statement(statement);
        }
        write!(self.c, "}}")?;
        Ok(())
    }

    fn emit_signature(&mut self, id: FunctionID, name: &FunctionSignature<'s>) -> fmt::Result {
        write!(
            self.c,
            "// Generated from {name} defined at {}",
            name.span.format()
        )?;
        write!(self.c, "ReObject func_{}(", Into::<u64>::into(id))?;
        for part in &name.name {
            match part {
                HIR::FunctionSignaturePart::Name { .. } => (),
                HIR::FunctionSignaturePart::Parameter {
                    name,
                    reference,
                    id,
                } => {
                    write!(self.c, "var_{}", id)?;
                }
            }
        }
        write!(self.c, ")")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_c() {
        compile_c(
            r#"
            #include <stdio.h>
            int main() {
                printf("Hello world\n");
                return 0;
            }
        "#,
        )
        .expect("failed to compile c");
    }
}
