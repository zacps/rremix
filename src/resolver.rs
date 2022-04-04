/// This module is responsible for name resolution, both for variables and functions.
///
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use crate::HIR::*;

pub struct Resolver<'s> {
    program: RefCell<Program<'s>>,
}

impl<'s> Resolver<'s> {
    pub fn resolve(program: Program<'s>) -> Program<'s> {
        let r = Self {
            program: RefCell::new(program),
        };
        r.resolve_names();
        r.program.into_inner()
    }

    fn resolve_names(&self) {
        use Statement::*;
        for statement in &mut self.program.borrow_mut().main {
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
                if let Some(_id) = self.program.borrow().functions.get(function) {
                    // map function call to function table
                    function.id = Some(_id.id)
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
