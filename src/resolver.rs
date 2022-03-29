use crate::HIR;

/// This module is responsible for name resolution, both for variables and functions.

pub fn resolve_names(ast: HIR::Program<'_>) -> HIR::Program<'_> {
    ast
}
