pub mod instruction;
pub mod value;

use crate::parser::{expr::*, visit::*};
use crate::vm::dialogue::*;

#[derive(Debug, Default, Clone)]
pub struct Compiler {
    pub program: Program<String>,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Visitor for Compiler {
    type Output = ();

    fn visit_atom(&mut self, _atom: &Atom) -> Self::Output {
        todo!()
    }

    fn visit_fn_call(&mut self, _fn_call: &FunctionCall) -> Self::Output {
        todo!()
    }

    fn visit_unary_expr(&mut self, _unary_expr: &UnaryExpr) -> Self::Output {
        todo!()
    }

    fn visit_bin_expr(&mut self, _bin_expr: &BinaryExpr) -> Self::Output {
        todo!()
    }

    fn visit_expr(&mut self, _expr: &Expr) -> Self::Output {
        todo!()
    }
}

#[cfg(test)]
mod test {}
