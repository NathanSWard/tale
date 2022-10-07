use super::expr::*;

pub trait Acceptor {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output;
}

pub trait Visitor {
    type Output;

    fn visit_atom(&mut self, atom: &Atom) -> Self::Output;
    fn visit_fn_call(&mut self, fn_call: &FunctionCall) -> Self::Output;
    fn visit_unary_expr(&mut self, unary_expr: &UnaryExpr) -> Self::Output;
    fn visit_bin_expr(&mut self, bin_expr: &BinaryExpr) -> Self::Output;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
}

// impls

impl Acceptor for Atom<'_> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_atom(self)
    }
}

impl Acceptor for UnaryExpr<'_> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_unary_expr(self)
    }
}

impl Acceptor for BinaryExpr<'_> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_bin_expr(self)
    }
}

impl Acceptor for FunctionCall<'_> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_fn_call(self)
    }
}

impl Acceptor for Expr<'_> {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
        visitor.visit_expr(self)
    }
}
