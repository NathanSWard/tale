#[derive(Debug, PartialEq, Clone)]
pub enum Atom<'ast> {
    Bool(bool),
    Num(f32),
    Variable(&'ast str),
    String(&'ast str),
    Jump(&'ast str),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    And,    // and | &&
    Or,     // or | ||
    Eq,     // ==
    Neq,    // !=
    Gr,     // >
    Less,   // <
    GrEq,   // >=
    LessEq, // <=
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr<'ast> {
    pub op: BinaryOp,
    pub lhs: Expr<'ast>,
    pub rhs: Expr<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr<'ast> {
    pub op: UnaryOp,
    pub expr: Expr<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall<'ast> {
    pub name: &'ast str,
    pub args: Vec<Expr<'ast>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign<'ast> {
    pub var: &'ast str,
    pub expr: Expr<'ast>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'ast> {
    Atom(Box<Atom<'ast>>),
    Unary(Box<UnaryExpr<'ast>>),
    Binary(Box<BinaryExpr<'ast>>),
    FunctionCall(FunctionCall<'ast>),
    Assign(Box<Assign<'ast>>),
}

impl<'ast> From<BinaryExpr<'ast>> for Expr<'ast> {
    fn from(expr: BinaryExpr<'ast>) -> Self {
        Expr::Binary(Box::new(expr))
    }
}

impl<'ast> From<UnaryExpr<'ast>> for Expr<'ast> {
    fn from(expr: UnaryExpr<'ast>) -> Self {
        Expr::Unary(Box::new(expr))
    }
}

impl<'ast> From<Atom<'ast>> for Expr<'ast> {
    fn from(expr: Atom<'ast>) -> Self {
        Expr::Atom(Box::new(expr))
    }
}

impl<'ast> From<FunctionCall<'ast>> for Expr<'ast> {
    fn from(expr: FunctionCall<'ast>) -> Self {
        Expr::FunctionCall(expr)
    }
}

impl<'ast> From<Assign<'ast>> for Expr<'ast> {
    fn from(expr: Assign<'ast>) -> Self {
        Expr::Assign(Box::new(expr))
    }
}
