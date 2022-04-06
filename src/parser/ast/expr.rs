use crate::parser::ast::{BinaryOperator, Function, Identifier, Query, UnaryOperator, Value};

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(Identifier),
    Value(Value),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FnCall(FnCall),
    IndexAccess(IndexAccess),
    Subquery(Query),
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    op: UnaryOperator,
    operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    op: BinaryOperator,
    left: Box<Expr>,
    right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    callee: Function,
    arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct IndexAccess {
    operand: Box<Expr>,
    index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct AliasedExpr {
    pub expr: Expr,
    pub alias: Option<String>,
}
