use crate::parser::ast::{
    BinaryOperator, Function, Identifier, Query, UnaryOperator, Value, Wildcard,
};

#[derive(Debug, Clone)]
pub enum Expr {
    Wildcard(Wildcard),
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
    pub op: UnaryOperator,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnCall {
    pub callee: Function,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct IndexAccess {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct AliasedExpr {
    pub expr: Expr,
    pub alias: Option<String>,
}
