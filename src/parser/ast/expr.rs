use crate::parser::ast::{
    BinaryOperator, FnName, Identifier, Query, UnaryOperator, Value, Wildcard,
};

use derive_more::From;

#[derive(Debug, Clone, From)]
pub enum Expr {
    Wildcard(Wildcard),
    Identifier(Identifier),
    QueryParameter(QueryParameter),
    Value(Value),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FnCall(FnCall),
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
    pub callee: FnName,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct QueryParameter {
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct AliasedExpr {
    pub expr: Expr,
    pub alias: Option<String>,
}
