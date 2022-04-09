use crate::parser::ast::{
    BinaryOperator, Collection, FnName, Identifier, Literal, Query, UnaryOperator, Wildcard,
};

use derive_more::{Constructor, From};

#[derive(Debug, Clone, From)]
pub enum Expr {
    Alias(Alias),
    Wildcard(Wildcard),
    Identifier(Identifier),
    QueryParameter(QueryParameter),
    Literal(Literal),
    Collection(Collection),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    FnCall(FnCall),
    Subquery(Query),
}

#[derive(Debug, Clone, Constructor)]
pub struct UnaryOp {
    pub op: UnaryOperator,
    pub operand: Box<Expr>,
}

#[derive(Debug, Clone, Constructor)]
pub struct BinaryOp {
    pub op: BinaryOperator,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, Constructor)]
pub struct FnCall {
    pub callee: FnName,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, Constructor)]
pub struct QueryParameter {
    pub index: usize,
}

#[derive(Debug, Clone, Constructor)]
pub struct Alias {
    pub inner: Box<Expr>,
    pub alias: String,
}
