use crate::parser::ast::{
    BinaryOperator, Collection, FnName, Identifier, Literal, Query, UnaryOperator,
};

use derive_more::{Constructor, From};

#[derive(Debug, Clone, From)]
pub enum Expr<'a> {
    Identifier(Identifier<'a>),
    QueryParameter(QueryParameter),
    Literal(Literal<'a>),
    Collection(Collection<'a>),
    UnaryOp(UnaryOp<'a>),
    BinaryOp(BinaryOp<'a>),
    FnCall(FnCall<'a>),
    Subquery(Query<'a>),
}

#[derive(Debug, Clone, Constructor)]
pub struct UnaryOp<'a> {
    pub op: UnaryOperator,
    pub operand: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Constructor)]
pub struct BinaryOp<'a> {
    pub op: BinaryOperator,
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Clone, Constructor)]
pub struct FnCall<'a> {
    pub callee: FnName<'a>,
    pub arguments: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, Constructor)]
pub struct QueryParameter {
    pub index: usize,
}
