use crate::parser::ast::{Expr, FnCall};
use bigdecimal::BigDecimal;
use std::borrow::Cow;

use crate::parser::Query;
use derive_more::{Constructor, From};

#[derive(Debug, Clone, From)]
pub enum DataType<'a> {
    Scalar(ScalarDataType),
    Compound(CompoundDataType<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ScalarDataType {
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    Serial32,
    Serial64,
    Serial128,
    USerial32,
    USerial64,
    USerial128,
    Decimal32 {
        scale: u8,
    },
    Decimal64 {
        scale: u8,
    },
    Float32,
    Float64,
    Boolean,
    /// fixed-length
    Chars {
        length: usize,
    },
    /// optional max length, 0 means unlimited
    String {
        max_length: usize,
    },
    Uuid,
    Date,
    Datetime,
}

#[derive(Debug, Clone)]
pub enum CompoundDataType<'a> {
    /// Array
    Array(Box<DataType<'a>>),
    /// Enum
    Enum(Vec<EnumBind<'a>>),
    /// Tuple
    Tuple(Vec<DataType<'a>>),
    /// Map
    Map(Box<DataType<'a>>, Box<DataType<'a>>),
    /// Special Dictionary
    Dictionary(Box<DataType<'a>>),
    /// Special Nullable
    Nullable(Box<DataType<'a>>),
}

#[derive(Debug, Clone, Constructor)]
pub struct EnumBind<'a> {
    pub id: usize,
    pub literal: Cow<'a, str>,
}

/// Entity is an identifier or an compound identifier.
#[derive(Debug, Clone, Eq, PartialEq, Constructor)]
pub struct Identifier<'a> {
    pub name: IdentifierName<'a>,
    pub qualifier: Option<&'a str>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IdentifierName<'a> {
    Wildcard,
    Word(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'a> {
    Integer(u128),
    /// BigDecimal is too big so box it.
    Float(Box<BigDecimal>),
    String(Cow<'a, str>),
    Boolean(bool),
    // Interval is special because users can only use its literal.
    Interval(u64, IntervalUnit),
    // Null is special
    Null,
}

#[derive(Debug, Clone, Constructor)]
pub struct Collection<'a> {
    typ: CollectionType,
    items: Vec<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CollectionType {
    Tuple,
    Map,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntervalUnit {
    Second,
    Minute,
    Hour,
    Day,
    Month,
    Year,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    BitwiseNot,
    Not,
    Neg,
    IsNull,
    IsNotNull,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multi,
    Div,
    Mod,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Eq,
    NotEq,
    And,
    Or,
    Xor,
    Like,
    NotLike,
    ILike,
    NotILike,
    In,
    NotIn,
    IndexAccess,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FnName<'a> {
    /// syntax sugar: `IF <expr> THEN <expr> ELSE <expr> END`
    If,
    /// syntax sugar: `CASE (WHEN <expr> THEN <expr>)... ELSE <expr> END`
    MultiIf,
    /// syntax sugar: `CASE <expr> (WHEN <expr> THEN <expr>)... ELSE <expr> END`
    CaseWhen,
    /// syntax sugar: `<expr> BETWEEN <expr> AND <expr>`
    Between,
    NotBetween,
    Exists,
    NotExists,
    /// other functions by name
    Others(&'a str),
}

#[derive(Debug, Clone, Constructor)]
pub struct ColumnDefinition<'a> {
    pub name: &'a str,
    pub typ: DataType<'a>,
    pub default: Option<Expr<'a>>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, Constructor)]
pub struct ConstraintDefinition<'a> {
    pub name: &'a str,
    pub check: Expr<'a>,
}

#[derive(Debug, Clone, Constructor)]
pub struct IndexDefinition<'a> {
    pub name: &'a str,
    pub indexer: FnCall<'a>,
}

#[derive(Debug, Clone)]
pub enum DatabaseEntity {
    Table,
    View,
}

#[derive(Debug, Clone, Constructor)]
pub struct TableDefinition<'a> {
    pub name: &'a str,
    pub columns: Vec<ColumnDefinition<'a>>,
    pub constraints: Vec<ConstraintDefinition<'a>>,
    pub indexes: Vec<IndexDefinition<'a>>,
    pub primary_key: Option<Vec<Expr<'a>>>,
    pub order_by: Option<Vec<Expr<'a>>>,
    pub partition_by: Option<Expr<'a>>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, Constructor)]
pub struct ViewDefinition<'a> {
    pub name: &'a str,
    pub strategy: &'a str,
    pub primary_key: Option<Vec<Expr<'a>>>,
    pub order_by: Option<Vec<Expr<'a>>>,
    pub partition_by: Option<Expr<'a>>,
    pub query: Query<'a>,
    pub comment: Option<Cow<'a, str>>,
}
