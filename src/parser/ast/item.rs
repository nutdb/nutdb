use crate::parser::ast::{Expr, FnCall};
use bigdecimal::BigDecimal;

use derive_more::{Constructor, From};

#[derive(Debug, Clone, From)]
pub enum DataType {
    Scalar(ScalarDataType),
    Compound(CompoundDataType),
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
pub enum CompoundDataType {
    /// Array
    Array(Box<DataType>),
    /// Enum
    Enum(Vec<EnumBind>),
    /// Tuple
    Tuple(Vec<DataType>),
    /// Map
    Map(Box<DataType>, Box<DataType>),
    /// Special Dictionary
    Dictionary(Box<DataType>),
    /// Special Nullable
    Nullable(Box<DataType>),
}

#[derive(Debug, Clone, Constructor)]
pub struct EnumBind {
    pub id: usize,
    pub literal: String,
}

/// Entity is an identifier or an compound identifier.
#[derive(Debug, Clone, Eq, PartialEq, Constructor)]
pub struct Identifier {
    pub name: String,
    pub qualifier: Option<String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Constructor)]
pub struct Wildcard {
    pub qualifier: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(u128),
    /// BigDecimal is too big so box it.
    Float(Box<BigDecimal>),
    String(String),
    Boolean(bool),
    // Interval is special because users can only use its literal.
    Interval(u64, IntervalUnit),
    // Null is special
    Null,
}

#[derive(Debug, Clone, Constructor)]
pub struct Collection {
    typ: CollectionType,
    items: Vec<Expr>,
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
pub enum FnName {
    /// syntax sugar: `IF <expr> THEN <expr> ELSE <expr> END`
    If,
    /// syntax sugar: `CASE (WHEN <expr> THEN <expr>)... ELSE <expr> END`
    MultiIf,
    /// syntax sugar: `CASE <expr> (WHEN <expr> THEN <expr>)... ELSE <expr> END`
    CaseWhen,
    /// syntax sugar: `<expr> BETWEEN <expr> AND <expr>`
    Between,
    NotBetween,
    /// other functions by id
    Others(Identifier),
}

#[derive(Debug, Clone, Constructor)]
pub struct ColumnDefinition {
    pub name: String,
    pub typ: DataType,
    pub default: Option<Expr>,
    pub comment: Option<String>,
}

#[derive(Debug, Clone, Constructor)]
pub struct ConstraintDefinition {
    pub name: String,
    pub check: Expr,
}

#[derive(Debug, Clone, Constructor)]
pub struct IndexDefinition {
    pub name: String,
    pub indexer: FnCall,
}

#[derive(Debug, Clone)]
pub enum DatabaseEntity {
    Table,
    View,
}
