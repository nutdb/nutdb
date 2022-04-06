use crate::parser::ast::{Expr, FnCall};
use bigdecimal::BigDecimal;
use core::num::NonZeroUsize;

#[derive(Debug, Clone)]
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
    Serial8,
    Serial16,
    Serial32,
    Serial64,
    Serial128,
    USerial8,
    USerial16,
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
    /// optional max length
    String {
        max_length: Option<NonZeroUsize>,
    },
    Uuid,
    Date,
    Datetime,
}

#[derive(Debug, Clone)]
pub enum CompoundDataType {
    /// Array
    Array(ScalarDataType),
    /// Enum
    Enum(Vec<EnumBind>),
    /// Tuple
    Tuple(Vec<TupleBind>),
    /// Map
    Map(ScalarDataType, ScalarDataType),
    /// Special Dictionary
    Dictionary(ScalarDataType),
    /// Special Nullable
    Nullable(ScalarDataType),
}

#[derive(Debug, Clone)]
pub struct EnumBind {
    pub id: usize,
    pub literal: ScalarDataType,
}

#[derive(Debug, Clone)]
pub struct TupleBind {
    pub name: Option<String>,
    pub typ: ScalarDataType,
}

/// Entity is an identifier or an compound identifier.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    name: String,
    parent: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Scalar(ScalarValue),
    Vector(VectorValue),
    // Interval is special because users can only use its literal.
    Interval(u64, IntervalUnit),
    // Null is special
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarValue {
    /// bool indicates whether negative
    Integer(u128, bool),
    /// BigDecimal is too big so box it.
    Float(Box<BigDecimal>),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VectorValue {
    Tuple(Vec<ScalarValue>),
    Array(Vec<ScalarValue>),
    Map(Vec<(ScalarValue, ScalarValue)>),
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
    IsNull,
    IsNotNull,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Assign,
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
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function {
    /// syntax sugar: IF <expr> THEN <expr> ELSE <expr> END
    If,
    /// syntax sugar: CASE (WHEN <expr> THEN <expr>)... ELSE <expr> END
    MultiIf,
    /// syntax sugar: CASE <expr> (WHEN <expr> THEN <expr>)... ELSE <expr> END
    CaseWhen,
    /// other functions by name string
    Others(String),
}

#[derive(Debug, Clone)]
pub struct ColumnDefinition {
    pub name: String,
    pub typ: DataType,
    pub default: Option<Expr>,
    pub comment: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ConstraintDefinition {
    pub name: String,
    pub check: Expr,
}

#[derive(Debug, Clone)]
pub struct IndexDefinition {
    pub name: String,
    pub indexer: FnCall,
}

#[derive(Debug, Clone)]
pub enum DatabaseEntity {
    Table,
    View,
}
