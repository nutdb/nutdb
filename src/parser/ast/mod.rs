mod alter;
mod expr;
mod item;
mod query;

pub use alter::*;
pub use expr::*;
pub use item::*;
pub use query::*;

use derive_more::{Constructor, From};

#[derive(Debug, Clone, From)]
pub enum Statement {
    Select(SelectStmt),
    Insert(InsertStmt),
    Explain(ExplainStmt),
    Alter(AlterStmt),
    CreateTable(CreateTableStmt),
    CreateView(CreateViewStmt),
    Describe(DescribeStmt),
    Drop(DropStmt),
    Truncate(TruncateStmt),
    Optimize(OptimizeStmt),
    Set(SetStmt),
}

#[derive(Debug, Clone, Constructor)]
pub struct SelectStmt {
    pub query: Query,
}

#[derive(Debug, Clone, Constructor)]
pub struct ExplainStmt {
    pub query: Query,
}

#[derive(Debug, Clone, Constructor)]
pub struct CreateTableStmt {
    pub name: String,
    pub if_not_exists: bool,
    pub columns: Vec<ColumnDefinition>,
    pub constraints: Vec<ConstraintDefinition>,
    pub indexes: Vec<IndexDefinition>,
    pub primary_key: Option<Vec<Expr>>,
    pub order_by: Option<Vec<Expr>>,
    pub partition_by: Option<Expr>,
    pub comment: Option<String>,
}

#[derive(Debug, Clone, Constructor)]
pub struct CreateViewStmt {
    pub name: String,
    pub if_not_exists: bool,
    pub strategy: String,
    pub primary_key: Option<Vec<Expr>>,
    pub order_by: Option<Vec<Expr>>,
    pub partition_by: Option<Expr>,
    pub query: Query,
    pub comment: Option<String>,
}

#[derive(Debug, Clone, Constructor)]
pub struct InsertStmt {
    pub table_name: String,
    pub column_list: Option<Vec<String>>,
    pub data: InsertSource,
}

#[derive(Debug, Clone)]
pub enum InsertSource {
    Rows { column_size: usize, data: Vec<Expr> },
    Subquery(Query),
    FnCall(FnCall),
}

#[derive(Debug, Clone, Constructor)]
pub struct AlterStmt {
    pub action: Alter,
}

#[derive(Debug, Clone)]
pub enum DescribeStmt {
    Database,
    Table { name: String },
    View { name: String },
}

#[derive(Debug, Clone, Constructor)]
pub struct OptimizeStmt {
    pub table_name: String,
    pub partition_key: Option<Expr>,
}

#[derive(Debug, Clone, Constructor)]
pub struct SetStmt {
    pub config_name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, Constructor)]
pub struct DropStmt {
    pub typ: DatabaseEntity,
    pub if_exists: bool,
    pub name: String,
}

#[derive(Debug, Clone, Constructor)]
pub struct TruncateStmt {
    pub typ: DatabaseEntity,
    pub if_exists: bool,
    pub name: String,
}
