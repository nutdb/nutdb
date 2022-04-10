use derive_more::{Constructor, From};

pub use alter::*;
pub use expr::*;
pub use item::*;
pub use query::*;

mod alter;
mod expr;
mod item;
mod query;

#[derive(Debug, Clone, From)]
pub enum Statement<'a> {
    Select(SelectStmt<'a>),
    Insert(InsertStmt<'a>),
    Explain(ExplainStmt<'a>),
    Alter(AlterStmt<'a>),
    Create(CreateStmt<'a>),
    Describe(DescribeStmt<'a>),
    Drop(DropStmt<'a>),
    Truncate(TruncateStmt<'a>),
    Optimize(OptimizeStmt<'a>),
    Set(SetStmt<'a>),
}

#[derive(Debug, Clone, Constructor)]
pub struct SelectStmt<'a> {
    pub query: Query<'a>,
}

#[derive(Debug, Clone, Constructor)]
pub struct ExplainStmt<'a> {
    pub query: Query<'a>,
}

#[derive(Debug, Clone, Constructor)]
pub struct CreateStmt<'a> {
    pub if_not_exists: bool,
    pub entity_def: CreatableEntity<'a>,
}

#[derive(Debug, Clone)]
pub enum CreatableEntity<'a> {
    Table(TableDefinition<'a>),
    View(ViewDefinition<'a>),
}

#[derive(Debug, Clone, Constructor)]
pub struct InsertStmt<'a> {
    pub table_name: &'a str,
    pub column_list: Option<Vec<&'a str>>,
    pub data: InsertSource<'a>,
}

#[derive(Debug, Clone)]
pub enum InsertSource<'a> {
    Rows {
        column_size: usize,
        data: Vec<Expr<'a>>,
    },
    Subquery(Query<'a>),
    FnCall(FnCall<'a>),
}

#[derive(Debug, Clone, Constructor)]
pub struct AlterStmt<'a> {
    pub alter: Alter<'a>,
}

#[derive(Debug, Clone, Constructor)]
pub struct DescribeStmt<'a> {
    entity: DescribableEntity<'a>,
}

#[derive(Debug, Clone)]
pub enum DescribableEntity<'a> {
    Database,
    Table(&'a str),
    View(&'a str),
}

#[derive(Debug, Clone, Constructor)]
pub struct OptimizeStmt<'a> {
    pub table_name: &'a str,
    pub partition_key: Option<Expr<'a>>,
}

#[derive(Debug, Clone, Constructor)]
pub struct SetStmt<'a> {
    pub config_name: &'a str,
    pub value: Expr<'a>,
}

#[derive(Debug, Clone, Constructor)]
pub struct DropStmt<'a> {
    pub typ: DatabaseEntity,
    pub if_exists: bool,
    pub name: &'a str,
}

#[derive(Debug, Clone, Constructor)]
pub struct TruncateStmt<'a> {
    pub typ: DatabaseEntity,
    pub if_exists: bool,
    pub name: &'a str,
}
