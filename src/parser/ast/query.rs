use crate::parser::ast::{Expr, Identifier};
use crate::parser::FnCall;
use derive_more::Constructor;
use std::fmt::Debug;

/// ```plain
/// [with clause]
/// select
/// [distinct clause]
/// list_expr
/// [from clause]
/// [sample clause?]
/// [array join clause]
/// [join clause]
/// [where clause]
/// [group clause]
/// [having clause]
/// [order clause]
/// [limit clause]
/// ```
#[derive(Debug, Clone, Constructor)]
pub struct QueryBody<'a> {
    pub with: Option<WithClause<'a>>,
    pub distinct: Option<DistinctClause<'a>>,
    pub columns: Vec<QueryExpr<'a>>,
    pub from: Option<FromClause<'a>>,
    /// TODO: array_join
    // pub array_join: Option<()>,
    pub joins: Vec<JoinClause<'a>>,
    pub r#where: Option<WhereClause<'a>>,
    pub group_by: Option<GroupByClause<'a>>,
    pub having: Option<HavingClause<'a>>,
    pub order_by: Option<OrderByClause<'a>>,
    pub limit: Option<LimitClause>,
}

/// `with <identifier> as <expr>`
#[derive(Debug, Clone, Constructor)]
pub struct WithClause<'a> {
    pub cte_list: Vec<QueryCTE<'a>>,
}

/// `distinct [on <columns>]`
#[derive(Debug, Clone, Constructor)]
pub struct DistinctClause<'a> {
    pub columns: Option<Vec<QueryExpr<'a>>>,
}

/// `from <table identifier | subquery | table function>`
#[derive(Debug, Clone, Constructor)]
pub struct FromClause<'a> {
    pub source: QuerySource<'a>,
}

/// ```plain
/// [inner | left | right | full]
/// [outer | semi | anti | asof?]
/// join <table identifier | subquery | table function>
/// (on <expr_list>) | (using <column_list>)
/// ```
#[derive(Debug, Clone, Constructor)]
pub struct JoinClause<'a> {
    pub typ: JoinType,
    pub source: QuerySource<'a>,
    pub condition: JoinCondition<'a>,
}

/// `where <expr>`
#[derive(Debug, Clone, Constructor)]
pub struct WhereClause<'a> {
    pub condition: Expr<'a>,
}

/// `group by <expr_list>`
#[derive(Debug, Clone, Constructor)]
pub struct GroupByClause<'a> {
    pub keys: Vec<QueryExpr<'a>>,
}

/// `having <expr>`
#[derive(Debug, Clone, Constructor)]
pub struct HavingClause<'a> {
    pub condition: Expr<'a>,
}

/// `order by <expr_list>`
#[derive(Debug, Clone, Constructor)]
pub struct OrderByClause<'a> {
    pub keys: Vec<QueryOrderKey<'a>>,
}

/// `(limit [o, ]n)|(limit n offset o) [with ties]`
#[derive(Debug, Clone, Constructor)]
pub struct LimitClause {
    pub size: usize,
    pub offset: usize,
    pub with_ties: bool,
}

#[derive(Debug, Clone)]
pub enum JoinType {
    Inner,
    FullOuter,
    LeftOuter,
    RightOuter,
    LeftSemi,
    RightSemi,
    LeftAnti,
    RightAnti,
    AsOf,
}

#[derive(Debug, Clone)]
pub enum JoinCondition<'a> {
    On(Box<Expr<'a>>),
    Using(Vec<Identifier<'a>>),
}

#[derive(Debug, Clone)]
pub enum Query<'a> {
    Single(Box<QueryBody<'a>>),
    Union {
        typ: UnionType,
        left: Box<Query<'a>>,
        right: Box<Query<'a>>,
    },
}

/// `intersect|union|except <another query>`
#[derive(Debug, Clone)]
pub enum UnionType {
    UnionAll,
    UnionDistinct,
    Intersect,
    Except,
}

#[derive(Debug, Clone)]
pub enum DataSource<'a> {
    TableFn(FnCall<'a>),
    Table(&'a str),
    Subquery(Query<'a>),
}

#[derive(Debug, Clone, Constructor)]
pub struct QueryOrderKey<'a> {
    expr: QueryExpr<'a>,
    direction: OrderDirection,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OrderDirection {
    ASC,
    DESC,
}

#[derive(Debug, Clone, Constructor)]
pub struct QueryCTE<'a> {
    subquery: Box<Query<'a>>,
    alias: &'a str,
}

#[derive(Debug, Clone, Constructor)]
pub struct QueryExpr<'a> {
    inner: Expr<'a>,
    alias: Option<&'a str>,
}

#[derive(Debug, Clone, Constructor)]
pub struct QuerySource<'a> {
    inner: DataSource<'a>,
    alias: Option<&'a str>,
}
