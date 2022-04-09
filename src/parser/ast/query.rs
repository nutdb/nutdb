use crate::parser::ast::{Expr, Identifier};
use derive_more::Constructor;

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
/// [limit on clause]
/// [limit clause]
/// ```
#[derive(Debug, Clone, Constructor)]
pub struct QueryBody {
    pub with: Option<WithClause>,
    pub distinct: Option<DistinctClause>,
    pub columns: Vec<Expr>,
    pub from: Option<FromClause>,
    /// TODO: array_join
    // pub array_join: Option<()>,
    pub join: Option<JoinClause>,
    pub r#where: Option<WhereClause>,
    pub group_by: Option<GroupByClause>,
    pub having: Option<HavingClause>,
    pub order_by: Option<OrderByClause>,
    pub limit: Option<LimitClause>,
}

/// `with <identifier> as <expr>`
#[derive(Debug, Clone, Constructor)]
pub struct WithClause {
    pub list: Vec<Expr>,
}

/// `distinct [on <columns>]`
#[derive(Debug, Clone, Constructor)]
pub struct DistinctClause {
    pub columns: Option<Vec<Expr>>,
}

/// `from <table identifier | subquery | table function>`
#[derive(Debug, Clone, Constructor)]
pub struct FromClause {
    pub source: Expr,
}

/// ```plain
/// [inner | left | right | full]
/// [outer | semi | anti | asof?]
/// join <table identifier | subquery | table function>
/// (on <expr_list>) | (using <column_list>)
/// ```
#[derive(Debug, Clone, Constructor)]
pub struct JoinClause {
    pub typ: JoinType,
    pub source: Expr,
    pub condition: JoinCondition,
}

/// `where <expr>`
#[derive(Debug, Clone, Constructor)]
pub struct WhereClause {
    pub condition: Expr,
}

/// `group by <expr_list>`
#[derive(Debug, Clone, Constructor)]
pub struct GroupByClause {
    pub keys: Vec<Expr>,
}

/// `having <expr>`
#[derive(Debug, Clone, Constructor)]
pub struct HavingClause {
    pub condition: Expr,
}

/// `order by <expr_list>`
#[derive(Debug, Clone, Constructor)]
pub struct OrderByClause {
    pub keys: Vec<Expr>,
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
pub enum JoinCondition {
    On(Box<Expr>),
    Using(Vec<Identifier>),
}

#[derive(Debug, Clone)]
pub enum Query {
    One(Box<QueryBody>),
    Merged {
        typ: MergeType,
        left: Box<Query>,
        right: Box<Query>,
    },
}

/// `intersect|union|except <another query>`
#[derive(Debug, Clone)]
pub enum MergeType {
    UnionAll,
    UnionDistinct,
    Intersect,
    Except,
}
