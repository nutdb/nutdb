use crate::parser::ast::{AliasedExpr, Expr, FnCall, Identifier};

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
#[derive(Debug, Clone)]
pub struct QueryBody {
    pub with: Option<WithClause>,
    pub distinct: Option<DistinctClause>,
    pub columns: ColumnsClause,
    pub from: Option<FromClause>,
    /// TODO: array_join
    // pub array_join: Option<()>,
    pub join: Option<JoinClause>,
    pub r#where: Option<WhereClause>,
    pub group_by: Option<GroupByClause>,
    pub having: Option<HavingClause>,
    pub order_by: Option<OrderByClause>,
    pub limit: Option<LimitClause>,
    pub limit_on: Option<LimitOnClause>,
}

/// `with <identifier> as <subquery>`
#[derive(Debug, Clone)]
pub struct WithClause {
    pub subquery_list: Vec<AliasedSubquery>,
}

/// `distinct [on <columns>]`
#[derive(Debug, Clone)]
pub struct DistinctClause {
    pub columns: Option<Vec<AliasedExpr>>,
}

/// `distinct [on <columns>]`
#[derive(Debug, Clone)]
pub struct ColumnsClause {
    pub columns: Vec<AliasedExpr>,
}

/// `from <table identifier | subquery | table function>`
#[derive(Debug, Clone)]
pub struct FromClause {
    pub source: Source,
}

/// ```plain
/// [inner | left | right | full]
/// [outer | semi | anti | asof?]
/// join <table identifier | subquery | table function>
/// (on <expr_list>) | (using <column_list>)
/// ```
#[derive(Debug, Clone)]
pub struct JoinClause {
    pub typ: JoinType,
    pub source: Source,
    pub condition: JoinCondition,
}

/// `where <expr>`
#[derive(Debug, Clone)]
pub struct WhereClause {
    pub condition: Expr,
}

/// `group by <expr_list>`
#[derive(Debug, Clone)]
pub struct GroupByClause {
    pub keys: Vec<AliasedExpr>,
}

/// `having <expr>`
#[derive(Debug, Clone)]
pub struct HavingClause {
    pub condition: Expr,
}

/// `order by <expr_list>`
#[derive(Debug, Clone)]
pub struct OrderByClause {
    pub keys: Vec<AliasedExpr>,
}

/// `(limit [o, ]n)|(limit n offset o) [with ties]`
#[derive(Debug, Clone)]
pub struct LimitClause {
    pub size: usize,
    pub offset: usize,
    pub with_ties: bool,
}

/// `(limit [o, ]n)|(limit n offset o) on <columns>`
#[derive(Debug, Clone)]
pub struct LimitOnClause {
    pub size: usize,
    pub offset: usize,
    pub keys: Vec<AliasedExpr>,
}

#[derive(Debug, Clone)]
pub struct AliasedSubquery {
    pub name: String,
    pub subquery: Query,
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
    On(Vec<Expr>),
    Using(Vec<Identifier>),
}

#[derive(Debug, Clone)]
pub enum Source {
    Table {
        // top-level is tables so use String directly
        raw: String,
        alias: Option<String>,
    },
    Fn {
        fn_call: FnCall,
        alias: Option<String>,
    },
    Subquery {
        subquery: Query,
        alias: Option<String>,
    },
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
