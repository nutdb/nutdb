use std::hint::unreachable_unchecked;
use std::str::FromStr;

use bigdecimal::BigDecimal;

pub use ast::*;
pub use error::*;
use keyword::*;
use literal::*;
use tokenizer::{Position, Token, TokenType, Tokenizer};

mod ast;
mod error;
mod keyword;
mod literal;
mod rewrite;
mod tokenizer;

type ParseResult = Result<Statement, ParseError>;

pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    peeked: Option<Token>,
}

impl Parser<'_> {
    pub fn parse(sql: &str) -> ParseResult {
        Parser::new(sql).parse_stmt()
    }
}

impl<'a> Parser<'a> {
    fn new(raw: &'a str) -> Parser<'a> {
        Parser {
            tokenizer: Tokenizer::new(raw),
            peeked: None,
        }
    }
}

macro_rules! never {
    () => {
        unsafe { unreachable_unchecked() }
    };
}

/// `emit_error!(error_type)`
macro_rules! emit_error {
    ($e:ident $($init:tt)?) => {
        Err(SyntaxError::$e $($init)?.into())
    }
}

/// - `test_keyword!(token_str, keyword)`
macro_rules! test_keyword {
    ($token_str:expr, $keyword:expr) => {
        $token_str.eq_ignore_ascii_case($keyword)
    };
}

/// - `test_keywords!(token_str, [keyword])`
macro_rules! test_keywords {
    ($token_str:expr, [$($kw:expr),+]) => {
        [$($kw),+].iter().any(|&s| test_keyword!(s, $token_str))
    };
}

/// - `next_expect!(self, expected_type)`
/// - `next_expect!(self, [expected_type,...])`
macro_rules! next_expect {
    ($self:ident, $expected:ident) => {{
        let token = $self.next()?;
        if (token.t != TokenType::$expected) {
            return emit_error!(NotExpectedTokenTypes {
                expected: vec![TokenType::$expected],
                actual: token.t,
                pos: $self.token_pos(&token)
            });
        }
        token
    }};
    ($self:ident, [$($expected:ident),+]) => {{
        let token = $self.next()?;
        if ($(token.t != TokenType::$expected)&&+) {
            return emit_error!(NotExpectedTokenTypes {
                expected: vec![$(TokenType::$expected),+],
                actual: token.t,
                pos: $self.token_pos(&token)
            });
        }
        token
    }};
}

/// - `peek_expect!(self, expected_type)`
/// - `peek_expect!(self, [expected_type,...])`
macro_rules! peek_expect {
    ($self:ident, $expected:ident) => {{
        let token = $self.peek()?;
        if (token.t != TokenType::$expected) {
            return emit_error!(NotExpectedTokenTypes {
                expected: vec![TokenType::$expected],
                actual: token.t,
                pos: $self.token_pos(&token)
            });
        }
        token
    }};
    ($self:ident, [$($expected:ident),+]) => {{
        let token = $self.peek()?;
        if ($(token.t != TokenType::$expected)&&+) {
            return emit_error!(NotExpectedTokenTypes {
                expected: vec![$(TokenType::$expected),+],
                actual: token.t,
                pos: $self.token_pos(&token)
            });
        }
        token
    }};
}

/// - `next_if!(self, expected_type)`
/// - `next_if!(self, [expected_type,...])`
macro_rules! next_if {
    ($self:ident, $expected:ident) => {{
        let token = $self.peek()?;
        if (token.t == TokenType::$expected) {
            $self.consume_peeked();
            true
        } else {
            false
        }
    }};
    ($self:ident, [$($expected:ident),+]) => {{
        let token = $self.peek()?;
        if ($(token.t == TokenType::$expected)||+) {
            $self.consume_peeked();
            true
        } else {
            false
        }
    }};
}

macro_rules! comma_separated {
    ($self:ident, $t:expr) => {
        loop {
            $t;
            if !next_if!($self, Comma) {
                break;
            }
        }
    };
}

impl Parser<'_> {
    fn parse_stmt(&mut self) -> ParseResult {
        macro_rules! try_parse_chain {
            ($($fn_call:expr),+) => {
                loop {
                    $(
                    let res = $fn_call;
                    if res.is_some() { break res }
                    )+
                    break None
                }
            }
        }

        let token = self.next()?;
        if token.is_terminator() {
            return emit_error!(EmptyQuery);
        }
        if !token.maybe_keyword() {
            return emit_error!(ParseFail {
                pos: self.token_pos(&token)
            });
        }
        let keyword = self.token_str(&token);
        if let Some(stmt) = try_parse_chain!(
            self.try_parse_select_stmt(keyword)?,
            self.try_parse_insert_stmt(keyword)?,
            self.try_parse_explain_stmt(keyword)?,
            self.try_parse_alter_stmt(keyword)?,
            self.try_parse_create_stmt(keyword)?,
            self.try_parse_describe_stmt(keyword)?,
            self.try_parse_drop_stmt(keyword)?,
            self.try_parse_truncate_stmt(keyword)?,
            self.try_parse_optimize_stmt(keyword)?,
            self.try_parse_set_stmt(keyword)?
        ) {
            // check terminator
            let token = self.peek()?;
            if token.is_terminator() {
                Ok(stmt)
            } else {
                emit_error!(ParseFail {
                    pos: self.token_pos(&token)
                })
            }
        } else {
            emit_error!(ParseFail {
                pos: self.token_pos(&token)
            })
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum QueryStartState {
    With,
    Select,
}

impl Parser<'_> {
    fn try_parse_select_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        let start_state = if test_keyword!(leading_keyword, WITH) {
            QueryStartState::With
        } else if test_keyword!(leading_keyword, SELECT) {
            QueryStartState::Select
        } else {
            return Ok(None);
        };
        let query = self.must_parse_query_tdop(start_state, SetOpPower::Terminator)?;
        Ok(Some(SelectStmt::new(query).into()))
    }

    fn must_parse_subquery(&mut self) -> Result<Query, ParseError> {
        self.must_parse_subquery_tdop(SetOpPower::Terminator)
    }

    fn must_parse_subquery_tdop(&mut self, power: SetOpPower) -> Result<Query, ParseError> {
        let has_paren = next_if!(self, LParen);

        let start_state = match self.must_parse_one_of_keywords(&[WITH, SELECT])? {
            0 => QueryStartState::With,
            1 => QueryStartState::Select,
            _ => never!(),
        };

        let query = self.must_parse_query_tdop(
            start_state,
            if has_paren {
                SetOpPower::Terminator
            } else {
                power
            },
        )?;

        if has_paren {
            next_expect!(self, RParen);
        }

        Ok(query)
    }

    fn must_parse_query_tdop(
        &mut self,
        start_state: QueryStartState,
        power: SetOpPower,
    ) -> Result<Query, ParseError> {
        let mut query = Query::One(Box::new(self.must_parse_query_body(start_state)?));
        loop {
            let token = self.peek()?;
            if !token.maybe_keyword() {
                break;
            }

            let token_str = self.token_str(&token);

            let (next_power, merge_type) = if test_keyword!(token_str, INTERSECT) {
                self.consume_peeked();
                (SetOpPower::Intersect, MergeType::Intersect)
            } else if test_keyword!(token_str, UNION) {
                self.consume_peeked();
                (
                    SetOpPower::Union,
                    match self.must_parse_one_of_keywords(&[ALL, DISTINCT])? {
                        0 => MergeType::UnionAll,
                        1 => MergeType::UnionDistinct,
                        _ => never!(),
                    },
                )
            } else if test_keyword!(token_str, EXCEPT) {
                self.consume_peeked();
                (SetOpPower::Except, MergeType::Except)
            } else {
                break;
            };
            if next_power <= power {
                break;
            }

            query = Query::Merged {
                typ: merge_type,
                left: Box::new(query),
                right: Box::new(self.must_parse_subquery_tdop(next_power)?),
            }
        }
        Ok(query)
    }

    /// assume that the first token of the query is consumed
    fn must_parse_query_body(
        &mut self,
        start_state: QueryStartState,
    ) -> Result<QueryBody, ParseError> {
        let with_clause = if start_state == QueryStartState::With {
            let w = self.must_parse_query_clause_with()?;
            // consume next `select`
            self.must_parse_keyword(SELECT)?;
            Some(w)
        } else {
            None
        };
        // no need to use loop since query clauses have strict order

        let distinct_clause = if self.try_parse_keyword(DISTINCT)? {
            Some(self.must_parse_query_clause_distinct()?)
        } else {
            None
        };

        let columns = self.must_parse_expr_list()?;

        // after <columns>, every clause starts with a keyword
        let from_clause = self.try_parse_query_clause_from()?;
        let join_clause = self.try_parse_query_clause_join()?;
        let where_clause = self.try_parse_query_clause_where()?;
        let group_by_clause = self.try_parse_query_clause_group_by()?;
        let having_clause = self.try_parse_query_clause_having()?;
        let order_by_clause = self.try_parse_query_clause_order_by()?;
        let limit_clause = self.try_parse_query_clause_limit()?;

        Ok(QueryBody::new(
            with_clause,
            distinct_clause,
            columns,
            from_clause,
            join_clause,
            where_clause,
            group_by_clause,
            having_clause,
            order_by_clause,
            limit_clause,
        ))
    }

    fn must_parse_query_clause_with(&mut self) -> Result<WithClause, ParseError> {
        let mut list = vec![];

        comma_separated!(self, {
            let expr = self.must_parse_expr()?;
            if !matches!(expr, Expr::Alias(_)) {
                // non-alias exprs are not allowed
                // use `must_parse_keyword` to emit error
                self.must_parse_keyword(AS)?;
                never!()
            }
            list.push(expr);
        });

        Ok(WithClause::new(list))
    }

    fn must_parse_query_clause_distinct(&mut self) -> Result<DistinctClause, ParseError> {
        let columns = if self.try_parse_keyword(ON)? {
            next_expect!(self, LParen);
            let exprs = self.must_parse_expr_list()?;
            next_expect!(self, RParen);
            Some(exprs)
        } else {
            None
        };

        Ok(DistinctClause::new(columns))
    }

    fn try_parse_query_clause_from(&mut self) -> Result<Option<FromClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, FROM) {
            self.consume_peeked();
            Ok(Some(FromClause::new(self.must_parse_expr()?)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_query_clause_join(&mut self) -> Result<Option<JoinClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        let join_type = if test_keyword!(token_str, INNER) {
            self.consume_peeked();
            JoinType::Inner
        } else if test_keyword!(token_str, FULL) {
            self.consume_peeked();
            self.try_parse_keyword(OUTER)?;
            JoinType::FullOuter
        } else if test_keyword!(token_str, LEFT) {
            self.consume_peeked();
            match self.must_parse_one_of_keywords(&[OUTER, SEMI, ANTI])? {
                0 => JoinType::LeftOuter,
                1 => JoinType::LeftSemi,
                2 => JoinType::LeftAnti,
                _ => never!(),
            }
        } else if test_keyword!(token_str, RIGHT) {
            self.consume_peeked();
            match self.must_parse_one_of_keywords(&[OUTER, SEMI, ANTI])? {
                0 => JoinType::RightOuter,
                1 => JoinType::RightSemi,
                2 => JoinType::RightAnti,
                _ => never!(),
            }
        } else if test_keyword!(token_str, JOIN) {
            JoinType::Inner
        } else {
            return Ok(None);
        };

        self.must_parse_keyword(JOIN)?;

        let source = self.must_parse_expr()?;

        let join_condition = match self.must_parse_one_of_keywords(&[ON, USING])? {
            0 => JoinCondition::On(Box::new(self.must_parse_expr()?)),
            1 => {
                next_expect!(self, LParen);
                let mut columns = vec![];
                comma_separated!(self, columns.push(self.must_parse_identifier()?));
                next_expect!(self, RParen);
                JoinCondition::Using(columns)
            }
            _ => never!(),
        };
        Ok(Some(JoinClause::new(join_type, source, join_condition)))
    }

    fn try_parse_query_clause_where(&mut self) -> Result<Option<WhereClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, WHERE) {
            self.consume_peeked();
            Ok(Some(WhereClause::new(self.must_parse_expr()?)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_query_clause_group_by(&mut self) -> Result<Option<GroupByClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, GROUP) {
            self.consume_peeked();
            self.must_parse_keyword(BY)?;
            Ok(Some(GroupByClause::new(self.must_parse_expr_list()?)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_query_clause_having(&mut self) -> Result<Option<HavingClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, HAVING) {
            self.consume_peeked();
            Ok(Some(HavingClause::new(self.must_parse_expr()?)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_query_clause_order_by(&mut self) -> Result<Option<OrderByClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, ORDER) {
            self.consume_peeked();
            self.must_parse_keyword(BY)?;
            Ok(Some(OrderByClause::new(self.must_parse_expr_list()?)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_query_clause_limit(&mut self) -> Result<Option<LimitClause>, ParseError> {
        let token = peek_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if !test_keyword!(token_str, LIMIT) {
            return Ok(None);
        }
        self.consume_peeked();
        let first = self.must_parse_integer_literal()?;
        let token = self.peek()?;
        let (size, offset) = match token.t {
            TokenType::Comma => {
                self.consume_peeked();
                // limit o, n
                let second = self.must_parse_integer_literal()?;
                (second, first)
            }
            TokenType::KeywordOrIdentifier => {
                let s = self.token_str(&token);
                if test_keyword!(s, OFFSET) {
                    // limit n offset o
                    self.consume_peeked();
                    let second = self.must_parse_integer_literal()?;
                    (first, second)
                } else {
                    (first, 0)
                }
            }
            // limit n
            _ => (first, 0),
        };
        let with_ties = if self.try_parse_keyword(WITH)? {
            self.must_parse_keyword(TIES)?;
            true
        } else {
            false
        };

        Ok(Some(LimitClause::new(size, offset, with_ties)))
    }
}

impl Parser<'_> {
    fn try_parse_insert_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, INSERT) {
            return Ok(None);
        }
        self.must_parse_keyword(INTO)?;
        let table_name = self.must_parse_identifier_string()?;
        let columns = if next_if!(self, LParen) {
            // (<columns>)
            let mut list = vec![];
            comma_separated!(
                self,
                list.push(self.must_parse_identifier_string()?.to_owned())
            );
            next_expect!(self, RParen);
            Some(list)
        } else {
            None
        };

        let report_token = self.peek()?;
        let source = match self.must_parse_one_of_keywords(&[VALUES, FROM, SELECT, WITH])? {
            // values
            0 => self.must_parse_insert_rows()?,
            // from function
            1 => InsertSource::FnCall(match self.must_parse_expr()? {
                Expr::FnCall(fn_call) => fn_call,
                _ => {
                    return emit_error!(ParseFail {
                        pos: self.token_pos(&report_token)
                    });
                }
            }),
            2 | 3 => InsertSource::Subquery(self.must_parse_subquery()?),
            _ => never!(),
        };

        Ok(Some(
            InsertStmt::new(table_name.to_owned(), columns, source).into(),
        ))
    }

    fn must_parse_insert_rows(&mut self) -> Result<InsertSource, ParseError> {
        // first tuple
        next_expect!(self, LParen);
        let mut data = vec![];
        let mut column_size = 0;
        comma_separated!(self, {
            data.push(self.must_parse_expr()?);
            column_size += 1;
        });
        next_expect!(self, RParen);
        // if have more tuples
        if next_if!(self, Comma) {
            // for (data,...), ...
            comma_separated!(self, {
                next_expect!(self, LParen);
                let mut this_size = 0;
                // for data,...
                comma_separated!(self, {
                    data.push(self.must_parse_expr()?);
                    this_size += 1;
                });
                if this_size != column_size {
                    let report_token = self.peek()?;
                    return emit_error!(Conflicts {
                        this: format!("value length {}", this_size),
                        that: format!("previous value length {}", column_size),
                        pos: self.token_pos(&report_token)
                    });
                }
                next_expect!(self, RParen);
            });
        }

        Ok(InsertSource::Rows { column_size, data })
    }
}

impl Parser<'_> {
    fn try_parse_explain_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, EXPLAIN) {
            return Ok(None);
        }

        let query = self.must_parse_subquery()?;

        Ok(Some(ExplainStmt::new(query).into()))
    }
}

impl Parser<'_> {
    fn try_parse_create_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, CREATE) {
            return Ok(None);
        }

        match self.must_parse_one_of_keywords(&[TABLE, VIEW])? {
            0 => self.must_parse_create_table_stmt(),
            1 => self.must_parse_create_view_stmt(),
            _ => never!(),
        }
    }

    fn must_parse_create_table_stmt(&mut self) -> Result<Option<Statement>, ParseError> {
        // parse optional [if exists]
        let if_not_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keywords(&[NOT, EXISTS])?;
            true
        } else {
            false
        };

        // parse <table_name>
        let table_name = self.must_parse_identifier_string()?;

        // parse (
        next_expect!(self, LParen);
        let mut columns = vec![];
        let mut indexes = vec![];
        let mut constraints = vec![];
        comma_separated!(
            self,
            if self.try_parse_keyword(INDEX)? {
                indexes.push(self.must_parse_index_def()?);
            } else if self.try_parse_keyword(CONSTRAINT)? {
                constraints.push(self.must_parse_constraint_def()?);
            } else {
                columns.push(self.must_parse_column_def()?)
            }
        );

        // parse )
        next_expect!(self, RParen);

        let mut stmt = CreateTableStmt::new(
            table_name.to_owned(),
            if_not_exists,
            columns,
            constraints,
            indexes,
            None,
            None,
            None,
            None,
        );

        // parse optional attrs
        loop {
            let token = self.peek()?;
            if !token.maybe_keyword() {
                break;
            }
            match self.must_parse_one_of_keywords(&[PRIMARY, ORDER, PARTITION, COMMENT])? {
                0 => {
                    if stmt.primary_key.is_some() {
                        return emit_error!(Conflicts {
                            this: "primary key".to_owned(),
                            that: "primary key".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(KEY)?;
                    let keys = self.must_parse_expr_list()?;
                    stmt.primary_key = Some(keys);
                }
                1 => {
                    if stmt.order_by.is_some() {
                        return emit_error!(Conflicts {
                            this: "order by".to_owned(),
                            that: "order by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    let keys = self.must_parse_expr_list()?;
                    stmt.order_by = Some(keys);
                }
                2 => {
                    if stmt.partition_by.is_some() {
                        return emit_error!(Conflicts {
                            this: "partition by".to_owned(),
                            that: "partition by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    let expr = self.must_parse_expr()?;
                    stmt.partition_by = Some(expr);
                }
                3 => {
                    if stmt.comment.is_some() {
                        return emit_error!(Conflicts {
                            this: COMMENT.to_owned(),
                            that: COMMENT.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    let comment = self.must_parse_string_literal()?;
                    stmt.comment = Some(comment);
                }
                _ => never!(),
            }
        }

        Ok(Some(stmt.into()))
    }

    fn must_parse_create_view_stmt(&mut self) -> Result<Option<Statement>, ParseError> {
        // parse optional [if exists]
        let if_not_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keywords(&[NOT, EXISTS])?;
            true
        } else {
            false
        };

        // parse <table_name>
        let view_name = self.must_parse_identifier_string()?;

        let mut strategy = None;
        let mut primary_key = None;
        let mut order_by = None;
        let mut partition_by = None;
        let mut comment = None;

        // parse optional attrs
        loop {
            let token = self.peek()?;
            match self
                .must_parse_one_of_keywords(&[AS, UPDATE, PRIMARY, ORDER, PARTITION, COMMENT])?
            {
                0 => {
                    // must have strategy
                    if strategy.is_none() {
                        return emit_error!(NotExpectedKeywords {
                            expected: vec![UPDATE.to_owned()],
                            actual: AS.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    break;
                }
                1 => {
                    if strategy.is_some() {
                        return emit_error!(Conflicts {
                            this: "update by".to_owned(),
                            that: "update by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    strategy = Some(self.must_parse_identifier_string()?.to_owned());
                }
                2 => {
                    if primary_key.is_some() {
                        return emit_error!(Conflicts {
                            this: "primary key".to_owned(),
                            that: "primary key".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(KEY)?;
                    let key = self.must_parse_expr_list()?;
                    primary_key = Some(key);
                }
                3 => {
                    if order_by.is_some() {
                        return emit_error!(Conflicts {
                            this: "order by".to_owned(),
                            that: "order by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    // paren is optional
                    let key = self.must_parse_expr_list()?;
                    order_by = Some(key);
                }
                4 => {
                    if partition_by.is_some() {
                        return emit_error!(Conflicts {
                            this: "partition by".to_owned(),
                            that: "partition by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    let expr = self.must_parse_expr()?;
                    partition_by = Some(expr);
                }
                5 => {
                    if comment.is_some() {
                        return emit_error!(Conflicts {
                            this: COMMENT.to_owned(),
                            that: COMMENT.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    comment = Some(self.must_parse_string_literal()?);
                }
                _ => never!(),
            }
        }

        // parse subquery
        let query = self.must_parse_subquery()?;

        Ok(Some(
            CreateViewStmt::new(
                view_name.to_owned(),
                if_not_exists,
                // strategy must be Some
                unsafe { strategy.unwrap_unchecked() },
                primary_key,
                order_by,
                partition_by,
                query,
                comment,
            )
            .into(),
        ))
    }

    fn must_parse_constraint_def(&mut self) -> Result<ConstraintDefinition, ParseError> {
        let name = self.must_parse_identifier_string()?;
        self.must_parse_keyword(CHECK)?;
        let expr = self.must_parse_expr()?;
        Ok(ConstraintDefinition::new(name.to_owned(), expr))
    }

    fn must_parse_index_def(&mut self) -> Result<IndexDefinition, ParseError> {
        let name = self.must_parse_identifier_string()?;
        let report_token = self.peek()?;
        let fn_call = match self.must_parse_expr()? {
            Expr::FnCall(f) => f,
            _ => {
                return emit_error!(ParseFail {
                    pos: self.token_pos(&report_token)
                });
            }
        };

        Ok(IndexDefinition::new(name.to_owned(), fn_call))
    }

    fn must_parse_column_def(&mut self) -> Result<ColumnDefinition, ParseError> {
        let name = self.must_parse_identifier_string()?;
        let datatype = self.must_parse_datatype()?;
        let mut def = ColumnDefinition::new(name.to_owned(), datatype, None, None);

        loop {
            let token = self.peek()?;
            if !token.maybe_keyword() {
                break;
            }
            match self.must_parse_one_of_keywords(&[DEFAULT, COMMENT])? {
                0 => {
                    if def.default.is_some() {
                        return emit_error!(Conflicts {
                            this: DEFAULT.to_owned(),
                            that: DEFAULT.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    def.default = Some(self.must_parse_expr()?);
                }
                1 => {
                    if def.comment.is_some() {
                        return emit_error!(Conflicts {
                            this: COMMENT.to_owned(),
                            that: COMMENT.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    def.comment = Some(self.must_parse_string_literal()?);
                }
                _ => never!(),
            }
        }

        Ok(def)
    }
}

impl Parser<'_> {
    fn try_parse_alter_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn try_parse_describe_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, DESCRIBE) {
            return Ok(None);
        }

        let stmt = match self.must_parse_one_of_keywords(&[TABLE, VIEW, DATABASE])? {
            0 => DescribeStmt::Table {
                name: self.must_parse_identifier_string()?.to_owned(),
            },
            1 => DescribeStmt::View {
                name: self.must_parse_identifier_string()?.to_owned(),
            },
            2 => DescribeStmt::Database,
            _ => never!(),
        };

        Ok(Some(stmt.into()))
    }
}

impl Parser<'_> {
    fn try_parse_drop_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, DROP) {
            return Ok(None);
        }

        // parse TABLE|VIEW
        let entity_type = match self.must_parse_one_of_keywords(&[TABLE, VIEW])? {
            0 => DatabaseEntity::Table,
            1 => DatabaseEntity::View,
            _ => never!(),
        };

        // parse optional [if exists]
        let if_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keyword(EXISTS)?;
            true
        } else {
            false
        };

        // parse <name>
        let name = self.must_parse_identifier_string()?;

        Ok(Some(
            DropStmt::new(entity_type, if_exists, name.to_owned()).into(),
        ))
    }
}

impl Parser<'_> {
    fn try_parse_truncate_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, TRUNCATE) {
            return Ok(None);
        }

        // parse TABLE|VIEW
        let entity_type = match self.must_parse_one_of_keywords(&[TABLE, VIEW])? {
            0 => DatabaseEntity::Table,
            1 => DatabaseEntity::View,
            _ => never!(),
        };

        // parse optional [if exists]
        let if_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keyword(EXISTS)?;
            true
        } else {
            false
        };

        // parse <name>
        let name = self.must_parse_identifier_string()?;

        Ok(Some(
            TruncateStmt::new(entity_type, if_exists, name.to_owned()).into(),
        ))
    }
}

impl Parser<'_> {
    /// `optimize table <name> [on partition <part>]`
    fn try_parse_optimize_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, OPTIMIZE) {
            return Ok(None);
        }

        // parse TABLE
        self.must_parse_keyword(TABLE)?;

        // parse <name>
        let table_name = self.must_parse_identifier_string()?;

        if self.peek()?.is_terminator() {
            return Ok(Some(OptimizeStmt::new(table_name.to_owned(), None).into()));
        }

        // parse optional [on partition <partition>]
        self.must_parse_keywords(&[ON, PARTITION])?;

        // parse <part>
        let part = self.must_parse_expr()?;

        Ok(Some(
            OptimizeStmt::new(table_name.to_owned(), Some(part)).into(),
        ))
    }
}

impl Parser<'_> {
    /// `set <config> = <value>`
    fn try_parse_set_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if !test_keyword!(leading_keyword, SET) {
            return Ok(None);
        }

        // parse `<config>` identifier
        let token = next_expect!(self, ConfigIdentifier);
        let config_name = self.token_str(&token);

        // parse `=`
        next_expect!(self, Eq);

        // parse `<value>`
        let expr = self.must_parse_expr()?;

        Ok(Some(SetStmt::new(config_name.to_owned(), expr).into()))
    }
}

impl Parser<'_> {
    fn must_parse_expr_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut res = vec![];
        comma_separated!(self, res.push(self.must_parse_expr()?));
        Ok(res)
    }

    fn must_parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.must_parse_expr_tdop(TokenPower::Terminator)
    }

    fn must_parse_expr_tdop(&mut self, power: TokenPower) -> Result<Expr, ParseError> {
        let mut expr = self.must_parse_expr_prefix()?;
        loop {
            let token = self.peek()?;
            let next_power = self.token_power(&token);
            if next_power <= power {
                return Ok(expr);
            }
            expr = self.must_parse_expr_infix(expr, next_power)?;
        }
    }

    fn must_parse_expr_prefix(&mut self) -> Result<Expr, ParseError> {
        use TokenType::*;

        let token = self.next()?;
        let s = self.token_str(&token);

        let expr = match token.t {
            LParen => {
                // tuple or subquery or just a wrapper
                let token = self.peek()?;
                let s = self.token_str(&token);
                let e = if token.maybe_keyword() && test_keywords!(s, [SELECT, WITH]) {
                    self.must_parse_subquery()?.into()
                } else {
                    let exprs = self.must_parse_expr_list()?;
                    match exprs.len() {
                        // parse_expr will throw errors if no expr found
                        0 => never!(),
                        1 => unsafe { exprs.into_iter().next().unwrap_unchecked() },
                        _ => Collection::new(CollectionType::Tuple, exprs).into(),
                    }
                };
                next_expect!(self, RParen);
                e
            }
            LBracket => {
                // array literal
                let e = Collection::new(CollectionType::Array, self.must_parse_expr_list()?).into();
                next_expect!(self, RBracket);
                e
            }
            LBrace => {
                // map literal
                let e = Collection::new(CollectionType::Map, self.must_parse_map()?).into();
                next_expect!(self, RBrace);
                e
            }
            Minus => {
                let e = self.must_parse_expr_prefix()?;
                UnaryOp::new(UnaryOperator::Neg, Box::new(e)).into()
            }
            Plus => self.must_parse_expr_prefix()?,
            BitNot => {
                let e = self.must_parse_expr_prefix()?;
                UnaryOp::new(UnaryOperator::BitwiseNot, Box::new(e)).into()
            }
            RawStringLiteral => Literal::String(s.to_owned()).into(),
            EscapedSQStringLiteral => Literal::String(unescape_single_quoted_string(s)?).into(),
            EscapedDQStringLiteral => Literal::String(unescape_double_quoted_string(s)?).into(),
            FloatLiteral => Literal::Float(Box::new(literal::decimal_from_str!(s))).into(),
            HexLiteral => Literal::Integer(literal::integer_from_str!(hex, u128, s)).into(),
            IntegerLiteral => Literal::Integer(literal::integer_from_str!(u128, s)).into(),
            KeywordOrIdentifier => {
                // maybe booleans, null, identifiers,  fn name.
                if test_keyword!(s, TRUE) {
                    Literal::Boolean(true).into()
                } else if test_keyword!(s, FALSE) {
                    Literal::Boolean(false).into()
                } else if test_keyword!(s, NULL) {
                    Literal::Null.into()
                } else if test_keyword!(s, NOT) {
                    let e = self.must_parse_expr_prefix()?;
                    UnaryOp::new(UnaryOperator::Not, Box::new(e)).into()
                } else if test_keyword!(s, INTERVAL) {
                    self.must_parse_interval()?.into()
                } else if test_keyword!(s, IF) {
                    self.must_parse_if_body()?.into()
                } else if test_keyword!(s, CASE) {
                    self.must_parse_case_when_body()?.into()
                } else {
                    let id = self.must_parse_identifier_based_prefix(s)?;
                    match self.try_parse_fn_call_args()? {
                        Some(args) => FnCall::new(FnName::Others(id), args).into(),
                        None => id.into(),
                    }
                }
            }
            DelimitedIdentifier => self.must_parse_identifier_based_prefix(s)?.into(),
            QueryParameter => ast::QueryParameter::new(self.must_parse_integer_literal()?).into(),
            _ => {
                return emit_error!(NotExpectedTokenTypes {
                    expected: vec![
                        // string literals
                        RawStringLiteral,
                        EscapedSQStringLiteral,
                        EscapedDQStringLiteral,
                        // float literals
                        FloatLiteral,
                        // integer literal
                        HexLiteral,
                        IntegerLiteral,
                        // query parameter
                        QueryParameter,
                        // identifier, function name
                        KeywordOrIdentifier,
                        DelimitedIdentifier,
                        // opening parens
                        LParen,   // subquery, sub expr, expr list (tuple)
                        LBracket, // array literal
                        LBrace,   // map literal
                        // unary operator
                        Minus,
                        Plus,
                        BitNot
                    ],
                    actual: token.t,
                    pos: self.token_pos(&token)
                });
            }
        };

        Ok(expr)
    }

    fn must_parse_expr_infix(
        &mut self,
        left: Expr,
        this_power: TokenPower,
    ) -> Result<Expr, ParseError> {
        use TokenType::*;

        macro_rules! emit_binary_op {
            ($self:ident, $op:ident, $left:expr, $power:expr) => {
                BinaryOp::new(
                    BinaryOperator::$op,
                    Box::new($left),
                    Box::new($self.must_parse_expr_tdop($power)?),
                )
                .into()
            };
            ($op:ident, $left:expr, $right:expr) => {
                BinaryOp::new(BinaryOperator::$op, Box::new($left), Box::new($right)).into()
            };
        }

        macro_rules! emit_unary_op {
            ($op:ident, $left:expr) => {
                UnaryOp::new(UnaryOperator::$op, Box::new($left)).into()
            };
        }

        let token = self.next()?;
        let expr = match token.t {
            Plus => emit_binary_op!(self, Plus, left, this_power),
            Minus => emit_binary_op!(self, Minus, left, this_power),
            Mul => emit_binary_op!(self, Multi, left, this_power),
            Div => emit_binary_op!(self, Div, left, this_power),
            Mod => emit_binary_op!(self, Mod, left, this_power),
            Gt => emit_binary_op!(self, Gt, left, this_power),
            Lt => emit_binary_op!(self, Lt, left, this_power),
            GtEq => emit_binary_op!(self, GtEq, left, this_power),
            LtEq => emit_binary_op!(self, LtEq, left, this_power),
            Eq => emit_binary_op!(self, Eq, left, this_power),
            NotEq => emit_binary_op!(self, NotEq, left, this_power),
            BitOr => emit_binary_op!(self, BitwiseOr, left, this_power),
            BitAnd => emit_binary_op!(self, BitwiseAnd, left, this_power),
            BitXor => emit_binary_op!(self, BitwiseXor, left, this_power),
            BitLShift => emit_binary_op!(self, BitwiseLeftShift, left, this_power),
            BitRShift => emit_binary_op!(self, BitwiseRightShift, left, this_power),
            LBracket => {
                let e = self.must_parse_expr()?;
                next_expect!(self, RBracket);
                emit_binary_op!(IndexAccess, left, e)
            }
            KeywordOrIdentifier => {
                match self.must_parse_one_of_keywords(&[NOT, IS, IN, LIKE, ILIKE, BETWEEN, AS])? {
                    0 => match self.must_parse_one_of_keywords(&[IN, LIKE, ILIKE, BETWEEN])? {
                        0 => emit_binary_op!(self, NotIn, left, this_power),
                        1 => emit_binary_op!(self, NotLike, left, this_power),
                        2 => emit_binary_op!(self, NotILike, left, this_power),
                        3 => {
                            let min = self.must_parse_expr_tdop(TokenPower::Between)?;
                            self.must_parse_keyword(AND)?;
                            let max = self.must_parse_expr_tdop(TokenPower::Between)?;
                            FnCall::new(FnName::NotBetween, vec![left, min, max]).into()
                        }
                        _ => never!(),
                    },
                    1 => match self.must_parse_one_of_keywords(&[NOT, NULL])? {
                        0 => {
                            self.must_parse_keyword(NULL)?;
                            emit_unary_op!(IsNotNull, left)
                        }
                        1 => emit_unary_op!(IsNull, left),
                        _ => never!(),
                    },
                    2 => emit_binary_op!(self, In, left, this_power),
                    3 => emit_binary_op!(self, Like, left, this_power),
                    4 => emit_binary_op!(self, ILike, left, this_power),
                    5 => {
                        let min = self.must_parse_expr_tdop(TokenPower::Between)?;
                        self.must_parse_keyword(AND)?;
                        let max = self.must_parse_expr_tdop(TokenPower::Between)?;
                        FnCall::new(FnName::Between, vec![left, min, max]).into()
                    }
                    6 => {
                        let report_token = self.peek()?;
                        let alias = self.must_parse_identifier_string()?;
                        // nested alias like `1 as a as b` is not allowed
                        match &left {
                            Expr::Alias(prev_alias) => {
                                return emit_error!(Conflicts {
                                    this: alias.to_owned(),
                                    that: prev_alias.alias.clone(),
                                    pos: self.token_pos(&report_token)
                                });
                            }
                            _ => Alias::new(Box::new(left), alias.to_owned()).into(),
                        }
                    }
                    _ => never!(),
                }
            }
            _ => unreachable!(), // leave check here
        };

        Ok(expr)
    }

    /// caller should ensure the first token is 'interval' keyword
    fn must_parse_interval(&mut self) -> Result<Literal, ParseError> {
        // parse INTERVAL
        next_expect!(self, KeywordOrIdentifier);
        // parse number
        let n = self.must_parse_integer_literal()?;
        let unit =
            match self.must_parse_one_of_keywords(&[SECOND, MINUTE, HOUR, DAY, MONTH, YEAR])? {
                0 => IntervalUnit::Second,
                1 => IntervalUnit::Minute,
                2 => IntervalUnit::Hour,
                3 => IntervalUnit::Day,
                4 => IntervalUnit::Month,
                5 => IntervalUnit::Year,
                _ => never!(),
            };
        Ok(Literal::Interval(n, unit))
    }

    /// the prefix is the str of DelimitedIdentifier or KeywordOrIdentifier
    fn must_parse_identifier_based_prefix(
        &mut self,
        prefix: &str,
    ) -> Result<Identifier, ParseError> {
        // only support up to one qualifier
        let id = if next_if!(self, Dot) {
            let token = next_expect!(self, [DelimitedIdentifier, KeywordOrIdentifier]);
            Identifier::new(self.token_str(&token).to_owned(), Some(prefix.to_owned()))
        } else {
            Identifier::new(prefix.to_owned(), None)
        };

        Ok(id)
    }

    fn must_parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = next_expect!(self, [DelimitedIdentifier, KeywordOrIdentifier]);
        let prefix = self.token_str(&token);
        self.must_parse_identifier_based_prefix(prefix)
    }

    fn try_parse_fn_call_args(&mut self) -> Result<Option<Vec<Expr>>, ParseError> {
        if next_if!(self, LParen) {
            if next_if!(self, RParen) {
                // empty args
                return Ok(Some(vec![]));
            }
            let args = self.must_parse_expr_list()?;
            next_expect!(self, RParen);
            return Ok(Some(args));
        }
        Ok(None)
    }

    fn must_parse_map(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut res = vec![];
        comma_separated!(self, {
            // key
            res.push(self.must_parse_expr()?);
            next_expect!(self, Colon);
            // value
            res.push(self.must_parse_expr()?);
        });
        Ok(res)
    }

    /// assume `IF` is consumed
    fn must_parse_if_body(&mut self) -> Result<FnCall, ParseError> {
        let scrutinee = self.must_parse_expr()?;
        self.must_parse_keyword(THEN)?;
        let branch = self.must_parse_expr()?;
        self.must_parse_keyword(ELSE)?;
        let else_branch = self.must_parse_expr()?;
        self.must_parse_keyword(END)?;
        Ok(FnCall::new(
            FnName::If,
            vec![scrutinee, branch, else_branch],
        ))
    }

    /// assume `CASE` is consumed
    fn must_parse_case_when_body(&mut self) -> Result<FnCall, ParseError> {
        let mut args = vec![];
        // optional scrutinee
        let fn_name = if self.try_parse_keyword(WHEN)? {
            FnName::MultiIf
        } else {
            args.push(self.must_parse_expr()?);
            self.must_parse_keyword(WHEN)?;
            FnName::CaseWhen
        };
        // branches
        loop {
            args.push(self.must_parse_expr()?);
            self.must_parse_keyword(THEN)?;
            args.push(self.must_parse_expr()?);

            match self.must_parse_one_of_keywords(&[WHEN, ELSE, END])? {
                0 => continue,
                1 => {
                    args.push(self.must_parse_expr()?);
                    self.must_parse_keyword(END)?;
                    break;
                }
                2 => {
                    // else branch is null by default
                    args.push(Literal::Null.into());
                    break;
                }
                _ => never!(),
            }
        }

        Ok(FnCall::new(fn_name, args))
    }
}

impl<'a> Parser<'a> {
    fn try_parse_keyword(&mut self, keyword: &'static str) -> Result<bool, ParseError> {
        let token = self.peek()?;
        if !token.maybe_keyword() {
            return Ok(false);
        }
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, keyword) {
            self.consume_peeked();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn must_parse_keyword(&mut self, keyword: &'static str) -> Result<(), ParseError> {
        let token = next_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        if test_keyword!(token_str, keyword) {
            Ok(())
        } else {
            emit_error!(NotExpectedKeywords {
                expected: vec![keyword.to_owned()],
                actual: token_str.to_owned(),
                pos: self.token_pos(&token),
            })
        }
    }

    fn must_parse_one_of_keywords(&mut self, keywords: &[&'static str]) -> Result<u8, ParseError> {
        let token = next_expect!(self, KeywordOrIdentifier);
        let token_str = self.token_str(&token);
        for (i, &keyword) in keywords.iter().enumerate() {
            if test_keyword!(token_str, keyword) {
                return Ok(i as u8);
            }
        }
        emit_error!(NotExpectedKeywords {
            expected: keywords.iter().map(|&s| s.to_owned()).collect(),
            actual: token_str.to_owned(),
            pos: self.token_pos(&token),
        })
    }

    fn must_parse_keywords(&mut self, keywords: &[&'static str]) -> Result<(), ParseError> {
        for &keyword in keywords {
            let token = next_expect!(self, KeywordOrIdentifier);
            let token_str = self.token_str(&token);
            if !test_keyword!(token_str, keyword) {
                return emit_error!(NotExpectedKeywords {
                    expected: vec![keyword.to_owned()],
                    actual: token_str.to_owned(),
                    pos: self.token_pos(&token),
                });
            }
        }
        Ok(())
    }

    /// include KeywordOrIdentifier and DelimitedIdentifier
    #[inline(always)]
    fn must_parse_identifier_string(&mut self) -> Result<&'a str, ParseError> {
        let token = next_expect!(self, [KeywordOrIdentifier, DelimitedIdentifier]);
        let token_str = self.token_str(&token);
        Ok(token_str)
    }

    fn must_parse_datatype(&mut self) -> Result<DataType, ParseError> {
        let datatype = match self.must_parse_one_of_keywords(&[
            // 0 - 4 int_x
            INT8, INT16, INT32, INT64, INT128, // 5 - 9 uint_x
            UINT8, UINT16, UINT32, UINT64, UINT128, // 10 - 12 serial_x
            SERIAL32, SERIAL64, SERIAL128, // 13 - 15 userial_x
            USERIAL32, USERIAL64, USERIAL128, // 16 - 17 decimal_x
            DECIMAL32, DECIMAL64, // 18 - 19 float_x
            FLOAT32, FLOAT64, // 20 boolean
            BOOLEAN, // 21 - 22 strings
            CHARS, STRING, // 23 uuid
            UUID,   // 24 - 25 times
            DATE, DATETIME, // 26 - 29 collections
            ARRAY, ENUM, TUPLE, MAP, // 30 - 31 special mark type
            DICTIONARY, NULLABLE,
        ])? {
            0 => ScalarDataType::Int8.into(),
            1 => ScalarDataType::Int16.into(),
            2 => ScalarDataType::Int32.into(),
            3 => ScalarDataType::Int64.into(),
            4 => ScalarDataType::Int128.into(),
            5 => ScalarDataType::UInt8.into(),
            6 => ScalarDataType::UInt16.into(),
            7 => ScalarDataType::UInt32.into(),
            8 => ScalarDataType::UInt64.into(),
            9 => ScalarDataType::UInt128.into(),
            10 => ScalarDataType::Serial32.into(),
            11 => ScalarDataType::Serial64.into(),
            12 => ScalarDataType::Serial128.into(),
            13 => ScalarDataType::USerial32.into(),
            14 => ScalarDataType::USerial64.into(),
            15 => ScalarDataType::USerial128.into(),
            16 => {
                next_expect!(self, LParen);
                let scale = self.must_parse_integer_literal()?;
                next_expect!(self, RParen);
                ScalarDataType::Decimal32 { scale }.into()
            }
            17 => {
                next_expect!(self, LParen);
                let scale = self.must_parse_integer_literal()?;
                next_expect!(self, RParen);
                ScalarDataType::Decimal64 { scale }.into()
            }
            18 => ScalarDataType::Float32.into(),
            19 => ScalarDataType::Float64.into(),
            20 => ScalarDataType::Boolean.into(),
            21 => {
                next_expect!(self, LParen);
                let length = self.must_parse_integer_literal()?;
                next_expect!(self, RParen);
                ScalarDataType::Chars { length }.into()
            }
            22 => {
                let max_length = if self.peek()?.t == TokenType::LParen {
                    next_expect!(self, LParen);
                    let l = self.must_parse_integer_literal()?;
                    next_expect!(self, RParen);
                    l
                } else {
                    0
                };
                ScalarDataType::String { max_length }.into()
            }
            23 => ScalarDataType::Uuid.into(),
            24 => ScalarDataType::Date.into(),
            25 => ScalarDataType::Datetime.into(),
            26 => {
                next_expect!(self, LParen);
                let datatype = self.must_parse_datatype()?;
                next_expect!(self, RParen);
                CompoundDataType::Array(Box::new(datatype)).into()
            }
            27 => {
                next_expect!(self, LParen);
                let binds = self.must_parse_enum_binds()?;
                next_expect!(self, RParen);
                CompoundDataType::Enum(binds).into()
            }
            28 => {
                next_expect!(self, LParen);
                let mut types = vec![];
                comma_separated!(self, types.push(self.must_parse_datatype()?));
                next_expect!(self, RParen);
                CompoundDataType::Tuple(types).into()
            }
            29 => {
                next_expect!(self, LParen);
                let key = self.must_parse_datatype()?;
                next_expect!(self, Comma);
                let value = self.must_parse_datatype()?;
                next_expect!(self, RParen);
                CompoundDataType::Map(Box::new(value), Box::new(key)).into()
            }
            30 => {
                next_expect!(self, LParen);
                let datatype = self.must_parse_datatype()?;
                next_expect!(self, RParen);
                CompoundDataType::Dictionary(Box::new(datatype)).into()
            }
            31 => {
                next_expect!(self, LParen);
                let datatype = self.must_parse_datatype()?;
                next_expect!(self, RParen);
                CompoundDataType::Nullable(Box::new(datatype)).into()
            }
            _ => never!(),
        };
        Ok(datatype)
    }

    fn must_parse_enum_binds(&mut self) -> Result<Vec<EnumBind>, ParseError> {
        let mut id = 0usize;
        let mut res = vec![];
        comma_separated!(self, {
            let literal = self.must_parse_string_literal()?;
            id = if next_if!(self, Eq) {
                self.must_parse_integer_literal()?
            } else {
                id
            };
            res.push(EnumBind::new(id, literal));
            id += 1;
        });
        Ok(res)
    }

    fn must_parse_integer_literal<T: Integer>(&mut self) -> Result<T, ParseError> {
        let token = next_expect!(self, [IntegerLiteral, HexLiteral]);
        let token_str = self.token_str(&token);
        let res = match token.t {
            TokenType::IntegerLiteral => literal::integer_from_str!(T, token_str),
            TokenType::HexLiteral => literal::integer_from_str!(hex, T, token_str),
            _ => never!(),
        };
        Ok(res)
    }

    fn must_parse_float_literal(&mut self) -> Result<BigDecimal, ParseError> {
        let token = next_expect!(self, FloatLiteral);
        let token_str = self.token_str(&token);
        let res = literal::decimal_from_str!(token_str);
        Ok(res)
    }

    fn must_parse_string_literal(&mut self) -> Result<String, ParseError> {
        let token = next_expect!(
            self,
            [
                RawStringLiteral,
                EscapedSQStringLiteral,
                EscapedDQStringLiteral
            ]
        );
        let s = self.token_str(&token);
        Ok(match token.t {
            TokenType::RawStringLiteral => s.to_owned(),
            TokenType::EscapedSQStringLiteral => unescape_single_quoted_string(s)?,
            TokenType::EscapedDQStringLiteral => unescape_double_quoted_string(s)?,
            _ => never!(),
        })
    }
}

impl<'a> Parser<'a> {
    fn peek(&mut self) -> Result<Token, ParseError> {
        if self.peeked.is_none() {
            loop {
                let token = self.tokenizer.next_token()?;
                if !token.is_whitespace() {
                    self.peeked = Some(token);
                    break;
                }
            }
        }

        Ok(unsafe { self.peeked.as_ref().unwrap_unchecked().clone() })
    }

    #[inline(always)]
    fn consume_peeked(&mut self) {
        self.peeked.take();
    }

    fn next(&mut self) -> Result<Token, ParseError> {
        let token = match self.peeked.take() {
            Some(t) => t,
            None => loop {
                let token = self.tokenizer.next_token()?;
                if !token.is_whitespace() {
                    break token;
                }
            },
        };
        Ok(token)
    }

    #[inline(always)]
    fn token_str(&self, token: &Token) -> &'a str {
        self.tokenizer.source().slice(&token.span)
    }

    #[inline(always)]
    fn token_pos(&self, token: &Token) -> Position {
        self.tokenizer.source().get_pos(token.span.start)
    }

    fn token_power(&self, token: &Token) -> TokenPower {
        use TokenType::*;

        match token.t {
            Eq | NotEq | Lt | LtEq | GtEq | Gt => TokenPower::Comparison,
            BitOr => TokenPower::BitOr,
            BitXor => TokenPower::BitXor,
            BitAnd => TokenPower::BitAnd,
            BitLShift | BitRShift => TokenPower::BitShift,
            Plus | Minus => TokenPower::PlusMinus,
            Mul | Div | Mod => TokenPower::MulDivMod,
            LBracket => TokenPower::Access,
            KeywordOrIdentifier => {
                let s = self.token_str(token);
                if test_keywords!(s, [NOT, IS, IN, LIKE, ILIKE]) {
                    TokenPower::Comparison
                } else if test_keyword!(s, BETWEEN) {
                    TokenPower::Between
                } else if test_keyword!(s, AS) {
                    TokenPower::Alias
                } else {
                    TokenPower::Terminator
                }
            }
            _ => TokenPower::Terminator,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum TokenPower {
    Terminator,
    Or,
    Xor,
    And,
    Not,
    Comparison,
    Between,
    Alias,
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    PlusMinus,
    MulDivMod,
    Access,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum SetOpPower {
    Terminator,
    Except,
    Union,
    Intersect,
}
