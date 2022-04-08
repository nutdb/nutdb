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
mod tokenizer;

type ParseResult = Result<Statement, ParseError>;

pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    peeked: Option<Token>,
}

impl Parser<'_> {
    pub fn parse(sql: &str) -> ParseResult {
        todo!()
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

/// `emit_stmt!(Stmt {...})`
macro_rules! emit_stmt {
    ($stmt:ident $($init:tt)?) => {
        Ok(Some($stmt $($init)?.into()))
    };
    ($stmt:expr) => {
        Ok(Some($stmt.into()))
    }
}

/// ```plain
/// returns_first!(
///     xxx(),
///     yyy()
/// )
/// ```
macro_rules! try_parse_chain {
    ($($fn_call:expr),+) => {
        loop {
            $(
            let res = $fn_call;
            if res.is_some() {
                break res
            })+
            break None
        }
    }
}

/// - `test_keyword!(token_str, keyword)`
macro_rules! test_keyword {
    ($token_str:expr, $keyword:expr) => {
        $token_str.eq_ignore_ascii_case($keyword)
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

impl Parser<'_> {
    fn parse_stmt(&mut self) -> ParseResult {
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

impl Parser<'_> {
    fn try_parse_select_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        if let Some(query) = self.try_parse_subquery(leading_keyword)? {
            emit_stmt!(SelectStmt { query })
        } else {
            Ok(None)
        }
    }

    fn must_parse_subquery(&mut self) -> Result<Query, ParseError> {
        let leading_with = match self.must_parse_one_of_keywords(&[WITH, SELECT])? {
            0 => true,
            1 => false,
            _ => never!(),
        };

        let query = self.must_parse_query(leading_with)?;

        Ok(query)
    }

    fn try_parse_subquery(&mut self, leading_keyword: &str) -> Result<Option<Query>, ParseError> {
        let leading_with = if test_keyword!(leading_keyword, WITH) {
            true
        } else if test_keyword!(leading_keyword, SELECT) {
            false
        } else {
            return Ok(None);
        };

        let query = self.must_parse_query(leading_with)?;

        Ok(Some(query))
    }

    fn must_parse_query(&mut self, leading_with: bool) -> Result<Query, ParseError> {
        todo!()
    }

    fn parse_query_clause_with(&mut self) -> Result<Option<Statement>, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn try_parse_insert_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        todo!()
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

        emit_stmt!(ExplainStmt { query })
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
        loop {
            if self.try_parse_keyword(INDEX)? {
                indexes.push(self.must_parse_index_def()?);
            } else if self.try_parse_keyword(CONSTRAINT)? {
                constraints.push(self.must_parse_constraint_def()?);
            } else {
                columns.push(self.must_parse_column_def()?)
            }
            if !next_if!(self, Comma) {
                break;
            }
        }
        // parse )
        next_expect!(self, RParen);

        let mut stmt = CreateTableStmt {
            if_not_exists,
            name: table_name.to_owned(),
            columns,
            constraints,
            indexes,
            primary_key: None,
            order_by: None,
            partition_by: None,
            comment: None,
        };

        // parse optional attrs
        loop {
            let token = self.peek()?;
            if !token.maybe_keyword() {
                break;
            }
            match self.must_parse_one_of_keywords(&[PRIMARY, ORDER, PARTITION, COMMENT])? {
                0 => {
                    if stmt.primary_key.is_some() {
                        return emit_error!(OptionConflicts {
                            this: "primary key".to_owned(),
                            that: "primary key".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(KEY)?;
                    // paren is optional
                    let has_paren = next_if!(self, LParen);
                    let keys = self.must_parse_expr_list()?;
                    if has_paren {
                        next_expect!(self, RParen);
                    }
                    stmt.primary_key = Some(keys);
                }
                1 => {
                    if stmt.order_by.is_some() {
                        return emit_error!(OptionConflicts {
                            this: "order by".to_owned(),
                            that: "order by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    // paren is optional
                    let has_paren = next_if!(self, LParen);
                    let keys = self.must_parse_expr_list()?;
                    if has_paren {
                        next_expect!(self, RParen);
                    }
                    stmt.order_by = Some(keys);
                }
                2 => {
                    if stmt.partition_by.is_some() {
                        return emit_error!(OptionConflicts {
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
                        return emit_error!(OptionConflicts {
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

        emit_stmt!(stmt)
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
                        return emit_error!(OptionConflicts {
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
                        return emit_error!(OptionConflicts {
                            this: "primary key".to_owned(),
                            that: "primary key".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(KEY)?;
                    // paren is optional
                    let has_paren = next_if!(self, LParen);
                    let key = self.must_parse_expr_list()?;
                    if has_paren {
                        next_expect!(self, RParen);
                    }
                    primary_key = Some(key);
                }
                3 => {
                    if order_by.is_some() {
                        return emit_error!(OptionConflicts {
                            this: "order by".to_owned(),
                            that: "order by".to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    self.must_parse_keyword(BY)?;
                    // paren is optional
                    let has_paren = next_if!(self, LParen);
                    let key = self.must_parse_expr_list()?;
                    if has_paren {
                        next_expect!(self, RParen);
                    }
                    order_by = Some(key);
                }
                4 => {
                    if partition_by.is_some() {
                        return emit_error!(OptionConflicts {
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
                        return emit_error!(OptionConflicts {
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

        emit_stmt!(CreateViewStmt {
            if_not_exists,
            name: view_name.to_owned(),
            // strategy must be Some
            strategy: unsafe { strategy.unwrap_unchecked() },
            primary_key,
            order_by,
            partition_by,
            query,
            comment
        })
    }

    fn must_parse_constraint_def(&mut self) -> Result<ConstraintDefinition, ParseError> {
        let name = self.must_parse_identifier_string()?;
        self.must_parse_keyword(CHECK)?;
        let expr = self.must_parse_expr()?;
        Ok(ConstraintDefinition {
            name: name.to_owned(),
            check: expr,
        })
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

        Ok(IndexDefinition {
            name: name.to_owned(),
            indexer: fn_call,
        })
    }

    fn must_parse_column_def(&mut self) -> Result<ColumnDefinition, ParseError> {
        let name = self.must_parse_identifier_string()?;
        let datatype = self.must_parse_datatype()?;
        let mut def = ColumnDefinition {
            name: name.to_owned(),
            typ: datatype,
            default: None,
            comment: None,
        };

        loop {
            let token = self.peek()?;
            if !token.maybe_keyword() {
                break;
            }
            match self.must_parse_one_of_keywords(&[DEFAULT, COMMENT])? {
                0 => {
                    if def.default.is_some() {
                        return emit_error!(OptionConflicts {
                            this: DEFAULT.to_owned(),
                            that: DEFAULT.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    def.default = Some(self.must_parse_expr()?);
                }
                1 => {
                    if def.comment.is_some() {
                        return emit_error!(OptionConflicts {
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

        emit_stmt!(stmt)
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

        emit_stmt!(DropStmt {
            typ: entity_type,
            if_exists,
            name: name.to_owned(),
        })
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

        emit_stmt!(TruncateStmt {
            typ: entity_type,
            if_exists,
            name: name.to_owned(),
        })
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
            return emit_stmt!(OptimizeStmt {
                table_name: table_name.to_owned(),
                partition_key: None,
            });
        }

        // parse optional [on partition <partition>]
        self.must_parse_keywords(&[ON, PARTITION])?;

        // parse <part>
        let part = self.must_parse_expr()?;

        emit_stmt!(OptimizeStmt {
            table_name: table_name.to_owned(),
            partition_key: Some(part),
        })
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

        emit_stmt!(SetStmt {
            config_name: config_name.to_owned(),
            value: expr,
        })
    }
}

impl Parser<'_> {
    fn must_parse_expr_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut res = vec![];
        loop {
            res.push(self.must_parse_expr()?);
            if !next_if!(self, Comma) {
                break;
            }
        }
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
                let e = if let Some(query) = {
                    if token.maybe_keyword() {
                        None
                    } else {
                        self.try_parse_subquery(self.token_str(&token))?
                    }
                } {
                    query.into()
                } else {
                    let exprs = self.must_parse_expr_list()?;
                    match exprs.len() {
                        // parse_expr will throw errors if no expr found
                        0 => never!(),
                        1 => unsafe { exprs.into_iter().next().unwrap_unchecked() },
                        _ => FnCall {
                            callee: FnName::Tuple,
                            arguments: exprs,
                        }
                        .into(),
                    }
                };
                next_expect!(self, RParen);
                e
            }
            LBracket => {
                // array literal
                let e = FnCall {
                    callee: FnName::Array,
                    arguments: self.must_parse_expr_list()?,
                }
                .into();
                next_expect!(self, RBracket);
                e
            }
            LBrace => {
                // map literal
                let e = FnCall {
                    callee: FnName::Map,
                    arguments: self.must_parse_map()?,
                }
                .into();
                next_expect!(self, RBrace);
                e
            }
            Minus => {
                let e = self.must_parse_expr_prefix()?;
                UnaryOp {
                    op: UnaryOperator::Neg,
                    operand: Box::new(e),
                }
                .into()
            }
            Plus => self.must_parse_expr_prefix()?,
            BitNot => {
                let e = self.must_parse_expr_prefix()?;
                UnaryOp {
                    op: UnaryOperator::BitwiseNot,
                    operand: Box::new(e),
                }
                .into()
            }
            RawStringLiteral => Value::String(s.to_owned()).into(),
            EscapedSQStringLiteral => Value::String(unescape_single_quoted_string(s)?).into(),
            EscapedDQStringLiteral => Value::String(unescape_double_quoted_string(s)?).into(),
            FloatLiteral => Value::Float(Box::new(literal::decimal_from_str!(s))).into(),
            HexLiteral => Value::Integer(literal::integer_from_str!(hex, u128, s)).into(),
            IntegerLiteral => Value::Integer(literal::integer_from_str!(u128, s)).into(),
            KeywordOrIdentifier => {
                // maybe booleans, null, identifiers,  fn name.
                if test_keyword!(s, TRUE) {
                    Value::Boolean(true).into()
                } else if test_keyword!(s, FALSE) {
                    Value::Boolean(false).into()
                } else if test_keyword!(s, NULL) {
                    Value::Null.into()
                } else if test_keyword!(s, INTERVAL) {
                    self.must_parse_interval()?.into()
                } else if test_keyword!(s, NOT) {
                    let e = self.must_parse_expr_prefix()?;
                    UnaryOp {
                        op: UnaryOperator::Not,
                        operand: Box::new(e),
                    }
                    .into()
                } else {
                    let id = self.must_parse_identifier()?;
                    if next_if!(self, LParen) {
                        // is fn call
                        let args = self.must_parse_expr_list()?;
                        next_expect!(self, RParen);
                        FnCall {
                            callee: FnName::Others(id),
                            arguments: args,
                        }
                        .into()
                    } else {
                        id.into()
                    }
                }
            }
            DelimitedIdentifier => self.must_parse_identifier()?.into(),
            QueryParameter => ast::QueryParameter {
                index: self.must_parse_integer_literal()?,
            }
            .into(),
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
                BinaryOp {
                    op: BinaryOperator::$op,
                    left: Box::new($left),
                    right: Box::new($self.must_parse_expr_tdop($power)?),
                }
                .into()
            };
        }

        macro_rules! emit_unary_op {
            ($op:ident, $left:expr) => {
                UnaryOp {
                    op: UnaryOperator::$op,
                    operand: Box::new($left),
                }
                .into()
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
                BinaryOp {
                    op: BinaryOperator::IndexAccess,
                    left: Box::new(left),
                    right: Box::new(e),
                }
                .into()
            }
            KeywordOrIdentifier => {
                match self.must_parse_one_of_keywords(&[NOT, IS, IN, LIKE, ILIKE, BETWEEN])? {
                    0 => match self.must_parse_one_of_keywords(&[IN, LIKE, ILIKE, BETWEEN])? {
                        0 => emit_binary_op!(self, NotIn, left, this_power),
                        1 => emit_binary_op!(self, NotLike, left, this_power),
                        2 => emit_binary_op!(self, NotILike, left, this_power),
                        3 => {
                            let min = self.must_parse_expr_tdop(TokenPower::Between)?;
                            self.must_parse_keyword(AND)?;
                            let max = self.must_parse_expr_tdop(TokenPower::Between)?;
                            FnCall {
                                callee: FnName::NotBetween,
                                arguments: vec![left, min, max],
                            }
                            .into()
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
                        FnCall {
                            callee: FnName::Between,
                            arguments: vec![left, min, max],
                        }
                        .into()
                    }
                    _ => never!(),
                }
            }
            _ => unreachable!(), // leave check here
        };

        Ok(expr)
    }

    /// caller should ensure the first token is 'interval' keyword
    fn must_parse_interval(&mut self) -> Result<Value, ParseError> {
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
        Ok(Value::Interval(n, unit))
    }

    fn must_parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = next_expect!(self, [DelimitedIdentifier, KeywordOrIdentifier]);
        let mut identifier = Identifier {
            name: self.token_str(&token).to_owned(),
            qualifier: None,
        };
        // only support up to one qualifier
        if next_if!(self, Dot) {
            let token = next_expect!(self, [DelimitedIdentifier, KeywordOrIdentifier]);
            identifier.qualifier = Some(self.token_str(&token).to_owned());
        }
        Ok(identifier)
    }

    fn must_parse_map(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut res = vec![];
        loop {
            // key
            res.push(self.must_parse_expr()?);
            next_expect!(self, Colon);
            // value
            res.push(self.must_parse_expr()?);
            if !next_if!(self, Comma) {
                break;
            }
        }
        Ok(res)
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
                loop {
                    types.push(self.must_parse_datatype()?);
                    if !next_if!(self, Comma) {
                        break;
                    }
                }
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
        loop {
            let literal = self.must_parse_string_literal()?;
            id = if next_if!(self, Eq) {
                self.must_parse_integer_literal()?
            } else {
                id
            };
            res.push(EnumBind { id, literal });
            id += 1;
            if !next_if!(self, Comma) {
                break;
            }
        }
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
                if token.is_whitespace() {
                    continue;
                }
                self.peeked = Some(token);
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
                let s = self.token_str(&token);
                if [NOT, IS, IN, LIKE, ILIKE]
                    .iter()
                    .any(|&kw| test_keyword!(s, kw))
                {
                    TokenPower::Comparison
                } else if test_keyword!(s, BETWEEN) {
                    TokenPower::Between
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
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    PlusMinus,
    MulDivMod,
    Access,
}
