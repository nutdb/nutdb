use bigdecimal::BigDecimal;
use std::str::FromStr;

use crate::parser::common::Integer;
use crate::parser::tokenizer::TokenType::LParen;
use crate::parser::DataType::Compound;
use crate::parser::Value::Scalar;
pub use ast::*;
use common::{unescape_double_quoted_string, unescape_single_quoted_string};
pub use error::*;
use tokenizer::{Position, Token, TokenType, Tokenizer};

mod ast;
mod common;
mod error;
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
/// declare_keywords! {
///     keyword_name:ident => "keyword",
///     ...
/// }
/// ```
macro_rules! declare_keywords {
    ($($name:ident => $lit:literal),+) => {
        $(static $name: &'static str = $lit;)+
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
        declare_keywords! {
            WITH => "with",
            SELECT => "select"
        }
        let leading_with = if test_keyword!(leading_keyword, WITH) {
            true
        } else if test_keyword!(leading_keyword, SELECT) {
            false
        } else {
            return Ok(None);
        };

        let query = self.must_parse_query(leading_with)?;

        emit_stmt!(SelectStmt { query })
    }

    fn must_parse_subquery(&mut self) -> Result<Query, ParseError> {
        declare_keywords! {
            WITH => "with",
            SELECT => "select"
        }

        let leading_with = match self.must_parse_one_of_keywords(&[WITH, SELECT])? {
            0 => true,
            1 => false,
            _ => unreachable!(),
        };

        let query = self.must_parse_query(leading_with)?;

        Ok(query)
    }

    fn must_parse_query(&mut self, leading_with: bool) -> Result<Query, ParseError> {
        declare_keywords! {
            WITH => "with",
            SELECT => "select",
            DISTINCT => "distinct",
            FROM => "from",
            WHERE => "where",
            GROUP => "group",
            HAVING => "having",
            ORDER => "order",
            LIMIT => "limit",
            BY => "by"
        }

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
        declare_keywords! {
            EXPLAIN => "explain"
        }

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
        declare_keywords! {
            CREATE => "create",
            TABLE => "table",
            VIEW => "view"
        }

        if !test_keyword!(leading_keyword, CREATE) {
            return Ok(None);
        }

        match self.must_parse_one_of_keywords(&[TABLE, VIEW])? {
            0 => self.must_parse_create_table_stmt(),
            1 => self.must_parse_create_view_stmt(),
            _ => unreachable!(),
        }
    }

    fn must_parse_create_table_stmt(&mut self) -> Result<Option<Statement>, ParseError> {
        declare_keywords! {
            IF => "if",
            NOT => "not",
            EXISTS => "exists",
            INDEX => "index",
            CONSTRAINT => "constraint",
            PRIMARY => "primary",
            KEY => "key",
            ORDER => "order",
            BY => "by",
            PARTITION => "partition",
            COMMENT => "comment"
        }

        // parse optional [if exists]
        let if_not_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keywords(&[NOT, EXISTS])?;
            true
        } else {
            false
        };

        // parse <table_name>
        let table_name = self.must_parse_identifier()?;

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
                    if stmt.partition_by.is_some() {
                        return emit_error!(OptionConflicts {
                            this: COMMENT.to_owned(),
                            that: COMMENT.to_owned(),
                            pos: self.token_pos(&token)
                        });
                    }
                    let comment = self.must_parse_string_literal()?;
                    stmt.comment = Some(comment);
                }
                _ => unreachable!(),
            }
        }

        emit_stmt!(stmt)
    }

    fn must_parse_create_view_stmt(&mut self) -> Result<Option<Statement>, ParseError> {
        declare_keywords! {
            IF => "if",
            NOT => "not",
            EXISTS => "exists",
            INDEX => "index",
            CONSTRAINT => "constraint",
            PRIMARY => "primary",
            KEY => "key",
            ORDER => "order",
            BY => "by",
            PARTITION => "partition",
            COMMENT => "comment"
        }
    }

    fn must_parse_constraint_def(&mut self) -> Result<ConstraintDefinition, ParseError> {
        declare_keywords! {
            CHECK => "check"
        }
        let name = self.must_parse_identifier()?;
        self.must_parse_keyword(CHECK)?;
        let expr = self.must_parse_expr()?;
        Ok(ConstraintDefinition {
            name: name.to_owned(),
            check: expr,
        })
    }

    fn must_parse_index_def(&mut self) -> Result<IndexDefinition, ParseError> {
        let name = self.must_parse_identifier()?;
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
        declare_keywords! {
            DEFAULT => "default",
            COMMENT => "comment"
        }
        let name = self.must_parse_identifier()?;
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
                _ => unreachable!(),
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
        declare_keywords! {
            DESCRIBE => "describe",
            TABLE => "table",
            VIEW => "view",
            DATABASE => "database"
        }

        if !test_keyword!(leading_keyword, DESCRIBE) {
            return Ok(None);
        }

        let stmt = match self.must_parse_one_of_keywords(&[TABLE, VIEW, DATABASE])? {
            0 => DescribeStmt::Table {
                name: self.must_parse_identifier()?.to_owned(),
            },
            1 => DescribeStmt::View {
                name: self.must_parse_identifier()?.to_owned(),
            },
            2 => DescribeStmt::Database,
            _ => unreachable!(),
        };

        emit_stmt!(stmt)
    }
}

impl Parser<'_> {
    fn try_parse_drop_stmt(
        &mut self,
        leading_keyword: &str,
    ) -> Result<Option<Statement>, ParseError> {
        declare_keywords! {
            DROP => "drop",
            TABLE => "table",
            VIEW => "view",
            IF => "if",
            EXISTS => "exists"
        }

        if !test_keyword!(leading_keyword, DROP) {
            return Ok(None);
        }

        // parse TABLE|VIEW
        let entity_type = match self.must_parse_one_of_keywords(&[TABLE, VIEW])? {
            0 => DatabaseEntity::Table,
            1 => DatabaseEntity::View,
            _ => unreachable!(),
        };

        // parse optional [if exists]
        let if_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keyword(EXISTS)?;
            true
        } else {
            false
        };

        // parse <name>
        let name = self.must_parse_identifier()?;

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
        declare_keywords! {
            TRUNCATE => "truncate",
            TABLE => "table",
            VIEW => "view",
            IF => "if",
            EXISTS => "exists"
        }

        if !test_keyword!(leading_keyword, TRUNCATE) {
            return Ok(None);
        }

        // parse TABLE|VIEW
        let entity_type = match self.must_parse_one_of_keywords(&[TABLE, VIEW])? {
            0 => DatabaseEntity::Table,
            1 => DatabaseEntity::View,
            _ => unreachable!(),
        };

        // parse optional [if exists]
        let if_exists = if self.try_parse_keyword(IF)? {
            self.must_parse_keyword(EXISTS)?;
            true
        } else {
            false
        };

        // parse <name>
        let name = self.must_parse_identifier()?;

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
        declare_keywords! {
            OPTIMIZE => "optimize",
            TABLE => "table",
            ON => "on",
            PARTITION => "partition"
        }

        if !test_keyword!(leading_keyword, OPTIMIZE) {
            return Ok(None);
        }

        // parse TABLE
        self.must_parse_keyword(TABLE)?;

        // parse <name>
        let table_name = self.must_parse_identifier()?;

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
        declare_keywords! {
            SET => "set"
        }

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
        todo!()
    }

    fn try_parse_expr(&mut self) -> Result<Option<Expr>, ParseError> {
        todo!()
    }

    fn try_parse_fn_call(&mut self, fn_name: &str) -> Result<Option<FnCall>, ParseError> {
        use TokenType::*;

        // if (
        if next_if!(self, LParen) {
            return Ok(None);
        }

        let args = self.must_parse_expr_list()?;

        // expect )
        next_expect!(self, RParen);

        Ok(Some(FnCall {
            callee: Function::Others(fn_name.to_owned()),
            arguments: args,
        }))
    }

    fn try_parse_scalar_value(
        &mut self,
        negative: bool,
    ) -> Result<Option<ScalarValue>, ParseError> {
        use TokenType::*;

        let token = self.peek()?;

        let s = self.token_str(&token);

        let value = match token.t {
            RawStringLiteral => ScalarValue::String(s.to_owned()),
            EscapedSQStringLiteral => ScalarValue::String(unescape_single_quoted_string(s)?),
            EscapedDQStringLiteral => ScalarValue::String(unescape_double_quoted_string(s)?),
            FloatLiteral => {
                let decimal =
                    BigDecimal::from_str(s).map_err(|e| SyntaxError::InvalidFloatLiteral {
                        literal: s.to_owned(),
                        source: e,
                    })?;
                ScalarValue::Float(Box::new(if negative { -decimal } else { decimal }))
            }
            HexLiteral => {
                let i =
                    u128::from_str_radix(s, 16).map_err(|e| SyntaxError::InvalidHexLiteral {
                        literal: s.to_owned(),
                        source: e,
                    })?;
                ScalarValue::Integer(i, negative)
            }
            IntegerLiteral => {
                let i = u128::from_str(s).map_err(|e| SyntaxError::InvalidIntegerLiteral {
                    literal: s.to_owned(),
                    source: e,
                })?;
                ScalarValue::Integer(i, negative)
            }
            _ => return Ok(None),
        };
        Ok(Some(value))
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
    fn must_parse_identifier(&mut self) -> Result<&'a str, ParseError> {
        let token = next_expect!(self, [KeywordOrIdentifier, DelimitedIdentifier]);
        let token_str = self.token_str(&token);
        Ok(token_str)
    }

    fn must_parse_datatype(&mut self) -> Result<DataType, ParseError> {
        declare_keywords! {
            INT8 => "int8", INT16 => "int16", INT32 => "int32", INT64 => "int64", INT128 => "int128",
            UINT8 => "uint8", UINT16 => "uint16", UINT32 => "uint32", UINT64 => "uint64", UINT128 => "uint128",
            SERIAL32 => "serial32", SERIAL64 => "serial64", SERIAL128 => "serial128",
            USERIAL32 => "userial32", USERIAL64 => "userial64", USERIAL128 => "userial128",
            DECIMAL32 => "decimal32", DECIMAL64 => "decimal64",
            FLOAT32 => "float32", FLOAT64 => "float64",
            BOOLEAN => "boolean",
            CHARS => "chars", STRING => "string",
            UUID => "uuid",
            DATE => "date", DATETIME => "datetime",
            ARRAY => "array", ENUM => "enum", TUPLE => "tuple", MAP => "map",
            DICTIONARY => "dictionary", NULLABLE => "nullable"
        }
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
                let max_length = if self.peek()?.t == LParen {
                    next_expect!(self, LParen);
                    let l = self.must_parse_integer_literal()?;
                    next_expect!(self, RParen);
                    Some(l)
                } else {
                    None
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
            _ => unreachable!(),
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
            res.push(EnumBind {
                id,
                literal,
            });
            id += 1;
            if !next_if!(self, Comma) {
                break;
            }
        }
        Ok(res)
    }

    fn must_parse_integer_literal<T: Integer>(&mut self) -> Result<T, ParseError> {
        let token = next_expect!(self, IntegerLiteral);
        let token_str = self.token_str(&token);
        let res = T::from_str(token_str).map_err(|e| SyntaxError::InvalidIntegerLiteral {
            literal: token_str.to_owned(),
            source: e,
        })?;
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
            _ => unreachable!(),
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
}
