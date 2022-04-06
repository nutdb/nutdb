use std::str::FromStr;

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

/// ```plain
/// emit_error!(error_type)
/// ```
macro_rules! emit_error {
    ($e:ident $($init:tt)?) => {
        Err(SyntaxError::$e $($init)?.into())
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
/// match_keywords!(self, token:expr => {
///     keyword:ident => fn_name:ident,
///     ...
/// })
/// ```
macro_rules! match_keywords {
    ($self:ident, $token:expr => { $($($keyword:ident)|+ => $fn_name:ident),+}) => {
        $(if $($self.test_keyword_ignores_type($token, $keyword))||+ {
            return $self.$fn_name();
        })+
    }
}

/// `debug_assert_keyword!(self, token:ident, type:ident)`
///
/// Calls `self.peek()` internally.
macro_rules! debug_assert_keyword {
    ($self:ident, $($keyword:ident)|+) => {
        #[cfg(debug_assertions)]
        {
            let token = $self.peek()?;
            assert!($($self.test_keyword(&token, $keyword))||+);
        }
    }
}

/// `assert_token_type!(self, token:ident, type:ident)`
macro_rules! assert_token_type {
    ($self:ident, $token:ident, $typ:ident) => {
        if ($token.t != TokenType::$typ) {
            return emit_error!(NotExpected {
                expected: TokenType::$typ,
                actual: $token.t,
                pos: $self.token_pos(&$token)
            });
        }
    };
}

impl Parser<'_> {
    fn parse_stmt(&mut self) -> ParseResult {
        declare_keywords! {
            WITH => "with",
            SELECT => "select",
            INSERT => "insert",
            EXPLAIN => "explain",
            CREATE => "create",
            ALTER => "alter",
            DESCRIBE => "describe",
            DROP => "drop",
            TRUNCATE => "truncate",
            OPTIMIZE => "optimize",
            SET => "set"
        }

        let token = self.peek()?;
        if token.is_eof() {
            return emit_error!(EmptyQuery);
        }
        if !token.maybe_keyword() {
            return emit_error!(ParseFail {
                pos: self.token_pos(&token)
            });
        }
        match_keywords!(self,
            &token => {
                SELECT | WITH => parse_select_stmt,
                INSERT => parse_insert_stmt,
                EXPLAIN => parse_explain_stmt,
                ALTER => parse_alter_stmt,
                CREATE => parse_create_stmt,
                DESCRIBE => parse_describe_stmt,
                DROP => parse_drop_stmt,
                TRUNCATE => parse_truncate_stmt,
                OPTIMIZE => parse_optimize_stmt,
                SET => parse_set_stmt
            }
        );
        emit_error!(ParseFail {
            pos: self.token_pos(&token)
        })
    }
}

impl Parser<'_> {
    fn parse_select_stmt(&mut self) -> Result<Statement, ParseError> {
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

    fn parse_select_with_clause(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_insert_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_explain_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_create_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_alter_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_describe_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_drop_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_truncate_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    fn parse_optimize_stmt(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }
}

impl Parser<'_> {
    /// `set <config> = <value>`
    fn parse_set_stmt(&mut self) -> Result<Statement, ParseError> {
        declare_keywords! {
            SET => "set"
        }

        debug_assert_keyword!(self, SET);
        self.consume_peeked(); // consume `set`

        // parse `<config>` identifier
        let token = self.next()?;
        assert_token_type!(self, token, ConfigIdentifier);

        let config_name = self.token_str(&token);

        // consume `=`
        let token = self.next()?;
        assert_token_type!(self, token, Eq);

        // parse `<value>`
        let token = self.next()?;
        // let config_value = match token.t {
        //     TokenType::StringLiteral => {
        //         ScalarValue::String(self.token_str(&token).into())
        //     }
        // }
        // let config_value = token_to_value();

        todo!()
    }
}

impl Parser<'_> {
    pub fn parse_value(&mut self) -> Result<ScalarValue, ParseError> {
        use TokenType::*;
        // let mut negative = false;
        //
        // let token = self.next()?;
        // match token.t {
        //     TokenType::Minus => negative = true,
        //
        // }
        // let s = self.token_str(token);
        // match token.t {
        //     SingleQuotedStringLiteral => ScalarValue::String(unescape_single_quoted_string(s)?),
        //     DoubleQuotedStringLiteral => ScalarValue::String(unescape_double_quoted_string(s)?),
        //     FloatLiteral => ScalarValue::Float(f64::from_str(s).map_err(||)?)
        // }
        todo!()
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
    fn test_keyword_ignores_type(&self, token: &Token, keyword: &'static str) -> bool {
        self.token_str(token).eq_ignore_ascii_case(keyword)
    }

    #[inline(always)]
    fn test_keyword(&self, token: &Token, keyword: &'static str) -> bool {
        token.maybe_keyword() && self.token_str(token).eq_ignore_ascii_case(keyword)
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
