pub use error::{TokenizeError, TokenizeErrorType};
pub use token::{Token, TokenType};
use utf8_iter::Utf8Iter;
pub use utf8_iter::{Position, Span};

mod error;
mod token;
mod utf8_iter;

pub type TokenizeResult = Result<Token, TokenizeError>;

/// Tokenizer.
pub struct Tokenizer<'a> {
    source: Utf8Iter<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(raw: &'a str) -> Self {
        Tokenizer {
            source: Utf8Iter::new(raw),
        }
    }
}

impl<'a> Tokenizer<'a> {
    #[inline(always)]
    pub fn source(&self) -> &Utf8Iter<'a> {
        &self.source
    }
}

macro_rules! emit_error {
    ($self:ident, $t:ident, $ctx:expr) => {
        Err(TokenizeError {
            t: TokenizeErrorType::$t,
            ctx: $ctx.into(),
            pos: $self.source.get_current_pos(),
        })
    };
}

macro_rules! consume_peeked_and_emit_token {
    ($self:ident, $t:ident on $span:expr) => {{
        $self.source.consume_peeked();
        Ok(Token::new(TokenType::$t, $span))
    }};
    ($self:ident, $t:ident) => {{
        $self.source.consume_peeked();
        Ok(Token::new(TokenType::$t, $self.source.cut_from_pinned()))
    }};
}

macro_rules! emit_token {
    ($t:ident on $span:expr) => {{
        Ok(Token::new(TokenType::$t, $span))
    }};
    ($self:ident, $t:ident) => {{
        Ok(Token::new(TokenType::$t, $self.source.cut_from_pinned()))
    }};
}

impl Tokenizer<'_> {
    pub fn next_token(&mut self) -> TokenizeResult {
        self.source.pin();

        if self.skip_whitespace() {
            return emit_token!(self, Whitespace);
        }

        match self.source.peek() {
            None => emit_token!(self, Eof),
            Some(ch) => match ch {
                '(' => consume_peeked_and_emit_token!(self, LParen),
                ')' => consume_peeked_and_emit_token!(self, RParen),
                '[' => consume_peeked_and_emit_token!(self, LBracket),
                ']' => consume_peeked_and_emit_token!(self, RBracket),
                '{' => consume_peeked_and_emit_token!(self, LBrace),
                '}' => consume_peeked_and_emit_token!(self, RBrace),
                ',' => consume_peeked_and_emit_token!(self, Comma),
                ':' => consume_peeked_and_emit_token!(self, Colon),
                '+' => consume_peeked_and_emit_token!(self, Plus),
                '-' => self.tokenize_inline_comment_or_minus(),
                '*' => consume_peeked_and_emit_token!(self, Mul),
                '/' => self.tokenize_block_comment_or_div(),
                '%' => consume_peeked_and_emit_token!(self, Mod),
                '=' => consume_peeked_and_emit_token!(self, Eq),
                '!' => self.tokenize_ne(),
                '<' => self.tokenize_lt_or_le_or_ne_or_lshift(),
                '>' => self.tokenize_gt_or_ge_or_rshift(),
                '&' => consume_peeked_and_emit_token!(self, BitAnd),
                '|' => consume_peeked_and_emit_token!(self, BitOr),
                '^' => consume_peeked_and_emit_token!(self, BitXor),
                '~' => consume_peeked_and_emit_token!(self, BitNot),
                ';' => consume_peeked_and_emit_token!(self, SemiColon),
                'a'..='z' | 'A'..='Z' | '_' => self.tokenize_keyword_or_identifier(),
                '`' => self.tokenize_delimited_identifier(),
                '.' | '0'..='9' => self.tokenize_dot_or_numeric(),
                '$' => self.tokenize_variable_identifier(),
                '@' => self.tokenize_config_identifier(),
                '\'' => self.tokenize_single_quoted_string(),
                '"' => self.tokenize_double_quoted_string(),
                _ => emit_error!(self, UnexpectedChar, ch),
            },
        }
    }
}

macro_rules! tokenize_string_literal {
    ($fn_name:ident, $quote:literal) => {
        fn $fn_name(&mut self) -> TokenizeResult {
            debug_assert!(matches!(self.source.peek(), Some($quote)));

            self.source.consume_peeked();

            // should not contains quotes
            self.source.pin();

            loop {
                match self.source.peek() {
                    Some(ch) => {
                        match ch {
                            $quote => {
                                let span = self.source.cut_from_pinned();
                                self.source.consume_peeked();
                                if matches!(self.source.peek(), Some($quote)) {
                                    // support unescape `''` to `'` and `""` to `"`
                                    self.source.consume_peeked();
                                } else {
                                    // end of literal
                                    break emit_token!(StringLiteral on span);
                                }
                            }
                            '\\' => {
                                self.source.consume_peeked();
                                // consume next char too
                                let next_ch = self.source.next();
                                // handle \r\n
                                if matches!(next_ch, Some('\r')) {
                                    if matches!(self.source.peek(), Some('\n')) {
                                        self.source.consume_peeked();
                                    }
                                };
                            }
                            // `\r` and `\n` are supported in string but should be escaped.
                            '\r' => {
                                break emit_error!(
                                    self,
                                    UnexpectedChar,
                                    "'\\r' in string is supported but should be escaped by '\\'"
                                )
                            }
                            '\n' => {
                                break emit_error!(
                                    self,
                                    UnexpectedChar,
                                    "'\\n' in string is supported but should be escaped by '\\'"
                                )
                            }
                            _ => self.source.consume_peeked(),
                        }
                    }
                    None => {
                        break emit_error!(self, UnexpectedEOF, "string literal is not complete")
                    }
                }
            }
        }
    };
}

// All functions assume that there is a peeked char in iter.
impl Tokenizer<'_> {
    tokenize_string_literal!(tokenize_single_quoted_string, '\'');
    tokenize_string_literal!(tokenize_double_quoted_string, '"');

    fn tokenize_dot_or_numeric(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('.' | '0'..='9')));

        self.source.pin();

        let span = self.source.take_while(|item| matches!(item, '0'..='9'));

        if self.source.slice(&span) == "0" {
            // may be hex or float
            match self.source.peek() {
                // meet EOF
                None => return emit_token!(IntegerLiteral on span),
                Some('x' | 'X') => {
                    // hex
                    self.source.consume_peeked();
                    // should not contain '0x'
                    let span = self
                        .source
                        .take_while(|item| matches!(item, '0'..='9' | 'A'..='F' | 'a'..='f'));
                    return emit_token!(HexLiteral on span);
                }
                Some('.') => {
                    // is float
                }
                // numbers starts with `0` without dot are not allowed
                Some(ch) => {
                    return emit_error!(
                        self,
                        UnexpectedChar,
                        format!("'{}' is invalid in numeric literal", ch)
                    );
                }
            }
        }

        match self.source.peek() {
            // accept dot
            Some('.') => self.source.consume_peeked(),
            next_ch => {
                return if let Some(ch) = get_char_if_invalid_end_of_numeric(next_ch) {
                    emit_error!(
                        self,
                        UnexpectedChar,
                        format!("'{}' cannot be a part of integer literal", ch)
                    )
                } else {
                    emit_token!(IntegerLiteral on span)
                };
            }
        }

        // consume fractional part
        self.source.skip_while(|item| matches!(item, '0'..='9'));

        let span = self.source.cut_from_pinned();

        if self.source.slice(&span) == "." {
            // dot
            emit_token!(Dot on span)
        } else {
            if let Some(ch) = get_char_if_invalid_end_of_numeric(self.source.peek()) {
                emit_error!(
                    self,
                    UnexpectedChar,
                    format!("'{}' cannot be a part of float literal", ch)
                )
            } else {
                emit_token!(FloatLiteral on span)
            }
        }
    }

    fn tokenize_keyword_or_identifier(&mut self) -> TokenizeResult {
        debug_assert!(!matches!(self.source.peek(), Some('0'..='9')));

        let span = self
            .source
            .take_while(|item| matches!(item, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'));

        if let Some(ch) = get_char_if_invalid_end_of_identifier(self.source.peek()) {
            emit_error!(
                self,
                UnexpectedChar,
                format!("'{}' cannot be a part of identifier or keyword", ch)
            )
        } else {
            emit_token!(KeywordOrIdentifier on span)
        }
    }

    fn tokenize_variable_identifier(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('$')));

        // consume prefix
        self.source.consume_peeked();

        // query parameter
        if matches!(self.source.peek(), Some('0'..='9')) {
            let span = self.source.take_while(|item| matches!(item, '0'..='9'));
            // at least one digit so no need to check span.is_empty
            return emit_token!(QueryParameter on span);
        }

        let span = self
            .source
            .take_while(|item| matches!(item, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'));
        if span.is_empty() {
            emit_error!(self, Incomplete, "identifier should have name")
        } else if let Some(ch) = get_char_if_invalid_end_of_identifier(self.source.peek()) {
            emit_error!(
                self,
                UnexpectedChar,
                format!("'{}' cannot be a part of variable identifier", ch)
            )
        } else {
            emit_token!(VariableIdentifier on span)
        }
    }

    fn tokenize_config_identifier(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('@')));

        // consume prefix
        self.source.consume_peeked();

        if matches!(self.source.peek(), Some('0'..='9')) {
            return emit_error!(
                self,
                UnexpectedChar,
                "config identifier cannot starts with numbers"
            );
        }
        let span = self
            .source
            .take_while(|item| matches!(item, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'));
        if span.is_empty() {
            emit_error!(self, Incomplete, "identifier should have name")
        } else if let Some(ch) = get_char_if_invalid_end_of_identifier(self.source.peek()) {
            emit_error!(
                self,
                UnexpectedChar,
                format!("'{}' cannot be a part of config identifier", ch)
            )
        } else {
            emit_token!(ConfigIdentifier on span)
        }
    }

    fn tokenize_delimited_identifier(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('`')));

        // remove '`'
        self.source.consume_peeked();

        let span = self
            .source
            .take_while(|item| !matches!(item, '`' | '\r' | '\n'));

        if span.is_empty() {
            return emit_error!(
                self,
                Incomplete,
                "delimited identifier cannot be an empty string"
            );
        }

        match self.source.peek() {
            Some('`') => {
                self.source.consume_peeked();
                emit_token!(DelimitedIdentifier on span)
            }
            Some(_) => emit_error!(
                self,
                UnexpectedChar,
                "'\\r' or '\\n' cannot be a part of delimited identifier"
            ),
            _ => {
                emit_error!(self, UnexpectedEOF, "delimited identifier is not complete")
            }
        }
    }

    fn tokenize_inline_comment_or_minus(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('-')));

        self.source.consume_peeked();
        match self.source.peek() {
            Some('-') => {
                self.source.consume_peeked();
                self.tokenize_inline_comment_body()
            }
            _ => emit_token!(self, Minus),
        }
    }

    fn tokenize_block_comment_or_div(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('/')));

        self.source.consume_peeked();
        match self.source.peek() {
            Some('*') => {
                self.source.consume_peeked();
                self.tokenize_block_comment_body()
            }
            _ => emit_token!(self, Div),
        }
    }

    fn tokenize_ne(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('!')));

        self.source.consume_peeked();
        if let Some('=') = self.source.peek() {
            self.source.consume_peeked();
            emit_token!(self, NotEq)
        } else {
            emit_error!(self, UnexpectedChar, "'!' can only be used with '='")
        }
    }

    fn tokenize_lt_or_le_or_ne_or_lshift(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('<')));

        self.source.consume_peeked();

        match self.source.peek() {
            Some('=') => consume_peeked_and_emit_token!(self, LtEq),
            Some('>') => consume_peeked_and_emit_token!(self, NotEq),
            Some('<') => consume_peeked_and_emit_token!(self, BitLShift),
            _ => emit_token!(self, Lt),
        }
    }

    fn tokenize_gt_or_ge_or_rshift(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('>')));

        self.source.consume_peeked();

        match self.source.peek() {
            Some('=') => consume_peeked_and_emit_token!(self, GtEq),
            Some('>') => consume_peeked_and_emit_token!(self, BitRShift),
            _ => emit_token!(self, Gt),
        }
    }

    fn tokenize_inline_comment_body(&mut self) -> TokenizeResult {
        // skip spaces
        self.source.skip(' ');

        let span = self.source.take_while(|item| item != '\n' && item != '\r');

        emit_token!(Comment on span)
    }

    fn tokenize_block_comment_body(&mut self) -> TokenizeResult {
        let start = self.source.cursor();
        let mut end = self.source.cursor();
        // 0 = not end
        // 1 = maybe end
        // 2 = end
        let mut comment_end = 0u8;
        loop {
            if comment_end == 2 {
                break;
            }
            match self.source.peek() {
                Some(ch) => {
                    self.source.consume_peeked();
                    if comment_end == 1 && ch == '/' {
                        comment_end = 2;
                    } else {
                        comment_end = (ch == '*').into();
                    }
                    if comment_end == 0 {
                        end = self.source.cursor();
                    }
                }
                None => {
                    return emit_error!(self, UnexpectedEOF, "block comment is not complete");
                }
            }
        }
        emit_token!(Comment on Span::new(start, end))
    }
}

impl Tokenizer<'_> {
    /// Skips whitespaces and returns whether skipped whitespaces.
    fn skip_whitespace(&mut self) -> bool {
        let start = self.source.cursor();
        self.source.skip_while(is_whitespace_char);
        start != self.source.cursor()
    }
}

#[inline(always)]
fn is_whitespace_char(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}

#[inline(always)]
fn get_char_if_invalid_end_of_identifier(ch: Option<char>) -> Option<char> {
    match ch {
        None
        | Some(
            '.' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '>' | '<' | '=' | '!' | ':' | ','
            | ';' | '[' | ']' | '(' | ')' | '{' | '}' | '\t' | '\n' | '\r' | ' ',
        ) => None,
        _ => ch,
    }
}

#[inline(always)]
fn get_char_if_invalid_end_of_numeric(ch: Option<char>) -> Option<char> {
    match ch {
        None
        | Some(
            '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '>' | '<' | '=' | '!' | ':' | ',' | ';'
            | ']' | ')' | '}' | '\t' | '\n' | '\r' | ' ',
        ) => None,
        _ => ch,
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::{Token, TokenType, Tokenizer};
    use TokenType::*;

    fn collect_tokens(tn: &mut Tokenizer<'_>) -> Vec<Token> {
        let mut result = vec![];
        while let Ok(t) = tn.next_token() {
            if t.is_eof() {
                break;
            }
            result.push(t);
        }
        result
    }

    fn assert_tokens_ignore_whitespace(actual: &Vec<Token>, expected: &Vec<TokenType>) {
        let mut i = 0;
        for token in actual {
            if token.t != Whitespace {
                assert_eq!(token.t, expected[i]);
                i += 1;
            }
        }
    }

    fn get_str<'a>(t: &Tokenizer<'a>, token: &Token) -> &'a str {
        t.source.slice(&token.span)
    }

    #[test]
    fn tokenize_whitespaces() {
        let test_case = ["    ", "\t\t", "\n", "\r\n", "\r"].join(" ");
        let tokens = collect_tokens(&mut Tokenizer::new(&test_case));
        // all whitespaces are combined into one
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].t, Whitespace);
    }

    #[test]
    fn tokenize_numerics() {
        let test_case = [
            ("510", IntegerLiteral), // integer
            ("0.123", FloatLiteral), // float
            (".123", FloatLiteral),  // float
            ("1.", FloatLiteral),    // float
            ("0x123", HexLiteral),   // hex
        ];
        for case in test_case {
            let token = Tokenizer::new(case.0).next_token().unwrap();
            assert_eq!(token.t, case.1);
        }
    }

    #[test]
    fn tokenize_numerics_fail() {
        assert!(Tokenizer::new("1d").next_token().is_err());
        // \n and \r should be escaped
        assert!(Tokenizer::new("1好").next_token().is_err());
        assert!(Tokenizer::new("1.d").next_token().is_err());
    }

    #[test]
    fn tokenize_strings() {
        let test_case = [
            (r#""hello""#, "hello"),            // if double-quoted
            ("'hello'", "hello"),               // if single-quoted
            ("'he''llo'", "he''llo"),           // if escape '
            (r#""he""llo""#, r#"he""llo"#),     // if escape "
            ("'h\\t i\\r\\n'", "h\\t i\\r\\n"), // if escape \t or \r\n
            ("\"\\\n\"", "\\\n"),               // if escape \n
        ];
        for case in test_case {
            let mut t = Tokenizer::new(case.0);
            let token = t.next_token().unwrap();
            assert_eq!(token.t, StringLiteral);
            assert_eq!(get_str(&t, &token), case.1);
        }
    }

    #[test]
    fn tokenize_strings_fail() {
        assert!(Tokenizer::new(r#""hello'"#).next_token().is_err());
        // \n and \r should be escaped
        assert!(Tokenizer::new("\"\n\"").next_token().is_err());
        assert!(Tokenizer::new("\"\r\"").next_token().is_err());
    }

    #[test]
    fn tokenize_identifiers() {
        let test_case = [
            ("hello_world", "hello_world", KeywordOrIdentifier), // bare
            ("`select`", "select", DelimitedIdentifier),         // delimited
            ("`你 好`", "你 好", DelimitedIdentifier),       // delimited
            ("$0", "0", QueryParameter),                         // query parameter
            ("$a", "a", VariableIdentifier),                     // variable
            ("@a", "a", ConfigIdentifier),                       // config
        ];
        for case in test_case {
            let mut t = Tokenizer::new(case.0);
            let token = t.next_token().unwrap();
            assert_eq!(token.t, case.2);
            assert_eq!(get_str(&t, &token), case.1);
        }
    }

    #[test]
    fn tokenize_identifiers_fail() {
        // empty
        assert!(Tokenizer::new("$").next_token().is_err());
        assert!(Tokenizer::new("``").next_token().is_err());
        assert!(Tokenizer::new("@").next_token().is_err());
        // not alphanumeric nor _
        assert!(Tokenizer::new("你好").next_token().is_err());
        assert!(Tokenizer::new("$你好").next_token().is_err());
        assert!(Tokenizer::new("@你好").next_token().is_err());
        // unexpected char
        assert!(Tokenizer::new("hello_你好").next_token().is_err());
    }

    #[test]
    fn tokenize_comment() {
        let test_case = [
            ("hello -- world", 2, "world"),     // inline
            ("/* hello */", 0, " hello "),      // block
            ("hello /* \n */world", 2, " \n "), // multiline block
        ];

        for case in test_case {
            let mut t = Tokenizer::new(case.0);
            let tokens = collect_tokens(&mut t);
            assert_eq!(get_str(&t, &tokens[case.1]), case.2);
        }
    }

    #[test]
    fn tokenize_comment_fail() {
        assert!(Tokenizer::new("/*").next_token().is_err());
        assert!(Tokenizer::new("/* /").next_token().is_err());
    }

    #[test]
    fn tokenize_symbols() {
        let test_case = [
            (".", Dot),
            ("+", Plus),
            ("-", Minus),
            ("*", Mul),
            ("/", Div),
            ("%", Mod),
            ("&", BitAnd),
            ("|", BitOr),
            ("^", BitXor),
            (">>", BitRShift),
            ("<<", BitLShift),
            ("=", Eq),
            ("!=", NotEq),
            ("<>", NotEq),
            (">", Gt),
            (">=", GtEq),
            ("<", Lt),
            ("<=", LtEq),
            (":", Colon),
            (",", Comma),
            (";", SemiColon),
            ("[", LBracket),
            ("]", RBracket),
            ("{", LBrace),
            ("}", RBrace),
            ("(", LParen),
            (")", RParen),
        ];

        for case in test_case {
            let mut t = Tokenizer::new(case.0);
            assert_eq!(t.next_token().unwrap().t, case.1);
        }
    }

    #[test]
    fn tokenize_symbol_fail() {
        assert!(Tokenizer::new("!").next_token().is_err());
    }

    #[test]
    fn tokenize_simple_query() {
        let tokens = collect_tokens(&mut Tokenizer::new(
            "
WITH $ev_type = IF $0 = 0 THEN 'EventA' ELSE 'EventB' END
SELECT *
FROM
(
    SELECT count() AS `c`
    FROM events
    WHERE event_type = $ev_type
    GROUP BY name
)",
        ));
        assert_tokens_ignore_whitespace(
            &tokens,
            &vec![
                KeywordOrIdentifier, // with
                VariableIdentifier,  // $ev_type
                Eq,
                KeywordOrIdentifier, // if
                QueryParameter,      // $0
                Eq,
                IntegerLiteral,      // 0
                KeywordOrIdentifier, // then
                StringLiteral,       // EventA
                KeywordOrIdentifier, // else
                KeywordOrIdentifier, // EventB
                KeywordOrIdentifier, // end
                KeywordOrIdentifier, // select
                Mul,
                KeywordOrIdentifier, // from
                LParen,
                KeywordOrIdentifier, // select
                KeywordOrIdentifier, // count
                LParen,
                RParen,
                KeywordOrIdentifier, // as
                KeywordOrIdentifier, // c
                KeywordOrIdentifier, // from
                KeywordOrIdentifier, // events
                KeywordOrIdentifier, // where
                KeywordOrIdentifier, // event_type
                Eq,
                VariableIdentifier,  // $ev_type
                KeywordOrIdentifier, // group
                KeywordOrIdentifier, // by
                KeywordOrIdentifier, // name
            ],
        )
    }
}
