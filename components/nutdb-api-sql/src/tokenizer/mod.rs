pub use error::TokenizeError;
pub use token::{Token, TokenType};
use utf8_iter::{Utf8Iter};
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

    pub fn token_str(&self, token: &Token) -> &'a str {
        self.source.slice(&token.span)
    }

    pub fn token_pos(&self, token: &Token) -> Position {
        self.source.get_pos(token.span.start)
    }
}

macro_rules! emit_error {
    ($t:ident $($init:tt)?) => {
        Err(TokenizeError::$t $($init)?)
    }
}

macro_rules! consume_peeked_and_emit_token {
    ($self:ident, $t:ident on $span:expr) => {{
        $self.source.consume_peeked();
        Ok(Token::new(TokenType::$t, $span))
    }};
    ($self:ident, $t:ident) => {{
        $self.source.consume_peeked();
        Ok(Token::new(TokenType::$t, $self.source.from_pinned()))
    }};
}

macro_rules! emit_token {
    ($t:ident on $span:expr) => {{
        Ok(Token::new(TokenType::$t, $span))
    }};
    ($self:ident, $t:ident) => {{
        Ok(Token::new(TokenType::$t, $self.source.from_pinned()))
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
                _ => emit_error!(UnexpectedChar { ch: ch.into() }),
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
                                self.source.consume_peeked();
                                if matches!(self.source.peek(), Some($quote)) {
                                    // support unescape `''` to `'` and `""` to `"`
                                    self.source.consume_peeked();
                                } else {
                                    // end of literal
                                    break emit_token!(self, StringLiteral);
                                }
                            }
                            '\\' => {
                                self.source.consume_peeked();
                                // consume next char too
                                self.source.next();
                            }
                            // `\r` and `\n` are supported in string but should be escaped.
                            '\r' => break emit_error!(UnexpectedChar { ch: "\\r".into() }),
                            '\n' => break emit_error!(UnexpectedChar { ch: "\\n".into() }),
                            _ => self.source.consume_peeked(),
                        }
                    }
                    None => {
                        break emit_error!(UnexpectedEOF {
                            ctx: "string literal".into()
                        })
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
                Some(ch) => return emit_error!(UnexpectedChar { ch: ch.into() }),
            }
        }

        match self.source.peek() {
            // accept dot
            Some('.') => self.source.consume_peeked(),
            _ => return emit_token!(IntegerLiteral on span),
        }

        // consume fractional part
        self.source.skip_while(|item| matches!(item, '0'..='9'));

        let span = self.source.from_pinned();

        if self.source.slice(&span) == "." {
            // dot
            emit_token!(Dot on span)
        } else {
            emit_token!(FloatLiteral on span)
        }
    }

    fn tokenize_keyword_or_identifier(&mut self) -> TokenizeResult {
        let span = self.take_identifier();
        emit_token!(KeywordOrIdentifier on span)
    }

    fn tokenize_variable_identifier(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('$')));

        // remove '$'
        self.source.consume_peeked();

        let span = self.take_identifier();
        emit_token!(VariableIdentifier on span)
    }

    fn tokenize_config_identifier(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('@')));

        // remove '@'
        self.source.consume_peeked();

        let span = self.take_identifier();
        emit_token!(ConfigIdentifier on span)
    }

    fn tokenize_delimited_identifier(&mut self) -> TokenizeResult {
        debug_assert!(matches!(self.source.peek(), Some('`')));

        // remove '`'
        self.source.consume_peeked();

        let span = self.take_identifier();
        match self.source.peek() {
            Some('`') => self.source.consume_peeked(),
            Some(ch) => return emit_error!(UnexpectedChar { ch: ch.into() }),
            None => {
                return emit_error!(UnexpectedEOF {
                    ctx: "delimited identifier".into()
                })
            }
        }
        emit_token!(DelimitedIdentifier on span)
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
            emit_error!(UnexpectedChar { ch: "!".into() })
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
            match self.source.peek() {
                Some(ch) => {
                    if comment_end == 2 {
                        break;
                    }

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
                    return emit_error!(UnexpectedEOF {
                        ctx: "multiline comment".into()
                    })
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
        self.source
            .skip_while(|item| matches!(item, ' ' | '\t' | '\n' | '\r'));
        start != self.source.cursor()
    }

    #[inline]
    fn take_identifier(&mut self) -> Span {
        self.source
            .take_while(|item| matches!(item, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
    }
}
