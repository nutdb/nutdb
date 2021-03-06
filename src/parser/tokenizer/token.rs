use derive_more::{Constructor, Display};

use crate::parser::tokenizer::utf8_iter::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum TokenType {
    /// Keyword or Identifier.
    /// Only part of ascii chars are allowed.
    KeywordOrIdentifier,
    /// Delimited Identifiers. e.g. `` `hello` `` or `` `select` ``.
    DelimitedIdentifier,
    /// Config Identifiers. e.g. `@max_thread`.
    /// The leading at symbol is removed.
    ConfigIdentifier,
    /// Query Parameters. e.g. `$0`.
    QueryParameter,
    /// Raw String Literal
    RawStringLiteral,
    /// Escaped Single Quoted String literals. e.g. `'wor''ld'`.
    #[display(fmt = "EscapedSingleQuotedStringLiteral")]
    EscapedSQStringLiteral,
    /// Escaped Double Quoted String literals. e.g. `"hel""lo"
    #[display(fmt = "EscapedDoubleQuotedStringLiteral")]
    EscapedDQStringLiteral,
    /// Numeric int literal. e.g. `1`.
    IntegerLiteral,
    /// Numeric float literal. e.g. `1.1`
    FloatLiteral,
    /// Numeric hex literal. e.g. `0xa0`
    HexLiteral,
    /// Comma.
    Comma,
    /// Dots outside numeric literals.
    Dot,
    /// Colon `:`.
    Colon,
    /// SemiColon `;`.
    SemiColon,
    /// Plus operator `+`.
    Plus,
    /// Minus operator `-`.
    Minus,
    /// Multiplication operator `*`.
    Mul,
    /// Division operator `/`.
    Div,
    /// Modulo Operator `%`.
    Mod,
    /// Equality operator `=` or `==`.
    Eq,
    /// Not Equals operator `!=`.
    NotEq,
    /// Less Than operator `<`.
    Lt,
    /// Greater Than operator `>`.
    Gt,
    /// Less Than Or Equals operator `<=`.
    LtEq,
    /// Greater Than Or Equals operator `>=`.
    GtEq,
    /// Left parenthesis `(`.
    LParen,
    /// Right parenthesis `)`.
    RParen,
    /// Left bracket `[`.
    LBracket,
    /// Right bracket `]`.
    RBracket,
    /// Left brace `{`.
    LBrace,
    /// Right brace `}`.
    RBrace,
    /// Bitwise and `&`.
    BitAnd,
    /// Bitwise or `|`.
    BitOr,
    /// Bitwise xor `^`.
    BitXor,
    /// Bitwise not `~`.
    BitNot,
    /// Bitwise left shift `<<`.
    BitLShift,
    /// Bitwise right shift `>>`.
    BitRShift,
    /// Comments.
    Comment,
    /// Whitespace.
    Whitespace,
    /// EOF.
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq, Constructor)]
pub struct Token {
    pub t: TokenType,
    pub span: Span,
}

impl Token {
    #[inline(always)]
    pub fn is_whitespace(&self) -> bool {
        // regard comment as whitespace too
        matches!(self.t, TokenType::Whitespace | TokenType::Comment)
    }

    #[inline(always)]
    pub fn maybe_keyword(&self) -> bool {
        self.t == TokenType::KeywordOrIdentifier
    }

    #[inline(always)]
    pub fn is_terminator(&self) -> bool {
        self.t == TokenType::EOF || self.t == TokenType::SemiColon
    }
}
