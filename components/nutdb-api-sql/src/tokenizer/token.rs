#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    /// Keyword or Identifier.
    /// Only part of ascii chars are allowed.
    KeywordOrIdentifier,
    /// Delimited Identifiers. e.g. `` `hello` `` or `` `select` ``.
    DelimitedIdentifier,
    /// Variable Identifiers. e.g. `$a`.
    /// The leading dollar symbol is removed.
    VariableIdentifier,
    /// Config Identifiers. e.g. `@max_thread`.
    /// The leading at symbol is removed.
    ConfigIdentifier,
    /// String literals. e.g. `"hello"` or `'world'`.
    StringLiteral,
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
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TokenSpan {
    pub start: usize,
    pub end: usize,
}

impl TokenSpan {
    pub fn new(start: usize, end: usize) -> Self {
        TokenSpan {
            start, end
        }
    }

    pub fn append(&mut self, len: usize) {
        self.end += len;
    }

    pub fn empty(&self) -> bool {
        self.start == self.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    t: TokenType,
    span: TokenSpan,
}

impl Token {
    pub fn new(t: TokenType, span: TokenSpan) -> Self {
        Token {
            t,
            span,
        }
    }
}

impl Token {
    pub fn is_whitespace(&self) -> bool {
        // regard comment as whitespace too
        matches!(self.t, TokenType::Whitespace | TokenType::Comment)
    }
}