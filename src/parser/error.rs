use crate::parser::tokenizer::{Position, TokenType, TokenizeError};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Lex Error: {0}")]
    LexError(#[from] TokenizeError),
    #[error("Syntax Error: {0}")]
    SyntaxError(#[from] SyntaxError),
}

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[error("expected token {expected} but found token {actual} at {pos}")]
    NotExpected {
        expected: TokenType,
        actual: TokenType,
        pos: Position,
    },
    #[error("fail to parse at {pos}")]
    ParseFail { pos: Position },
    #[error("empty query")]
    EmptyQuery,
    #[error("invalid escaped unicode '\\u{{{hex}}}' in string literal")]
    InvalidEscapedUnicode { hex: String },
}
