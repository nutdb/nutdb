use std::num::ParseIntError;

use bigdecimal::ParseBigDecimalError;
use thiserror::Error;

use crate::parser::tokenizer::{Position, TokenType, TokenizeError};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Lex Error: {0}")]
    LexError(#[from] TokenizeError),
    #[error("Syntax Error: {0}")]
    SyntaxError(#[from] SyntaxError),
}

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[
        error("expected token ({expected}) but found token {actual} at {pos}",
        expected = expected.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", "))
    ]
    NotExpectedTokenTypes {
        expected: Vec<TokenType>,
        actual: TokenType,
        pos: Position,
    },
    #[
        error("expected keyword ({expected}) but found token {actual} at {pos}",
        expected = expected.join(", "))
    ]
    NotExpectedKeywords {
        expected: Vec<String>,
        actual: String,
        pos: Position,
    },
    #[error("fail to parse ({msg}) at {pos}")]
    ParseFail { msg: String, pos: Position },
    #[error("empty query")]
    EmptyQuery,
    #[error("invalid escaped unicode '\\u{{{hex}}}' in string literal")]
    InvalidEscapedUnicode { hex: String },
    #[error("invalid float '{raw}'")]
    InvalidFloatLiteral {
        raw: String,
        source: ParseBigDecimalError,
    },
    #[error("invalid hex '0x{raw}'")]
    InvalidHexLiteral { raw: String, source: ParseIntError },
    #[error("invalid integer '{raw}'")]
    InvalidIntegerLiteral { raw: String, source: ParseIntError },
    #[error("({this}) conflicts with ({that}) near {pos}")]
    Conflicts {
        this: String,
        that: String,
        pos: Position,
    },
}
