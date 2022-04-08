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
    #[error("fail to parse at {pos}")]
    ParseFail { pos: Position },
    #[error("empty query")]
    EmptyQuery,
    #[error("invalid escaped unicode '\\u{{{hex}}}' in string literal")]
    InvalidEscapedUnicode { hex: String },
    #[error("invalid float '{literal}'")]
    InvalidFloatLiteral {
        literal: String,
        source: ParseBigDecimalError,
    },
    #[error("invalid hex '0x{literal}'")]
    InvalidHexLiteral {
        literal: String,
        source: ParseIntError,
    },
    #[error("invalid integer '{literal}'")]
    InvalidIntegerLiteral {
        literal: String,
        source: ParseIntError,
    },
    #[error("({that}) conflicts with ({this}) at {pos}")]
    OptionConflicts {
        this: String,
        that: String,
        pos: Position,
    },
}
