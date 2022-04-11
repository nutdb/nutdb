use std::fmt;

use thiserror::Error;

use crate::parser::tokenizer::Position;

#[derive(Debug, Eq, PartialEq)]
pub enum TokenizeErrorType {
    UnexpectedEOF,
    UnexpectedChar,
    Incomplete,
}

impl fmt::Display for TokenizeErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenizeErrorType::UnexpectedEOF => f.write_str("Unexpected EOF"),
            TokenizeErrorType::UnexpectedChar => f.write_str("Unexpected Char"),
            TokenizeErrorType::Incomplete => f.write_str("Incomplete Token"),
        }
    }
}

#[derive(Debug, Error)]
#[error("{t}: {ctx} near {pos}")]
pub struct TokenizeError {
    pub t: TokenizeErrorType,
    pub ctx: String,
    pub pos: Position,
}
