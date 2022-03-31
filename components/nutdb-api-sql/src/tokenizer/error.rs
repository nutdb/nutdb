use crate::tokenizer::Position;
use std::fmt;
use thiserror::Error;

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
            TokenizeErrorType::UnexpectedChar => f.write_str("Unexpected char"),
            TokenizeErrorType::Incomplete => f.write_str("Incomplete token"),
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
