use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TokenizeError {
    #[error("unexpected EOF while tokenizing {ctx}")]
    UnexpectedEOF { ctx: String },
    #[error("unexpected char '{ch}'")]
    UnexpectedChar { ch: String },
}
