use std::fmt;
use std::str::from_utf8_unchecked;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn is_empty(&self) -> bool {
        self.end == self.start
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

impl Position {
    pub fn new(line: usize, col: usize) -> Position {
        Position { line, col }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {} col {}", self.line, self.col)
    }
}

pub struct Utf8Iter<'a> {
    raw: &'a [u8],
    cursor: usize,
    /// peeked char
    peeked: char,
    /// byte-length of peeked char
    peeked_len: u8,
    /// for cutting Span
    pinned: usize,
}

impl<'a> Utf8Iter<'a> {
    pub fn new(raw: &'a str) -> Self {
        Utf8Iter {
            raw: raw.as_bytes(),
            cursor: 0,
            peeked: '\0',
            peeked_len: 0,
            pinned: 0,
        }
    }
}

impl<'a> Utf8Iter<'a> {
    #[inline]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    #[inline]
    pub fn slice(&self, span: &Span) -> &'a str {
        unsafe { from_utf8_unchecked(&self.raw[span.start..span.end]) }
    }

    #[inline]
    pub fn slice_from_to(&self, from: usize, to: usize) -> &'a str {
        unsafe { from_utf8_unchecked(&self.raw[from..to]) }
    }

    #[inline]
    pub fn pin(&mut self) {
        self.pinned = self.cursor
    }

    #[inline]
    pub fn cut_from_pinned(&self) -> Span {
        Span::new(self.pinned, self.cursor)
    }

    pub fn get_pos(&self, cursor: usize) -> Position {
        let mut before = self.slice_from_to(0, cursor).chars().peekable();

        let mut col = 1usize;
        let mut line = 1usize;

        while let Some(ch) = before.next() {
            match ch {
                '\r' => {
                    before.next_if_eq(&'\n');
                    line += 1;
                    col = 1;
                }
                '\n' => {
                    line += 1;
                    col = 1;
                }
                '\t' => {
                    col += 4;
                }
                _ => {
                    col += 1;
                }
            }
        }

        Position::new(line, col)
    }

    pub fn get_current_pos(&self) -> Position {
        self.get_pos(self.cursor)
    }
}

impl<'a> Utf8Iter<'a> {
    /// Peek next one char.
    /// Buffer size is 1.
    pub fn peek(&mut self) -> Option<char> {
        if self.peeked_len != 0 {
            return Some(self.peeked);
        }

        if self.cursor == self.raw.len() {
            return None;
        }

        self.peeked_len += 1;

        let x = self.raw[self.cursor];
        if x < 128 {
            self.peeked = unsafe { char::from_u32_unchecked(x as u32) };
            return Some(self.peeked);
        }

        // Multibyte case follows
        // Decode from a byte combination out of: [[[x y] z] w]
        // NOTE: Performance is sensitive to the exact formulation here
        let init = utf8_first_byte(x, 2);
        // SAFETY: `bytes` produces an UTF-8-like string,
        // so the iterator must produce a value here.
        self.peeked_len += 1;
        let y = self.raw[self.cursor + 1];
        let mut ch = utf8_acc_cont_byte(init, y);
        if x >= 0xE0 {
            // [[x y z] w] case
            // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
            // SAFETY: `bytes` produces an UTF-8-like string,
            // so the iterator must produce a value here.
            self.peeked_len += 1;
            let z = self.raw[self.cursor + 2];
            let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
            ch = init << 12 | y_z;
            if x >= 0xF0 {
                // [x y z w] case
                // use only the lower 3 bits of `init`
                // SAFETY: `bytes` produces an UTF-8-like string,
                // so the iterator must produce a value here.
                self.peeked_len += 1;
                let w = self.raw[self.cursor + 3];
                ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
            }
        }

        self.peeked = unsafe { char::from_u32_unchecked(ch) };
        Some(self.peeked)
    }

    /// Get next char and consume it.
    pub fn next(&mut self) -> Option<char> {
        let ch = self.peek();
        self.consume_peeked();
        ch
    }

    /// Consume peeked char if exists.
    /// Returns the range of consumed char.
    #[inline]
    pub fn consume_peeked(&mut self) {
        self.cursor += self.peeked_len as usize;
        self.peeked_len = 0;
    }

    #[inline]
    pub fn take_while<P>(&mut self, mut predicate: P) -> Span
    where
        P: FnMut(char) -> bool,
    {
        let start = self.cursor;
        loop {
            if let Some(ch) = self.peek() {
                if predicate(ch) {
                    self.consume_peeked();
                    continue;
                }
            }
            break;
        }
        Span::new(start, self.cursor)
    }

    #[inline]
    pub fn skip_while<P>(&mut self, mut predicate: P)
    where
        P: FnMut(char) -> bool,
    {
        loop {
            if let Some(ch) = self.peek() {
                if predicate(ch) {
                    self.consume_peeked();
                    continue;
                }
            }
            break;
        }
    }

    #[inline]
    pub fn skip(&mut self, ch: char) {
        loop {
            if let Some(item) = self.peek() {
                if item == ch {
                    self.consume_peeked();
                    continue;
                }
            }
            break;
        }
    }
}

const CONT_MASK: u8 = 0b0011_1111;

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

#[cfg(test)]
mod tests {
    use crate::parser::tokenizer::utf8_iter::{Position, Utf8Iter};

    #[test]
    fn test_utf8iter_peek_next() {
        let raw = "hello world 你好世界 ❤";
        let mut iter = Utf8Iter::new(raw);
        assert_eq!(iter.peek(), Some('h'));
        // buffer size is 1.
        assert_eq!(iter.peek(), Some('h'));
        iter.consume_peeked();
        assert_eq!(iter.peek(), Some('e'));
        assert_eq!(iter.next(), Some('e'));
        iter.skip_while(|ch| ch != ' ');
        assert_eq!(iter.next(), Some(' '));
        let span = iter.take_while(|ch| ch != ' ');
        assert_eq!(iter.slice(&span), "world");
        assert_eq!(iter.next(), Some(' '));
        let span = iter.take_while(|ch| ch != ' ');
        assert_eq!(iter.slice(&span), "你好世界");
        assert_eq!(iter.next(), Some(' '));
        assert_eq!(iter.peek(), Some('❤'));
        assert_eq!(iter.next(), Some('❤'));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.peek(), None);
    }

    #[test]
    fn test_utf8iter_pos() {
        let raw = "select * \n\t你好 ❤\r\n1";
        let mut iter = Utf8Iter::new(raw);
        iter.skip_while(|ch| ch != ' ');
        assert_eq!(iter.get_current_pos(), Position::new(1, 7));
        iter.next(); // consume ' '
        iter.next(); // consume '*'
        iter.next(); // consume ' '
        assert_eq!(iter.get_current_pos(), Position::new(1, 10));
        iter.next(); // consume '\n'
        assert_eq!(iter.get_current_pos(), Position::new(2, 1));
        iter.next(); // consume '\t'
        assert_eq!(iter.get_current_pos(), Position::new(2, 5));
        iter.next(); // consume '你'
        iter.next(); // consume '好'
        assert_eq!(iter.get_current_pos(), Position::new(2, 7));
        iter.next(); // consume ' '
        iter.next(); // consume heart
        assert_eq!(iter.get_current_pos(), Position::new(2, 9));
        iter.next(); // consume '\r'
        iter.next(); // consume '\n'
        assert_eq!(iter.get_current_pos(), Position::new(3, 1));
        assert_eq!(iter.next(), Some('1'));
    }
}
