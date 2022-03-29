use crate::tokenizer::token::TokenSpan;
use std::str::from_utf8_unchecked;

pub struct Utf8Iter<'a> {
    raw: &'a [u8],
    cursor: usize,
    /// peeked char
    peeked: char,
    /// byte-length of peeked char
    peeked_len: u8,
    /// for cutting TokenSpan
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
    #[inline(always)]
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    #[inline(always)]
    pub fn slice(&self, span: &TokenSpan) -> &'a str {
        unsafe { from_utf8_unchecked(&self.raw[span.start..span.end]) }
    }

    #[inline(always)]
    pub fn span_from(&self, start: usize) -> TokenSpan {
        TokenSpan::new(start, self.cursor)
    }

    #[inline]
    pub fn pin(&mut self) {
        self.pinned = self.cursor
    }

    #[inline]
    pub fn cut_span(&self) -> TokenSpan {
        TokenSpan::new(self.pinned, self.cursor)
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
    pub fn take_while<P>(&mut self, mut predicate: P) -> TokenSpan
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
        TokenSpan::new(start, self.cursor)
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
    use crate::tokenizer::utf8_iter::Utf8Iter;

    #[test]
    fn test_utf8iter() {
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
}
