use crate::parser::SyntaxError;
use std::num::{NonZeroUsize, ParseIntError};
use std::str::FromStr;

macro_rules! unescape_string_impl {
    ($fn_name:ident, $quote:literal) => {
        /// - replace `''` to `'` if single-quoted
        /// - replace `""` to `"` if double-quoted
        /// - replace `\t` to `\t`
        /// - replace `\n` to `\n`
        /// - replace `\r` to `\r`
        /// - replace `\{char}` to char
        /// - replace `\u{xxxx}` to unicode
        pub fn $fn_name(raw: &str) -> Result<String, SyntaxError> {
            let mut res = String::with_capacity(raw.len());

            let mut chars = raw.chars();

            while let Some(ch) = chars.next() {
                match ch {
                    $quote => {
                        // Tokenizer won't add a x-quote into x-quoted string literal unless it's
                        // escaped. So escape directly.
                        chars.next();
                        res.push($quote);
                    }
                    '\\' => {
                        match chars.next() {
                            // When tokenizing a string literal, any char behind '\' is added into
                            // token, otherwise an error will be raised. So it's unreachable.
                            None => {
                                unreachable!()
                            }
                            Some(next_char) => res.push(match next_char {
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                'u' => {
                                    if let Some('{') = chars.next() {
                                        let hex: String =
                                            chars.by_ref().take_while(|&ch| ch != '}').collect();
                                        let unicode = match u32::from_str_radix(&hex, 16) {
                                            Ok(u) => u,
                                            Err(_) => {
                                                return Err(SyntaxError::InvalidEscapedUnicode {
                                                    hex,
                                                })
                                            }
                                        };
                                        match char::from_u32(unicode) {
                                            Some(ch) => ch,
                                            None => {
                                                return Err(SyntaxError::InvalidEscapedUnicode {
                                                    hex,
                                                })
                                            }
                                        }
                                    } else {
                                        'u'
                                    }
                                }
                                _ => next_char,
                            }),
                        }
                    }
                    _ => res.push(ch),
                }
            }

            Ok(res)
        }
    };
}

unescape_string_impl!(unescape_single_quoted_string, '\'');
unescape_string_impl!(unescape_double_quoted_string, '"');

pub trait Integer: FromStr<Err = ParseIntError> {}

macro_rules! impl_trait {
    ($trait:ident, $typ:ident) => {
        impl $trait for $typ {}
    };
}

impl_trait!(Integer, u8);
impl_trait!(Integer, usize);
impl_trait!(Integer, u128);
impl_trait!(Integer, NonZeroUsize);

#[cfg(test)]
mod tests {
    use crate::parser::common::{unescape_double_quoted_string, unescape_single_quoted_string};

    #[test]
    fn unescape_string() {
        let test_cases = [
            ("'", "'"),
            ("'hello'", "'hello'"),
            (r#"h""i"#, r#"h"i"#),
            (r#"\r\n\t\\hello 你好"#, "\r\n\t\\hello 你好"),
            (r#"\u{767D}"#, "白"),
            ("\\\r\\\n", "\r\n"),
        ];
        for case in test_cases {
            assert_eq!(unescape_double_quoted_string(case.0).unwrap(), case.1)
        }

        let test_cases = [
            (r#"""#, r#"""#),
            (r#""hello""#, r#""hello""#),
            (r#"h''i"#, r#"h'i"#),
            (r#"\r\n\t\\hello 你好"#, "\r\n\t\\hello 你好"),
            (r#"\u{767D}"#, "白"),
            ("\\\r\\\n", "\r\n"),
        ];
        for case in test_cases {
            assert_eq!(unescape_single_quoted_string(case.0).unwrap(), case.1)
        }
    }
}
