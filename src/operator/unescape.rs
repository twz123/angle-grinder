use std::borrow::Cow;

use crate::data::Record;
use crate::operator;
use crate::operator::expr::Expr;
use crate::operator::{EvalError, UnaryPreAggFunction};

#[derive(Clone)]
pub(crate) struct Unescape {
    pub(crate) from: Option<Expr>,
    pub(crate) as_: String,
}

impl UnaryPreAggFunction for Unescape {
    fn process(&self, rec: Record) -> Result<Option<Record>, EvalError> {
        let escaped = operator::get_input(&rec, &self.from)?;
        let unescaped = unescape_c_string(escaped).into_owned();
        let rec = rec.put(self.as_.clone(), crate::data::Value::Str(unescaped));
        Ok(Some(rec))
    }
}

fn unescape_c_string<'a>(escaped: impl Into<Cow<'a, str>>) -> Cow<'a, str> {
    let escaped = escaped.into();

    enum Front {
        Skipped(usize),
        Unescaped(String),
    }

    let mut front = Front::Skipped(0);
    let mut back = escaped.as_ref();

    while let Some((left, right)) = back.split_once('\\') {
        (front, back) = match (front, unescape_char(right)) {
            (Front::Skipped(skipped), None) => (
                Front::Skipped(skipped + left.len() + '\\'.len_utf8()),
                right,
            ),
            (Front::Skipped(skipped), Some((ch, right))) => {
                let mut s = String::with_capacity(escaped.len());
                s.push_str(&escaped[..skipped + left.len()]);
                s.push(ch);
                (Front::Unescaped(s), right)
            }
            (Front::Unescaped(mut s), unescaped_char) => {
                let (ch, right) = unescaped_char.unwrap_or(('\\', right));
                s.push_str(left);
                s.push(ch);
                (Front::Unescaped(s), right)
            }
        };
    }

    match front {
        Front::Skipped(_) => escaped,
        Front::Unescaped(mut s) => {
            s.push_str(back);
            Cow::Owned(s)
        }
    }
}

fn unescape_char(s: &str) -> Option<(char, &str)> {
    let mut chars = s.chars();
    Some(match chars.next()? {
        'a' => ('\x07', chars.as_str()),  // Alert (Beep, Bell)
        'b' => ('\x08', chars.as_str()),  // Backspace
        'e' => ('\x1B', chars.as_str()),  // Escape character
        'f' => ('\x0C', chars.as_str()),  // Formfeed Page Break
        'n' => ('\x0A', chars.as_str()),  // Newline (Line Feed)
        'r' => ('\x0D', chars.as_str()),  // Carriage Return
        't' => ('\x09', chars.as_str()),  // Horizontal Tab
        'v' => ('\x0B', chars.as_str()),  // Vertical Tab
        '\\' => ('\x5C', chars.as_str()), // Backslash
        '\'' => ('\x27', chars.as_str()), // Apostrophe or single quotation mark
        '"' => ('\x22', chars.as_str()),  // Double quotation mark
        '?' => ('\x3F', chars.as_str()),  // Question mark
        '0'..='7' => parse_octal(s)?,
        'x' => parse_hex(chars.as_str())?,
        'u' => parse_unicode::<4>(chars.as_str())?,
        'U' => parse_unicode::<8>(chars.as_str())?,

        _ => return None,
    })
}

fn parse_octal(s: &str) -> Option<(char, &str)> {
    parse_digits::<8, 3>(s).map(|(ch, _, len)| (ch, &s[len..]))
}

fn parse_hex(s: &str) -> Option<(char, &str)> {
    parse_digits::<16, { usize::MAX }>(s).map(|(ch, _, len)| (ch, &s[len..]))
}

fn parse_unicode<const DIGITS: usize>(s: &str) -> Option<(char, &str)> {
    match parse_digits::<16, DIGITS>(s)? {
        (ch, digits, len) if digits == DIGITS => Some((ch, &s[len..])),
        _ => None,
    }
}

fn parse_digits<const RADIX: u32, const MAX_DIGITS: usize>(
    s: &str,
) -> Option<(char, usize, usize)> {
    let mut agg = 0u32;
    let mut digits = 0;
    let mut len = 0;

    for (idx, ch) in s.char_indices().take(MAX_DIGITS) {
        match ch.to_digit(RADIX) {
            Some(digit) => {
                agg = agg.checked_mul(RADIX)? + digit;
                digits += 1;
                len = idx + ch.len_utf8();
            }
            None => break,
        }
    }

    if len == 0 {
        None
    } else {
        Some((char::from_u32(agg)?, digits, len))
    }
}

#[cfg(test)]
mod tests {
    use cool_asserts::assert_matches;

    use super::*;

    #[test]
    fn unescape_c_string() {
        use super::unescape_c_string as under_test;
        use Cow::Borrowed as B;

        // Basic unescaped strings
        assert_matches!(under_test(r"hello world"), B("hello world"));
        assert_eq!(under_test(r"hello, world!"), B("hello, world!"));

        // Strings with special characters that don't need to be escaped
        assert_eq!(under_test(r"hello ~ world"), B("hello ~ world"));
        assert_eq!(under_test(r"hello / world"), B("hello / world"));
        assert_eq!(under_test(r"hello : world"), B("hello : world"));

        // Strings with escape sequences that shouldn't be interpreted
        assert_eq!(under_test(r"hello \\n world"), B("hello \\n world"));
        assert_eq!(under_test(r"hello \a world"), "hello \x07 world");

        // Mix of escaped and unescaped characters
        assert_eq!(under_test(r"hello \t \x41 world"), "hello \t A world");

        // Single-character escapes
        assert_eq!(under_test(r"hello\nworld"), "hello\nworld");
        assert_eq!(under_test(r"hello\tworld"), "hello\tworld");
        assert_eq!(under_test(r"hello\\world"), "hello\\world");
        assert_eq!(under_test(r"hello\'world"), "hello'world");
        assert_eq!(under_test(r#"hello\"world"#), r#"hello"world"#);

        // Null character
        assert_eq!(under_test(r"hello\0world"), "hello\0world");

        // Octal escapes
        assert_eq!(under_test(r"hello\0world"), "hello\0world");
        assert_eq!(under_test(r"hello\77world"), "hello\x3Fworld");
        assert_eq!(under_test(r"hello\377world"), "hello\u{FF}world");
        assert_eq!(under_test(r"hello\141\157\137world"), "helloao_world");
        assert_eq!(under_test(r"hello\3771world"), "hello\u{FF}1world");
        assert_eq!(under_test(r"hello\888world"), B(r"hello\888world"));

        // Hexadecimal escapes
        assert_eq!(under_test(r"hello\x41world"), "helloAworld");
        assert_eq!(under_test(r"hello\x7Eworld"), "hello~world");
        assert_eq!(under_test(r"hello\x00world"), "hello\0world");
        assert_eq!(under_test(r"hello\xFFworld"), "hello\u{FF}world");
        assert_eq!(under_test(r"hello\x1F600world"), "hello😀world");
        assert_eq!(
            under_test(r"hello\xFFFFFFFFworld"),
            B(r"hello\xFFFFFFFFworld")
        );

        // Unicode escapes
        assert_eq!(under_test(r"hello\u20world"), B(r"hello\u20world"));
        assert_eq!(under_test(r"hello\u0020world"), "hello world");
        assert_eq!(under_test(r"hello\U0001F600world"), "hello😀world");

        // Trailing backslash
        assert_eq!(under_test(r"hello\x20world\"), r"hello world\");
    }

    #[test]
    fn unescape_record() {
        let input = Record::new(r"foo\tbar");
        let under_test = Unescape {
            from: None,
            as_: String::from("out"),
        };

        let output = under_test
            .process(input)
            .expect("no error")
            .expect("a record");
        let val = operator::get_input(
            &output,
            &Some(Expr::NestedColumn {
                head: String::from("out"),
                rest: Vec::new(),
            }),
        );

        assert_matches!(val, Ok(val) => assert_eq!(val.as_ref(), "foo\tbar"));
    }
}
