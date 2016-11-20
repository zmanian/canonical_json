use std::{cmp, io, str};

use serde::iter::LineColIterator;

use super::error::{Error, SyntaxError};

/// Trait used by the deserializer for iterating over input. This is manually
/// "specialized" for iterating over &[u8]. Once feature(specialization) is
/// stable we can use actual specialization.
pub trait Read {
    fn next(&mut self) -> io::Result<Option<u8>>;
    fn peek(&mut self) -> io::Result<Option<u8>>;

    /// Only valid after a call to peek(). Discards the peeked byte.
    fn discard(&mut self);

    /// Position of the most recent call to next().
    ///
    /// The most recent call was probably next() and not peek(), but this method
    /// should try to return a sensible result if the most recent call was
    /// actually peek() because we don't always know.
    ///
    /// Only called in case of an error, so performance is not important.
    fn position(&self) -> Position;

    /// Position of the most recent call to peek().
    ///
    /// The most recent call was probably peek() and not next(), but this method
    /// should try to return a sensible result if the most recent call was
    /// actually next() because we don't always know.
    ///
    /// Only called in case of an error, so performance is not important.
    fn peek_position(&self) -> Position;

    /// Assumes the previous byte was a quotation mark. Parses a JSON-escaped
    /// string until the next quotation mark using the given scratch space if
    /// necessary. The scratch space is initially empty.
    fn parse_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>
    ) -> Result<&'s str, Error>;
}

pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub struct IteratorRead<Iter>
    where Iter: Iterator<Item = io::Result<u8>>,
{
    iter: LineColIterator<Iter>,
    /// Temporary storage of peeked byte.
    ch: Option<u8>,
}

/// Specialization for Iter=&[u8]. This is more efficient than other iterators
/// because peek() can be read-only and we can compute line/col position only if
/// an error happens.
pub struct SliceRead<'a> {
    slice: &'a [u8],
    /// Index of the *next* byte that will be returned by next() or peek().
    index: usize,
}

/// Elide UTF-8 checks by assuming that the input is valid UTF-8.
pub struct StrRead<'a> {
    delegate: SliceRead<'a>,
}

//////////////////////////////////////////////////////////////////////////////

impl<Iter> IteratorRead<Iter>
    where Iter: Iterator<Item = io::Result<u8>>,
{
    pub fn new(iter: Iter) -> Self {
        IteratorRead {
            iter: LineColIterator::new(iter),
            ch: None,
        }
    }
}

impl<Iter> Read for IteratorRead<Iter>
    where Iter: Iterator<Item = io::Result<u8>>,
{
    #[inline]
    fn next(&mut self) -> io::Result<Option<u8>> {
        match self.ch.take() {
            Some(ch) => Ok(Some(ch)),
            None => {
                match self.iter.next() {
                    Some(Err(err)) => Err(err),
                    Some(Ok(ch)) => Ok(Some(ch)),
                    None => Ok(None),
                }
            }
        }
    }

    #[inline]
    fn peek(&mut self) -> io::Result<Option<u8>> {
        match self.ch {
            Some(ch) => Ok(Some(ch)),
            None => {
                match self.iter.next() {
                    Some(Err(err)) => Err(err),
                    Some(Ok(ch)) => {
                        self.ch = Some(ch);
                        Ok(self.ch)
                    }
                    None => Ok(None),
                }
            }
        }
    }

    #[inline]
    fn discard(&mut self) {
        self.ch = None;
    }

    fn position(&self) -> Position {
        Position {
            line: self.iter.line(),
            column: self.iter.col(),
        }
    }

    fn peek_position(&self) -> Position {
        // The LineColIterator updates its position during peek() so it has the
        // right one here.
        self.position()
    }

    fn parse_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>
    ) -> Result<&'s str, Error> {
        loop {
            let ch = match try!(self.next().map_err(Error::Io)) {
                Some(ch) => ch,
                None => {
                    return error(self, SyntaxError::EOFWhileParsingString);
                }
            };
            if !ESCAPE[ch as usize] {
                scratch.push(ch);
                continue;
            }
            match ch {
                b'"' => {
                    return as_str(self, scratch);
                }
                b'\\' => {
                    try!(parse_escape(self, scratch));
                }
                _ => {
                    return error(self, SyntaxError::InvalidUnicodeCodePoint);
                }
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

impl<'a> SliceRead<'a> {
    pub fn new(slice: &'a [u8]) -> Self {
        SliceRead {
            slice: slice,
            index: 0,
        }
    }

    fn position_of_index(&self, i: usize) -> Position {
        let mut pos = Position {
            line: 1,
            column: 0,
        };
        for ch in &self.slice[..i] {
            match *ch {
                b'\n' => {
                    pos.line += 1;
                    pos.column = 0;
                }
                _ => {
                    pos.column += 1;
                }
            }
        }
        pos
    }

    /// The big optimization here over IteratorRead is that if the string
    /// contains no backslash escape sequences, the returned &str is a slice of
    /// the raw JSON data so we avoid copying into the scratch space.
    fn parse_str_bytes<'s, T, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        result: F
    ) -> Result<T, Error>
        where T: 's,
              F: FnOnce(&'s Self, &'s [u8]) -> Result<T, Error>,
    {
        // Index of the first byte not yet copied into the scratch space.
        let mut start = self.index;

        loop {
            while self.index < self.slice.len() &&
                  !ESCAPE[self.slice[self.index] as usize] {
                self.index += 1;
            }
            if self.index == self.slice.len() {
                return error(self, SyntaxError::EOFWhileParsingString);
            }
            match self.slice[self.index] {
                b'"' => {
                    let string = if scratch.is_empty() {
                        // Fast path: return a slice of the raw JSON without any
                        // copying.
                        &self.slice[start..self.index]
                    } else {
                        scratch.extend_from_slice(&self.slice[start .. self.index]);
                        // "as &[u8]" is required for rustc 1.8.0
                        scratch as &[u8]
                    };
                    self.index += 1;
                    return result(self, string);
                }
                b'\\' => {
                    scratch.extend_from_slice(&self.slice[start..self.index]);
                    self.index += 1;
                    try!(parse_escape(self, scratch));
                    start = self.index;
                }
                _ => {
                    return error(self, SyntaxError::InvalidUnicodeCodePoint);
                }
            }
        }
    }
}

impl<'a> Read for SliceRead<'a> {
    #[inline]
    fn next(&mut self) -> io::Result<Option<u8>> {
        // `Ok(self.slice.get(self.index).map(|ch| { self.index += 1; *ch }))`
        // is about 10% slower.
        Ok(if self.index < self.slice.len() {
            let ch = self.slice[self.index];
            self.index += 1;
            Some(ch)
        } else {
            None
        })
    }

    #[inline]
    fn peek(&mut self) -> io::Result<Option<u8>> {
        // `Ok(self.slice.get(self.index).map(|ch| *ch))` is about 10% slower
        // for some reason.
        Ok(if self.index < self.slice.len() {
            Some(self.slice[self.index])
        } else {
            None
        })
    }

    #[inline]
    fn discard(&mut self) {
        self.index += 1;
    }

    fn position(&self) -> Position {
        self.position_of_index(self.index)
    }

    fn peek_position(&self) -> Position {
        // Cap it at slice.len() just in case the most recent call was next()
        // and it returned the last byte.
        self.position_of_index(cmp::min(self.slice.len(), self.index + 1))
    }

    fn parse_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>
    ) -> Result<&'s str, Error> {
        self.parse_str_bytes(scratch, as_str)
    }
}

//////////////////////////////////////////////////////////////////////////////

impl<'a> StrRead<'a> {
    pub fn new(s: &'a str) -> Self {
        StrRead {
            delegate: SliceRead::new(s.as_bytes()),
        }
    }
}

impl<'a> Read for StrRead<'a> {
    #[inline]
    fn next(&mut self) -> io::Result<Option<u8>> {
        self.delegate.next()
    }

    #[inline]
    fn peek(&mut self) -> io::Result<Option<u8>> {
        self.delegate.peek()
    }

    #[inline]
    fn discard(&mut self) {
        self.delegate.discard();
    }

    fn position(&self) -> Position {
        self.delegate.position()
    }

    fn peek_position(&self) -> Position {
        self.delegate.peek_position()
    }

    fn parse_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>
    ) -> Result<&'s str, Error> {
        self.delegate.parse_str_bytes(scratch, |_, bytes| {
            // The input is assumed to be valid UTF-8 and the \u-escapes are
            // checked along the way, so don't need to check here.
            Ok(unsafe { str::from_utf8_unchecked(bytes) })
        })
    }
}

//////////////////////////////////////////////////////////////////////////////

const QU: bool = true; // quote \x22
const BS: bool = true; // backslash \x5C
const O: bool = false; // allow unescaped

// Lookup table of bytes that must be escaped. A value of true at index i means
// that byte i requires an escape sequence in the input.
#[cfg_attr(rustfmt, rustfmt_skip)]
static ESCAPE: [bool; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 0
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 1
     O,  O, QU,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 2
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 3
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 4
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, BS,  O,  O,  O, // 5
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 6
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 7
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 8
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // 9
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // A
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // B
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // C
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // D
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // E
     O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O,  O, // F
];

fn error<R: Read, T>(read: &R, reason: SyntaxError) -> Result<T, Error> {
    let pos = read.position();
    Err(Error::Syntax(reason, pos.line, pos.column))
}

fn as_str<'s, R: Read>(read: &R, slice: &'s [u8]) -> Result<&'s str, Error> {
    str::from_utf8(slice)
        .or_else(|_| error(read, SyntaxError::InvalidUnicodeCodePoint))
}

/// Parses a JSON escape sequence and appends it into the scratch space. Assumes
/// the previous byte read was a backslash.
fn parse_escape<R: Read>(read: &mut R, scratch: &mut Vec<u8>) -> Result<(), Error> {
    let ch = match try!(read.next().map_err(Error::Io)) {
        Some(ch) => ch,
        None => {
            return error(read, SyntaxError::EOFWhileParsingString);
        }
    };

    match ch {
        b'"' => scratch.push(b'"'),
        b'\\' => scratch.push(b'\\'),
        _ => {
            return error(read, SyntaxError::InvalidEscape);
        }
    }

    Ok(())
}
