//! Errors encountered during serialization and deserialization.

use std::error;
use std::fmt;
use std::io;

use serde::de;
use serde::ser;

/// Syntax and parsing errors.
#[derive(Clone, PartialEq, Debug)]
pub enum SyntaxError {
    /// Catchall for syntax error messages
    Custom(String),

    /// Incorrect type from value
    InvalidType(de::Type),

    /// Incorrect value
    InvalidValue(String),

    /// Invalid length
    InvalidLength(usize),

    /// Unknown variant in an enum.
    UnknownVariant(String),

    /// Unknown field in struct.
    UnknownField(String),

    /// Struct is missing a field.
    MissingField(&'static str),

    /// EOF while parsing a list.
    EOFWhileParsingList,

    /// EOF while parsing an object.
    EOFWhileParsingObject,

    /// EOF while parsing a string.
    EOFWhileParsingString,

    /// EOF while parsing a JSON value.
    EOFWhileParsingValue,

    /// Expected this character to be a `':'`.
    ExpectedColon,

    /// Expected this character to be either a `','` or a `]`.
    ExpectedListCommaOrEnd,

    /// Expected this character to be either a `','` or a `}`.
    ExpectedObjectCommaOrEnd,

    /// Expected to parse either a `true`, `false`, or a `null`.
    ExpectedSomeIdent,

    /// Expected this character to start a JSON value.
    ExpectedSomeValue,

    /// Unexpected whitespace.
    UnexpectedWhitespace,

    /// Invalid hex escape code.
    InvalidEscape,

    /// Invalid number.
    InvalidNumber,

    /// Number is bigger than the maximum value of its type.
    NumberOutOfRange,

    /// Invalid unicode code point.
    InvalidUnicodeCodePoint,

    /// Object key is not a string.
    KeyMustBeAString,

    /// Object keys must not be repeated.
    RepeatedKey,

    /// Object keys must be listed in lexicographical order.
    UnsortedKey,

    /// Lone leading surrogate in hex escape.
    LoneLeadingSurrogateInHexEscape,

    /// JSON has non-whitespace trailing characters after the value.
    TrailingCharacters,

    /// Unexpected end of hex excape.
    UnexpectedEndOfHexEscape,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SyntaxError::Custom(ref msg) => write!(f, "{}", msg),
            SyntaxError::InvalidType(ref ty) => write!(f, "invalid type: {}", ty),
            SyntaxError::InvalidValue(ref msg) => {
                write!(f, "invalid value: {}", msg)
            }
            SyntaxError::InvalidLength(ref len) => {
                write!(f, "invalid value length {}", len)
            }
            SyntaxError::UnknownVariant(ref variant) => {
                write!(f, "unknown variant \"{}\"", variant)
            }
            SyntaxError::UnknownField(ref field) => {
                write!(f, "unknown field \"{}\"", field)
            }
            SyntaxError::MissingField(field) => {
                write!(f, "missing field \"{}\"", field)
            }
            SyntaxError::EOFWhileParsingList => "EOF while parsing a list".fmt(f),
            SyntaxError::EOFWhileParsingObject => {
                "EOF while parsing an object".fmt(f)
            }
            SyntaxError::EOFWhileParsingString => {
                "EOF while parsing a string".fmt(f)
            }
            SyntaxError::EOFWhileParsingValue => {
                "EOF while parsing a value".fmt(f)
            }
            SyntaxError::ExpectedColon => "expected `:`".fmt(f),
            SyntaxError::ExpectedListCommaOrEnd => "expected `,` or `]`".fmt(f),
            SyntaxError::ExpectedObjectCommaOrEnd => "expected `,` or `}`".fmt(f),
            SyntaxError::ExpectedSomeIdent => "expected ident".fmt(f),
            SyntaxError::ExpectedSomeValue => "expected value".fmt(f),
            SyntaxError::UnexpectedWhitespace => "unexpected whitespace".fmt(f),
            SyntaxError::InvalidEscape => "invalid escape".fmt(f),
            SyntaxError::InvalidNumber => "invalid number".fmt(f),
            SyntaxError::NumberOutOfRange => "number out of range".fmt(f),
            SyntaxError::InvalidUnicodeCodePoint => {
                "invalid unicode code point".fmt(f)
            }
            SyntaxError::KeyMustBeAString => "key must be a string".fmt(f),
            SyntaxError::RepeatedKey => "object contains a repeated key".fmt(f),
            SyntaxError::UnsortedKey => "object keys are unsorted".fmt(f),
            SyntaxError::LoneLeadingSurrogateInHexEscape => {
                "lone leading surrogate in hex escape".fmt(f)
            }
            SyntaxError::TrailingCharacters => "trailing characters".fmt(f),
            SyntaxError::UnexpectedEndOfHexEscape => {
                "unexpected end of hex escape".fmt(f)
            }
        }
    }
}

/// All possible errors that can occur when serializing or deserializing a
/// value into JSON.
#[derive(Debug)]
pub enum Error {
    /// Syntax error with associated line and column number.
    Syntax(SyntaxError, usize, usize),

    /// IO error that occurred when serializing or deserializing a value.
    Io(io::Error),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Syntax(..) => "syntax error",
            Error::Io(ref error) => error::Error::description(error),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Io(ref error) => Some(error),
            _ => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Syntax(ref code, line, col) => {
                write!(fmt, "{} at line {} column {}", code, line, col)
            }
            Error::Io(ref error) => fmt::Display::fmt(error, fmt),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::Io(error)
    }
}

impl From<de::value::Error> for Error {
    fn from(error: de::value::Error) -> Error {
        match error {
            de::value::Error::Custom(e) => {
                Error::Syntax(SyntaxError::Custom(e), 0, 0)
            }
            de::value::Error::EndOfStream => de::Error::end_of_stream(),
            de::value::Error::InvalidType(ty) => {
                Error::Syntax(SyntaxError::InvalidType(ty), 0, 0)
            }
            de::value::Error::InvalidValue(msg) => {
                Error::Syntax(SyntaxError::InvalidValue(msg), 0, 0)
            }
            de::value::Error::InvalidLength(len) => {
                Error::Syntax(SyntaxError::InvalidLength(len), 0, 0)
            }
            de::value::Error::UnknownVariant(variant) => {
                Error::Syntax(SyntaxError::UnknownVariant(variant), 0, 0)
            }
            de::value::Error::UnknownField(field) => {
                Error::Syntax(SyntaxError::UnknownField(field), 0, 0)
            }
            de::value::Error::MissingField(field) => {
                Error::Syntax(SyntaxError::MissingField(field), 0, 0)
            }
        }
    }
}

impl de::Error for Error {
    fn custom<T: Into<String>>(msg: T) -> Error {
        Error::Syntax(SyntaxError::Custom(msg.into()), 0, 0)
    }

    fn end_of_stream() -> Error {
        Error::Syntax(SyntaxError::EOFWhileParsingValue, 0, 0)
    }

    fn invalid_type(ty: de::Type) -> Error {
        Error::Syntax(SyntaxError::InvalidType(ty), 0, 0)
    }

    fn invalid_value(msg: &str) -> Error {
        Error::Syntax(SyntaxError::InvalidValue(msg.to_owned()), 0, 0)
    }

    fn invalid_length(len: usize) -> Error {
        Error::Syntax(SyntaxError::InvalidLength(len), 0, 0)
    }

    fn unknown_variant(variant: &str) -> Error {
        Error::Syntax(SyntaxError::UnknownVariant(String::from(variant)), 0, 0)
    }

    fn unknown_field(field: &str) -> Error {
        Error::Syntax(SyntaxError::UnknownField(String::from(field)), 0, 0)
    }

    fn missing_field(field: &'static str) -> Error {
        Error::Syntax(SyntaxError::MissingField(field), 0, 0)
    }
}

impl ser::Error for Error {
    /// Raised when there is general error when deserializing a type.
    fn custom<T: Into<String>>(msg: T) -> Error {
        Error::Syntax(SyntaxError::Custom(msg.into()), 0, 0)
    }
}
