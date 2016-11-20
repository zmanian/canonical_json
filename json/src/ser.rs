//! Serialize JSON values into bytes.
//!
//! This module provides for JSON serialization with the type `Serializer`.

use std::io;
use std::num::FpCategory;

use serde::ser;
use super::error::{Error, ErrorCode};

use itoa;
use dtoa;

/// A structure for serializing Rust values into JSON.
pub struct Serializer<W>(pub W);

#[doc(hidden)]
#[derive(Eq, PartialEq)]
pub enum State {
    Empty,
    First,
    Rest,
}

#[doc(hidden)]
#[derive(Eq, PartialEq)]
pub struct MapState {
    state: State,
    cur_key: Option<String>,
}

impl<W> ser::Serializer for Serializer<W>
    where W: io::Write,
{
    type Error = Error;

    type SeqState = State;
    type TupleState = State;
    type TupleStructState = State;
    type TupleVariantState = State;
    type MapState = MapState;
    type StructState = MapState;
    type StructVariantState = MapState;

    #[inline]
    fn serialize_bool(&mut self, value: bool) -> Result<(), Error> {
        if value {
            self.0.write_all(b"true").map_err(From::from)
        } else {
            self.0.write_all(b"false").map_err(From::from)
        }
    }

    #[inline]
    fn serialize_isize(&mut self, value: isize) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_i8(&mut self, value: i8) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_i16(&mut self, value: i16) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_i32(&mut self, value: i32) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_i64(&mut self, value: i64) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_usize(&mut self, value: usize) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_u8(&mut self, value: u8) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_u16(&mut self, value: u16) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_u32(&mut self, value: u32) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_u64(&mut self, value: u64) -> Result<(), Error> {
        itoa::write(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_f32(&mut self, value: f32) -> Result<(), Error> {
        fmt_f32_or_null(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_f64(&mut self, value: f64) -> Result<(), Error> {
        fmt_f64_or_null(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_char(&mut self, value: char) -> Result<(), Error> {
        escape_char(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_str(&mut self, value: &str) -> Result<(), Error> {
        escape_str(&mut self.0, value).map_err(From::from)
    }

    #[inline]
    fn serialize_bytes(&mut self, value: &[u8]) -> Result<(), Error> {
        let mut state = try!(self.serialize_seq(Some(value.len())));
        for byte in value {
            try!(self.serialize_seq_elt(&mut state, byte));
        }
        self.serialize_seq_end(state)
    }

    #[inline]
    fn serialize_unit(&mut self) -> Result<(), Error> {
        self.0.write_all(b"null").map_err(From::from)
    }

    #[inline]
    fn serialize_unit_struct(&mut self, _name: &'static str) -> Result<(), Error> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_unit_variant(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        variant: &'static str
    ) -> Result<(), Error> {
        self.serialize_str(variant)
    }

    /// Serialize newtypes without an object wrapper.
    #[inline]
    fn serialize_newtype_struct<T>(
        &mut self,
        _name: &'static str,
        value: T
    ) -> Result<(), Error>
        where T: ser::Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_newtype_variant<T>(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        variant: &'static str,
        value: T
    ) -> Result<(), Error>
        where T: ser::Serialize,
    {
        try!(self.0.write_all(b"{"));
        try!(self.serialize_str(variant));
        try!(self.0.write_all(b":"));
        try!(value.serialize(self));
        self.0.write_all(b"}").map_err(From::from)
    }

    #[inline]
    fn serialize_none(&mut self) -> Result<(), Error> {
        self.serialize_unit()
    }

    #[inline]
    fn serialize_some<T>(&mut self, value: T) -> Result<(), Error>
        where T: ser::Serialize,
    {
        value.serialize(self)
    }

    #[inline]
    fn serialize_seq(&mut self, len: Option<usize>) -> Result<State, Error> {
        if len == Some(0) {
            try!(self.0.write_all(b"[]"));
            Ok(State::Empty)
        } else {
            try!(self.0.write_all(b"["));
            Ok(State::First)
        }
    }

    #[inline]
    fn serialize_seq_elt<T: ser::Serialize>(
        &mut self,
        state: &mut State,
        value: T
    ) -> Result<(), Error>
        where T: ser::Serialize,
    {
        if *state != State::First {
            try!(self.0.write_all(b","));
        }
        *state = State::Rest;

        value.serialize(self)
    }

    #[inline]
    fn serialize_seq_end(&mut self, state: State) -> Result<(), Error> {
        match state {
            State::Empty => Ok(()),
            _ => self.0.write_all(b"]").map_err(From::from),
        }
    }

    #[inline]
    fn serialize_seq_fixed_size(&mut self, size: usize) -> Result<State, Error> {
        self.serialize_seq(Some(size))
    }

    #[inline]
    fn serialize_tuple(&mut self, len: usize) -> Result<State, Error> {
        self.serialize_seq(Some(len))
    }

    #[inline]
    fn serialize_tuple_elt<T: ser::Serialize>(
        &mut self,
        state: &mut State,
        value: T
    ) -> Result<(), Error> {
        self.serialize_seq_elt(state, value)
    }

    #[inline]
    fn serialize_tuple_end(&mut self, state: State) -> Result<(), Error> {
        self.serialize_seq_end(state)
    }

    #[inline]
    fn serialize_tuple_struct(
        &mut self,
        _name: &'static str,
        len: usize
    ) -> Result<State, Error> {
        self.serialize_seq(Some(len))
    }

    #[inline]
    fn serialize_tuple_struct_elt<T: ser::Serialize>(
        &mut self,
        state: &mut State,
        value: T
    ) -> Result<(), Error> {
        self.serialize_seq_elt(state, value)
    }

    #[inline]
    fn serialize_tuple_struct_end(&mut self, state: State) -> Result<(), Error> {
        self.serialize_seq_end(state)
    }

    #[inline]
    fn serialize_tuple_variant(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        variant: &'static str,
        len: usize
    ) -> Result<State, Error> {
        try!(self.0.write_all(b"{"));
        try!(self.serialize_str(variant));
        try!(self.0.write_all(b":"));
        self.serialize_seq(Some(len))
    }

    #[inline]
    fn serialize_tuple_variant_elt<T: ser::Serialize>(
        &mut self,
        state: &mut State,
        value: T
    ) -> Result<(), Error> {
        self.serialize_seq_elt(state, value)
    }

    #[inline]
    fn serialize_tuple_variant_end(&mut self, state: State) -> Result<(), Error> {
        try!(self.serialize_seq_end(state));
        self.0.write_all(b"}").map_err(From::from)
    }

    #[inline]
    fn serialize_map(&mut self, len: Option<usize>) -> Result<MapState, Error> {
        if len == Some(0) {
            try!(self.0.write_all(b"{}"));
            Ok(MapState { state: State::Empty, cur_key: None })
        } else {
            try!(self.0.write_all(b"{"));
            Ok(MapState { state: State::First, cur_key: None })
        }
    }

    #[inline]
    fn serialize_map_key<T: ser::Serialize>(
        &mut self,
        state: &mut MapState,
        key: T,
    ) -> Result<(), Error> {
        if state.state != State::First {
            try!(self.0.write_all(b","));
        }

        state.state = State::Rest;
        state.cur_key = {
            let mut key_serializer = AscendingKeySerializer {
                ser: self,
                cur_key: state.cur_key.take(),
            };
            try!(key.serialize(&mut key_serializer));
            key_serializer.cur_key.take()
        };

        self.0.write_all(b":").map_err(From::from)
    }

    #[inline]
    fn serialize_map_value<T: ser::Serialize>(
        &mut self,
        _: &mut MapState,
        value: T,
    ) -> Result<(), Error> {
        value.serialize(self)
    }

    #[inline]
    fn serialize_map_end(&mut self, state: MapState) -> Result<(), Error> {
        match state.state {
            State::Empty => Ok(()),
            _ => self.0.write_all(b"}").map_err(From::from),
        }
    }

    #[inline]
    fn serialize_struct(
        &mut self,
        _name: &'static str,
        len: usize
    ) -> Result<MapState, Error> {
        self.serialize_map(Some(len))
    }

    #[inline]
    fn serialize_struct_elt<V: ser::Serialize>(
        &mut self,
        state: &mut MapState,
        key: &'static str,
        value: V
    ) -> Result<(), Error> {
        try!(self.serialize_map_key(state, key));
        self.serialize_map_value(state, value)
    }

    #[inline]
    fn serialize_struct_end(&mut self, state: MapState) -> Result<(), Error> {
        self.serialize_map_end(state)
    }

    #[inline]
    fn serialize_struct_variant(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        variant: &'static str,
        len: usize
    ) -> Result<MapState, Error> {
        try!(self.0.write_all(b"{"));
        try!(self.serialize_str(variant));
        try!(self.0.write_all(b":"));
        self.serialize_map(Some(len))
    }

    #[inline]
    fn serialize_struct_variant_elt<V: ser::Serialize>(
        &mut self,
        state: &mut MapState,
        key: &'static str,
        value: V
    ) -> Result<(), Error> {
        self.serialize_struct_elt(state, key, value)
    }

    #[inline]
    fn serialize_struct_variant_end(&mut self, state: MapState) -> Result<(), Error> {
        try!(self.serialize_struct_end(state));
        self.0.write_all(b"}").map_err(From::from)
    }
}

struct AscendingKeySerializer<'a, W: 'a> {
    ser: &'a mut Serializer<W>,
    cur_key: Option<String>,
}

impl<'a, W> ser::Serializer for AscendingKeySerializer<'a, W>
    where W: io::Write,
{
    type Error = Error;

    #[inline]
    fn serialize_str(&mut self, value: &str) -> Result<(), Error> {
        match self.cur_key {
            Some(ref cur_key) if value == cur_key => {
                Err(Error::Syntax(ErrorCode::RepeatedKey, 0, 0))
            }
            Some(ref cur_key) if value < cur_key => {
                Err(Error::Syntax(ErrorCode::UnsortedKey, 0, 0))
            }
            _ => {
                self.cur_key = Some(value.to_string());
                self.ser.serialize_str(value)
            }
        }
    }

    type SeqState = ();
    type TupleState = ();
    type TupleStructState = ();
    type TupleVariantState = ();
    type MapState = ();
    type StructState = ();
    type StructVariantState = ();

    fn serialize_bool(&mut self, _value: bool) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_isize(&mut self, _value: isize) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_i8(&mut self, _value: i8) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_i16(&mut self, _value: i16) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_i32(&mut self, _value: i32) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_i64(&mut self, _value: i64) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_usize(&mut self, _value: usize) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_u8(&mut self, _value: u8) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_u16(&mut self, _value: u16) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_u32(&mut self, _value: u32) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_u64(&mut self, _value: u64) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_f32(&mut self, _value: f32) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_f64(&mut self, _value: f64) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_char(&mut self, _value: char) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_bytes(&mut self, _value: &[u8]) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_unit(&mut self) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_unit_struct(&mut self, _name: &'static str) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_unit_variant(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        _variant: &'static str
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_newtype_struct<T>(
        &mut self,
        _name: &'static str,
        _value: T
    ) -> Result<(), Error>
        where T: ser::Serialize,
    {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_newtype_variant<T>(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        _variant: &'static str,
        _value: T
    ) -> Result<(), Error>
        where T: ser::Serialize,
    {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_none(&mut self) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_some<T>(&mut self, _value: T) -> Result<(), Error>
        where T: ser::Serialize,
    {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_seq(&mut self, _len: Option<usize>) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_seq_elt<T: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _value: T
    ) -> Result<(), Error>
        where T: ser::Serialize,
    {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_seq_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_seq_fixed_size(&mut self, _size: usize) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple(&mut self, _len: usize) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_elt<T: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _value: T
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_struct(
        &mut self,
        _name: &'static str,
        _len: usize
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_struct_elt<T: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _value: T
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_struct_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_variant(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        _variant: &'static str,
        _len: usize
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_variant_elt<T: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _value: T
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_tuple_variant_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_map(&mut self, _len: Option<usize>) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_map_key<T: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _key: T,
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_map_value<T: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _value: T,
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_map_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_struct(
        &mut self,
        _name: &'static str,
        _len: usize
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_struct_elt<V: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _key: &'static str,
        _value: V
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_struct_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_struct_variant(
        &mut self,
        _name: &'static str,
        _variant_index: usize,
        _variant: &'static str,
        _len: usize
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_struct_variant_elt<V: ser::Serialize>(
        &mut self,
        _state: &mut (),
        _key: &'static str,
        _value: V
    ) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }

    fn serialize_struct_variant_end(&mut self, _state: ()) -> Result<(), Error> {
        Err(Error::Syntax(ErrorCode::KeyMustBeAString, 0, 0))
    }
}

/// Serializes and escapes a `&str` into a JSON string.
pub fn escape_str<W>(wr: &mut W, value: &str) -> Result<(), Error>
    where W: io::Write,
{
    let bytes = value.as_bytes();

    try!(wr.write_all(b"\""));

    let mut start = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        let escape = ESCAPE[byte as usize];
        if escape == 0 {
            continue;
        }

        if start < i {
            try!(wr.write_all(&bytes[start..i]));
        }

        try!(wr.write_all(&[b'\\', escape]));

        start = i + 1;
    }

    if start != bytes.len() {
        try!(wr.write_all(&bytes[start..]));
    }

    try!(wr.write_all(b"\""));
    Ok(())
}

const QU: u8 = b'"';  // \x22
const BS: u8 = b'\\'; // \x5C

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
#[cfg_attr(rustfmt, rustfmt_skip)]
static ESCAPE: [u8; 256] = [
    //  1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 0
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 1
    0,  0, QU,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 2
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 3
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 4
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, BS,  0,  0,  0, // 5
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 6
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 7
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 8
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 9
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // A
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // B
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // C
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // D
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // E
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // F
];

#[inline]
fn escape_char<W>(wr: &mut W, value: char) -> Result<(), Error>
    where W: io::Write,
{
    // FIXME: this allocation is required in order to be compatible with stable
    // rust, which doesn't support encoding a `char` into a stack buffer.
    let mut s = String::new();
    s.push(value);
    escape_str(wr, &s)
}

fn fmt_f32_or_null<W>(wr: &mut W, value: f32) -> Result<(), Error>
    where W: io::Write,
{
    match value.classify() {
        FpCategory::Nan | FpCategory::Infinite => try!(wr.write_all(b"null")),
        _ => try!(dtoa::write(wr, value)),
    }

    Ok(())
}

fn fmt_f64_or_null<W>(wr: &mut W, value: f64) -> Result<(), Error>
    where W: io::Write,
{
    match value.classify() {
        FpCategory::Nan | FpCategory::Infinite => try!(wr.write_all(b"null")),
        _ => try!(dtoa::write(wr, value)),
    }

    Ok(())
}

/// Encode the specified struct into a json `[u8]` writer.
#[inline]
pub fn to_writer<W: ?Sized, T>(writer: &mut W, value: &T) -> Result<(), Error>
    where W: io::Write,
          T: ser::Serialize,
{
    let mut ser = Serializer(writer);
    try!(value.serialize(&mut ser));
    Ok(())
}

/// Encode the specified struct into a json `[u8]` buffer.
#[inline]
pub fn to_vec<T>(value: &T) -> Result<Vec<u8>, Error>
    where T: ser::Serialize,
{
    // We are writing to a Vec, which doesn't fail. So we can ignore
    // the error.
    let mut writer = Vec::with_capacity(128);
    try!(to_writer(&mut writer, value));
    Ok(writer)
}

/// Encode the specified struct into a json `String` buffer.
#[inline]
pub fn to_string<T>(value: &T) -> Result<String, Error>
    where T: ser::Serialize,
{
    let vec = try!(to_vec(value));
    let string = unsafe {
        // We do not emit invalid UTF-8.
        String::from_utf8_unchecked(vec)
    };
    Ok(string)
}
