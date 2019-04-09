//! Serialization and deserialization for Canonical JSON.
//!
//! # What is Canonical JSON?
//!
//! Canonical JSON is a variant of JSON in which values each have a single,
//! unambiguous serialized form. It provides meaningful and repeatable hashes
//! of encoded data. Canonical JSON can be parsed by regular JSON parsers provided
//! that no control characters appear in strings.
//!
//! Data types that can be represented include booleans, integers, strings,
//! arrays, maps and null values. See [`Value`] for more details.
//!
//! [`Value`]: value/enum.Value.html
//!
//! ## Compared to JSON
//!
//! - Whitespace between tokens is disallowed. Leading and trailing whitespace
//!   is likewise disallowed.
//! - Floating point numbers, exponents and "minus zero" are all disallowed.
//! - Object keys must appear in lexiographical order and must not be repeated.
//! - Strings are uninterpreted bytes, with the only escaped byte values being
//!   backslash and quote. Escaping is mandatory for those two characters.
//! - String contents are not guaranteed be parsable as UTF-8. Be aware that
//!   encoded data may contain embedded control characters and nulls.
//!
//! *Note:* This library deviates from the spec by additionally requiring that
//! strings are, in fact, valid UTF-8. This is a convenience tradeoff so that
//! users get to handle strings directly rather than converting back-and-forth
//! between raw bytes.
//!
//! ## Example
//!
//! This JSON value:
//!
//! ```json
//! {
//!   "foo": "bar",
//!   "abc": 9e3,
//!   "snowman": "\u2603",
//!   "zoo":
//!     [
//!       "zorilla",
//!       "anteater"
//!     ]
//! }
//! ```
//!
//! becomes this in Canonical JSON:
//!
//! ```json
//! {"abc":9000,"foo":"bar","snowman":"☃","zoo":["zorilla","anteater"]}
//! ```
//!
//! # Type-based serialization and deserialization
//!
//! Structs and enums can be serialized and deserialized to/from Canonical JSON
//! without writing boilerplate code. To do this, it must implement the
//! [`Serialize`] and [`Deserialize`] traits. Serde provides provides an
//! annotation to automatically derive these traits.
//!
//! To derive [`Serialize`] and [`Deserialize`], add this to your crate root:
//!
//! [`Serialize`]: ../serde/ser/trait.Serialize.html
//! [`Deserialize`]: ../serde/de/trait.Deserialize.html
//!
//! ```ignore
//!
//! #[macro_use]
//! extern crate serde_derive;
//! # fn main() {}
//! ```
//!
//! then annotate your data structure like this:
//!
//! ```ignore
//! # #[macro_use]
//! # extern crate serde_derive;
//! #[derive(Serialize, Deserialize)]
//! struct Point {
//!     x: i64,
//!     y: i64,
//! }
//!
//! # fn main() {}
//! ```
//!
//! *Note:* Struct fields must be defined in lexiographical order when deriving
//! [`Serialize`].
//!
//! To customize how a data structure is serialized, for example by renaming
//! fields, see the [Serde documentation on attributes].
//!
//! [Serde documentation on attributes]: https://serde.rs/attributes.html
//!
//! # Examples of use
//!
//! ## Serializing and deserializing a struct
//!
//! ```ignore
//! #[macro_use]
//! extern crate serde_derive;
//! extern crate canonical_json;
//!
//! #[derive(Debug, Serialize, Deserialize)]
//! struct Point {
//!     x: i64,
//!     y: i64,
//! }
//!
//! fn main() {
//!     let point = Point { x: 1, y: 2 };
//!
//!     let point_string: String = canonical_json::to_string(&point).unwrap();
//!     println!("{}", point_string);
//!     // {"x":1,"y":2}
//!
//!     let point: Point = canonical_json::from_str(&point_string).unwrap();
//!     println!("{:?}", point);
//!     // Point { x: 1, y: 2 }
//! }
//! ```
//!
//! ## Parsing a `str` into a generic Canonical JSON `Value`
//!
//! ```
//! extern crate canonical_json;
//!
//! use canonical_json::Value;
//!
//! fn main() {
//!     let value: Value = canonical_json::from_str(r#"{"bar":"baz","foo":13}"#).unwrap();
//!     println!("value: {:?}", value);
//!     // value: {"bar":"baz","foo":13}
//!     println!("object? {}", value.is_object());
//!     // object? true
//!
//!     let obj = value.as_object().unwrap();
//!     let foo = obj.get("foo").unwrap();
//!     println!("array? {:?}", foo.as_array());
//!     // array? None
//!     println!("u64? {:?}", foo.as_u64());
//!     // u64? Some(13u64)
//!
//!     for (key, value) in obj.iter() {
//!         println!("{}: {}", key, match *value {
//!             Value::U64(v) => format!("{} (u64)", v),
//!             Value::String(ref v) => format!("{} (string)", v),
//!             _ => format!("other")
//!         });
//!     }
//!     // bar: baz (string)
//!     // foo: 13 (u64)
//! }
//! ```
//!
//! ## Calculating a checksum of a regular JSON document
//!
//! ```ignore
//! extern crate canonical_json;
//! extern crate serde_json;

//! extern crate sha2;
//!
//! use canonical_json as cjson;
//! use serde_json as json;
//! use sha2::{Sha256, Digest};
//!
//! fn main() {
//!     // Whitespace and the order of keys can be changed here
//!     // while the checksum below will stay the same
//!     let json_str: &'static str = r#"
//!         {
//!             "when you press": {
//!                 "a": "parachute goes up",
//!                 "b": "parachute turns green"
//!             }
//!         }
//!     "#;
//!
//!     let value: json::Value = json::from_str(json_str).unwrap();
//!     let canonical_value: cjson::Value = cjson::Value::from(value);
//!     let canonical_json_str: String = cjson::to_string(&canonical_value).unwrap();
//!     let checksum = Sha256::digest(canonical_json_str.as_bytes());
//!     println!("{}", hex_from_bytes(checksum.as_ref()));
//!     // 8b3199db6006876d3ac0d9e6078090c87e96ba4ba2c241e27e3e44e2bb102ce1
//! }
//!
//! fn hex_from_bytes(bytes: &[u8]) -> String {
//!     use std::fmt::Write;
//!
//!     let mut hex = String::new();
//!     for &byte in bytes {
//!         write!(&mut hex, "{:02x}", byte).unwrap();
//!     }
//!     hex
//! }
//! ```

extern crate num_traits;
extern crate core;
#[macro_use]
extern crate serde;
extern crate serde_json;
extern crate itoa;
extern crate dtoa;
#[cfg(test)]
#[macro_use]
extern crate serde_derive;

pub use self::de::{Deserializer, StreamDeserializer, from_iter, from_reader,
                   from_slice, from_str};
pub use self::error::{Error, SyntaxError};
pub use self::ser::{Serializer, to_string, to_vec, to_writer};
pub use self::value::{Value, from_value, to_value};

pub mod de;
pub mod error;
pub mod ser;
pub mod value;

mod read;

#[cfg(test)]
mod tests;
