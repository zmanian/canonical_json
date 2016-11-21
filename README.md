# canonical_json

Rust library for serializing and deserializing Canonical JSON.

- [Documentation](https://vtllf.org/rustdoc/canonical_json/canonical_json/)

## What is Canonical JSON?

Canonical JSON is a subset of JSON in which values each have a single,
unambiguous serialized form. It provides meaningful and repeatable hashes
of encoded data. Canonical JSON is parsable with any full JSON parser.

Compared to JSON:

- Whitespace between tokens is disallowed. Leading and trailing whitespace
  is likewise disallowed.
- Floating point numbers, exponents and "minus zero" are all disallowed.
- Object keys must appear in lexiographical order and must not be repeated.
- Strings are uninterpreted bytes, with the only escaped byte values being
  backslash and quote. Escaping is mandatory for those two characters.
- String contents are not guaranteed be parsable as UTF-8. Be aware that
  encoded data may contain embedded control characters and nulls.

Full grammar in [spec.txt](spec.txt).

*Note:* This library deviates from the spec by additionally requiring that
strings are, in fact, valid UTF-8. This is a convenience tradeoff so that
users get to handle strings directly rather than converting back-and-forth
between raw bytes.

### Example

This JSON value:

```json
{
  "foo": "bar",
  "abc": 9e3,
  "snowman": "\u2603",
  "zoo":
    [
      "zorilla",
      "anteater"
    ]
}

```

becomes this in Canonical JSON:

```json
{"abc":9000,"foo":"bar","snowman":"☃","zoo":["zorilla","anteater"]}

```

## Usage

This crate is currently only available on nightly Rust.

Add this to your `Cargo.toml`:

```toml
[dependencies]
canonical_json = "0.8"
serde_derive = "0.8" # Optional, for deriving Serialize and Deserialize
serde = "0.8"        # Optional, for deriving Serialize and Deserialize
serde_json = "0.8"   # Optional, for converting to/from regular JSON
```

and this to your crate root:

```rust
extern crate canonical_json;
```

## Type-based serialization and deserialization

Structs and enums can be serialized and deserialized to/from Canonical JSON
without writing boilerplate code. To do this, it must implement the
`Serialize` and `Deserialize` traits. Serde provides provides an
annotation to automatically derive these traits.

To derive `Serialize` and `Deserialize`, add this to your crate root:

```rust
#![feature(proc_macro)]

#[macro_use]
extern crate serde_derive;
```

then annotate your data structure like this:

```rust
#[derive(Serialize, Deserialize)]
struct Point {
    x: i64,
    y: i64,
}

```

*Note:* Struct fields must be defined in lexiographical order when deriving
`Serialize`.

To customize how a data structure is serialized, for example by renaming
fields, see the [Serde documentation on attributes].

[Serde documentation on attributes]: https://serde.rs/attributes.html

## Examples of use

### Serializing and deserializing a struct

```rust
#![feature(proc_macro)]

#[macro_use]
extern crate serde_derive;
extern crate canonical_json;

#[derive(Debug, Serialize, Deserialize)]
struct Point {
    x: i64,
    y: i64,
}

fn main() {
    let point = Point { x: 1, y: 2 };

    let point_string: String = canonical_json::to_string(&point).unwrap();
    println!("{}", point_string);
    // {"x":1,"y":2}

    let point: Point = canonical_json::from_str(&point_string).unwrap();
    println!("{:?}", point);
    // Point { x: 1, y: 2 }
}
```

### Parsing a `str` into a generic Canonical JSON `Value`

```rust
extern crate canonical_json;

use canonical_json::Value;

fn main() {
    let value: Value = canonical_json::from_str(r#"{"bar":"baz","foo":13}"#).unwrap();
    println!("value: {:?}", value);
    // value: {"bar":"baz","foo":13}
    println!("object? {}", value.is_object());
    // object? true

    let obj = value.as_object().unwrap();
    let foo = obj.get("foo").unwrap();
    println!("array? {:?}", foo.as_array());
    // array? None
    println!("u64? {:?}", foo.as_u64());
    // u64? Some(13u64)

    for (key, value) in obj.iter() {
        println!("{}: {}", key, match *value {
            Value::U64(v) => format!("{} (u64)", v),
            Value::String(ref v) => format!("{} (string)", v),
            _ => format!("other")
        });
    }
    // bar: baz (string)
    // foo: 13 (u64)
}
```

### Calculating a checksum of a regular JSON document

```rust
#![feature(try_from)]

extern crate canonical_json;
extern crate serde_json;
extern crate ring;

use std::convert::TryFrom;

use canonical_json as cjson;
use serde_json as json;
use ring::digest;

fn main() {
    // Whitespace and the order of keys can be changed here
    // while the checksum below will stay the same
    let json_str: &'static str = r#"
        {
            "when you press": {
                "a": "parachute goes up",
                "b": "parachute turns green"
            }
        }
    "#;

    let value: json::Value = json::from_str(json_str).unwrap();
    let canonical_value: cjson::Value = cjson::Value::try_from(value).unwrap();
    let canonical_json_str: String = cjson::to_string(&canonical_value).unwrap();
    let checksum = digest::digest(&digest::SHA256, canonical_json_str.as_bytes());
    println!("{}", hex_from_bytes(checksum.as_ref()));
    // 8b3199db606876d3ac0d9e678090c87e96ba4ba2c241e27e3e44e2bb102ce1
}

fn hex_from_bytes(bytes: &[u8]) -> String {
    use std::fmt::Write;

    let mut hex = String::new();
    for &byte in bytes {
        write!(&mut hex, "{:x}", byte).unwrap();
    }
    hex
}
```

## Acknowledgements

Thanks to Erick Tryzelaar, David Tolnay and other contributors to the excellent
[serde_json] library. This library is a derivative of serde_json and shares much
of its code.

[serde_json]: https://github.com/serde-rs/json
