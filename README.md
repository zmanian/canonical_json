# canonical_json

Rust library for serializing and deserializing Canonical JSON.

- [Documentation](https://vtllf.org/rustdoc/canonical_json/)

## What is Canonical JSON?

Canonical JSON is a subset of JSON in which values each have a single,
unambiguous serialized form. It provides meaningful and repeatable hashes
of encoded data.

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

Add this to your `Cargo.toml`:

```toml
[dependencies]
canonical_json = { git = "https://github.com/vtduncan/canonical_json.git" }
serde_derive = "0.8" # Optional, for deriving Serialize and Deserialise
serde = "0.8"        # Optional, for deriving Serialize and Deserialise
serde_json = "0.8"   # Optional, for converting to/from regular JSON

```

and this to your crate root:

```rust
extern crate canonical_json;
```

or add this if you would also like to derive `Serialize` and `Deserialize`:

```rust
#![feature(proc_macro)]

#[macro_use]
extern crate serde_derive;
extern crate canonical_json;
```

## Example

```rust
#![feature(proc_macro)]

#[macro_use]
extern crate serde_derive;
extern crate canonical_json;

use canonical_json::Value;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Point {
    x: i64,
    y: i64,
}

fn main() {
    let point = Point { x: 1, y: 2 };

    // Serialize Point into a String
    let point_string: String = canonical_json::to_string(&point).unwrap();
    assert_eq!(point_string, r#"{"x":1,"y":2}"#);

    // Deserialize String back into a Point
    let point: Point = canonical_json::from_str(&point_string).unwrap();
    assert_eq!(point, Point { x: 1, y: 2 });

    // Deserialize String into a generic JSON Value
    let point_value: Value = canonical_json::from_str(&point_string).unwrap();
    assert!(point_value.is_object());
    assert_eq!(point_value.find("x").unwrap().as_i64().unwrap(), 1);
}

```

## Acknowledgements

Thanks to Erick Tryzelaar, David Tolnay and other contributors to the excellent
[serde_json] library. This library is a derivative of serde_json and shares much
of its code.

[serde_json]: https://github.com/serde-rs/json
