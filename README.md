Canonical JSON Serialization Library
====================================

This crate is a Rust library for parsing and generating the
[JSON](http://json.org) (JavaScript Object Notation) file format. It is built
upon [Serde](https://github.com/serde-rs/serde), a high performance generic
serialization framework.

Installation
============

This crate works with Cargo and can be found on
[crates.io](https://crates.io/crates/canonical_json) with a `Cargo.toml` like:

```toml
[dependencies]
serde = "0.8"
canonical_json = "0.8"
```

Using Canonical JSON
====================

`canonical_json` is very simple to use out of the box:

```rust
extern crate serde;
extern crate canonical_json;

use canonical_json::Map;

fn main() {
    let mut map = Map::new();
    map.insert("x".to_string(), 1.0);
    map.insert("y".to_string(), 2.0);

    let s = canonical_json::to_string(&map).unwrap();
    assert_eq!(s, "{\"x\":1.0,\"y\":2.0}");

    let deserialized_map: Map<String, f64> = canonical_json::from_str(&s).unwrap();
    assert_eq!(map, deserialized_map);
}
```

It also can be used with Serde's automatic serialization library,
`serde_derive`. First add this to `Cargo.toml`:

```toml
[dependencies]
...
serde = "0.8"
serde_derive = "0.8"
...
```

Then run:

```rust
#![feature(proc_macro)]

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate canonical_json;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Point {
    x: f64,
    y: f64,
}

fn main() {
    let point = Point { x: 1.0, y: 2.0 };

    let s = canonical_json::to_string(&point).unwrap();
    assert_eq!(s, "{\"x\":1.0,\"y\":2.0}");

    let deserialized_point: Point = canonical_json::from_str(&s).unwrap();
    assert_eq!(point, deserialized_point);
}
```
