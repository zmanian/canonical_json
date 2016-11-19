use std::collections::BTreeMap;
use std::fmt::Debug;
use std::i64;
use std::marker::PhantomData;
use std::u64;

use serde::de;
use serde::ser;
use serde::bytes::{ByteBuf, Bytes};

use canonical_json::{
    self,
    StreamDeserializer,
    Value,
    from_iter,
    from_slice,
    from_str,
    from_value,
    to_value,
};

use canonical_json::error::{Error, ErrorCode};

macro_rules! treemap {
    () => {
        BTreeMap::new()
    };
    ($($k:expr => $v:expr),+) => {
        {
            let mut m = BTreeMap::new();
            $(m.insert($k, $v);)+
            m
        }
    };
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
enum Animal {
    Dog,
    Frog(String, Vec<isize>),
    Cat { age: usize, name: String },
    AntHive(Vec<String>),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct Inner {
    a: (),
    b: usize,
    c: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
struct Outer {
    inner: Vec<Inner>,
}

fn test_encode_ok<T>(errors: &[(T, &str)])
    where T: PartialEq + Debug + ser::Serialize,
{
    for &(ref value, out) in errors {
        let out = out.to_string();

        let s = canonical_json::to_string(value).unwrap();
        assert_eq!(s, out);

        let v = to_value(&value);
        let s = canonical_json::to_string(&v).unwrap();
        assert_eq!(s, out);
    }
}

#[test]
fn test_write_null() {
    let tests = &[
        ((), "null"),
    ];
    test_encode_ok(tests);
}

#[test]
fn test_write_u64() {
    let tests = &[
        (3u64, "3"),
        (u64::MAX, &u64::MAX.to_string()),
    ];
    test_encode_ok(tests);
}

#[test]
fn test_write_i64() {
    let tests = &[
        (3i64, "3"),
        (-2i64, "-2"),
        (-1234i64, "-1234"),
        (i64::MIN, &i64::MIN.to_string()),
    ];
    test_encode_ok(tests);
}

#[test]
fn test_encode_nonfinite_float_yields_null() {
    let v = to_value(::std::f64::NAN);
    assert!(v.is_null());

    let v = to_value(::std::f64::INFINITY);
    assert!(v.is_null());

    let v = to_value(::std::f32::NAN);
    assert!(v.is_null());

    let v = to_value(::std::f32::INFINITY);
    assert!(v.is_null());
}

#[test]
fn test_write_str() {
    let tests = &[
        ("", "\"\""),
        ("foo", "\"foo\""),
        ("\\", "\"\\\\\""),
        ("\"", "\"\\\"\""),
        ("\n", "\"\n\""),
        ("\r", "\"\r\""),
        ("\t", "\"\t\""),
        ("\u{2603}", "\"\u{2603}\""),
    ];
    test_encode_ok(tests);
}

#[test]
fn test_write_bool() {
    let tests = &[
        (true, "true"),
        (false, "false"),
    ];
    test_encode_ok(tests);
}

#[test]
fn test_write_list() {
    test_encode_ok(&[
        (vec![], "[]"),
        (vec![true], "[true]"),
        (vec![true, false], "[true,false]"),
    ]);

    test_encode_ok(&[
        (vec![vec![], vec![], vec![]], "[[],[],[]]"),
        (vec![vec![1, 2, 3], vec![], vec![]], "[[1,2,3],[],[]]"),
        (vec![vec![], vec![1, 2, 3], vec![]], "[[],[1,2,3],[]]"),
        (vec![vec![], vec![], vec![1, 2, 3]], "[[],[],[1,2,3]]"),
    ]);

    let long_test_list = Value::Array(vec![
        Value::Bool(false),
        Value::Null,
        Value::Array(vec![Value::String("foo\nbar".to_string()), Value::U64(4)])]);

    test_encode_ok(&[
        (
            long_test_list.clone(),
            "[false,null,[\"foo\nbar\",4]]",
        ),
    ]);
}

#[test]
fn test_write_object() {
    test_encode_ok(&[
        (treemap!(), "{}"),
        (treemap!("a".to_string() => true), "{\"a\":true}"),
        (
            treemap!(
                "a".to_string() => true,
                "b".to_string() => false
            ),
            "{\"a\":true,\"b\":false}"),
    ]);

    test_encode_ok(&[
        (
            treemap![
                "a".to_string() => treemap![],
                "b".to_string() => treemap![],
                "c".to_string() => treemap![]
            ],
            "{\"a\":{},\"b\":{},\"c\":{}}",
        ),
        (
            treemap![
                "a".to_string() => treemap![
                    "a".to_string() => treemap!["a" => vec![1,2,3]],
                    "b".to_string() => treemap![],
                    "c".to_string() => treemap![]
                ],
                "b".to_string() => treemap![],
                "c".to_string() => treemap![]
            ],
            "{\"a\":{\"a\":{\"a\":[1,2,3]},\"b\":{},\"c\":{}},\"b\":{},\"c\":{}}",
        ),
        (
            treemap![
                "a".to_string() => treemap![],
                "b".to_string() => treemap![
                    "a".to_string() => treemap!["a" => vec![1,2,3]],
                    "b".to_string() => treemap![],
                    "c".to_string() => treemap![]
                ],
                "c".to_string() => treemap![]
            ],
            "{\"a\":{},\"b\":{\"a\":{\"a\":[1,2,3]},\"b\":{},\"c\":{}},\"c\":{}}",
        ),
        (
            treemap![
                "a".to_string() => treemap![],
                "b".to_string() => treemap![],
                "c".to_string() => treemap![
                    "a".to_string() => treemap!["a" => vec![1,2,3]],
                    "b".to_string() => treemap![],
                    "c".to_string() => treemap![]
                ]
            ],
            "{\"a\":{},\"b\":{},\"c\":{\"a\":{\"a\":[1,2,3]},\"b\":{},\"c\":{}}}",
        ),
    ]);

    let complex_obj = Value::Object(treemap!(
        "b".to_string() => Value::Array(vec![
            Value::Object(treemap!("c".to_string() => Value::String("\x0c\x1f\r".to_string()))),
            Value::Object(treemap!("d".to_string() => Value::String("".to_string())))
        ])
    ));

    test_encode_ok(&[
        (
            complex_obj.clone(),
            "{\
                \"b\":[\
                    {\"c\":\"\x0c\x1f\r\"},\
                    {\"d\":\"\"}\
                ]\
            }"
        ),
    ]);
}

#[test]
fn test_write_tuple() {
    test_encode_ok(&[
        (
            (5,),
            "[5]",
        ),
    ]);

    test_encode_ok(&[
        (
            (5, (6, "abc")),
            "[5,[6,\"abc\"]]",
        ),
    ]);
}

#[test]
fn test_write_enum() {
    test_encode_ok(&[
        (
            Animal::Dog,
            "\"Dog\"",
        ),
        (
            Animal::Frog("Henry".to_string(), vec![]),
            "{\"Frog\":[\"Henry\",[]]}",
        ),
        (
            Animal::Frog("Henry".to_string(), vec![349]),
            "{\"Frog\":[\"Henry\",[349]]}",
        ),
        (
            Animal::Frog("Henry".to_string(), vec![349, 102]),
            "{\"Frog\":[\"Henry\",[349,102]]}",
        ),
        (
            Animal::Cat { age: 5, name: "Kate".to_string() },
            "{\"Cat\":{\"age\":5,\"name\":\"Kate\"}}"
        ),
        (
            Animal::AntHive(vec!["Bob".to_string(), "Stuart".to_string()]),
            "{\"AntHive\":[\"Bob\",\"Stuart\"]}",
        ),
    ]);
}

#[test]
fn test_write_option() {
    test_encode_ok(&[
        (None, "null"),
        (Some("jodhpurs"), "\"jodhpurs\""),
    ]);

    test_encode_ok(&[
        (None, "null"),
        (Some(vec!["foo", "bar"]), "[\"foo\",\"bar\"]"),
    ]);
}

#[test]
fn test_write_newtype_struct() {
    #[derive(Serialize, PartialEq, Debug)]
    struct Newtype(BTreeMap<String, i32>);

    let inner = Newtype(treemap!(String::from("inner") => 123));
    let outer = treemap!(String::from("outer") => to_value(&inner));

    test_encode_ok(&[
        (inner, r#"{"inner":123}"#),
    ]);

    test_encode_ok(&[
        (outer, r#"{"outer":{"inner":123}}"#),
    ]);
}

fn test_parse_ok<T>(tests: Vec<(&str, T)>)
    where T: Clone + Debug + PartialEq + ser::Serialize + de::Deserialize,
{
    for (s, value) in tests {
        let v: T = from_str(s).unwrap();
        assert_eq!(v, value.clone());

        let v: T = from_slice(s.as_bytes()).unwrap();
        assert_eq!(v, value.clone());

        let v: T = from_iter(s.bytes().map(Ok)).unwrap();
        assert_eq!(v, value.clone());

        // Make sure we can deserialize into a `Value`.
        let json_value: Value = from_str(s).unwrap();
        assert_eq!(json_value, to_value(&value));

        // Make sure we can deserialize from a `Value`.
        let v: T = from_value(json_value.clone()).unwrap();
        assert_eq!(v, value);

        // Make sure we can round trip back to `Value`.
        let json_value2: Value = from_value(json_value.clone()).unwrap();
        assert_eq!(json_value2, json_value);
    }
}

// For testing representations that the deserializer accepts but the serializer
// never generates. These do not survive a round-trip through Value.
fn test_parse_unusual_ok<T>(tests: Vec<(&str, T)>)
    where T: Clone + Debug + PartialEq + ser::Serialize + de::Deserialize,
{
    for (s, value) in tests {
        let v: T = from_str(s).unwrap();
        assert_eq!(v, value.clone());

        let v: T = from_slice(s.as_bytes()).unwrap();
        assert_eq!(v, value.clone());

        let v: T = from_iter(s.bytes().map(Ok)).unwrap();
        assert_eq!(v, value.clone());
    }
}

macro_rules! test_parse_err {
    ($name:ident::<$($ty:ty),*>($arg:expr) => $err:expr) => {
        match ($err, &$name::<$($ty),*>($arg).unwrap_err()) {
            (
                &Error::Syntax(ref expected_code, expected_line, expected_col),
                &Error::Syntax(ref actual_code, actual_line, actual_col),
            ) if expected_code == actual_code
                && expected_line == actual_line
                && expected_col == actual_col => { /* pass */ }
            (expected_err, actual_err) => {
                panic!("unexpected {} error: {}, expected: {}", stringify!($name), actual_err, expected_err)
            }
        }
    };
}

// FIXME (#5527): these could be merged once UFCS is finished.
fn test_parse_err<T>(errors: Vec<(&'static str, Error)>)
    where T: Debug + PartialEq + de::Deserialize,
{
    for &(s, ref err) in &errors {
        test_parse_err!(from_str::<T>(s) => err);
        test_parse_err!(from_slice::<T>(s.as_bytes()) => err);
        test_parse_err!(from_iter::<_, T>(s.bytes().map(Ok)) => err);
    }
}

fn test_parse_slice_err<T>(errors: Vec<(&[u8], Error)>)
    where T: Debug + PartialEq + de::Deserialize,
{
    for &(s, ref err) in &errors {
        test_parse_err!(from_slice::<T>(s) => err);
        test_parse_err!(from_iter::<_, T>(s.iter().cloned().map(Ok)) => err);
    }
}

#[test]
fn test_parse_null() {
    test_parse_err::<()>(vec![
        ("n", Error::Syntax(ErrorCode::ExpectedSomeIdent, 1, 1)),
        ("nul", Error::Syntax(ErrorCode::ExpectedSomeIdent, 1, 3)),
        ("nulla", Error::Syntax(ErrorCode::TrailingCharacters, 1, 5)),
    ]);

    test_parse_ok(vec![
        ("null", ()),
    ]);
}

#[test]
fn test_parse_bool() {
    test_parse_err::<bool>(vec![
        ("t", Error::Syntax(ErrorCode::ExpectedSomeIdent, 1, 1)),
        ("truz", Error::Syntax(ErrorCode::ExpectedSomeIdent, 1, 4)),
        ("f", Error::Syntax(ErrorCode::ExpectedSomeIdent, 1, 1)),
        ("faz", Error::Syntax(ErrorCode::ExpectedSomeIdent, 1, 3)),
        ("truea", Error::Syntax(ErrorCode::TrailingCharacters, 1, 5)),
        ("falsea", Error::Syntax(ErrorCode::TrailingCharacters, 1, 6)),
        (" true ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 1)),
        (" false ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 1)),
    ]);

    test_parse_ok(vec![
        ("true", true),
        ("false", false),
    ]);
}

#[test]
fn test_parse_number_errors() {
    test_parse_err::<i64>(vec![
        ("+", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 1)),
        (".", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 1)),
        ("-", Error::Syntax(ErrorCode::InvalidNumber, 1, 1)),
        ("00", Error::Syntax(ErrorCode::InvalidNumber, 1, 2)),
        ("01", Error::Syntax(ErrorCode::InvalidNumber, 1, 2)),
        ("0x80", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("\\0", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 1)),
        ("-0", Error::Syntax(ErrorCode::InvalidNumber, 1, 2)),
        ("1.", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("1.a", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("1.e1", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("1e", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("1e10", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("1e+", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("1a", Error::Syntax(ErrorCode::TrailingCharacters, 1, 2)),
        ("18446744073709551616", // 2^64
           Error::Syntax(ErrorCode::NumberOutOfRange, 1, 20)),
        ("-9223372036854775809", // -2^63 - 1
           Error::Syntax(ErrorCode::NumberOutOfRange, 1, 20)),
        ("100000000000000000000", // 1e21
           Error::Syntax(ErrorCode::NumberOutOfRange, 1, 21)),
    ]);
}

#[test]
fn test_parse_i64() {
    test_parse_err::<bool>(vec![
        (" -1234 ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 1)),
    ]);

    test_parse_ok(vec![
        ("-2", -2),
        ("-1234", -1234),
        (&i64::MIN.to_string(), i64::MIN),
        (&i64::MAX.to_string(), i64::MAX),
    ]);
}

#[test]
fn test_parse_u64() {
    test_parse_ok(vec![
        ("0", 0u64),
        ("3", 3u64),
        ("1234", 1234),
        (&u64::MAX.to_string(), u64::MAX),
    ]);
}

#[test]
fn test_parse_string() {
    test_parse_err::<String>(vec![
        ("\"", Error::Syntax(ErrorCode::EOFWhileParsingString, 1, 1)),
        ("\"lol", Error::Syntax(ErrorCode::EOFWhileParsingString, 1, 4)),
        (" \"foo\" ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 1)),
        ("\"lol\"a", Error::Syntax(ErrorCode::TrailingCharacters, 1, 6)),
        ("\"\\b\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
        ("\"\\n\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
        ("\"\\r\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
        ("\"\\t\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
        ("\"\\u12ab\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
        ("\"\\uAB12\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
        ("\"\\uD83C\\uDF95\"", Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
    ]);

    test_parse_slice_err::<String>(vec![
        (&[b'"', b'\\', b'n', 159, 146, 150, b'"'],
            Error::Syntax(ErrorCode::InvalidEscape, 1, 3)),
    ]);

    test_parse_ok(vec![
        ("\"\"", "".to_string()),
        ("\"foo\"", "foo".to_string()),
        ("\"\\\"\"", "\"".to_string()),
        ("\"\\\\\"", "\\".to_string()),
        ("\"\n\"", "\n".to_string()),
        ("\"\r\"", "\r".to_string()),
        ("\"\t\"", "\t".to_string()),
        ("\"\u{2603}\"", "\u{2603}".to_string()),
    ]);
}

#[test]
fn test_parse_list() {
    test_parse_err::<Vec<i64>>(vec![
        ("[", Error::Syntax(ErrorCode::EOFWhileParsingList, 1, 1)),
        ("[ ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 2)),
        ("[1", Error::Syntax(ErrorCode::EOFWhileParsingList,  1, 2)),
        ("[1,", Error::Syntax(ErrorCode::EOFWhileParsingValue, 1, 3)),
        ("[1,]", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 4)),
        ("[]a", Error::Syntax(ErrorCode::TrailingCharacters, 1, 3)),
        ("[ ]", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 2)),
        ("[ 1 ]", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 2)),
        ("[1 2]", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 3)),
        ("[1, 2]", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 4)),
        ("[1,2 ]", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 5)),
    ]);

    test_parse_ok(vec![
        ("[]", vec![]),
        ("[null]", vec![()]),
    ]);

    test_parse_ok(vec![
        ("[true]", vec![true]),
    ]);

    test_parse_ok(vec![
        ("[3,1]", vec![3u64, 1]),
    ]);

    test_parse_ok(vec![
        ("[[3],[1,2]]", vec![vec![3u64], vec![1, 2]]),
    ]);

    test_parse_ok(vec![
        ("[1]", (1u64,)),
    ]);

    test_parse_ok(vec![
        ("[1,2]", (1u64, 2u64)),
    ]);

    test_parse_ok(vec![
        ("[1,2,3]", (1u64, 2u64, 3u64)),
    ]);

    test_parse_ok(vec![
        ("[1,[2,3]]", (1u64, (2u64, 3u64))),
    ]);

    let v: () = from_str("[]").unwrap();
    assert_eq!(v, ());
}

#[test]
fn test_parse_object() {
    test_parse_err::<BTreeMap<String, u32>>(vec![
        ("{", Error::Syntax(ErrorCode::EOFWhileParsingObject, 1, 1)),
        ("{ ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 2)),
        ("{1", Error::Syntax(ErrorCode::KeyMustBeAString, 1, 2)),
        ("{ \"a\"", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 2)),
        ("{\"a\"", Error::Syntax(ErrorCode::EOFWhileParsingObject, 1, 4)),
        ("{\"a\" ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 5)),
        ("{\"a\"1", Error::Syntax(ErrorCode::ExpectedColon, 1, 5)),
        ("{\"a\":", Error::Syntax(ErrorCode::EOFWhileParsingValue, 1, 5)),
        ("{\"a\":1", Error::Syntax(ErrorCode::EOFWhileParsingObject, 1, 6)),
        ("{\"a\":1 ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 7)),
        ("{\"a\":\"1\"1", Error::Syntax(ErrorCode::ExpectedObjectCommaOrEnd, 1, 9)),
        ("{\"a\":1,", Error::Syntax(ErrorCode::EOFWhileParsingValue, 1, 7)),
        ("{\"a\":1,}", Error::Syntax(ErrorCode::KeyMustBeAString, 1, 8)),
        ("{}a", Error::Syntax(ErrorCode::TrailingCharacters, 1, 3)),
        ("{ }", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 2)),
        ("{\"a\":1,\"a\":2}", Error::Syntax(ErrorCode::RepeatedKey, 1, 10)),
        ("{\"b\":1,\"a\":2}", Error::Syntax(ErrorCode::UnsortedKey, 1, 10)),
        ("{\"a\":1,\"c\":2,\"b\":3}", Error::Syntax(ErrorCode::UnsortedKey, 1, 16)),
    ]);

    test_parse_err::<BTreeMap<String, BTreeMap<String, i32>>>(vec![
        ("{\"a\":{},\"b\":{\"i\":1},\"c\":{\"z\":1,\"\":2}}", Error::Syntax(ErrorCode::UnsortedKey, 1, 33)),
    ]);

    test_parse_ok(vec![
        ("{}", treemap!()),
        (
            "{\"a\":3}",
            treemap!("a".to_string() => 3u64)
        ),
        (
            "{\"a\":3,\"b\":4}",
            treemap!("a".to_string() => 3, "b".to_string() => 4)
        ),
    ]);

    test_parse_ok(vec![
        (
            "{\"a\":{\"b\":3,\"c\":4}}",
            treemap!(
                "a".to_string() => treemap!(
                    "b".to_string() => 3u64,
                    "c".to_string() => 4
                )
            ),
        ),
    ]);
}

#[test]
fn test_parse_struct() {
    test_parse_err::<Outer>(vec![
        ("5", Error::Syntax(ErrorCode::InvalidType(de::Type::U64), 1, 1)),
        ("\"hello\"", Error::Syntax(ErrorCode::InvalidType(de::Type::Str), 1, 7)),
        ("{\"inner\":true}", Error::Syntax(ErrorCode::InvalidType(de::Type::Bool), 1, 13)),
        ("{}", Error::Syntax(ErrorCode::MissingField("inner"), 1, 2)),
        (r#"{"inner":[{"b":42,"c":[]}]}"#, Error::Syntax(ErrorCode::MissingField("a"), 1, 25)),
    ]);

    test_parse_ok(vec![
        (
            "{\"inner\":[]}",
            Outer {
                inner: vec![]
            },
        ),
        (
            "{\"inner\":[{\"a\":null,\"b\":2,\"c\":[\"abc\",\"xyz\"]}]}",
            Outer {
                inner: vec![
                    Inner { a: (), b: 2, c: vec!["abc".to_string(), "xyz".to_string()] }
                ]
            },
        ),
    ]);

    let v: Outer = from_str("[[[null,2,[\"abc\",\"xyz\"]]]]").unwrap();
    assert_eq!(
        v,
        Outer {
            inner: vec![
                Inner { a: (), b: 2, c: vec!["abc".to_string(), "xyz".to_string()] }
            ],
        }
    );
}

#[test]
fn test_parse_option() {
    test_parse_ok(vec![
        ("null", None::<String>),
        ("\"jodhpurs\"", Some("jodhpurs".to_string())),
    ]);

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    struct Foo {
        x: Option<isize>,
    }

    let value: Foo = from_str("{}").unwrap();
    assert_eq!(value, Foo { x: None });

    test_parse_ok(vec![
        ("{\"x\":null}", Foo { x: None }),
        ("{\"x\":5}", Foo { x: Some(5) }),
    ]);
}

#[test]
fn test_parse_enum_errors() {
    test_parse_err::<Animal>(vec![
        ("{}", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 2)),
        ("[]", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 1)),
        ("\"unknown\"", Error::Syntax(ErrorCode::UnknownVariant("unknown".to_string()), 1, 9)),
        ("{\"unknown\":[]}", Error::Syntax(ErrorCode::UnknownVariant("unknown".to_string()), 1, 10)),
        ("{\"Dog\":", Error::Syntax(ErrorCode::EOFWhileParsingValue, 1, 7)),
        ("{\"Dog\":}", Error::Syntax(ErrorCode::ExpectedSomeValue, 1, 8)),
        ("{\"Dog\":{}}", Error::Syntax(ErrorCode::InvalidType(de::Type::Map), 1, 8)),
        ("{\"Dog\":[0]}", Error::Syntax(ErrorCode::TrailingCharacters, 1, 9)),
        ("\"Frog\"", Error::Syntax(ErrorCode::EOFWhileParsingValue, 1, 6)),
        ("{\"Frog\":{}}", Error::Syntax(ErrorCode::InvalidType(de::Type::Map), 1, 9)),
        ("{\"Cat\":[]}", Error::Syntax(ErrorCode::InvalidLength(0), 1, 9)),
        ("{\"Cat\":[0]}", Error::Syntax(ErrorCode::InvalidLength(1), 1, 10)),
        ("{\"Cat\":[0,\"\",2]}", Error::Syntax(ErrorCode::TrailingCharacters, 1, 13)),
        (
            "{\"Cat\":{\"age\":5,\"foo\":\"bar\",\"name\":\"Kate\"}",
            Error::Syntax(ErrorCode::UnknownField("foo".to_string()), 1, 21)
        ),
    ]);
}

#[test]
fn test_parse_enum() {
    test_parse_ok(vec![
        ("\"Dog\"", Animal::Dog),
        (
            "{\"Frog\":[\"Henry\",[]]}",
            Animal::Frog("Henry".to_string(), vec![]),
        ),
        (
            "{\"Frog\":[\"Henry\",[349,102]]}",
            Animal::Frog("Henry".to_string(), vec![349, 102]),
        ),
        (
            "{\"Cat\":{\"age\":5,\"name\":\"Kate\"}}",
            Animal::Cat { age: 5, name: "Kate".to_string() },
        ),
        (
            "{\"AntHive\":[\"Bob\",\"Stuart\"]}",
            Animal::AntHive(vec!["Bob".to_string(), "Stuart".to_string()]),
        ),
    ]);

    test_parse_unusual_ok(vec![
        ("{\"Dog\":[]}", Animal::Dog),
    ]);

    test_parse_ok(vec![
        (
            "{\"a\":\"Dog\",\"b\":{\"Frog\":[\"Henry\",[]]}}",
            treemap!(
                "a".to_string() => Animal::Dog,
                "b".to_string() => Animal::Frog("Henry".to_string(), vec![])
            )
        ),
    ]);
}

#[test]
fn test_parse_trailing_whitespace() {
    test_parse_err::<[i64; 2]>(vec![
        ("[1,2] ", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 6)),
        ("[1,2]\n", Error::Syntax(ErrorCode::UnexpectedWhitespace, 2, 0)),
        ("[1,2]\t", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 6)),
        ("[1,2]\t \n", Error::Syntax(ErrorCode::UnexpectedWhitespace, 1, 6)),
    ]);
}

#[test]
fn test_missing_option_field() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct Foo {
        x: Option<u32>,
    }

    let value: Foo = from_str("{}").unwrap();
    assert_eq!(value, Foo { x: None });

    let value: Foo = from_str("{\"x\":5}").unwrap();
    assert_eq!(value, Foo { x: Some(5) });

    let value: Foo = from_value(Value::Object(treemap!())).unwrap();
    assert_eq!(value, Foo { x: None });

    let value: Foo = from_value(Value::Object(treemap!(
        "x".to_string() => Value::I64(5)
    ))).unwrap();
    assert_eq!(value, Foo { x: Some(5) });
}

#[test]
fn test_missing_nonoption_field() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct Foo {
        x: u32,
    }

    test_parse_err::<Foo>(vec![
        ("{}", Error::Syntax(ErrorCode::MissingField("x"), 1, 2)),
    ]);
}

#[test]
fn test_missing_renamed_field() {
    #[derive(Debug, PartialEq, Deserialize)]
    struct Foo {
        #[serde(rename="y")]
        x: Option<u32>,
    }

    let value: Foo = from_str("{}").unwrap();
    assert_eq!(value, Foo { x: None });

    let value: Foo = from_str("{\"y\":5}").unwrap();
    assert_eq!(value, Foo { x: Some(5) });

    let value: Foo = from_value(Value::Object(treemap!())).unwrap();
    assert_eq!(value, Foo { x: None });

    let value: Foo = from_value(Value::Object(treemap!(
        "y".to_string() => Value::I64(5)
    ))).unwrap();
    assert_eq!(value, Foo { x: Some(5) });
}

#[test]
fn test_find_path() {
    let obj: Value = canonical_json::from_str(r#"{"x":{"a":1},"y":2}"#).unwrap();

    assert!(obj.find_path(&["x", "a"]).unwrap() == &Value::U64(1));
    assert!(obj.find_path(&["y"]).unwrap() == &Value::U64(2));
    assert!(obj.find_path(&["z"]).is_none());
}

#[test]
fn test_serialize_seq_with_no_len() {
    #[derive(Clone, Debug, PartialEq)]
    struct MyVec<T>(Vec<T>);

    impl<T> ser::Serialize for MyVec<T>
        where T: ser::Serialize,
    {
        #[inline]
        fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
            where S: ser::Serializer,
        {
            let mut state = try!(serializer.serialize_seq(None));
            for elem in &self.0 {
                try!(serializer.serialize_seq_elt(&mut state, elem));
            }
            serializer.serialize_seq_end(state)
        }
    }

    struct Visitor<T> {
        marker: PhantomData<MyVec<T>>,
    }

    impl<T> de::Visitor for Visitor<T>
        where T: de::Deserialize,
    {
        type Value = MyVec<T>;

        #[inline]
        fn visit_unit<E>(&mut self) -> Result<MyVec<T>, E>
            where E: de::Error,
        {
            Ok(MyVec(Vec::new()))
        }

        #[inline]
        fn visit_seq<V>(&mut self, mut visitor: V) -> Result<MyVec<T>, V::Error>
            where V: de::SeqVisitor,
        {
            let mut values = Vec::new();

            while let Some(value) = try!(visitor.visit()) {
                values.push(value);
            }

            try!(visitor.end());

            Ok(MyVec(values))
        }
    }

    impl<T> de::Deserialize for MyVec<T>
        where T: de::Deserialize,
    {
        fn deserialize<D>(deserializer: &mut D) -> Result<MyVec<T>, D::Error>
            where D: de::Deserializer,
        {
            deserializer.deserialize_map(Visitor { marker: PhantomData })
        }
    }

    let mut vec = Vec::new();
    vec.push(MyVec(Vec::new()));
    vec.push(MyVec(Vec::new()));
    let vec: MyVec<MyVec<u32>> = MyVec(vec);

    test_encode_ok(&[
        (
            vec.clone(),
            "[[],[]]",
        ),
    ]);
}

#[test]
fn test_serialize_map_with_no_len() {
    #[derive(Clone, Debug, PartialEq)]
    struct MyMap<K, V>(BTreeMap<K, V>);

    impl<K, V> ser::Serialize for MyMap<K, V>
        where K: ser::Serialize + Ord,
              V: ser::Serialize,
    {
        #[inline]
        fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
            where S: ser::Serializer,
        {
            let mut state = try!(serializer.serialize_map(None));
            for (k, v) in &self.0 {
                try!(serializer.serialize_map_key(&mut state, k));
                try!(serializer.serialize_map_value(&mut state, v));
            }
            serializer.serialize_map_end(state)
        }
    }

    struct Visitor<K, V> {
        marker: PhantomData<MyMap<K, V>>,
    }

    impl<K, V> de::Visitor for Visitor<K, V>
        where K: de::Deserialize + Eq + Ord,
              V: de::Deserialize,
    {
        type Value = MyMap<K, V>;

        #[inline]
        fn visit_unit<E>(&mut self) -> Result<MyMap<K, V>, E>
            where E: de::Error,
        {
            Ok(MyMap(BTreeMap::new()))
        }

        #[inline]
        fn visit_map<Visitor>(&mut self, mut visitor: Visitor) -> Result<MyMap<K, V>, Visitor::Error>
            where Visitor: de::MapVisitor,
        {
            let mut values = BTreeMap::new();

            while let Some((key, value)) = try!(visitor.visit()) {
                values.insert(key, value);
            }

            try!(visitor.end());

            Ok(MyMap(values))
        }
    }

    impl<K, V> de::Deserialize for MyMap<K, V>
        where K: de::Deserialize + Eq + Ord,
              V: de::Deserialize,
    {
        fn deserialize<D>(deserializer: &mut D) -> Result<MyMap<K, V>, D::Error>
            where D: de::Deserializer,
        {
            deserializer.deserialize_map(Visitor { marker: PhantomData })
        }
    }

    let mut map = BTreeMap::new();
    map.insert("a", MyMap(BTreeMap::new()));
    map.insert("b", MyMap(BTreeMap::new()));
    let map: MyMap<_, MyMap<u32, u32>> = MyMap(map);

    test_encode_ok(&[
        (
            map.clone(),
            "{\"a\":{},\"b\":{}}",
        ),
    ]);
}

#[test]
fn test_deserialize_from_stream() {
    use std::net;
    use std::io::Read;
    use std::thread;
    use serde::Deserialize;

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    struct Message {
        message: String,
    }

    let l = net::TcpListener::bind("localhost:20000").unwrap();

    thread::spawn(|| {
        let l = l;
        for stream in l.incoming() {
            let mut stream = stream.unwrap();
            let read_stream = stream.try_clone().unwrap();

            let mut de = canonical_json::Deserializer::new(read_stream.bytes());
            let request = Message::deserialize(&mut de).unwrap();
            let response = Message { message: request.message };
            canonical_json::to_writer(&mut stream, &response).unwrap();
        }
    });

    let mut stream = net::TcpStream::connect("localhost:20000").unwrap();
    let request = Message { message: "hi there".to_string() };
    canonical_json::to_writer(&mut stream, &request).unwrap();

    let mut de = canonical_json::Deserializer::new(stream.bytes());
    let response = Message::deserialize(&mut de).unwrap();

    assert_eq!(request, response);
}

#[test]
fn test_serialize_rejects_non_key_maps() {
    let map = treemap!(
        1 => 2,
        3 => 4
    );

    match canonical_json::to_vec(&map).unwrap_err() {
        canonical_json::Error::Syntax(canonical_json::ErrorCode::KeyMustBeAString, 0, 0) => {}
        _ => panic!("integers used as keys"),
    }
}

#[test]
fn test_bytes_ser() {
    let buf = vec![];
    let bytes = Bytes::from(&buf);
    assert_eq!(canonical_json::to_string(&bytes).unwrap(), "[]".to_string());

    let buf = vec![1, 2, 3];
    let bytes = Bytes::from(&buf);
    assert_eq!(canonical_json::to_string(&bytes).unwrap(), "[1,2,3]".to_string());
}

#[test]
fn test_byte_buf_ser() {
    let bytes = ByteBuf::new();
    assert_eq!(canonical_json::to_string(&bytes).unwrap(), "[]".to_string());

    let bytes = ByteBuf::from(vec![1, 2, 3]);
    assert_eq!(canonical_json::to_string(&bytes).unwrap(), "[1,2,3]".to_string());
}

#[test]
fn test_byte_buf_de() {
    let bytes = ByteBuf::new();
    let v: ByteBuf = canonical_json::from_str("[]").unwrap();
    assert_eq!(v, bytes);

    let bytes = ByteBuf::from(vec![1, 2, 3]);
    let v: ByteBuf = canonical_json::from_str("[1,2,3]").unwrap();
    assert_eq!(v, bytes);
}

#[test]
fn test_json_stream() {
    let stream = "{\"x\":39}{\"x\":40}{\"x\":41}{\"x\":42}".to_string();
    let mut parsed: StreamDeserializer<Value, _> = StreamDeserializer::new(
        stream.as_bytes().iter().map(|byte| Ok(*byte))
    );

    assert_eq!(parsed.next().unwrap().ok().unwrap().find("x").unwrap(),
               &Value::U64(39));
    assert_eq!(parsed.next().unwrap().ok().unwrap().find("x").unwrap(),
               &Value::U64(40));
    assert_eq!(parsed.next().unwrap().ok().unwrap().find("x").unwrap(),
               &Value::U64(41));
    assert_eq!(parsed.next().unwrap().ok().unwrap().find("x").unwrap(),
               &Value::U64(42));
    assert!(parsed.next().is_none());
}

#[test]
fn test_json_stream_truncated() {
    let stream = "{\"x\":40}{\"x\":".to_string();
    let mut parsed: StreamDeserializer<Value, _> = StreamDeserializer::new(
        stream.as_bytes().iter().map(|byte| Ok(*byte))
    );

    assert_eq!(parsed.next().unwrap().ok().unwrap().find("x").unwrap(),
               &Value::U64(40));
    assert!(parsed.next().unwrap().is_err());
    assert!(parsed.next().is_none());
}

#[test]
fn test_json_stream_empty() {
    let stream = "".to_string();
    let mut parsed: StreamDeserializer<Value, _> = StreamDeserializer::new(
        stream.as_bytes().iter().map(|byte| Ok(*byte))
    );

    assert!(parsed.next().is_none());
}

#[test]
fn test_json_pointer() {
    // Test case taken from https://tools.ietf.org/html/rfc6901#page-5
    let data: Value = canonical_json::from_str(r#"{"":0," ":7,"a/b":1,"c%d":2,"e^f":3,"foo":["bar","baz"],"g|h":4,"i\\j":5,"k\"l":6,"m~n":8}"#).unwrap();
    assert_eq!(data.pointer("").unwrap(), &data);
    assert_eq!(data.pointer("/foo").unwrap(),
        &Value::Array(vec![Value::String("bar".to_owned()),
                           Value::String("baz".to_owned())]));
    assert_eq!(data.pointer("/foo/0").unwrap(),
        &Value::String("bar".to_owned()));
    assert_eq!(data.pointer("/").unwrap(), &Value::U64(0));
    assert_eq!(data.pointer("/a~1b").unwrap(), &Value::U64(1));
    assert_eq!(data.pointer("/c%d").unwrap(), &Value::U64(2));
    assert_eq!(data.pointer("/e^f").unwrap(), &Value::U64(3));
    assert_eq!(data.pointer("/g|h").unwrap(), &Value::U64(4));
    assert_eq!(data.pointer("/i\\j").unwrap(), &Value::U64(5));
    assert_eq!(data.pointer("/k\"l").unwrap(), &Value::U64(6));
    assert_eq!(data.pointer("/ ").unwrap(), &Value::U64(7));
    assert_eq!(data.pointer("/m~0n").unwrap(), &Value::U64(8));
    // Invalid pointers
    assert!(data.pointer("/unknown").is_none());
    assert!(data.pointer("/e^f/ertz").is_none());
    assert!(data.pointer("/foo/00").is_none());
    assert!(data.pointer("/foo/01").is_none());
}
