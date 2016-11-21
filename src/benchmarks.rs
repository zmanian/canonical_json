use std::{i64, u64};
use test::Bencher;

#[bench]
fn deserialize_i64(b: &mut Bencher) {
    let s = super::to_string(&i64::MIN).unwrap();
    b.bytes = s.len() as u64;

    b.iter(|| {
        let _s: i64 = super::from_str(&s).unwrap();
    });
}

#[bench]
fn deserialize_u64(b: &mut Bencher) {
    let s = super::to_string(&u64::MAX).unwrap();
    b.bytes = s.len() as u64;

    b.iter(|| {
        let _s: u64 = super::from_str(&s).unwrap();
    });
}

fn make_string(pattern: &str) -> String {
    let times = 1000;
    let mut s = String::with_capacity(pattern.len() * times + 2);

    s.push('"');

    for _ in 0..times {
        s.push_str(pattern);
    }

    s.push('"');

    s
}

#[bench]
fn deserialize_string(b: &mut Bencher) {
    let s = make_string("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456790");
    b.bytes = s.len() as u64;

    b.iter(|| {
        let _s: String = super::from_str(&s).unwrap();
    });
}

#[bench]
fn deserialize_string_escapes(b: &mut Bencher) {
    let s = make_string(r#"\\\""#);
    b.bytes = s.len() as u64;

    b.iter(|| {
        let _s: String = super::from_str(&s).unwrap();
    });
}
