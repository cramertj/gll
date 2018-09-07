#![feature(
    box_patterns,
    impl_header_lifetime_elision,
    in_band_lifetimes,
    slice_patterns,
    rustc_private
)]

extern crate gll;
extern crate luster;
extern crate rustc_apfloat;

pub mod check_luster;

pub mod parse {
    include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

pub fn with_chunk<'s, R>(
    input: &'s str,
    f: impl for<'i> FnOnce(parse::Handle<'_, 'i, &'s str, [parse::Stat<'_, 'i, &'s str>]>) -> R,
) -> R {
    parse::Chunk::parse_with(input, |_, result| {
        f(result.unwrap().one().unwrap().block.one().unwrap().stats)
    })
}

#[test]
fn parse() {
    with_chunk(
        r#"   foo123.n=x+y --[[dart ]] 1+1 "- stuff'" --parser
"#,
        |stats| {
            let parser = stats.parser;

            let v = stats.collect::<Result<Vec<_>, _>>().unwrap();

            assert_eq!(
                v.iter()
                    .map(|x| format!("{:?}: {:?}", parser.input(x.node.range), x))
                    .collect::<Vec<String>>(),
                [
                    r#""foo123": 3..9 => Token::Name(3..9)"#,
                    r#"".": 9..10 => Token::Punct(9..10)"#,
                    r#""n": 10..11 => Token::Name(10..11)"#,
                    r#""=": 11..12 => Token::Punct(11..12)"#,
                    r#""x": 12..13 => Token::Name(12..13)"#,
                    r#""+": 13..14 => Token::Punct(13..14)"#,
                    r#""y": 14..15 => Token::Name(14..15)"#,
                    r#""--[[dart ]]": 16..27 => Token::Comment(16..27)"#,
                    r#""1": 28..29 => Token::Numeral(28..29)"#,
                    r#""+": 29..30 => Token::Punct(29..30)"#,
                    r#""1": 30..31 => Token::Numeral(30..31)"#,
                    "\"\\\"- stuff\\\'\\\"\": 32..42 => Token::LiteralString(32..42 => LiteralString::DQ { \
                        contents: 33..41 \
                    })",
                    r#""--parser\n": 43..52 => Token::Comment(43..52)"#,
                ]
            );
        },
    );
}
