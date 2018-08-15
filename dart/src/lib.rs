#![feature(decl_macro, stmt_expr_attributes)]

extern crate gll;

pub mod parse {
    include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

pub fn with_lib_def<'s, R>(
    input: &'s str,
    f: impl for<'i> FnOnce(parse::Handle<'_, 'i, &'s str, [parse::LibraryDefinition<'_, 'i, &'s str>]>)
        -> R,
) -> R {
    parse::Tokens::parse_with(input, |_, result| f(result.unwrap().one().unwrap().items))
}

#[test]
fn lex() {
    with_lib_def(
        r#"   foo123.n=x+y/*dart /**/ parser*/ 1+1 "${- stuff}" ' '"#,
        |lib_def| {
            let parser = lib_def.parser;

            let v = lib_def.collect::<Result<Vec<_>, _>>().unwrap();

            assert_eq!(
                v.iter()
                    .map(|x| format!("{:?}: {:?}", parser.input(x.node.range), x))
                    .collect::<Vec<String>>(),
                #[cfg_attr(rustfmt, rustfmt_skip)]
                [
                    r#""foo123": 1:4-1:10 => Token::Ident(1:4-1:10)"#,
                    r#"".": 1:10-1:11 => Token::Punct(1:10-1:11)"#,
                    r#""n": 1:11-1:12 => Token::Ident(1:11-1:12)"#,
                    r#""=": 1:12-1:13 => Token::Punct(1:12-1:13)"#,
                    r#""x": 1:13-1:14 => Token::Ident(1:13-1:14)"#,
                    r#""+": 1:14-1:15 => Token::Punct(1:14-1:15)"#,
                    r#""y": 1:15-1:16 => Token::Ident(1:15-1:16)"#,
                    r#""/*dart /**/ parser*/": 1:16-1:36 => Token::Comment(1:16-1:36)"#,
                    r#""1": 1:37-1:38 => Token::Int(1:37-1:38)"#,
                    r#""+": 1:38-1:39 => Token::Punct(1:38-1:39)"#,
                    r#""1": 1:39-1:40 => Token::Int(1:39-1:40)"#,
                    "\"\\\"${- stuff}\\\"\": 1:41-1:53 => Token::Str(1:41-1:53 => StrLit::DQ { \
                        prefix: 1:42-1:42, \
                        interp: 1:42-1:52 => [1:42-1:52 => StrInterpol { \
                            tts: 1:44-1:51 => [\
                                1:44-1:45 => TokenTree::Token { \
                                    token: 1:44-1:45 => Token::Punct(1:44-1:45) \
                                }, \
                                1:46-1:51 => TokenTree::Token { \
                                    token: 1:46-1:51 => Token::Ident(1:46-1:51) \
                                }\
                            ] \
                        }], \
                        suffix: 1:52-1:52 \
                    })",
                    "\"\\\' \\\'\": 1:54-1:57 => Token::Str(1:54-1:57 => StrLit::SQ { prefix: 1:55-1:56, suffix: 1:56-1:56 })",
                ]
            );
        },
    );
}
