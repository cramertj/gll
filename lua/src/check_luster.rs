use rustc_apfloat::ieee::Double;
use rustc_apfloat::Float;
use std::char;

pub trait Check<T: ?Sized> {
    type Error;
    fn check(&self, other: &T) -> Result<(), Self::Error>;
}

impl<'a, T: Check<U>, U: ?Sized> Check<&'a U> for T {
    type Error = T::Error;
    fn check(&self, other: &&U) -> Result<(), Self::Error> {
        self.check(*other)
    }
}

impl<T: Check<U>, U: ?Sized> Check<Box<U>> for T {
    type Error = T::Error;
    fn check(&self, other: &Box<U>) -> Result<(), Self::Error> {
        self.check(&**other)
    }
}

impl<T: Check<[U]>, U> Check<Vec<U>> for T {
    type Error = T::Error;
    fn check(&self, other: &Vec<U>) -> Result<(), Self::Error> {
        self.check(&other[..])
    }
}

impl<T: Check<U>, U> Check<Option<U>> for Option<T> {
    type Error = Option<T::Error>;
    fn check(&self, other: &Option<U>) -> Result<(), Self::Error> {
        match (self, other) {
            (Some(a), Some(b)) => a.check(b)?,
            (None, None) => {}
            _ => return Err(None),
        }
        Ok(())
    }
}

impl<'a, 'i, 's> Check<[u8]> for ::parse::Handle<'a, 'i, &'s str, ::parse::Name<'a, 'i, &'s str>> {
    type Error = ::parse::Handle<'a, 'i, &'s str, ::parse::Any>;
    fn check(&self, other: &[u8]) -> Result<(), Self::Error> {
        if self.source().as_bytes() != other {
            return Err(Self::Error::from(*self));
        }
        Ok(())
    }
}

impl<'a, 'i, 's, T, U> Check<[U]> for ::parse::Handle<'a, 'i, &'s str, [T]>
where
    ::parse::Handle<'a, 'i, &'s str, T>:
        Check<U, Error = ::parse::Handle<'a, 'i, &'s str, ::parse::Any>>,
{
    type Error = ::parse::Handle<'a, 'i, &'s str, ::parse::Any>;
    fn check(&self, other: &[U]) -> Result<(), Self::Error> {
        let other = match other {
            [] => ::parse::ListHead::Nil,
            [head, tail..] => ::parse::ListHead::Cons((head, tail)),
        };
        self.check(&other)
    }
}

impl<'a, 'i, 's, T, U> Check<::parse::ListHead<(&'_ U, &'_ [U])>>
    for ::parse::Handle<'a, 'i, &'s str, [T]>
where
    ::parse::Handle<'a, 'i, &'s str, T>:
        Check<U, Error = ::parse::Handle<'a, 'i, &'s str, ::parse::Any>>,
{
    type Error = ::parse::Handle<'a, 'i, &'s str, ::parse::Any>;
    fn check(&self, other: &::parse::ListHead<(&U, &[U])>) -> Result<(), Self::Error> {
        match (self.all_list_heads(), other) {
            (::parse::ListHead::Nil, ::parse::ListHead::Nil) => {}
            (::parse::ListHead::Cons(ref mut iter), ::parse::ListHead::Cons((b_head, b_tail))) => {
                let whole = Self::Error::from(*self);
                let mut smallest = whole;
                for (a_head, a_tail) in iter {
                    match a_head.check(b_head).and_then(|()| a_tail.check(b_tail)) {
                        Ok(()) => return Ok(()),
                        Err(e)
                            if e.node.range.len() < smallest.node.range.len()
                                || smallest == whole =>
                        {
                            smallest = e;
                        }
                        _ => {}
                    }
                }
                return Err(smallest);
            }
            _ => return Err(Self::Error::from(*self)),
        }
        Ok(())
    }
}

macro_rules! check {
    ($gll:ident {$($a:tt)*}, $luster:ident {$($b:tt)*} => $body:block) => {
        check!(a: $gll, b: $luster => {
            let ::parse::$gll {$($a)*} = a;
            let ::luster::parser::$luster {$($b)*} = b;
            $body
        });
    };
    ($a:ident: $gll:ident, $b:ident: $luster:ident => $body:block) => {
        check!($a: $gll, $b: ::luster::parser::$luster => $body);
    };
    ($a:ident: $gll:ident, $b:ident: $luster:ty => $body:block) => {
        impl Check<$luster> for ::parse::Handle<'a, 'i, &'s str, ::parse::$gll<'a, 'i, &'s str>> {
            type Error = ::parse::Handle<'a, 'i, &'s str, ::parse::Any>;
            fn check(&self, other: &$luster) -> Result<(), Self::Error> {
                let whole = Self::Error::from(*self);
                let mut smallest = whole;
                for x in self.all() {
                    match x.check(other) {
                        Ok(()) => return Ok(()),
                        Err(Some(e)) if e.node.range.len() < smallest.node.range.len() => {
                            smallest = e;
                        }
                        _ => {}
                    }
                }
                Err(smallest)
            }
        }
        impl Check<$luster> for ::parse::$gll<'a, 'i, &'s str> {
            type Error = Option<::parse::Handle<'a, 'i, &'s str, ::parse::Any>>;
            fn check(&self, other: &$luster) -> Result<(), Self::Error> {
                let $a = self;
                let $b = other;
                Ok($body)
            }
        }
    };
}

check!(Chunk { block: a }, Chunk { block: b } => {
    a.check(b)?
});

check!(Block { stats: a_s, ret_stat: a_r }, Block { statements: b_s, return_statement: b_r } => {
    a_s.check(b_s)?;
    a_r.check(b_r)?
});

check!(RetStat { exps: a }, ReturnStatement { returns: b } => {
    a.check(b)?
});

check!(a: Stat, b: Statement => {
    use parse::Stat as A;
    use luster::parser::Statement as B;
    use luster::parser::{AssignmentStatement, ForStatement, FunctionCallStatement, FunctionStatement, GotoStatement, IfStatement,
        LabelStatement, LocalStatement, RepeatStatement, WhileStatement};
    match (a,b) {
        (A::Assign { vars, exps }, B::Assignment(AssignmentStatement { targets, values })) => {
                vars.check(targets)?;
                exps.check(values)?
        }
        (A::Call{lhs, call}, B::FunctionCall(FunctionCallStatement{head, call: b_call})) => {
            lhs.check(head)?;
            call.check(b_call)?
        }
        (A::Label { name: a }, B::Label(LabelStatement { name: b })) => {
            a.check(b)?
        }
        (A::Break(_), B::Break) => {}
        (A::Goto { label }, B::Goto(GotoStatement { name })) => {
            label.check(name)?
        }
        (A::Do { body }, B::Do(block)) => {
            body.check(block)?
        }
        (A::While { cond, body }, B::While(WhileStatement { condition, block })) => {
            cond.check(condition)?;
            body.check(block)?
        }
        (A::Repeat { body, cond }, B::Repeat(RepeatStatement { body: b_body, until })) => {
            body.check(&b_body)?;
            cond.check(&until)?
        }
        (A::If { cond, body, else_ifs, else_body}, B::If(IfStatement { if_part, else_if_parts, else_part })) => {
            cond.check(&if_part.0)?;
            body.check(&if_part.1)?;
            else_ifs.check(&else_if_parts)?;
            else_body.check(&else_part)?
        }
        (A::For { var, init, cond, step, body }, B::For(ForStatement::Num { name, initial, limit, step: b_step, body: b_body })) => {
            var.check(name)?;
            init.check(initial)?;
            cond.check(limit)?;
            step.check(b_step)?;
            body.check(b_body)?
        }
        (A::ForEach { names, exps, body }, B::For(ForStatement::List { names: b_names, lists, body: b_body })) => {
            names.check(b_names)?;
            exps.check(lists)?;
            body.check(b_body)?
        }
        (A::Function { local: None, path, body }, B::Function(FunctionStatement { name, definition })) => {
            path.check(name)?;
            body.check(definition)?
        }
        (A::Function { local: Some(_), path, body }, B::LocalFunction(FunctionStatement { name, definition })) => {
            path.check(name)?;
            body.check(definition)?
        }
        (A::Local { names, exps }, B::LocalStatement(LocalStatement { names: b_names, values })) => {
            names.check(b_names)?;
            let values = if !values.is_empty() {
                Some(values)
            } else {
                None
            };
            exps.check(&values)?
        }
        _ => return Err(None),
    }
});

check!(a: ElseIf, b: (::luster::parser::Expression, ::luster::parser::Block) => {
    a.cond.check(&b.0)?;
    a.body.check(&b.1)?
});

struct LusterSubExpression<'a> {
    head: &'a ::luster::parser::HeadExpression,
    tail: &'a [(
        ::luster::operators::BinaryOperator,
        ::luster::parser::Expression,
    )],
}

check!(a: Exp, b: Expression => {
    a.check(&LusterSubExpression {
        head: &b.head,
        tail: &b.tail,
    })?
});

check!(a: Exp, b: LusterSubExpression<'_> => {
    use parse::Exp as A;
    use luster::operators::BinaryOperator as B;
        match (a, b.tail) {
            (A::Binop {lhs, binop, rhs}, [tail.., (b_binop, b_rhs)]) => {
                match (binop.source(), b_binop) {
                    ("+", B::Add) | ("*", B::Mul) | ("~", B::BitXor) | ("&", B::BitAnd) | ("|", B::BitOr)  |
                    ("/", B::Div) | ("%", B::Mod) | (">>", B::ShiftRight) | ("<<", B::ShiftLeft) | ("^", B::Pow) |
                    ("<", B::LessThan) | ("<=", B::LessEqual) | (">", B::GreaterThan) | (">=", B::GreaterEqual) |
                    ("==", B::Equal) | ("~=", B::NotEqual) | ("//", B::IDiv) | ("..", B::Concat) |
                    ("-", B::Sub) | ("and", B::And) | ("or", B::Or) => {}
                    _ => return Err(None),
                }
                lhs.check(&LusterSubExpression {
                    head: b.head,
                    tail
                })?;
                rhs.check(b_rhs)?
            }
            (A::Binop {..}, _) => return Err(None),
            (_, []) => {
                a.check(b.head)?
            }
            _ => return Err(None),
        }
});

check!(a:Exp, b:HeadExpression => {
    use parse::Exp as A;
    use luster::parser::HeadExpression as B;
    use ::luster::operators::UnaryOperator as U;
    match(a, b) {
        (A::Unop { unop: a_unop, exp: a_exp}, B::UnaryOperator(b_unop, b_exp)) => {
            match (a_unop.source(), b_unop) {
                ("not", U::Not) | ("#", U::Len) | ("~", U::BitNot) | ("-", U::Minus) => {}
                _ => return Err(None),
            }
            a_exp.check(b_exp)?
        }
        (A::Unop {..}, _) => return Err(None),
        (_, B::Simple(b)) => {
            a.check(b)?
        }
        _ => return Err(None),
    }
});

check!(a:Exp, b:SimpleExpression => {
    use parse::Exp as A;
    use luster::parser::SimpleExpression as B;
    match (a, b) {
        (A::Ellipsis(_), B::VarArgs) => {}
        (A::Nil(_), B::Nil) | (A::True(_), B::True) | (A::False(_), B::False) => {}
        (A::Num(a), B::Integer(b)) => {
            let s = a.source();
            let a = if s.starts_with("0x") || s.starts_with("0X") {
                i64::from_str_radix(&s[2..], 16)
            } else {
                s.parse()
            };
            if a != Ok(*b) {
                return Err(None);
            }
        }
        (A::Num(a), B::Float(b)) => {
            let mut a = a.source().to_string();
            if (a.starts_with("0x") || a.starts_with("0X")) && !(a.contains("p") || a.contains("P")) {
                a.push_str("p0");
            }
            if f64::from_bits(a.parse::<Double>().unwrap().to_bits() as u64) != *b {
                return Err(None);
            }
        }
        (A::Lit(a), B::String(b)) => {
            a.check(b)?
        }
        (A::FuncDef(a), B::Function(b)) => {
            a.check(b)?
        }
        (A::TableCons(a), B::TableConstructor(b)) => {
            a.check(b)?
        }
        (A::Prefix(a), B::Suffixed(b)) => {
            a.check(b)?
        }
        _ => return Err(None),
    }
});

fn unescape<T>(handle: ::parse::Handle<&str, T>) -> Vec<u8> {
    let mut out = vec![];
    let s = handle.source();
    let mut chars = s.bytes().peekable();
    while let Some(c) = chars.next() {
        let c = if c == b'\\' {
            let c = chars.next().unwrap();
            match c {
                b't' => b'\t',
                b'n' => b'\n',
                b'r' => b'\r',
                b'\\' | b'\'' | b'\"' => c,
                b'a' => 0x07,
                b'b' => 0x08,
                b'v' => 0x0b,
                b'f' => 0x0c,
                b'z' => {
                    while chars.peek().map_or(false, |&c| (c as char).is_whitespace()) {
                        chars.next();
                    }
                    continue;
                }
                b'x' => {
                    ((chars.next().unwrap() as char).to_digit(16).unwrap() * 16
                        + (chars.next().unwrap() as char).to_digit(16).unwrap())
                        as u8
                }
                b'0'..=b'9' => {
                    let mut x = (c as char).to_digit(10).unwrap();
                    for _ in 0..2 {
                        match chars.peek().and_then(|&c| (c as char).to_digit(10)) {
                            Some(d) => {
                                x = x * 10 + d;
                                chars.next();
                            }
                            None => break,
                        }
                    }
                    let c = x as u8;
                    assert_eq!(c as u32, x);
                    c
                }
                b'u' => {
                    assert_eq!(chars.next().unwrap(), b'{');
                    let mut x = 0;
                    loop {
                        match chars.next().unwrap() {
                            b'}' => break,
                            c => x = x * 16 + (c as char).to_digit(16).unwrap(),
                        }
                    }
                    out.extend(
                        char::from_u32(x)
                            .unwrap()
                            .encode_utf8(&mut [0; 4])
                            .as_bytes(),
                    );
                    continue;
                }
                c => unimplemented!("escape sequence \\{:?}", c as char),
            }
        } else {
            c
        };
        out.push(c);
    }
    out
}

check!(a: LiteralString, b:[u8] => {
    use parse::LiteralString as A;
    match *a {
        A::SQ { contents } => {
            if unescape(contents) != b {
                return Err(None);
            }
        }
        A::DQ { contents } => {
            if unescape(contents) != b {
                return Err(None);
            }
        }
        A::Raw { raw } => {
            let s = raw.source();
            if let Some(p) = s.find("[") {
                let raw = &s[p + 1..s.len() - p - 1];
                if raw.as_bytes() != b {
                    return Err(None)
                }
            }
        }
    }
});

struct LusterSubSuffixedExpression<'a> {
    primary: &'a ::luster::parser::PrimaryExpression,
    suffixes: &'a [::luster::parser::SuffixPart],
}

check!(a: PrefixExp, b: SuffixedExpression => {
    a.check(&LusterSubSuffixedExpression {
        primary: &b.primary,
        suffixes: &b.suffixes,
    })?
});

check!(a: PrefixExp, b: LusterSubSuffixedExpression<'_> => {
    use parse::PrefixExp as A;
    use luster::parser::{PrimaryExpression, SuffixPart};
    match (a, b.suffixes) {
        (A::Call { lhs, call }, [suffixes.., SuffixPart::Call(suffix)]) => {
            call.check(suffix)?;
            lhs.check(&LusterSubSuffixedExpression{
                primary: &b.primary,
                suffixes,
            })?
        }
        (A::Parens { exp: a }, []) => {
            match b.primary {
                PrimaryExpression::GroupedExpression(ref b) => {
                    a.check(&b)?
                }
                _ => return Err(None),
            }
        }
        (A::Var(var), _) => {
            var.check(&b)?
        }
        _ => return Err(None),
    }
});

check!(a: Var, b: LusterSubSuffixedExpression<'_> => {
    use parse::Var as A;
    use luster::parser::{FieldSuffix, SuffixPart};
    match (a, b.suffixes) {
        (A::Field { exp: a_exp, name: a_name }, [suffixes.., SuffixPart::Field(FieldSuffix::Named(ref c_name))]) => {
            a_name.check(c_name)?;
            a_exp.check(&LusterSubSuffixedExpression{
                primary: &b.primary,
                suffixes,
            })?
        }
        (A::Index { exp: a_exp, index: a_index }, [suffixes.., SuffixPart::Field(FieldSuffix::Indexed(ref c_exp))]) => {
            a_index.check(c_exp)?;
            a_exp.check(&LusterSubSuffixedExpression{
                primary: &b.primary,
                suffixes,
            })?
        }
        (_, []) => a.check(&b.primary)?,
        _ => return Err(None),
    }
});

check!(a:Var, b:PrimaryExpression => {
    use parse::Var as A;
    use luster::parser::PrimaryExpression as B;
    match (a, b) {
        (A::Name(a), B::Name(ref b)) => {
            a.check(b)?
        }
        _ => return Err(None),
    }
});

check!(a: Var, b: AssignmentTarget => {
    use parse::Var as A;
    use luster::parser::AssignmentTarget as B;
    use luster::parser::FieldSuffix;
    match(a, b) {
        (A::Name(a_name), B::Name(b_name)) => {
            a_name.check(b_name)?
        }
        (A::Field { exp, name }, B::Field(b_exp, FieldSuffix::Named(b_name))) => {
            exp.check(b_exp)?;
            name.check(b_name)?
        }
        (A::Index { exp, index }, B::Field(b_exp, FieldSuffix::Indexed(b_index))) => {
            exp.check(b_exp)?;
            index.check(b_index)?
        }
        _ => return Err(None),
    }
});

check!(a: FunctionCall, b: CallSuffix => {
    use parse::FunctionCall as A;
    use luster::parser::CallSuffix as B;
    match (a, b) {
        (A::Regular(args), B::Function(bs)) => {
            args.check(&bs)?
        }
        (A::Method { name, args }, B::Method(m_name, bs)) => {
            name.check(&m_name)?;
            args.check(&bs)?
        }
        _ => return Err(None),
    }
});

check!(a: Args, b: [::luster::parser::Expression] => {
    use parse::Args as A;
    use luster::parser::{Expression, HeadExpression, SimpleExpression};
    match (a, b) {
        (A::Params{ exps }, _) => {
            exps.check(b)?
        }
        (A::TableCons(a), [Expression {
            head:box HeadExpression::Simple(SimpleExpression::TableConstructor(ref b)),
            tail
        }]) if tail.is_empty() => a.check(&b)?,
        (A::Lit(a), [Expression {
            head:box HeadExpression::Simple(SimpleExpression::String(ref b)),
            tail
        }]) if tail.is_empty() => a.check(b)?,
        _ => return Err(None),
    }
});

check!(a:TableConstructor, b:TableConstructor => {
    let b_fields = if b.fields.is_empty() {
        None
    } else {
        Some(&b.fields)
    };
    a.fields.check(&b_fields)?
});

check!(a:Field, b:ConstructorField => {
    use parse::Field as A;
    use luster::parser::ConstructorField as B;
    use luster::parser::RecordKey;
    match (a, b) {
        (A::Unnamed(a), B::Array(b)) => {
            a.check(b)?
        }
        (A::Computed { index: a_index, exp: a_exp }, B::Record(RecordKey::Indexed(b_index), b_exp)) => {
            a_index.check(b_index)?;
            a_exp.check(b_exp)?
        }
        (A::Named { name: a_name, exp: a_exp }, B::Record(RecordKey::Named(b_name), b_exp)) => {
            a_name.check(b_name)?;
            a_exp.check(b_exp)?
        }
        _ => return Err(None),
    }
});

check!(a: FuncPath, b: FunctionName => {
    a.segments.check(&::parse::ListHead::Cons((&b.name, &b.fields[..])))?;
    a.method.check(&b.method)?;
});
check!(a: FuncBody, b: FunctionDefinition => {
    a.body.check(&b.body)?;
    if let Some(a_params) = a.params {
        a_params.check(b)?
    } else {
        if !b.parameters.is_empty() {
            return Err(None);
        }
    }
});
check!(a: FunctionDef, b: FunctionDefinition => {
    a.body.check(b)?
});
check!(a:Params, b: FunctionDefinition => {
    use parse::Params as A;
    match a {
        A::Named(names) => {
            if b.has_varargs {
                return Err(None);
            }
            names.check(&b.parameters)?
        }
        A::Ellipsis(_) => {
            if !b.has_varargs || b.parameters.len() != 0 {
                return Err(None);
            }
        }
        A::NamedAndEllipsis { names } => {
            if !b.has_varargs {
                return Err(None);
            }
            names.check(&b.parameters)?
        }
    }
});
