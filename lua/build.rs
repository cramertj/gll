extern crate gll;

use gll::scannerless;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let mut grammar = stringify!{
        Whitespace = {
            " " | "\t" | "\n" | "\r" |
            "--" { !"\n" .. }* "\n" |
            "--[" LiteralStringRaw "]"
        }* !" " !"\t" !"\n" !"\r" !"--";

        Shebang = "#!" { !"\n" .. }* "\n";

        NameStart = 'a'..='z' | 'A'..='Z' | "_";
        NameCont = NameStart | '0'..='9';
        NotName = !'a'..='z' !'A'..='Z' !"_" !'0'..='9';
        Name = NameStart NameCont* NotName;

        Numeral =
            '0'..='9'+ { "." '0'..='9'* }? { { "e" | "E" } { "+" | "-" }? '0'..='9'+ }? NotName |
            { "0x" | "0X" } { '0'..='9' | 'a'..='f' | 'A'..='F' }+ { "." { '0'..='9' | 'a'..='f' | 'A'..='F' }* }? { { "p" | "P" } { "+" | "-" }? '0'..='9'+ }? NotName;

        LiteralString =
            SQ:{ "'" contents:LiteralStringSQContents "'" } |
            DQ:{ "\"" contents:LiteralStringDQContents "\"" } |
            Raw:{ "[" raw:LiteralStringRaw "]" };

        LiteralStringSQContents = { { !"\\" !"'" !"\n" !"\r" .. } | LiteralStringEscape }*;
        LiteralStringDQContents = { { !"\\" !"\"" !"\n" !"\r" .. } | LiteralStringEscape }*;
        LiteralStringEscape =
            "\\" !"z" .. |
            "\\z" { " " | "\t" | "\n" | "\r" }+  !" " !"\t" !"\n" !"\r";
        LiteralStringRaw =
            "[" LiteralStringRawContents "]" |
            "=" LiteralStringRaw "=";
        LiteralStringRawContents = ..*;

        BREAK = "break" NotName;
        GOTO = "goto" NotName;
        DO = "do" NotName;
        END = "end" NotName;
        WHILE = "while" NotName;
        REPEAT = "repeat" NotName;
        UNTIL = "until" NotName;
        IF = "if" NotName;
        THEN = "then" NotName;
        ELSE = "else" NotName;
        FOR = "for" NotName;
        IN = "in" NotName;
        FUNCTION = "function" NotName;
        LOCAL = "local" NotName;
        ELSEIF = "elseif" NotName;
        RETURN = "return" NotName;
        NIL = "nil" NotName;
        FALSE = "false" NotName;
        TRUE = "true" NotName;
        AND = "and" NotName;
        OR = "or" NotName;
        NOT = "not" NotName;
        MINUS = "-" !"-";

    }.parse::<scannerless::Grammar>().unwrap();

    grammar.extend(
        stringify!{
            Chunk = Shebang? block:Block;
            Block = ";"* stats:Stat* % { ";"* } ";"* ret_stat:RetStat?;
            Stat =
                Assign:{ vars:Var+ % "," "=" exps:Exp+ % "," } |
                Call:{ lhs:PrefixExp call:FunctionCall } |
                Label:{ "::" name:Name "::" } |
                Break:BREAK |
                Goto:{ GOTO label:Name } |
                Do:{ DO body:Block END } |
                While:{ WHILE cond:Exp DO body:Block END } |
                Repeat:{ REPEAT body:Block UNTIL cond:Exp } |
                If:{
                    IF cond:Exp THEN body:Block
                    else_ifs:ElseIf*
                    { ELSE else_body:Block }?
                    END
                } |
                For:{ FOR var:Name "=" init:Exp "," cond:Exp { "," step:Exp }? DO body:Block END } |
                ForEach:{ FOR names:Name+ % "," IN exps:Exp+ % "," DO body:Block END } |
                Function:{ local:LOCAL? FUNCTION path:FuncPath body:FuncBody } |
                Local:{ LOCAL names:Name+ % "," { "=" exps:Exp+ % "," }? };
            ElseIf = ELSEIF cond:Exp THEN body:Block;
            RetStat = RETURN exps:Exp* % "," ";"?;
            FuncPath = segments:Name+ % "." { ":" method:Name }?;
            Var =
                Name:Name |
                Index:{ exp:PrefixExp "[" index:Exp "]" } |
                Field:{ exp:PrefixExp "." name:Name };
            Exp =
                Nil:NIL |
                False:FALSE |
                True:TRUE |
                Num:Numeral |
                Lit:LiteralString |
                Ellipsis:"..." |
                FuncDef:FunctionDef |
                Prefix:PrefixExp |
                TableCons:TableConstructor |
                Binop:{ lhs:Exp binop:Binop rhs:Exp } |
                Unop:{ unop:Unop exp:Exp };
            PrefixExp =
                Var:Var |
                Call:{ lhs:PrefixExp call:FunctionCall } |
                Parens:{ "(" exp:Exp ")" };
            FunctionCall =
                Regular:Args |
                Method:{ ":" name:Name args:Args };
            Args =
                Params:{ "(" exps:Exp* % "," ")" } |
                TableCons:TableConstructor |
                Lit:LiteralString;
            FunctionDef = FUNCTION body:FuncBody;
            FuncBody = "(" params:Params? ")" body:Block END;
            Params =
                Named:Name+ % "," |
                Ellipsis:"..." |
                NamedAndEllipsis:{ names:Name+ % "," "," "..." };
            TableConstructor = "{" { fields:Field+ % FieldSep FieldSep? }? "}";
            Field =
                Computed:{ "[" index:Exp "]" "=" exp:Exp } |
                Named:{ name:Name "=" exp:Exp } |
                Unnamed:Exp;
            FieldSep = "," | ";";
            Binop =
                "+" | MINUS | "*" | "/" | "//" | "^" | "%" | "&" | "~" | "|" | ">>" | "<<" | ".." |
                "<" | "<=" | ">" | ">=" | "==" | "~=" | AND | OR;
            Unop = "-" | NOT | "#" | "~";
        }
        .parse::<scannerless::Grammar>()
        .unwrap()
        .insert_whitespace(gll::grammar::call("Whitespace")),
    );
    fs::write(&out_dir.join("parse.rs"), grammar.generate_rust()).unwrap();
}
