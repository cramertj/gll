#![feature(decl_macro)]
#![recursion_limit = "128"]

extern crate gll;

use gll::scannerless;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let mut dart = stringify!{

        //Whitespace = { " " | "\t" | "\n" | "\r" }* !" " !"\t" !"\n" !"\r";
        Whitespace =
            { " " | "\t" | "\n" }+ |
            "//" !"\n"* "\n"?;

        Shebang = "#!" { !"\n" .. }* "\n";

        IdentStart = 'a'..='z' | 'A'..='Z' | "_" | "$";
        IdentCont = IdentStart | '0'..='9';
        NotIdent = !'a'..='z' !'A'..='Z' !"_" !"$" !'0'..='9';
        Ident = IdentStart IdentCont* NotIdent;

        Comment =
            "//" { !"\n" .. }* "\n" |
            MultiLineComment
        ;
        MultiLineComment = "/*" { !"/*" !"*/" .. | MultiLineComment }* "*/";

        Punct =
            "." | "," | ":" | ";" | "+" | "*" | "-" | { "/" !"/" !"*" } | "=" | "^" | "%" | "#" |
            "{" | "}" | "[" | "]" | "(" | ")" | "&" | "|" | "!" | "<" | ">" | "@" | "~" | "?"
        ;

        Num =
            '0'..='9'+ { "." '0'..='9'+ }? { { "e" | "E" } { "+" | "-" }? '0'..='9'+ }? NotIdent |
            { "." '0'..='9'+ }? { { "e" | "E" } { "+" | "-" }? '0'..='9'+ }? NotIdent |
            { "0x" | "0X" } { '0'..='9' | 'a'..='f' | 'A'..='F' }+ NotIdent;

        StrLit =
            SQ:{ "'" prefix:StrLitSQContents { interp:StrInterpol+ % StrLitSQContents StrLitSQContents }? suffix:{} "'" } |
            DQ:{ "\"" prefix:StrLitDQContents { interp:StrInterpol+ % StrLitDQContents StrLitDQContents }? suffix:{} "\"" } |
            TSQ:{ "'''" prefix:StrLitTSQContents { interp:StrInterpol+ % StrLitTSQContents StrLitTSQContents }? suffix:{} "'''" } |
            TDQ:{ "\"\"\"" prefix:StrLitTDQContents { interp:StrInterpol+ % StrLitTDQContents StrLitTDQContents }? suffix:{} "\"\"\"" } |
            RawSQ:{ "r'" contents:StrLitRawSQContents "'" } |
            RawDQ:{ "r\"" contents:StrLitRawDQContents "\"" } |
            RawTSQ:{ "r'''" contents:StrLitRawTSQContents "'''" } |
            RawTDQ:{ "r\"\"\"" contents:StrLitRawTDQContents "\"\"\"" }
        ;

        StrLitSQContents = { { !"\\" !"'" !"$" !"\n" !"\r" .. } | { "\\" !"\n" !"\r" .. } }*;
        StrLitDQContents = { { !"\\" !"\"" !"$" !"\n" !"\r" .. } | { "\\" !"\n" !"\r" .. } }*;
        StrLitTSQContents = { { !"\\" !"'''" !"$" .. } | { "\\" .. } }*;
        StrLitTDQContents = { { !"\\" !"\"\"\"" !"$" .. } | { "\\" .. } }*;
        StrLitRawSQContents = { !"'" !"\n" !"\r" .. }*;
        StrLitRawDQContents = { !"\"" !"\n" !"\r" .. }*;
        StrLitRawTSQContents = { !"'''" .. }*;
        StrLitRawTDQContents = { !"\"\"\"" .. }*;

        IdentNoDollarStart = 'a'..='z' | 'A'..='Z' | "_";
        IdentNoDollarCont = IdentNoDollarStart | '0'..='9';
        IdentNoDollar = IdentNoDollarStart IdentNoDollarCont* NotIdent;

        StrInterpol = { "$" { ident:IdentNoDollar | "{" Whitespace expr:Expression Whitespace "}" } };

    }.parse::<scannerless::Grammar>().unwrap();

    dart.extend(
        stringify!{
            LibraryDefinition =
                shebang:Shebang? libraryName:LibraryName? importOrExport:ImportOrExport*
                partDirective:PartDirective* topLevelDefinition:TopLevelDefinition*;
            LibraryName = metadata:Metadata "library" ident:Ident+ % "." ";";
            Metadata = { "@" qualified:Qualified { "." ident:Ident }? args:Arguments? }*;
            PartDirective = meta:Metadata "part" uri:StrLit;
            ImportOrExport =
                Import:{ meta:Metadata spec:ImportSpecification } |
                Export:{ meta:Metadata "export" uri:StrLit comb:Combinator* };
            ImportSpecification =
                Simple:{ "import" uri:StrLit { "as" ident:Ident }? comb:Combinator* ";" } |
                Deferred:{ "import" uri:StrLit "deferred" "as" ident:Ident comb:Combinator* ";" };
            Combinator =
                Show:{ "show" idents:Ident+ % "." } |
                Hide:{ "hide" idents:Ident+ % "." };
            TopLevelDefinition =
                ClassDef:ClassDefinition |
                EnumTy:EnumType |
                Alias:TypeAlias |
                FunctionSig:{ "external"? sig:FunctionSignature ";" } |
                Getter:{ "external"? sig:GetterSignature ";" } |
                Setter:{ "external"? sig:SetterSignature ";" } |
                Function:{ sig:FunctionSignature body:FunctionBody } |
                GetBody:{ ret:ReturnType? "get" ident:Ident body:FunctionBody } |
                SetBody:{ ret:ReturnType? "set" ident:Ident params:FormalParameterList body:FunctionBody } |
                DeclarationList:{ { "final" | "const" } ty:Type? declList:StaticFinalDeclaration+ % "," ";" } |
                VarDeclaration:{ varDecl:VariableDeclaration ";" };
            GetterSignature = ret:ReturnType? "get" ident:Ident;
            SetterSignature = ret:ReturnType? "set" ident:Ident params:FormalParameterList;
            TypeAlias = meta:Metadata "typedef" body:FunctionTypeAlias;
            FunctionTypeAlias = prefix:FunctionPrefix typeParams:TypeParameters? formalParams:FormalParameterList;
            FunctionPrefix = ret:ReturnType? ident:Ident;
            StaticFinalDeclaration = ident:Ident "=" exp:Expression;
            VariableDeclaration = declIdent:DeclaredIdentifier ","? idents:Ident* % ","; //help
            DeclaredIdentifier = meta:Metadata ty:FinalConstVarOrType ident:Ident;
            EnumType = meta:Metadata "enum" name:Ident "{" ident:Ident+ % "," ","? "}";
            ClassDefinition =
                Class:{meta1:Metadata "abstract"? "class" ident:Ident params:TypeParameters? { superclass:Superclass
                    mixins:Mixins? }? interfaces:Interfaces? "{" { meta2:Metadata member:ClassMemberDefinition }* "}" } |
                Mixin:{ meta:Metadata "abstract"? "class" mixinClass:MixinApplicationClass };
            TypeParameters = "<" params:TypeParameter+ % "," ">" ;
            TypeParameter = meta:Metadata ident:Ident { "extends" ty:Type }?;
            Superclass = "extends" ty:Type;
            Mixins = "with" types:Type+ % ",";
            Interfaces = "implements" types:Type+ % ",";
            MixinApplicationClass = ident:Ident params:TypeParameters? "=" mixinApp:MixinApplication ";";
            MixinApplication = ty:Type mixins:Mixins interfaces:Interfaces?;
            ClassMemberDefinition =
                Field:{ decl:Declaration ";" } |
                Method:{ sig:MethodSignature body:FunctionBody };
            MethodSignature =
                Constructor:{ sig:ConstructorSignature init:Initializers? } |
                Factory:FactoryConstructorSignature |
                Function:{"static"? sig:FunctionSignature } |
                Getter:{"static"? sig:GetterSignature } |
                Setter:{"static"? sig:SetterSignature } |
                Operator:OperatorSignature;
            FactoryConstructorSignature = "factory" { prefix:Ident "." }? name:Ident params:FormalParameterList ; //help
            Declaration =
                ConstantConstructor:{ sig:ConstantConstructorSignature { redirect:Redirection | initialize:Initializers }? } |
                Constructor:{ sig:ConstructorSignature { redirect:Redirection | initialize:Initializers }? } |
                ExternalConstant:{ "external" sig:ConstantConstructorSignature } |
                ExternalConstructor:{ "external" sig:ConstructorSignature } |
                Getter:{ { "external" "static"? }? sig:GetterSignature } |
                Setter:{ { "external" "static"? }? sig:SetterSignature } |
                Operator:{ "external"? sig:OperatorSignature } |
                Function:{ { "external" "static"? }? sig:FunctionSignature } |
                StaticFinal:{ "static" { "final" | "const" } ty:Type? declList:StaticFinalDeclaration+ % "," } |
                Final:{ "final" ty:Type? identList:InitializedIdentifier+ % "," } |
                Initialized:{ "static"? { "var" | ty:Type } identList:InitializedIdentifier+ % "," };
            ConstantConstructorSignature = "const" qualified:Qualified params:FormalParameterList;
            ConstructorSignature = { "." prefix:Ident }? name:Ident params:FormalParameterList; //help
            Redirection = ":" "this" { "." ident:Ident }? args:Arguments;
            Initializers = ":" init:SuperCallOrFieldInitializer+ % ",";
            SuperCallOrFieldInitializer =
                Super:{ "super" arg:Arguments } |
                SuperCall:{ "super" "." ident:Ident args:Arguments } |
                Field:FieldInitializer;
            FieldInitializer = { "this" "." }? ident:Ident "=" cond:ConditionalExpression cascade:CascadeSection*;
            OperatorSignature = ret:ReturnType? "operator" op:Operator params:FormalParameterList;
            InitializedIdentifier = ident:Ident { "=" exp:Expression }?;
            Qualified = { prefix:Ident "." }? name:Ident;
            Arguments = "(" list:ArgumentList ")";
            ArgumentList =
                Named:{ args:NamedArgument+ % "," } |
                Exps:{ expList:Expression+ % "," args:NamedArgument* % "," }; //help
            NamedArgument = label:Label exp:Expression;
            Label = ident:Ident ":";
            Expression =
                Assignment:{ lhs:AssignableExpression assOp:AssignmentOperator rhs:Expression } |
                Conditional:{ cond:ConditionalExpression cascade:CascadeSection* } |
                Throw:{ "throw" exp:Expression };
            AssignableExpression =
                Primary:{ primary:Primary { args:Arguments* assignable:AssignableSelector }+ } |
                Unconditional:{ "super" unconditionalAssignable:UnconditionalAssignableSelector } |
                Ident:Ident;
            AssignmentOperator = "=" | CompoundAssignmentOperator;
            CompoundAssignmentOperator =
                "*=" | "/=" | "~/=" | "\\%=" | "+=" | "-=" | "<" | "<=" |
                ">" | ">=" | "&=" | "^=" | "|=" | "??=";
            ConditionalExpression = cond:IfNullExpression { "?" thenExp:ExpressionWithoutCascade
                ":" elseExp:ExpressionWithoutCascade }?;
            CascadeSection = ".." { cascadeSel:CascadeSelector args1:Arguments* }
                { assignableSel:AssignableSelector args2:Arguments* }*
                { assOp:AssignmentOperator exp:ExpressionWithoutCascade }?;
            Primary =
                This:"this" |
                Super:{ "super" unconditional:UnconditionalAssignableSelector } |
                FunctionExp:FunctionExpression |
                Literal:Literal |
                Ident:Ident |
                NewExp:NewExpression |
                Instantiation:{ "new" ty:Type "#" { "." ident:Ident }? } |
                Const:ConstObjectExpression |
                Exp:{ "(" exp:Expression ")" };
            Literal =
                NullLit:"null" |
                BoolLit:{ "false" | "true" } |
                NumLit:Num |
                StrLit:StrLit |
                SymbolLit:SymbolLiteral |
                MapLit:MapLiteral |
                ListLit:ListLiteral;
            SymbolLiteral = "#" { op:Operator | { idents:Ident+ % "." } };
            MapLiteral = "const"? args:TypeArguments? "{" { entry:MapLiteralEntry+ % "," ","? }? "}";
            ListLiteral = "const"? args:TypeArguments? "[" { exps:Expression+ % "," ","? }? "]";
            NewExpression = "new" ty:Type { "." ident:Ident }? args:Arguments;
            ConstObjectExpression = "const" ty:Type { "." ident:Ident }? args:Arguments;
            MapLiteralEntry = lhs:Expression ":" rhs:Expression;
            AssignableSelector =
                Unconditional:UnconditionalAssignableSelector |
                Conditional:{ "?." ident:Ident };
            UnconditionalAssignableSelector =
                Exp:{ "[" exp:Expression "]" } |
                Ident:{ "." ident:Ident };
            IfNullExpression = logOrExps:LogicalOrExpression+ % "??";
            ExpressionWithoutCascade =
                Assignment:{ lhs:AssignableExpression assOp:AssignmentOperator rhs:ExpressionWithoutCascade } |
                Conditional:ConditionalExpression |
                Throw:{ "throw" exp:ExpressionWithoutCascade };
            CascadeSelector =
                Exp:{ "[" exp:Expression "]" } |
                Ident:Ident;
            LogicalOrExpression = logAndExps:LogicalAndExpression+ % "||";
            LogicalAndExpression = eqExps:EqualityExpression+ % "&&";
            EqualityExpression =
                Equality:{ relExp1:RelationalExpression { eqOp:EqualityOperator relExp2:RelationalExpression }? } |
                Super:{ "super" eqOp:EqualityOperator relExp:RelationalExpression };
            RelationalExpression =
                bitOrExp1:BitwiseOrExpression { test:TypeTest | cast:TypeCast | relOp:RelationalOperator bitOrExp2:BitwiseOrExpression }? |
                "super" relOp:RelationalOperator bitOrExp:BitwiseOrExpression;
            EqualityOperator = "==" | "!=";
            RelationalOperator = ">=" | ">" | "<=" | "<";
            BitwiseOrExpression =
                Simple:{ bitXorExps:BitwiseXorExpression+ % "|" } |
                Super:{ "super" { "|" BitwiseXorExpression }+ };
            BitwiseXorExpression =
                Simple:{ BitwiseAndExpression+ % "^" } |
                Super:{ "super" { "^" BitwiseAndExpression }+ };
            BitwiseAndExpression =
                Simple:{ shifts:ShiftExpression+ % "&" } |
                Super:{ "super" { "&" shifts:ShiftExpression }+ };
            ShiftExpression =
                Simple:{ exps:AdditiveExpression+ % ShiftOperator } |
                Super:{ "super" { ShiftOperator exps:AdditiveExpression }+ };
            ShiftOperator = "<<" | ">>";
            AdditiveExpression =
                Simple:{ exps:MultiplicativeExpression+ % AdditiveOperator } |
                Super:{ "super" { AdditiveOperator exps:MultiplicativeExpression }+ };
            AdditiveOperator = "+" | "-";
            MultiplicativeExpression =
                Simple:{ UnaryExpression+ % MultiplicativeOperator } |
                Super:{ "super" { MultiplicativeOperator UnaryExpression }+ };
            MultiplicativeOperator = "*" | "/" | "\\%" | "~/";
            UnaryExpression =
                Prefixed:{ { "-" | "!" | "~" } unary:UnaryExpression } |
                Await:{ "await" unary:UnaryExpression} |
                Post:PostfixExpression |
                Super:{ { "-" | "~" } "super" } |
                Incremented:{ op:IncrementOperator exp:AssignableExpression };
            PostfixExpression =
                PostOp:{ exp:AssignableExpression op:IncrementOperator } |
                Primary:{ primary:Primary { Selector* | { "#" { { Ident "="? } | Operator } } } };
            IncrementOperator = "++" | "--";
            Selector =
                Assignable:AssignableSelector |
                Args:Arguments;
            Operator = "~" | BinaryOperator | "[" "]" | "[" "]" "=";
            BinaryOperator =
                Mul:MultiplicativeOperator |
                Add:AdditiveOperator |
                Shift:ShiftOperator |
                Rel:RelationalOperator |
                Equality:"==" |
                Bit:BitwiseOperator;
            BitwiseOperator = "&" | "^" | "|";
            TypeTest = "is" "!"? ty:Type;
            TypeCast = "as" ty:Type;
            FunctionExpression = params:FormalParameterList body:FunctionBody;
            FormalParameterList =
                Parens:"()" |
                Normal:{ "(" normParams:NormalFormalParameter+ % "," { "," optParams:OptionalFormalParameters}? ")" } |
                Optional:{ "(" optParams:OptionalFormalParameters ")" };
            NormalFormalParameter =
                Signature:FunctionSignature |
                FieldParam:FieldFormalParameter |
                SimpleParam:SimpleFormalParameter;
            OptionalFormalParameters =
                Positional:{ "[" params:DefaultFormalParameter+ % "," "]" } |
                Named:{ "{" params:DefaultNamedParameter+ % "," "}" };
            DefaultFormalParameter = NormalFormalParameter { "=" exp:Expression }?;
            DefaultNamedParameter = NormalFormalParameter { ":" exp:Expression }?;
            FieldFormalParameter = meta:Metadata ty:FinalConstVarOrType? "this"
                "." ident:Ident params:FormalParameterList?;
            SimpleFormalParameter =
                Declared:DeclaredIdentifier |
                Undeclared:{ meta:Metadata ident:Ident };
            FinalConstVarOrType =
                Final:{ "final" ty:Type? } |
                Const:{ "const" ty:Type? } |
                VarOrType:VarOrType;
            VarOrType =
                Var:"var" |
                Type:Type;
            Type = typeName:Qualified typeArgs:TypeArguments?;
            TypeArguments = "<" types:Type+ % "," ">";
            FunctionSignature = meta:Metadata returnType:ReturnType? ident:Ident params:FormalParameterList;
            ReturnType =
                Void:"void" |
                Ty:Type;
            FunctionBody =
                Exp:{ "async"? "=>" exp:Expression ";" } |
                Block:{ { "async" | "async"* | "sync"* }? block:Block };
            Block = "{" stats:Statement* "}";
            Statement = labels:Label* stat:NonLabelledStatement;
            NonLabelledStatement =
                Block:Block |
                Variable:LocalVariableDeclaration |
                For:ForStatement |
                While:WhileStatement |
                Do:DoStatement |
                Switch:SwitchStatement |
                If:IfStatement |
                Rethrow:RethrowStatement |
                Try:TryStatement |
                Break:BreakStatement |
                Continue:ContinueStatement |
                Return:ReturnStatement |
                Yield:YieldStatement |
                YieldEach:YieldEachStatement |
                Expression:ExpressionStatement |
                Assert:AssertStatement |
                Function:LocalFunctionDeclaration;
            LocalVariableDeclaration = InitializedVariableDeclaration ";";
            InitializedVariableDeclaration = declIdent:DeclaredIdentifier { "=" exp:Expression }? { "," initIdent:InitializedIdentifier }*;
            ForStatement = "await"? "for" "(" parts:ForLoopParts ")" stat:Statement;
            ForLoopParts =
                Simple:{ stat:ForInitializerStatement exp:Expression? ";" { list:Expression* % "," }? } |
                ForEach:{ ident:DeclaredIdentifier "in" exp:Expression } |
                { ident:Ident "in" exp:Expression };
            ForInitializerStatement =
                Variable:LocalVariableDeclaration |
                Expression:{ exp:Expression? ";" };
            WhileStatement = "while" "(" exp:Expression ")" stat:Statement;
            DoStatement = "do" stat:Statement "while" "(" exp:Expression ")" ";";
            SwitchStatement = "switch" "(" exp:Expression ")" "{" switch:SwitchCase* default:DefaultCase? "}";
            SwitchCase = labels:Label* "case" exp:Expression ":" stats:Statement*;
            DefaultCase = labels:Label* "default" ":" stats:Statement*;
            IfStatement = "if" "(" cond:Expression ")" thenStat:Statement { "else" elseStat:Statement }? ;
            RethrowStatement = "rethrow" ";" ;
            TryStatement = "try" block:Block { { onParts:OnPart+ finallyPart:FinallyPart? } | finallyPart:FinallyPart } ; //help
            OnPart =
                Catch:{ catchPart:CatchPart block:Block } |
                OnCatch:{ "on" ty:Type catchPart:CatchPart? block:Block };
            CatchPart = "catch" "(" ident:Ident { "," ident2:Ident }? ")"; //help
            FinallyPart = "finally" block:Block;
            BreakStatement = "break" ident:Ident? ";";
            ContinueStatement = "continue" ident:Ident? ";";
            ReturnStatement = "return" exp:Expression? ";";
            YieldStatement = "yield" exp:Expression ";";
            YieldEachStatement = "yield"* exp:Expression ";";
            ExpressionStatement = expr:Expression? ";";
            AssertStatement = "assert" "(" cond:ConditionalExpression ")" ";";
            LocalFunctionDeclaration = sig:FunctionSignature body:FunctionBody;
        }
        .parse::<scannerless::Grammar>()
        .unwrap()
        .insert_whitespace(gll::grammar::call("Whitespace")),
    );
    fs::write(&out_dir.join("parse.rs"), dart.generate_rust()).unwrap();
}
