use super::{dialogue::*, expr::*};
use peg::parser;

parser!(grammar dialogue() for str {
    // dialogue
    rule eval_begin() = quiet!{ "[" }
    rule eval_end() = quiet!{ "]" }

    rule eof() = quiet!{ ![_] }
    rule spaces() = quiet!{ [' ' | '\t' ] }

    pub rule tag() -> Tag<'input>
        = "#" tag:$([^' ' | '\n' | '\t' | '\r']+) { Tag(tag) }
    pub rule tags() -> Tags<'input>
        = tags:(tag() ** (spaces()+)) (spaces()*)? { Tags(tags) }

    pub rule line_escape() = "\\#" / "\\[" / "\\->" / "\\\n"
    pub rule not_escape() = !("#" / "[" / "->" / "\n")
    pub rule line() -> Line<'input>
        = text:$((line_escape() / not_escape() [_])+)
            spaces()* condition:condition()?
            spaces()* jump:jump()?
            spaces()* tags:tags()
            { Line {text: text.trim_end(), condition, jump: jump.map(Jump), tags } }

    pub rule condition() -> Expr<'input> = eval_begin() _ e:expr() _ eval_end() { e }
    pub rule choice() -> Choice<'input>
        = level:"*"+ spaces()* line:line() { Choice { level: level.len() - 1, line } }

    pub rule node_name() -> &'input str = "===" name:$(_ident()) "===" { name }

    rule _ident() = (['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*);

    pub rule comment() = quiet!{ "//" (!("\n\r" / "\n") [_])* }

    // exprs
    rule _() = quiet!{ [' ' | '\t' | '\r' | '\n' ]* }
    rule comma() = "," _

    pub rule ident() -> &'input str = $(_ident())
    pub rule var() -> &'input str
        = var:$("$" ident()) { &var[1..] }
    pub rule string() -> &'input str
        = "\"" s:$(("\\\"" / !"\"" [_])*) "\"" { s }
    pub rule num() -> f32
        = s:$("-"? ['0'..='9']+ ("." ['0'..='9']*)?) {? s.parse::<f32>().or(Err("num")) }
    pub rule bool() -> bool
        = b:$("true" / "false") {? match b { "true" => Ok(true), "false" => Ok(false), _ => Err("bool") } }
    pub rule jump() -> &'input str
        = "->" spaces()* jump:$(_ident()) { jump }

    pub rule atom() -> Expr<'input> =
          v:var()    { Atom::Variable(v).into() }
        / s:string() { Atom::String(s).into() }
        / n:num()    { Atom::Num(n).into() }
        / b:bool()   { Atom::Bool(b).into() }
        / j:jump()   { Atom::Jump(j).into() }
        / "(" _ e:expr() _ ")" { e }

    rule comparison_op() -> BinaryOp
        = "==" { BinaryOp::Eq } /
          "!=" { BinaryOp::Neq } /
          ">=" { BinaryOp::GrEq } /
          "<=" { BinaryOp::LessEq } /
          ">"  { BinaryOp::Gr } /
          "<"  { BinaryOp::Less}

    rule func_arg() -> Expr<'input> = _ e:expr() _ { e }
    pub rule func() -> Expr<'input>
        = name:ident() _ "(" _ args:(func_arg() ** ",") ","? _ ")" { FunctionCall { name, args }.into() }

    pub rule expr() -> Expr<'input> = precedence! {
        var:var() _ "=" _ expr:(@) { Assign{ var, expr }.into() }
        --
        lhs:(@) _ op:comparison_op() _ rhs:@ { BinaryExpr{ op, lhs, rhs }.into() }
        --
        lhs:(@) _ "+" _ rhs:@ { BinaryExpr{ op: BinaryOp::Add, lhs, rhs }.into() }
        lhs:(@) _ "-" _ rhs:@ { BinaryExpr{ op: BinaryOp::Sub, lhs, rhs }.into() }
        --
        lhs:(@) _ "*" _ rhs:@ { BinaryExpr{ op: BinaryOp::Mul, lhs, rhs }.into() }
        lhs:(@) _ "/" _ rhs:@ { BinaryExpr{ op: BinaryOp::Div, lhs, rhs }.into() }
        --
        lhs:(@) _ "||" _ rhs:@ { BinaryExpr{ op: BinaryOp::Or, lhs, rhs }.into() }
        lhs:(@) _ "&&" _ rhs:@ { BinaryExpr{ op: BinaryOp::And, lhs, rhs }.into() }
        --
        "!" _ expr:(@) { UnaryExpr{ op: UnaryOp::Not, expr }.into() }
        "-" _ expr:(@) { UnaryExpr{ op: UnaryOp::Neg, expr }.into() }
        --
        f:func() { f }
        a:atom() { a }
    }

});

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_ident() {
        assert_eq!("a", dialogue::ident("a").unwrap());
        assert_eq!("abc", dialogue::ident("abc").unwrap());
        assert_eq!("a123", dialogue::ident("a123").unwrap());
        assert_eq!("a1_c_", dialogue::ident("a1_c_").unwrap());
        assert_eq!("_a", dialogue::ident("_a").unwrap());

        assert!(dialogue::ident("$_a").is_err());
        assert!(dialogue::ident("").is_err());
        assert!(dialogue::ident(" ").is_err());
    }

    #[test]
    fn parse_var() {
        assert_eq!("a", dialogue::var("$a").unwrap());
        assert_eq!("abc", dialogue::var("$abc").unwrap());
        assert_eq!("a123", dialogue::var("$a123").unwrap());
        assert_eq!("a1_c_", dialogue::var("$a1_c_").unwrap());
        assert_eq!("_a", dialogue::var("$_a").unwrap());

        assert!(dialogue::var("a").is_err());
        assert!(dialogue::var("$ a").is_err());
        assert!(dialogue::var("").is_err());
        assert!(dialogue::var(" ").is_err());
    }

    #[test]
    fn parse_string() {
        assert_eq!("", dialogue::string("\"\"").unwrap());
        assert_eq!(
            "hello world!",
            dialogue::string("\"hello world!\"").unwrap()
        );
        assert_eq!("a1/-!😀", dialogue::string("\"a1/-!😀\"").unwrap());
        assert_eq!("\\\"", dialogue::string("\"\\\"\"").unwrap());

        assert!(dialogue::string("a").is_err());
        assert!(dialogue::string("").is_err());
        assert!(dialogue::string("'not a string'").is_err());
        assert!(dialogue::string("\"").is_err());
    }

    #[test]
    fn parse_num() {
        assert_eq!(0., dialogue::num("0").unwrap());
        assert_eq!(42., dialogue::num("42").unwrap());
        assert_eq!(-42., dialogue::num("-42").unwrap());
        assert_eq!(1.23, dialogue::num("1.23").unwrap());
        assert_eq!(-1.23, dialogue::num("-1.23").unwrap());
        assert_eq!(100., dialogue::num("100.").unwrap());
        assert_eq!(101., dialogue::num("101.0").unwrap());

        assert!(dialogue::num("forty").is_err());
        assert!(dialogue::num("").is_err());
    }

    #[test]
    fn parse_jump() {
        assert_eq!("jump", dialogue::jump("-> jump").unwrap());
        assert_eq!("jump", dialogue::jump("->jump").unwrap());
        assert_eq!("jump", dialogue::jump("->  jump").unwrap());
        assert_eq!("_12jump", dialogue::jump("-> _12jump").unwrap());

        assert!(dialogue::jump("->").is_err());
        assert!(dialogue::jump("-> ").is_err());
    }

    fn expr<'ast>(into: impl Into<Expr<'ast>>) -> Expr<'ast> {
        into.into()
    }

    #[test]
    fn parse_atom() {
        assert_eq!(expr(Atom::Bool(true)), dialogue::expr("true").unwrap());
        assert_eq!(expr(Atom::Num(1.23)), dialogue::expr("1.23").unwrap());
        assert_eq!(
            expr(Atom::String("hello")),
            dialogue::expr("\"hello\"").unwrap()
        );
        assert_eq!(expr(Atom::Variable("var")), dialogue::expr("$var").unwrap());
        assert_eq!(expr(Atom::Num(1.)), dialogue::expr("(  1.)").unwrap());
        assert_eq!(expr(Atom::Jump("jump")), dialogue::expr("-> jump").unwrap());
    }

    fn bin_expr<'ast>(
        lhs: impl Into<Expr<'ast>>,
        op: BinaryOp,
        rhs: impl Into<Expr<'ast>>,
    ) -> Expr<'ast> {
        BinaryExpr {
            op,
            lhs: lhs.into(),
            rhs: rhs.into(),
        }
        .into()
    }

    fn unary_expr<'ast>(op: UnaryOp, expr: impl Into<Expr<'ast>>) -> Expr<'ast> {
        UnaryExpr {
            op,
            expr: expr.into(),
        }
        .into()
    }

    #[test]
    fn parse_arithmetic() {
        assert_eq!(
            bin_expr(Atom::Variable("a"), BinaryOp::Add, Atom::Num(1.)),
            dialogue::expr("$a + 1").unwrap()
        );

        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Add, Atom::Num(2.)),
            dialogue::expr("1 + 2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Sub, Atom::Num(2.)),
            dialogue::expr("1  -   2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Mul, Atom::Num(2.)),
            dialogue::expr("1  * 2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Div, Atom::Num(2.)),
            dialogue::expr("1 /  2").unwrap()
        );

        // associativity
        assert_eq!(
            bin_expr(
                bin_expr(Atom::Num(1.), BinaryOp::Mul, Atom::Num(2.)),
                BinaryOp::Add,
                Atom::Num(3.)
            ),
            dialogue::expr("1 * 2 + 3").unwrap()
        );
        assert_eq!(
            bin_expr(
                Atom::Num(1.),
                BinaryOp::Sub,
                bin_expr(Atom::Num(2.), BinaryOp::Div, Atom::Num(3.)),
            ),
            dialogue::expr("1 - 2 / 3").unwrap()
        );

        assert_eq!(
            bin_expr(
                bin_expr(Atom::Num(1.), BinaryOp::Add, Atom::Num(2.)),
                BinaryOp::Mul,
                Atom::Num(3.),
            ),
            dialogue::expr("(1 + 2) * 3").unwrap()
        )
    }

    #[test]
    fn test_negation() {
        assert_eq!(
            unary_expr(UnaryOp::Neg, Atom::Num(42.)),
            dialogue::expr("-(42)").unwrap()
        );

        assert_eq!(
            unary_expr(UnaryOp::Neg, Atom::Variable("hello")),
            dialogue::expr("-$hello").unwrap()
        );
    }

    #[test]
    fn parse_comparison() {
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Eq, Atom::Num(2.)),
            dialogue::expr("1 == 2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Neq, Atom::Num(2.)),
            dialogue::expr("1  !=   2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Gr, Atom::Num(2.)),
            dialogue::expr("1  > 2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::Less, Atom::Num(2.)),
            dialogue::expr("1 <  2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::GrEq, Atom::Num(2.)),
            dialogue::expr("1 >=  2").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Num(1.), BinaryOp::LessEq, Atom::Num(2.)),
            dialogue::expr("1 <=  2").unwrap()
        );

        assert_eq!(
            bin_expr(Atom::Variable("a"), BinaryOp::LessEq, Atom::Variable("b")),
            dialogue::expr("$a <=  $b").unwrap()
        );

        // nested expression
        assert_eq!(
            bin_expr(
                Atom::Num(1.),
                BinaryOp::LessEq,
                bin_expr(Atom::Num(2.), BinaryOp::Sub, Atom::Num(4.)),
            ),
            dialogue::expr("1 <= (2 -  4)").unwrap(),
        );
    }

    #[test]
    fn test_logical() {
        assert_eq!(
            bin_expr(Atom::Bool(true), BinaryOp::Or, Atom::Bool(false)),
            dialogue::expr("true || false").unwrap()
        );
        assert_eq!(
            bin_expr(Atom::Bool(false), BinaryOp::And, Atom::Bool(true)),
            dialogue::expr("false && true").unwrap()
        );

        assert_eq!(
            bin_expr(
                bin_expr(Atom::Bool(false), BinaryOp::Or, Atom::Variable("a")),
                BinaryOp::And,
                Atom::Bool(true),
            ),
            dialogue::expr("false || $a && true").unwrap()
        );

        assert_eq!(
            bin_expr(
                Atom::Bool(false),
                BinaryOp::Or,
                bin_expr(Atom::Variable("a"), BinaryOp::And, Atom::Variable("b"))
            ),
            dialogue::expr("false || ($a && $b)").unwrap()
        );
    }

    #[test]
    fn test_not() {
        assert_eq!(
            unary_expr(UnaryOp::Not, Atom::Bool(true)),
            dialogue::expr("!true").unwrap()
        );

        assert_eq!(
            unary_expr(UnaryOp::Not, Atom::Bool(false)),
            dialogue::expr("!   false").unwrap()
        );

        // precedence
        assert_eq!(
            unary_expr(
                UnaryOp::Not,
                bin_expr(
                    unary_expr(UnaryOp::Not, Atom::Bool(false)),
                    BinaryOp::Or,
                    Atom::Bool(true)
                )
            ),
            dialogue::expr("! ( !  false || true)").unwrap()
        );
    }

    #[test]
    fn test_assign() {
        fn assign<'ast>(var: &'ast str, expr: impl Into<Expr<'ast>>) -> Expr<'ast> {
            Expr::Assign(Box::new(Assign {
                var,
                expr: expr.into(),
            }))
        }

        assert_eq!(
            assign("hello", Atom::Num(42.)),
            dialogue::expr("$hello  =  42.").unwrap(),
        );

        assert_eq!(
            assign(
                "hello",
                bin_expr(Atom::Num(42.), BinaryOp::Add, Atom::Num(1.))
            ),
            dialogue::expr("$hello  = 42 + 1").unwrap(),
        );

        assert!(dialogue::expr("hello = 42").is_err());
    }

    #[test]
    fn test_function() {
        assert_eq!(
            Expr::FunctionCall(FunctionCall {
                name: "f",
                args: vec![]
            }),
            dialogue::func("f()").unwrap()
        );

        assert_eq!(
            Expr::FunctionCall(FunctionCall {
                name: "f",
                args: vec![Atom::Num(42.).into()]
            }),
            dialogue::func("f( 42 , )").unwrap()
        );

        assert_eq!(
            Expr::FunctionCall(FunctionCall {
                name: "f",
                args: [
                    Atom::Num(42.),
                    Atom::String("hello"),
                    Atom::Variable("var"),
                    Atom::Bool(true)
                ]
                .into_iter()
                .map(|a| a.into())
                .collect()
            }),
            dialogue::func("f(42, \"hello\", $var, true)").unwrap()
        );

        assert_eq!(
            Expr::FunctionCall(FunctionCall {
                name: "f",
                args: vec![
                    bin_expr(Atom::Num(1.), BinaryOp::Add, Atom::Num(2.)),
                    unary_expr(UnaryOp::Not, Atom::Bool(true)),
                    FunctionCall {
                        name: "nested",
                        args: vec![Atom::Variable("var").into()]
                    }
                    .into(),
                ]
            }),
            dialogue::func("f(1 + 2, !true, nested($var))").unwrap()
        );
    }
}

#[cfg(test)]
mod test_dialogue {
    use super::*;

    #[test]
    fn parse_tag() {
        assert_eq!(Tag("hello"), dialogue::tag("#hello").unwrap());
        assert_eq!(Tag("123"), dialogue::tag("#123").unwrap());
        assert_eq!(Tag("h123"), dialogue::tag("#h123").unwrap());
        assert_eq!(Tag("_h123😀\""), dialogue::tag("#_h123😀\"").unwrap());

        assert!(dialogue::tag("hello").is_err());
        assert!(dialogue::tag("#").is_err());
    }

    #[test]
    fn parse_tags() {
        assert_eq!(Tags(vec![]), dialogue::tags("").unwrap());
        assert_eq!(Tags(vec![Tag("hello")]), dialogue::tags("#hello").unwrap());
        assert_eq!(
            Tags(vec![Tag("a"), Tag("b")]),
            dialogue::tags("#a #b ").unwrap()
        );

        assert_eq!(
            Tags(vec![Tag("a"), Tag("b#c")]),
            dialogue::tags("#a #b#c").unwrap()
        );
    }

    #[test]
    fn parse_line() {
        let escaped = ["\\#", "\\->", "\\[", "\\n"];
        for esc in escaped {
            let line = format!("hello {}", esc);
            assert_eq!(Line::new(&line), dialogue::line(&line).unwrap())
        }

        assert_eq!(Line::new("hello"), dialogue::line("hello").unwrap());
        assert_eq!(Line::new("hello"), dialogue::line("hello ").unwrap());

        let with_tags = Line {
            text: "hello",
            condition: None,
            jump: None,
            tags: Tags(vec![Tag("a"), Tag("b")]),
        };
        assert_eq!(with_tags, dialogue::line("hello#a #b").unwrap());
        assert_eq!(with_tags, dialogue::line("hello #a #b").unwrap());

        let with_condition = Line {
            text: "hello",
            condition: Some(Atom::Bool(true).into()),
            jump: None,
            tags: Tags(vec![]),
        };
        assert_eq!(with_condition, dialogue::line("hello [ true ]").unwrap());
        assert_eq!(with_condition, dialogue::line("hello[ true ]").unwrap());

        let with_jump = Line {
            text: "hello",
            condition: None,
            jump: Some(Jump("label")),
            tags: Tags(vec![]),
        };
        assert_eq!(with_jump, dialogue::line("hello -> label").unwrap());
        assert_eq!(with_jump, dialogue::line("hello ->label").unwrap());
        assert_eq!(with_jump, dialogue::line("hello-> label").unwrap());

        let with_all_three = Line {
            text: "hello",
            condition: Some(Atom::Bool(true).into()),
            jump: Some(Jump("label")),
            tags: Tags(vec![Tag("a"), Tag("b")]),
        };

        assert_eq!(
            with_all_three,
            dialogue::line("hello [true] -> label #a #b").unwrap()
        );

        // enforces ordering text / condition / label / tags
        assert!(dialogue::line("hello -> label [ true ]").is_err());
        assert!(dialogue::line("hello #a -> label").is_err());
    }

    #[test]
    fn parse_choice() {
        assert_eq!(
            Choice {
                level: 0,
                line: Line::new("hello")
            },
            dialogue::choice("* hello").unwrap()
        );

        assert_eq!(
            Choice {
                level: 0,
                line: Line::new("hello")
            },
            dialogue::choice("*hello").unwrap()
        );

        assert_eq!(
            Choice {
                level: 2,
                line: Line::new("hello")
            },
            dialogue::choice("*** hello").unwrap()
        );
    }

    #[test]
    fn parse_comment() {
        assert!(dialogue::comment("// this is a comment 123 ??000").is_ok());
    }

    #[test]
    fn parse_node_name() {
        assert_eq!("hello", dialogue::node_name("===hello===").unwrap());
        assert_eq!("_a123_", dialogue::node_name("===_a123_===").unwrap());

        assert!(dialogue::node_name("hello").is_err());
        assert!(dialogue::node_name("=hello=").is_err());
        assert!(dialogue::node_name("===he llo====").is_err());
        assert!(dialogue::node_name("===he.llo====").is_err());
    }
}
