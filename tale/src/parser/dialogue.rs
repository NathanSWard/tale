use super::expr::*;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Tag<'ast>(pub &'ast str);

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Tags<'ast>(pub Vec<Tag<'ast>>);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Jump<'ast>(pub &'ast str);

#[derive(Debug, PartialEq, Clone)]
pub struct Line<'ast> {
    pub text: &'ast str,
    pub condition: Option<Expr<'ast>>,
    pub jump: Option<Jump<'ast>>,
    pub tags: Tags<'ast>,
}

impl<'ast> Line<'ast> {
    pub fn new(text: &'ast str) -> Self {
        Self {
            text,
            condition: None,
            jump: None,
            tags: Default::default(),
        }
    }

    pub fn new_with_tags(text: &'ast str, tags: Tags<'ast>) -> Self {
        Self {
            text,
            condition: None,
            jump: None,
            tags,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Choice<'ast> {
    pub level: usize,
    pub line: Line<'ast>,
}

impl<'ast> Choice<'ast> {
    pub fn new(text: &'ast str) -> Self {
        Self {
            level: 0,
            line: Line::new(text),
        }
    }
}

// #[cfg(test)]
// mod test {
//     use super::{super::grammar::*, *};
//     use pest_consume::Parser;

//     #[test]
//     fn parse_tag() {
//         let parse = |input| {
//             let inputs = Grammar::parse(Rule::tag, input).unwrap();
//             Grammar::tag(inputs.single().unwrap()).unwrap()
//         };

//         assert_eq!(Tag("tag"), parse("#tag"));
//         assert_eq!(Tag("123"), parse("#123"));
//         assert_eq!(Tag("tag123"), parse("#tag123"));
//         assert_eq!(Tag("1a_-😀"), parse("#1a_-😀"));

//         // a tag MUST have content
//         assert!(Grammar::parse(Rule::tag, "#").is_err());
//     }

//     #[test]
//     fn parse_tags() {
//         let parse = |input| {
//             let inputs = Grammar::parse(Rule::tags, input).unwrap();
//             Grammar::tags(inputs.single().unwrap()).unwrap()
//         };

//         assert_eq!(Tags(vec![]), parse(""));
//         assert_eq!(Tags(vec![]), parse("#"));
//         assert_eq!(Tags(vec![Tag("tag")]), parse("#tag"));
//         assert_eq!(Tags(vec![Tag("a#b"), Tag("c")]), parse("#a#b #c "));
//         assert_eq!(
//             Tags(vec![Tag("a"), Tag("b"), Tag("😀")]),
//             parse("#a #b #😀")
//         );
//     }

//     fn parse_line_impl(input: &str) -> Line {
//         let inputs = Grammar::parse(Rule::line, input).unwrap();
//         Grammar::line(inputs.single().unwrap()).unwrap()
//     }

//     #[test]
//     fn parse_line() {
//         assert_eq!(Line::new("hello 123?😀"), parse_line_impl("hello 123?😀"));
//         assert_eq!(Line::new("hello 123?"), parse_line_impl("hello 123?\nnope"));
//         assert_eq!(Line::new("hello\\#world"), parse_line_impl("hello\\#world"));
//     }

//     #[test]
//     fn parse_line_with_tags() {
//         assert_eq!(
//             Line::new_with_tags("hello", Tags(vec![Tag("a")])),
//             parse_line_impl("hello #a")
//         );

//         assert_eq!(
//             Line::new_with_tags("hello\\#", Tags(vec![Tag("a")])),
//             parse_line_impl("hello\\##a")
//         );

//         assert_eq!(
//             Line::new_with_tags("hello", Tags(vec![Tag("a")])),
//             parse_line_impl("hello#a")
//         );

//         assert_eq!(
//             Line::new_with_tags("hello", Tags(vec![])),
//             parse_line_impl("hello\n#a")
//         );

//         assert_eq!(
//             Line::new_with_tags("hello", Tags(vec![Tag("a"), Tag("b")])),
//             parse_line_impl("hello #a #b")
//         );

//         assert_eq!(
//             Line::new_with_tags("hello", Tags(vec![Tag("a"), Tag("b")])),
//             parse_line_impl("hello #a #b\n next")
//         );
//     }
//     #[test]
//     fn parse_line_error() {
//         assert!(Grammar::parse(Rule::line, "").is_err());
//         assert!(Grammar::parse(Rule::line, "#tag").is_err());
//     }

//     #[test]
//     fn parse_choice() {
//         let parse = |input| {
//             let inputs = Grammar::parse(Rule::choice, input).unwrap();
//             Grammar::choice(inputs.single().unwrap()).unwrap()
//         };

//         assert_eq!(Choice::new("hello"), parse("*hello"));
//         assert_eq!(Choice::new("hello"), parse("***hello"));
//         assert_eq!(Choice::new("hello"), parse("* hello"));
//         assert_eq!(Choice::new("hello"), parse("** * hello"));
//         assert_eq!(
//             Choice::new("This is a choice! 123"),
//             parse("*  This is a choice! 123")
//         );

//         assert_eq!(
//             Choice {
//                 line: Line {
//                     text: "hello",
//                     tags: Tags(vec![Tag("a"), Tag("b")])
//                 },
//                 condition: None
//             },
//             parse("*  hello  #a #b")
//         );
//         assert_eq!(
//             Choice {
//                 line: Line {
//                     text: "hello",
//                     tags: Tags(vec![Tag("a"), Tag("b")])
//                 },
//                 condition: None
//             },
//             parse("*  hello#a #b")
//         );
//         assert_eq!(
//             Choice {
//                 line: Line {
//                     text: "hello\\#",
//                     tags: Tags(vec![Tag("a"), Tag("b")])
//                 },
//                 condition: None
//             },
//             parse("*  hello\\##a #b")
//         );
//     }
// }
