//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
pub mod types;
pub mod statements;
pub mod objects;
pub mod expressions;
pub mod toplevel;

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::analysis::{FileSpec, parse_file};
    use crate::analysis::parsing::tree::ZeroRange;
    use crate::vfs::TextFile;

    use super::expressions::ExpressionKind;
    use super::toplevel::TopLevel;

    #[test]
    fn character_literal_sanity() {
        for (literal, diagnostic) in [
            ("'a'", None),
            ("' '", None),
            ("'~'", None),
            ("'\\\\'", None),
            ("'\\''", None),
            ("'\\n'", None),
            ("'\\r'", None),
            ("'\\t'", None),
            ("'\\b'", None),
            ("'ab'", Some("character constants must be a single ASCII char")),
            ("'\u{1f3e7}'", Some("character constants must be a single ASCII char")),
            ("'\\q'", Some("unrecognised character escape sequence")),
            ("'\\0'", Some("unrecognised character escape sequence")),
            ("'\\x41'", Some("unrecognised character escape sequence")),
            ("'\\nn'", Some("unrecognised character escape sequence")),
            ("'\\\"'", Some("unrecognised character escape sequence")),
        ] {
            let source = format!("dml 1.4;\nconstant c = {};", literal);
            let file: TextFile = source.parse().unwrap();
            let path = Path::new("character-literal.dml");
            let spec = FileSpec { path, file: &file };
            let (ast, _, errors) = parse_file(path, spec).unwrap();
            if let Some(message) = diagnostic {
                assert_eq!(errors.len(), 1, "{:?}: {:?}", literal, errors);
                assert_eq!(errors[0].description, message);
                assert_eq!(errors[0].span.range,
                           ZeroRange::from_u32(
                               1, 1, 13, 13 + literal.encode_utf16().count() as u32));
            } else {
                assert!(errors.is_empty(), "{:?}: {:?}", literal, errors);
            }
            let mut report = vec![];
            let top = TopLevel::from_ast(&ast, &mut report, spec);
            assert!(report.is_empty(), "{:?}: {:?}", literal, report);
            assert_eq!(top.spec.constants.len(), 1);
            match top.spec.constants[0].obj.value.as_ref() {
                ExpressionKind::CharLiteral(value) =>
                    assert_eq!(value.val, literal[1..literal.len() - 1]),
                other => panic!("Expected character literal, got {:?}", other),
            }
        }
    }

    #[test]
    fn multiline_character_literals_do_not_panic() {
        for literal in ["'a\n'", "'a\r\n'", "'\\\r'"] {
            let file: TextFile = format!("dml 1.4;\nconstant c = {};", literal)
                .parse().unwrap();
            let path = Path::new("multiline-character.dml");
            let spec = FileSpec { path, file: &file };
            let (ast, _, errors) = parse_file(path, spec).unwrap();
            let mut report = vec![];
            TopLevel::from_ast(&ast, &mut report, spec);
            assert!(!errors.is_empty() || !report.is_empty(),
                    "Expected diagnostics for {:?}", literal);
        }
    }

    #[test]
    fn unicode_documentation() {
        let file: TextFile = concat!(
            "dml 1.4;\n",
            "bank b \"\u{1f3e7}\" {}\n",
            "/* \u{1f3e7} */ bank c \"\u{e9}\u{4e2d}\" {}\n",
        ).parse().unwrap();
        let path = Path::new("unicode-documentation.dml");
        let spec = FileSpec { path, file: &file };
        let (ast, _, errors) = parse_file(path, spec).unwrap();
        assert!(errors.is_empty(), "{:?}", errors);
        let mut report = vec![];
        let top = TopLevel::from_ast(&ast, &mut report, spec);
        assert!(report.is_empty(), "{:?}", report);
        assert_eq!(top.spec.objects.len(), 2);
        for (object, name, text, range) in [
            (&top.spec.objects[0].obj, "b", "\u{1f3e7}",
             ZeroRange::from_u32(1, 1, 7, 11)),
            (&top.spec.objects[1].obj, "c", "\u{e9}\u{4e2d}",
             ZeroRange::from_u32(2, 2, 16, 20)),
        ] {
            assert_eq!(object.object.name.val, name);
            match object.doc.as_deref().unwrap() {
                ExpressionKind::StringLiteral(value) => {
                    assert_eq!(value.val, text);
                    assert_eq!(value.span.range, range);
                },
                other => panic!("Expected string literal, got {:?}", other),
            }
        }
    }
}
