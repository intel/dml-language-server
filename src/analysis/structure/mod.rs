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
