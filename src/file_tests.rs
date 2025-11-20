//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

// These tests are not exhaustive, they only check copyright header existence for file types with well-specified
// header format. Some tracked files are not supposed to have headers, or are sufficiently unique cases that
// they are just listed here instead:
// 
// Files without headers:
// LICENSE-APACHE
// LICENSE-MIT
// Cargo.toml
// deny.toml
// .gitignore
// example_files/example_lint_cfg.json
// all workflow files (under .github)
//
// Files with headers that are not tested:
// example_files/example_lint_cfg.README

#[cfg(test)]
mod test {
    fn get_files_with_extension(extension: &'static str) ->  Vec<String> {
        let files = walkdir::WalkDir::new(".");
        files.into_iter()
            .filter_map(|e|e.ok())
            .filter(|e|!e.path().starts_with("./target"))
            .filter_map(move |entry| {
                let path = entry.path().to_str()
                    .expect("Invalid Unicode in file path");
                if path.ends_with(extension) {
                    Some(path.to_string())
                } else {
                    None
                }
            }).collect()
    }

    fn compare_linewise(canon: &str, content: &str) -> bool {
        for (canon_line, content_line) in canon.lines().zip(content.lines()) {
            if canon_line != content_line {
                return false;
            }
        }
        true
    }

    #[test]
    fn check_copyright_headers_rs() {
        let mut missmatching_files = vec![];
        for path in get_files_with_extension(".rs") {
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|e|panic!("Could not read file {}, {}", path, e));
            if !compare_linewise(
"//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT",
                                 content.as_str()) {
                missmatching_files.push(path.to_string());
            }
        }
        if !missmatching_files.is_empty() {
            panic!("The following files do not have the correct copyright header:\n{}",
                missmatching_files.join("\n"));
        }
    }

    #[test]
    fn check_copyright_headers_md() {
        let mut missmatching_files = vec![];
        for path in get_files_with_extension(".md") {
            let content = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("Could not read file {}, {}", path, e));
            if !compare_linewise(
"<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->",
                                 content.as_str()) {
                missmatching_files.push(path.to_string());
            }
        }
        if !missmatching_files.is_empty() {
            panic!("The following files do not have the correct copyright header:\n{}",
                missmatching_files.join("\n"));
        }
    }
}
