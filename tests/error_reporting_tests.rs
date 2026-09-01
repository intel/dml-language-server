//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

//! Fixtures declare the errors they expect via `@error` annotations, either
//! as a trailing comment on the same line where the error's span is
//! expected to start:
//!
//! ```dml
//! param x default 1;
//! param x default 2; // @error[7]="Name collision"
//! ```
//!
//! or stacked on standalone comment lines directly above it (useful when
//! multiple errors are expected on the same line/col):
//!
//! ```dml
//! // @error[7]="Name collision"
//! // @error[7]="Name collision"
//! param x default 2;
//! ```
//!
//! `[7]` is the 1-indexed column of the expected error's starting span, and
//! the quoted string must appear as a substring of the error's
//! description
//!
//! A diagnostic's `related` entries are declared with `@related[col]of[row]`
//! where 'row' is the row where the annotation specifying the error it is
//! related to is declared in the file. Or with '..of[row@file.dml]' when
//! the original diagnostic is in a different file
//!
//! ```dml
//! param x default 1; // @related[7]of[2]="Previously defined here"
//! param x default 2; // @error[7]="Name collision"
//! ```

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::LazyLock;
use std::time::SystemTime;

use regex::Regex;

use dls::actions::DeviceAnalysisJobOptions;
use dls::analysis::{DMLError, DeviceAnalysis, IsolatedAnalysis};
use dls::analysis::parsing::tree::ZeroSpan;
use dls::actions::analysis_storage::TimestampedStorage;
use dls::concurrency::JobStatusKeeper;
use dls::file_management::CanonPath;
use dls::vfs::TextFile;

/// Returns all collected errors plus a map of filename -> raw file content
/// (needed to resolve `@error`/`@related` annotations against). `filenames`
/// must be non-empty; the first entry is the primary/device file that device
/// analysis (if requested) is run for, with every other file included as an
/// additional analysis base -- this lets a `related` diagnostic span into a
/// companion file (see the module docs' `of[row@file]` syntax).
#[track_caller]
fn analyze_and_read_files(filenames: &[&str], run_device_analysis: bool) -> (Vec<DMLError>, HashMap<String, String>) {
    assert!(!filenames.is_empty(), "at least one filename must be provided");
    let base_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/test_files");
    let (_keeper, status) = JobStatusKeeper::new();

    let mut contents = HashMap::new();
    let mut isolated_analyses = Vec::new();
    for &filename in filenames {
        let file_path = base_dir.join(filename);
        let content = std::fs::read_to_string(&file_path)
            .unwrap_or_else(|e| panic!("failed to read {}: {}", file_path.display(), e));

        let canon_path = CanonPath::from_path_buf(file_path.clone())
            .unwrap_or_else(|| panic!("failed to canonicalize path: {}", file_path.display()));

        let text_file = TextFile::from_str(&content)
            .unwrap_or_else(|_| panic!("failed to parse text file: {}", file_path.display()));

        let isolated = IsolatedAnalysis::new(
            &canon_path,
            &file_path,
            text_file,
            status.clone(),
        ).unwrap_or_else(|e| panic!("isolated analysis failed for {}: {:?}", filename, &e));

        contents.insert(filename.to_string(), content);
        isolated_analyses.push(isolated);
    }

    let primary = isolated_analyses[0].clone();
    let mut errors: Vec<DMLError> = primary.errors.clone();

    if run_device_analysis {
        assert!(primary.toplevel.device.is_some(),
                "{} must have a 'device' declaration to run device analysis",
                filenames[0]);
        let timestamp = SystemTime::UNIX_EPOCH;
        // The device's own file must also be present among the "bases"
        // (this is how the device's own templates/typedefs get registered,
        // see `templates_from_device_and_bases`/`DeviceAnalysis::new`); any
        // other supplied filenames are included the same way, so their
        // declarations are visible too (e.g. as targets of `related` spans).
        let timed_bases = isolated_analyses.into_iter()
            .map(|stored| TimestampedStorage { timestamp, stored })
            .collect();
        let device_analysis = DeviceAnalysis::new(
            primary,
            timed_bases,
            HashMap::default(),
            DeviceAnalysisJobOptions { max_reference_cache_size: 0 },
            status,
        ).unwrap_or_else(|e| panic!("device analysis failed for {}: {:?}", filenames[0], &e));
        // Remove minimal errors related to missing builtin files
        errors.extend(device_analysis.errors.into_values().flatten()
                      .filter(|e| !e.description.starts_with("Could not find file")));
    }

    (errors, contents)
}

/// A single `@error[col]="substring"` annotation.
#[derive(Debug, Clone)]
struct ExpectedError {
    /// File this annotation was found in; the expected span must be in
    /// this same file.
    file: String,
    /// 0-indexed line and column of the expected span
    line: u32,
    col: u32,
    substring: String,
    /// 0-indexed source line (within `file`) this annotation's comment was
    /// physically written on (before stacked-comment redirection). Used as
    /// an identifier so `@related` annotations can reference the specific
    /// error they belong to.
    annotated_at: u32,
}

/// A single `@related[col]of[row]` or `@related[col]of[row@file]` annotation,
/// matched against an entry in a diagnostic's `related` list, by the related
/// span's own line/col (not the main error's span).
#[derive(Debug, Clone)]
struct ExpectedRelated {
    /// File this annotation was found in; the expected related span must be
    /// in this same file.
    file: String,
    /// 0-indexed line and column of the expected related span
    line: u32,
    col: u32,
    substring: String,
    /// File of the owning `@error` annotation's `annotated_at` line.
    /// Defaults to `file` (i.e. `of[row]` with no `@file` suffix means the
    /// owning `@error` is in the same file as this `@related`).
    of_file: String,
    /// 0-indexed source line (within `of_file`) of the owning `@error`
    /// annotation's `annotated_at`.
    of_line: u32,
}

static ERROR_ANNOTATION_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"@error\[(\d+)\]="([^"]*)""#).unwrap()
});

static RELATED_ANNOTATION_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"@related\[(\d+)\]of\[(\d+)(?:@([A-Za-z0-9_./-]+))?\]="([^"]*)""#).unwrap()
});

/// Build a map from each line to the next non-comment, non-empty code line.
fn build_next_code_line_map(lines: &[&str]) -> Vec<Option<u32>> {
    let mut result = vec![None; lines.len()];
    let mut next_code: Option<u32> = None;

    for i in (0..lines.len()).rev() {
        result[i] = next_code;
        let trimmed = lines[i].trim();
        if !trimmed.is_empty() && !trimmed.starts_with("//") {
            next_code = Some(i as u32);
        }
    }

    result
}

/// Annotations on a standalone comment line apply to the next actual code
/// line, so multiple annotations can be stacked above it. The very first
/// line is exempt: some diagnostics carry a synthetic span at the literal
/// start of the file (e.g. a builtin type with no real declaration site),
/// which can only be targeted by an annotation placed on line 1 itself.
fn effective_line(line_idx: usize, lines: &[&str], next_code_line: &[Option<u32>]) -> u32 {
    let line_num = line_idx as u32;
    if line_num > 0 && lines[line_idx].trim().starts_with("//") {
        next_code_line[line_idx].unwrap_or(line_num)
    } else {
        line_num
    }
}

fn parse_expected_errors(filename: &str, content: &str) -> Vec<ExpectedError> {
    let mut expected = vec![];
    let lines: Vec<&str> = content.lines().collect();
    let next_code_line = build_next_code_line_map(&lines);

    for (line_idx, line) in lines.iter().enumerate() {
        let eff_line = effective_line(line_idx, &lines, &next_code_line);
        for cap in ERROR_ANNOTATION_RE.captures_iter(line) {
            let col: u32 = cap[1].parse().expect("annotation column must be a number");
            expected.push(ExpectedError {
                file: filename.to_string(),
                line: eff_line,
                // Annotation columns are 1-indexed in source, compensate
                col: col - 1,
                substring: cap[2].to_string(),
                annotated_at: line_idx as u32,
            });
        }
    }
    expected
}

fn parse_expected_related(filename: &str, content: &str) -> Vec<ExpectedRelated> {
    let mut expected = vec![];
    let lines: Vec<&str> = content.lines().collect();
    let next_code_line = build_next_code_line_map(&lines);

    for (line_idx, line) in lines.iter().enumerate() {
        let eff_line = effective_line(line_idx, &lines, &next_code_line);
        for cap in RELATED_ANNOTATION_RE.captures_iter(line) {
            let col: u32 = cap[1].parse().expect("annotation column must be a number");
            let of_line: u32 = cap[2].parse().expect("annotation 'of' line must be a number");
            let of_file = cap.get(3).map_or_else(|| filename.to_string(), |m| m.as_str().to_string());
            expected.push(ExpectedRelated {
                file: filename.to_string(),
                line: eff_line,
                col: col - 1,
                substring: cap[4].to_string(),
                of_file,
                // 'of[X]' is also a 1-indexed source line, compensate
                of_line: of_line - 1,
            });
        }
    }
    expected
}

fn rel_to_test_files(p: &Path) -> String {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/test_files");
    let canon_base = base.canonicalize().unwrap_or(base);
    let raw = p.strip_prefix(&canon_base)
        .map(|r| r.to_string_lossy().into_owned())
        .unwrap_or_else(|_| p.to_string_lossy().into_owned());
    if std::path::MAIN_SEPARATOR != '/' {
        raw.replace(std::path::MAIN_SEPARATOR, "/")
    } else {
        raw
    }
}

#[track_caller]
fn assert_errors_match(actual: &[DMLError], contents: &HashMap<String, String>) {
    let mut expected_errors = vec![];
    let mut expected_related = vec![];
    for (filename, content) in contents {
        expected_errors.extend(parse_expected_errors(filename, content));
        expected_related.extend(parse_expected_related(filename, content));
    }

    // Match main errors against actual diagnostics, tracking which actual
    // error matched which annotation so its (file, annotated_at) can be
    // used to scope the related-info matching below.
    let mut claimed_actual = vec![false; actual.len()];
    let mut owner_of_actual: Vec<Option<(String, u32)>> = vec![None; actual.len()];
    let mut missing_errors = vec![];

    for exp in &expected_errors {
        let found = actual.iter().enumerate().position(|(i, e)| {
            !claimed_actual[i]
                && rel_to_test_files(&e.span.path()) == exp.file
                && e.span.range.row_start.0 == exp.line
                && e.span.range.col_start.0 == exp.col
                && e.description.contains(&exp.substring)
        });
        match found {
            Some(i) => {
                claimed_actual[i] = true;
                owner_of_actual[i] = Some((exp.file.clone(), exp.annotated_at));
            }
            None => missing_errors.push(exp.clone()),
        }
    }
    let unexpected_errors: Vec<&DMLError> = actual.iter().enumerate()
        .filter(|(i, _)| !claimed_actual[*i])
        .map(|(_, e)| e)
        .collect();

    // Match related entries: each is only matched against `@related`
    // annotations whose 'of[row(@file)]' refers to the same `@error`
    // annotation that the owning actual error was matched against.
    let mut claimed_related = vec![false; expected_related.len()];
    let mut unexpected_related: Vec<(&ZeroSpan, &String)> = vec![];

    for (i, e) in actual.iter().enumerate() {
        let owner = &owner_of_actual[i];
        for (span, msg) in &e.related {
            let found = expected_related.iter().enumerate().position(|(j, exp)| {
                !claimed_related[j]
                    && matches!(owner, Some((f, l)) if *f == exp.of_file && *l == exp.of_line)
                    && rel_to_test_files(&span.path()) == exp.file
                    && span.range.row_start.0 == exp.line
                    && span.range.col_start.0 == exp.col
                    && msg.contains(&exp.substring)
            });
            match found {
                Some(j) => claimed_related[j] = true,
                None => unexpected_related.push((span, msg)),
            }
        }
    }
    let missing_related: Vec<&ExpectedRelated> = expected_related.iter().enumerate()
        .filter(|(j, _)| !claimed_related[*j])
        .map(|(_, exp)| exp)
        .collect();

    if missing_errors.is_empty() && unexpected_errors.is_empty()
        && missing_related.is_empty() && unexpected_related.is_empty() {
        return;
    }

    let mut msg = String::new();
    if !missing_errors.is_empty() {
        msg.push_str("Missing errors (annotation with no matching actual error):\n");
        for exp in &missing_errors {
            msg.push_str(&format!(
                "  - {} line {} col {}: expected substring \"{}\"\n",
                exp.file, exp.line + 1, exp.col + 1, exp.substring));
        }
    }
    if !unexpected_errors.is_empty() {
        msg.push_str("Unexpected/unclaimed errors (no matching @error annotation):\n");
        for e in &unexpected_errors {
            msg.push_str(&format!(
                "  - {} line {} col {}: {}\n",
                e.span.path().display(),
                e.span.range.row_start.0 + 1,
                e.span.range.col_start.0 + 1,
                e.description));
        }
    }
    if !missing_related.is_empty() {
        msg.push_str("Missing related info (annotation with no matching actual related entry):\n");
        for exp in &missing_related {
            msg.push_str(&format!(
                "  - {} line {} col {} of[{}@{}]: expected substring \"{}\"\n",
                exp.file, exp.line + 1, exp.col + 1, exp.of_line + 1, exp.of_file, exp.substring));
        }
    }
    if !unexpected_related.is_empty() {
        msg.push_str("Unexpected/unclaimed related info (no matching @related annotation):\n");
        for (span, message) in &unexpected_related {
            msg.push_str(&format!(
                "  - {} line {} col {}: {}\n",
                span.path().display(),
                span.range.row_start.0 + 1,
                span.range.col_start.0 + 1,
                message));
        }
    }
    panic!("{}", msg);
}

#[track_caller]
fn check_isolated_errors(filenames: &[&str]) {
    let (errors, contents) = analyze_and_read_files(filenames, false);
    assert_errors_match(&errors, &contents);
}

#[track_caller]
fn check_device_errors(filenames: &[&str]) {
    let (errors, contents) = analyze_and_read_files(filenames, true);
    assert_errors_match(&errors, &contents);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bad_version_error() {
        check_isolated_errors(&["errors_bad_version.dml"]);
    }

    #[test]
    fn test_isolated_misc_errors() {
        check_isolated_errors(&["errors_isolated_misc.dml"]);
    }

    #[test]
    fn test_object_param_errors() {
        check_device_errors(&["errors_object_params.dml"]);
    }

    #[test]
    fn test_template_trait_errors() {
        check_device_errors(&["errors_templates_traits.dml"]);
    }

    #[test]
    fn test_template_cycle() {
        check_device_errors(&["errors_template_cycle.dml"]);
    }

    #[test]
    fn test_typedef_cyclic_errors() {
        check_device_errors(&["errors_typedef_cyclic.dml"]);
    }

    #[test]
    fn test_method_errors() {
        check_device_errors(&["errors_methods.dml"]);
    }

    #[test]
    fn test_extern_typedef_unknown_type_errors() {
        check_device_errors(&["extern_typedef_unknown_type.dml"]);
    }

    #[test]
    fn test_duplicate_typedef_name_errors() {
        check_device_errors(&["errors_typedef_duplicate.dml"]);
    }

    #[test]
    fn test_typedef_self_ref_pointer_errors() {
        check_device_errors(&["errors_typedef_self_ref_pointer.dml"]);
    }

    #[test]
    fn test_unknown_type_errors() {
        check_device_errors(&["errors_unknown_type.dml"]);
    }

    #[test]
    fn test_builtin_type_lookup_errors() {
        check_device_errors(&["errors_builtin_type_lookup.dml"]);
    }
}
