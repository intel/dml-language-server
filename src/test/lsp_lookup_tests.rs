//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

use std::convert::TryFrom;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;
use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

use regex::Regex;

use crate::actions::analysis_storage::{AnalysisStorage, TimestampedStorage, AnalysisLookupError};
use crate::actions::InitActionContext;
use crate::concurrency::JobStatusKeeper;
use crate::analysis::{DeviceAnalysis, IsolatedAnalysis, ZeroFilePosition};
use crate::analysis::parsing::tree::{ZeroPosition, ZeroSpan};
use crate::analysis::structure::objects::Import;
use crate::vfs::{TextFile, Vfs};
use crate::span::{Position, ZeroIndexed};
use crate::file_management::CanonPath;
use crate::server::io::Output;
use crate::actions::semantic_lookup::{definitions_at_fp, declarations_at_fp,
                                       implementations_at_fp, references_at_fp, DLSLimitation};

// Mock output for testing
#[derive(Clone, Debug)]
struct MockOutput;

impl Output for MockOutput {
    fn response(&self, _output: String) {
        // Do nothing in tests
    }

    fn provide_id(&self) -> crate::server::RequestId {
        use crate::server::message::RequestId as MessageRequestId;
        MessageRequestId::Num(1)
    }
}

// Location declaration - @loc[col]=name
#[derive(Debug, Clone, PartialEq, Eq)]
struct LocationDeclaration {
    position: Position<ZeroIndexed>,
    name: String,
}

impl LocationDeclaration {
    fn line(&self) -> u32 {
        self.position.row.0
    }

    fn col(&self) -> u32 {
        self.position.col.0
    }
}

// Operation types for annotations that reference locations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperationType {
    GotoDefinition,  // @goto-def -> name
    GotoDeclaration, // @goto-decl -> name
    GotoImplementation, // @goto-impl -> name,name,...
    FindReferences,  // @goto-ref -> name,name,...
}

impl std::fmt::Display for OperationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperationType::GotoDefinition => write!(f, "goto-def"),
            OperationType::GotoDeclaration => write!(f, "goto-decl"),
            OperationType::GotoImplementation => write!(f, "goto-impl"),
            OperationType::FindReferences => write!(f, "goto-ref"),
        }
    }
}

/// Source location for an annotation - where the annotation appears in the test file
#[derive(Debug, Clone)]
struct SourceLocation {
    position: Position<ZeroIndexed>,
    file_path: Option<PathBuf>,  // Full canonical path of the file this location is in
}

impl SourceLocation {
    fn new(line: u32, col: u32, file_path: Option<PathBuf>) -> Self {
        SourceLocation {
            position: Position::<ZeroIndexed>::from_u32(line, col),
            file_path,
        }
    }

    fn line(&self) -> u32 {
        self.position.row.0
    }

    fn col(&self) -> u32 {
        self.position.col.0
    }
}

/// Target specifier - a symbolic reference to where an annotation should resolve.
/// These are specified in test files as names like "my_func" or "file:my_func"
/// and get resolved to concrete positions via the name mapping.
#[derive(Debug, Clone)]
struct TargetSpec {
    name: String,
    filename: Option<String>,  // Bare filename (e.g. "imported_file.dml"), not a full path
    negated: bool,             // If true, this target must NOT appear in results
}

impl TargetSpec {
    fn new_named(name: String) -> Self {
        TargetSpec {
            name,
            filename: None,
            negated: false,
        }
    }

    fn new_negated(name: String) -> Self {
        TargetSpec {
            name,
            filename: None,
            negated: true,
        }
    }

    fn new_named_with_file(file: String, name: String) -> Self {
        TargetSpec {
            name,
            filename: Some(file),
            negated: false,
        }
    }

    fn new_negated_with_file(file: String, name: String) -> Self {
        TargetSpec {
            name,
            filename: Some(file),
            negated: true,
        }
    }

    /// The lookup key for this target in the name map.
    /// Produces `"file.dml:name"` for cross-file targets, or just `"name"`.
    fn key(&self) -> String {
        match self.filename {
            Some(ref filename) => format!("{}:{}", filename, self.name),
            None => self.name.clone(),
        }
    }
}

/// A resolved target - the concrete position that a symbolic target resolved to
#[derive(Debug, Clone)]
struct ResolvedTarget {
    position: Position<ZeroIndexed>,
    filename: Option<String>,  // Bare filename (e.g. "imported_file.dml"), not a full path
    resolved_name: String,     // The key used to resolve this target (may include file prefix)
    negated: bool,             // If true, this target must NOT appear in results
}

impl ResolvedTarget {
    fn line(&self) -> u32 {
        self.position.row.0
    }

    fn col(&self) -> u32 {
        self.position.col.0
    }
}

/// Unresolved annotation from test file (contains symbolic target names)
#[derive(Debug, Clone)]
struct UnresolvedAnnotation {
    operation_type: OperationType,
    location: SourceLocation,           // Where this annotation is (the thing being tested)
    targets: Vec<TargetSpec>,           // Symbolic names to resolve
    token: String,                      // The source token at the lookup position
}

/// Resolved annotation ready for comparison (contains concrete positions)
#[derive(Debug, Clone)]
struct Annotation {
    operation_type: OperationType,
    location: SourceLocation,           // Where this annotation is (the thing being tested)
    targets: Vec<ResolvedTarget>,       // Where it should point to (resolved positions)
    token: String,                      // The source token at the lookup position
}

/// Extract the token starting at a 0-indexed byte position in a line.
/// A token is a contiguous run of `[0-9a-zA-Z_]` characters.
/// Returns `"<eol>"` if `col` is at or past the end of the line.
fn extract_token_at(line: &str, col: usize) -> String {
    if col >= line.len() {
        return "<eol>".to_string();
    }
    let rest = &line[col..];
    let end = rest
        .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .unwrap_or(rest.len());
    if end == 0 {
        // The character at col is itself a delimiter — take just that one char
        rest.chars().next().map_or("<eol>".to_string(), |c| c.to_string())
    } else {
        rest[..end].to_string()
    }
}

/// Regex for `@loc[col]=name`
static RE_LOC: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@loc\[(\d+)\]=([A-Za-z_][A-Za-z0-9_]*)").unwrap()
});

/// Regex for operation annotations.
/// `goto-def-decl` is a shorthand that expands into both a `goto-def` and `goto-decl` annotation.
///
/// The target group `([^@]*)` captures everything after `->` up to the next `@`
/// (without consuming it), so consecutive annotations on the same line are all
/// found by `captures_iter`.  Any trailing `//` comment or whitespace in the
/// captured text is stripped during post-processing in `parse_annotations`.
static RE_OP: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@(goto-def-decl|goto-def|goto-decl|goto-impl|goto-ref)\[(\d+)\]->([^@]*)").unwrap()
});

/// Regex for an operation annotation that is missing the required column bracket.
/// Used only for producing helpful error messages.
static RE_OP_NO_COL: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@(goto-def-decl|goto-def|goto-decl|goto-impl|goto-ref)->").unwrap()
});

/// Catch-all regex for any `@word[...]->` or `@word->` pattern.
/// Used to detect unrecognized annotation types that are likely typos.
static RE_UNKNOWN_OP: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@([a-zA-Z][-a-zA-Z0-9]*)(\[[^\]]*\])?->").unwrap()
});

/// Parse annotations from a DML test file
///
/// Annotation format:
/// - @loc[col]=name - Declares a named location at the specified column (1-indexed)
/// - @goto-def[col]->name - Tests goto-definition at column, expecting it to resolve to 'name'
/// - @goto-decl[col]->name - Tests goto-declaration at column, expecting it to resolve to 'name'
/// - @goto-def-decl[col]->name - Shorthand: expands to both goto-def and goto-decl with same target
/// - @goto-impl[col]->name - Tests goto-implementation at column, expecting it to resolve to 'name'
/// - @goto-ref[col]->name1,name2 - Tests find-references at column, expecting multiple results
/// - @goto-def[col]-> - Tests that lookup returns no results (empty target list)
///
/// Negated targets:
/// - @goto-def[col]->!name - Asserts that 'name' is NOT in the goto-def results
/// - @goto-ref[col]->!loc1,loc2 - loc1 must NOT be in results, loc2 must be
/// - Negated targets are still validated against @loc declarations
///
/// Questionable targets:
/// - @goto-def[col]->?name - Same as ->name but documents that the result is a known and expected LSP bug
///
/// Cross-file references:
/// - @goto-def[col]->file.dml:name - References a location in another file
///
/// Returns a tuple of (location_declarations, unresolved_operations)
#[track_caller]
fn parse_annotations(content: &str, file_path: Option<PathBuf>) -> (Vec<LocationDeclaration>, Vec<UnresolvedAnnotation>) {
    let mut locations = Vec::new();
    let mut operations = Vec::new();
    let lines: Vec<&str> = content.lines().collect();

    // Build a map to find the next code line for any given line
    let next_code_line = build_next_code_line_map(&lines);

    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = u32::try_from(line_idx)
            .unwrap_or_else(|_| panic!("file too large: line index {} overflows u32", line_idx));

        // Determine the effective line for annotations:
        // - If this is a comment-only line, use the next code line
        // - Otherwise use the current line
        let trimmed = line.trim();
        let is_comment_only = trimmed.starts_with("//");
        let effective_line = if is_comment_only {
            next_code_line[line_idx].unwrap_or(line_num)
        } else {
            line_num
        };

        // Check for missing-column errors first (better diagnostics)
        if let Some(m) = RE_OP_NO_COL.find(line) {
            // Only report if it's not also matched by the proper RE_OP
            // Safety: RE_OP_NO_COL always matches strings containing "->"
            let arrow_offset = m.as_str().find("->")
                .expect("BUG: RE_OP_NO_COL matched without '->' present");
            let prefix_end = m.start() + arrow_offset;
            let prefix_text = &line[m.start()..prefix_end];
            if !RE_OP.is_match(line) {
                panic!("annotation '{}' is missing a column: use {}[col]->target\n  \
                        on line {}: {}",
                       m.as_str(), prefix_text, line_num + 1, line.trim());
            }
        }

        // Parse @loc annotations
        // Columns in annotations are 1-indexed (matching editor display),
        // so subtract 1 to convert to internal 0-indexed representation.
        for cap in RE_LOC.captures_iter(line) {
            let col: u32 = cap[1].parse()
                .unwrap_or_else(|_| panic!("column '{}' overflows u32", &cap[1]));
            assert!(col > 0, "@loc column must be 1-indexed (got 0) on line {}", line_num + 1);
            let name = cap[2].to_string();
            locations.push(LocationDeclaration {
                position: Position::<ZeroIndexed>::from_u32(effective_line, col - 1),
                name,
            });
        }

        // Parse operation annotations
        for cap in RE_OP.captures_iter(line) {
            let op_types: Vec<OperationType> = match &cap[1] {
                "goto-def"  => vec![OperationType::GotoDefinition],
                "goto-decl" => vec![OperationType::GotoDeclaration],
                "goto-def-decl" => vec![OperationType::GotoDefinition, OperationType::GotoDeclaration],
                "goto-impl" => vec![OperationType::GotoImplementation],
                "goto-ref" => vec![OperationType::FindReferences],
                other => unreachable!("regex does not match '{}'", other),
            };
            let col: u32 = cap[2].parse()
                .unwrap_or_else(|_| panic!("column '{}' overflows u32", &cap[2]));
            assert!(col > 0, "@{} column must be 1-indexed (got 0) on line {}", &cap[1], line_num + 1);
            let raw_target = cap.get(3)
                .map(|m| m.as_str())
                .unwrap_or("");
            // Strip any trailing //… comment, then trim whitespace
            let target_str = raw_target.split("//").next().unwrap_or("").trim();
            let targets = parse_target_list(target_str, line_num + 1, line);
            // Extract the token at the lookup column from the effective code line
            let effective_line_text = lines.get(effective_line as usize).unwrap_or(&"");
            let token = extract_token_at(effective_line_text, (col - 1) as usize);
            for op_type in op_types {
                operations.push(UnresolvedAnnotation {
                    operation_type: op_type,
                    location: SourceLocation::new(effective_line, col - 1, file_path.clone()),
                    targets: targets.clone(),
                    token: token.clone(),
                });
            }
        }

        // Check for any unrecognized @...-> patterns (likely typos)
        for cap in RE_UNKNOWN_OP.captures_iter(line) {
            let tag = &cap[1];
            let known = matches!(tag,
                "goto-def" | "goto-decl" | "goto-def-decl" | "goto-impl" | "goto-ref");
            if !known {
                panic!(
                    "unrecognized annotation '@{}' on line {}, \
                     expected one of: @goto-def, @goto-decl, @goto-def-decl, @goto-impl, @goto-ref\n  \
                     line {}: {}",
                    tag, line_num + 1, line_num + 1, line.trim()
                );
            }
        }
    }

    (locations, operations)
}

/// Build a mapping from symbolic names to actual locations.
#[track_caller]
fn build_name_mapping(locations: &[LocationDeclaration]) -> HashMap<String, Position<ZeroIndexed>> {
    let mut name_map: HashMap<String, Position<ZeroIndexed>> = HashMap::new();

    for loc in locations {
        if let Some(existing) = name_map.insert(loc.name.clone(), loc.position) {
            panic!("Duplicate @loc name '{}': first at line {}, second at line {}",
                   loc.name, existing.row.0 + 1, loc.position.row.0 + 1);
        }
    }
    name_map
}

/// Resolve symbolic names in targets to actual locations.
/// Panics if any target cannot be resolved.
#[track_caller]
fn resolve_annotations(
    unresolved: Vec<UnresolvedAnnotation>,
    name_map: &HashMap<String, Position<ZeroIndexed>>
) -> Vec<Annotation> {
    // First pass: validate that every target name exists in the map.
    let mut errors = Vec::new();
    for ann in &unresolved {
        for target in &ann.targets {
            let key = target.key();
            if !name_map.contains_key(&key) {
                errors.push(format!(
                    "Annotation at line {} references unknown target '{}' (operation: {:?})",
                    ann.location.line() + 1, key, ann.operation_type
                ));
            }
        }
    }
    if !errors.is_empty() {
        panic!(
            "Found {} unresolved annotation target(s):\n  {}",
            errors.len(),
            errors.join("\n  ")
        );
    }

    // Second pass: build resolved annotations (all lookups are guaranteed to succeed).
    let resolved = unresolved.into_iter().map(|ann| {
        let targets = ann.targets.iter().map(|target| {
            let key = target.key();
            let &position = name_map.get(&key)
                .expect("BUG: validated in first pass");
            ResolvedTarget {
                position,
                filename: target.filename.clone(),
                resolved_name: key,
                negated: target.negated,
            }
        }).collect();
        Annotation {
            operation_type: ann.operation_type,
            location: ann.location,
            targets,
            token: ann.token,
        }
    }).collect();

    resolved
}

/// Build a map from each line to the next non-comment, non-empty code line.
/// Uses a single reverse pass for O(n) complexity.
fn build_next_code_line_map(lines: &[&str]) -> Vec<Option<u32>> {
    let mut result = vec![None; lines.len()];
    let mut next_code: Option<u32> = None;

    for i in (0..lines.len()).rev() {
        // Store the current "next code line" for line i
        result[i] = next_code;
        // Update next_code if line i itself is a code line
        let trimmed = lines[i].trim();
        if !trimmed.is_empty() && !trimmed.starts_with("//") {
            // Line indices in test files will never exceed u32::MAX
            next_code = Some(i as u32);
        }
    }

    result
}

/// Regex for a single target entry: optional `!` or `?` prefix, optional `file.dml:`, then `name`.
static RE_TARGET: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^([!?])?(?:([A-Za-z0-9_.]+):)?([A-Za-z_][A-Za-z0-9_]*)$").unwrap()
});

/// Parse a comma-separated target list such as `name1,file.dml:name2,!negated`.
/// An empty string means "expect no results" and returns an empty vec.
///
/// A `!` prefix on a target marks it as *negated*: the annotation asserts that
/// the location must **not** appear in the lookup results.
///
/// A `?` prefix is a documentation-only marker with no semantic effect.
/// It signals to a reader that the target *is* found by the LSP but is
/// considered a known-incorrect result.
#[track_caller]
fn parse_target_list(s: &str, source_line_num: u32, source_line: &str) -> Vec<TargetSpec> {
    let s = s.trim();
    if s.is_empty() {
        return Vec::new();
    }

    s.split(',')
        .map(|part| {
            let trimmed = part.trim();
            let cap = RE_TARGET.captures(trimmed)
                .unwrap_or_else(|| panic!(
                    "invalid target '{}', expected [!?]name or [!?]file:name\n  \
                     on line {}: {}",
                    trimmed, source_line_num, source_line.trim()));
            let negated = cap.get(1).map_or(false, |m| m.as_str() == "!");
            let name = cap[3].to_string();
            match cap.get(2) {
                Some(file_match) if negated => TargetSpec::new_negated_with_file(
                    file_match.as_str().to_string(), name),
                Some(file_match) => TargetSpec::new_named_with_file(
                    file_match.as_str().to_string(), name),
                None if negated => TargetSpec::new_negated(name),
                None => TargetSpec::new_named(name),
            }
        })
        .collect()
}

/// Load a DML file, run isolated analysis, and store the result.
///
/// If the file contains a `device` declaration, a preliminary device analysis
/// is also created (with an empty import map).  Multi-file setups should call
/// `rebuild_device_analysis_with_imports` afterwards to replace it.
#[track_caller]
fn load_and_analyze_file(
    analysis_storage: &Arc<Mutex<AnalysisStorage>>,
    vfs: &Arc<Vfs>,
    file_path: &Path,
) -> CanonPath {
    let content = std::fs::read_to_string(file_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {}", file_path.display(), e));

    let canon_path = CanonPath::from_path_buf(file_path.to_path_buf())
        .unwrap_or_else(|| panic!("failed to canonicalize path: {}", file_path.display()));

    let text_file = TextFile::from_str(&content)
        .unwrap_or_else(|_| panic!("failed to parse text file: {}", file_path.display()));

    vfs.set_file(&canon_path, &content);

    let (_keeper, status) = JobStatusKeeper::new();
    let isolated = IsolatedAnalysis::new(
        &canon_path,
        &file_path.to_path_buf(),
        text_file,
        status.clone(),
    ).unwrap_or_else(|e| panic!("isolated analysis failed for {}: {}", file_path.display(), e));

    let is_device_file = isolated.toplevel.device.is_some();

    let timestamp = SystemTime::UNIX_EPOCH;
    let mut analysis = analysis_storage.lock().unwrap();
    analysis.isolated_analysis.insert(
        canon_path.clone(),
        TimestampedStorage { timestamp, stored: isolated },
    );

    if is_device_file {
        let isolated = analysis.isolated_analysis[&canon_path].stored.clone();
        insert_preliminary_device_analysis(&mut analysis, &canon_path, isolated, status, timestamp);
    }

    canon_path
}

/// Create a preliminary device analysis from a single isolated analysis.
///
/// Uses an empty import map — suitable for single-file tests.  Multi-file
/// setups replace this via `rebuild_device_analysis_with_imports`.
#[track_caller]
fn insert_preliminary_device_analysis(
    analysis: &mut AnalysisStorage,
    canon_path: &CanonPath,
    isolated: IsolatedAnalysis,
    status: crate::concurrency::AliveStatus,
    timestamp: SystemTime,
) {
    let timed_bases = vec![TimestampedStorage {
        timestamp,
        stored: isolated.clone(),
    }];

    let device_analysis = DeviceAnalysis::new(
        isolated,
        timed_bases,
        HashMap::new(),
        status,
    ).unwrap_or_else(|e| panic!("device analysis failed for {}: {}", canon_path.as_path().display(), e));

    analysis.device_analysis.insert(
        canon_path.clone(),
        TimestampedStorage { timestamp, stored: device_analysis },
    );

    analysis.device_triggers
        .entry(canon_path.clone())
        .or_default()
        .insert(canon_path.clone());
}

/// Test helper to create a ZeroFilePosition
fn make_file_position(path: &Path, line: u32, col: u32) -> ZeroFilePosition {
    ZeroFilePosition::new(ZeroPosition::from_u32(line, col), path.to_path_buf())
}

/// Create an InitActionContext for testing purposes with a given DML file
///
/// This function sets up a minimal but functional InitActionContext that can be used
/// to test LSP operations like goto-definition, find-references, etc.
///
/// # Arguments
/// * `analysis` - The analysis storage containing parsed file data
/// * `vfs` - The virtual file system containing loaded files
///
/// # Returns
/// An InitActionContext configured for testing
fn create_test_init_context<O: Output>(
    analysis: Arc<Mutex<AnalysisStorage>>,
    vfs: Arc<Vfs>,
) -> InitActionContext<O> {
    let ctx = InitActionContext::new_for_testing(Arc::clone(&analysis), vfs);

    // Populate device_active_contexts with all devices in the analysis storage
    // This is necessary for semantic lookup to work properly
    {
        let analysis_lock = analysis.lock().unwrap();
        let mut active_contexts = ctx.device_active_contexts.lock().unwrap();
        for device_path in analysis_lock.device_analysis.keys() {
            // Add all device paths to support cross-file queries
            active_contexts.insert(crate::actions::ContextDefinition::Device(device_path.clone()));
        }
    }

    ctx
}

/// Helper to verify that a file has no parse errors
#[track_caller]
fn verify_no_parse_errors(
    analysis_storage: &Arc<Mutex<AnalysisStorage>>,
    canon_path: &CanonPath,
    filename: &str,
) {
    let analysis = analysis_storage.lock().unwrap();
    let isolated = analysis.get_isolated_analysis(canon_path)
        .unwrap_or_else(|e| panic!("Failed to get isolated analysis for {}: {:?}", filename, e));

    if !isolated.errors.is_empty() {
        panic!(
            "Parse errors found in {}:\n{}",
            filename,
            isolated.errors.iter()
                .map(|e| format!("  - Line {}: {}", e.span.range.row_start.0 + 1, e.description))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Result of setting up a test with annotation-based DML files.
struct TestSetup {
    ctx: InitActionContext<MockOutput>,
    analysis: Arc<Mutex<AnalysisStorage>>,
    main_file: PathBuf,
    main_canon_path: CanonPath,
    annotations: Vec<Annotation>,
}

impl std::fmt::Debug for TestSetup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TestSetup")
            .field("main_file", &self.main_file)
            .field("main_canon_path", &self.main_canon_path.as_path())
            .field("annotations", &self.annotations.len())
            .finish_non_exhaustive()
    }
}

/// A file that has been loaded and analyzed during test setup.
struct LoadedFile {
    canon_path: CanonPath,
    file_path: PathBuf,
    filename: String,
    locations: Vec<LocationDeclaration>,
    operations: Vec<UnresolvedAnnotation>,
}

/// Parse annotations from a single file and tag them with origin info.
///
/// Location names and bare target references are prefixed with the filename
/// so that all symbolic names are fully qualified as `file.dml:name`.
///
/// Targets that already carry a filename (e.g. `other.dml:tag`) are left as-is.
#[track_caller]
fn collect_file_annotations(
    file_path: &Path,
    filename: &str,
    canon_path: &CanonPath,
) -> (Vec<LocationDeclaration>, Vec<UnresolvedAnnotation>) {
    let content = std::fs::read_to_string(file_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", filename, e));

    let (mut locations, mut operations) = parse_annotations(&content, Some(canon_path.to_path_buf()));

    // Prefix @loc names with the declaring filename.
    for loc in &mut locations {
        loc.name = format!("{}:{}", filename, loc.name);
    }

    // Auto-prefix bare (no-file) target references with the declaring filename,
    // so that same-file `@goto-def[col]->tag` resolves to `file.dml:tag`.
    for op in &mut operations {
        for target in &mut op.targets {
            if target.filename.is_none() {
                target.filename = Some(filename.to_string());
            }
        }
    }

    (locations, operations)
}

/// Load all test files, analyze them, and collect per-file annotations.
///
/// The first filename is treated as the main/device file.
#[track_caller]
fn load_all_files(
    filenames: &[&str],
    analysis: &Arc<Mutex<AnalysisStorage>>,
    vfs: &Arc<Vfs>,
    base_dir: &Path,
) -> Vec<LoadedFile> {
    filenames.iter().map(|filename| {
        let file_path = base_dir.join(filename);
        let canon = load_and_analyze_file(analysis, vfs, &file_path);
        verify_no_parse_errors(analysis, &canon, filename);

        let (locations, operations) =
            collect_file_annotations(&file_path, filename, &canon);

        LoadedFile {
            canon_path: canon,
            file_path,
            filename: filename.to_string(),
            locations,
            operations,
        }
    }).collect()
}

/// Rebuild device analysis with a proper import map so cross-file lookups work.
///
/// Only needed when more than one file is loaded. Replaces the preliminary
/// device analysis (created by `load_and_analyze_file`) with one that knows
/// about the imported files.
#[track_caller]
fn rebuild_device_analysis_with_imports(
    analysis: &Arc<Mutex<AnalysisStorage>>,
    device_canon: &CanonPath,
    loaded_files: &[LoadedFile],
) {
    let mut analysis_lock = analysis.lock().unwrap();

    let device_isolated = analysis_lock.get_isolated_analysis(device_canon)
        .expect("Failed to get device isolated analysis")
        .clone();

    // Build import map from the device's import statements
    let mut imp_map: HashMap<Import, String> = HashMap::new();
    for import_decl in &device_isolated.toplevel.spec.imports {
        let import_name = import_decl.obj.name.val.trim_matches('"');
        for loaded in loaded_files {
            if loaded.filename == import_name {
                imp_map.insert(
                    import_decl.obj.clone(),
                    loaded.canon_path.as_path().to_string_lossy().to_string(),
                );
                break;
            }
        }
    }

    // Collect all isolated analyses as bases
    let timestamp = SystemTime::UNIX_EPOCH;
    let mut timed_bases = Vec::new();
    for loaded in loaded_files {
        if let Ok(isolated) = analysis_lock.get_isolated_analysis(&loaded.canon_path) {
            timed_bases.push(TimestampedStorage {
                timestamp,
                stored: isolated.clone(),
            });
        }
    }

    let (_keeper, status) = JobStatusKeeper::new();

    let device_analysis = DeviceAnalysis::new(
        device_isolated,
        timed_bases,
        imp_map,
        status,
    ).expect("Failed to create device analysis with imports");

    analysis_lock.device_analysis.insert(
        device_canon.clone(),
        TimestampedStorage {
            timestamp,
            stored: device_analysis,
        },
    );

    // Register imported files in device_triggers so lookups from them
    // find the main device analysis.
    for loaded in loaded_files {
        if loaded.canon_path != *device_canon {
            analysis_lock.device_triggers
                .entry(loaded.canon_path.clone())
                .or_default()
                .insert(device_canon.clone());
        }
    }
}

/// Helper to load test files and parse annotations from all files.
/// Accepts a slice of filenames — all files are loaded and analyzed, and
/// annotations from all files are parsed and merged.
#[track_caller]
fn setup_test_with_annotations(
    filenames: &[&str],
) -> TestSetup {
    assert!(!filenames.is_empty(), "At least one filename must be provided");

    let vfs = Arc::new(Vfs::new());
    let (sender, _receiver) = crossbeam::channel::unbounded::<crate::server::ServerToHandle>();
    let analysis = Arc::new(Mutex::new(AnalysisStorage::init(sender)));
    let base_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/test/test_files");

    let loaded_files = load_all_files(filenames, &analysis, &vfs, &base_dir);

    let main = &loaded_files[0];
    let main_file = main.file_path.clone();
    let main_canon = main.canon_path.clone();

    if loaded_files.len() > 1 {
        rebuild_device_analysis_with_imports(&analysis, &main_canon, &loaded_files);
    }

    let all_locations: Vec<_> = loaded_files.iter()
        .flat_map(|f| &f.locations)
        .cloned()
        .collect();
    let all_operations: Vec<_> = loaded_files.into_iter()
        .flat_map(|f| f.operations)
        .collect();

    let name_map = build_name_mapping(&all_locations);
    let annotations = resolve_annotations(all_operations, &name_map);

    let ctx: InitActionContext<MockOutput> = create_test_init_context(
        Arc::clone(&analysis),
        Arc::clone(&vfs),
    );

    TestSetup { ctx, analysis, main_file, main_canon_path: main_canon, annotations }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Initialize env_logger for tests. Safe to call from multiple tests —
    /// `try_init` silently succeeds on the first call and is a no-op thereafter.
    fn init_logging() {
        let _ = env_logger::try_init();
    }

    #[test]
    fn test_can_load_and_parse_dml_file() {
        init_logging();
        let setup = setup_test_with_annotations(&["basic_lookup.dml"]);

        // Verify we can get the isolated analysis
        let analysis_lock = setup.analysis.lock().unwrap();
        let isolated = analysis_lock.get_isolated_analysis(&setup.main_canon_path);
        assert!(isolated.is_ok(), "Failed to get isolated analysis");
    }

    #[test]
    fn test_device_analysis_created() {
        init_logging();
        let setup = setup_test_with_annotations(&["basic_lookup.dml"]);

        // Check that device analysis was created
        let analysis_lock = setup.analysis.lock().unwrap();
        let device_count = analysis_lock.device_analysis.len();

        assert!(device_count > 0, "No device analysis was created");
    }

    #[test]
    fn test_toplevel_structure_parsed() {
        init_logging();
        let setup = setup_test_with_annotations(&["basic_lookup.dml"]);

        let analysis_lock = setup.analysis.lock().unwrap();
        let isolated = analysis_lock.get_isolated_analysis(&setup.main_canon_path)
            .expect("Failed to get isolated analysis");

        // Verify device was parsed
        assert!(isolated.toplevel.device.is_some(), "Device not found in toplevel");

        // Count templates
        let template_count = isolated.toplevel.templates.len();
        assert!(template_count >= 2, "Expected at least 2 templates");
    }

    #[test]
    fn test_symbols_created_in_device_analysis() {
        init_logging();
        let setup = setup_test_with_annotations(&["basic_lookup.dml"]);

        let analysis_lock = setup.analysis.lock().unwrap();

        // Get the device analysis for this file
        let device_analysis = analysis_lock.get_device_analysis(&setup.main_canon_path)
            .expect("Failed to get device analysis");

        // Check that symbols were created
        let all_symbols: Vec<_> = device_analysis.symbol_info.all_symbols().collect();
        assert!(!all_symbols.is_empty(), "No symbols were created");
    }

    #[test]
    fn test_parse_annotations_from_test_file() {
        init_logging();
        let content = concat!(
            "dml 1.4;\n",
            "device d;\n",
            "template t { // @loc[10]=t_def\n",
            "    param p default 0; // @loc[11]=p_def\n",
            "}\n",
            "bank b is t { // @goto-def[11]->t_def\n",
            "    param p default 1; // @goto-decl[11]->p_def\n",
            "}\n",
        );

        let (locations, operations) = parse_annotations(content, None);

        assert_eq!(locations.len(), 2, "expected 2 @loc declarations");
        assert_eq!(operations.len(), 2, "expected 2 operation annotations");

        // Verify the loc names
        assert!(locations.iter().any(|l| l.name == "t_def"), "missing t_def");
        assert!(locations.iter().any(|l| l.name == "p_def"), "missing p_def");

        // Verify operation types
        assert!(operations.iter().any(|o| o.operation_type == OperationType::GotoDefinition),
                "expected a goto-def operation");
        assert!(operations.iter().any(|o| o.operation_type == OperationType::GotoDeclaration),
                "expected a goto-decl operation");
    }

    #[test]
    fn test_stacked_annotations_get_correct_line() {
        init_logging();
        // Two comment-only lines followed by a code line.
        // All three @loc annotations should land on the code line (line 2, 0-indexed).
        let content = concat!(
            "// @loc[11]=upper\n",
            "// @loc[21]=middle\n",
            "code_here(); // @loc[1]=on_code\n",
        );

        let (locations, _) = parse_annotations(content, None);

        let upper  = locations.iter().find(|l| l.name == "upper")
            .expect("missing upper");
        let middle = locations.iter().find(|l| l.name == "middle")
            .expect("missing middle");
        let on_code = locations.iter().find(|l| l.name == "on_code")
            .expect("missing on_code");

        // All three should resolve to the same effective line
        assert_eq!(upper.line(), on_code.line(),
                   "upper should be on the code line");
        assert_eq!(middle.line(), on_code.line(),
                   "middle should be on the code line");

        // Columns should be preserved as written (minus 1, since file is 1-indexed)
        assert_eq!(upper.col(), 10);
        assert_eq!(middle.col(), 20);
        assert_eq!(on_code.col(), 0);
    }

    #[test]
    fn test_negated_target_parsing() {
        init_logging();
        let content = concat!(
            "dml 1.4;\n",
            "device d;\n",
            "// @loc[10]=t_def\n",
            "// @loc[24]=other_def\n",
            "template t is other { // @goto-def[10]->!t_def,other_def\n",
            "}\n",
        );

        let (locations, operations) = parse_annotations(content, None);
        assert_eq!(locations.len(), 2);
        assert_eq!(operations.len(), 1);

        let op = &operations[0];
        assert_eq!(op.targets.len(), 2, "expected 2 targets");
        assert!(op.targets[0].negated, "first target should be negated");
        assert_eq!(op.targets[0].name, "t_def");
        assert!(!op.targets[1].negated, "second target should be positive");
        assert_eq!(op.targets[1].name, "other_def");
    }

    #[test]
    fn test_extract_token_at() {
        let line = "    method foo_bar(int x) -> (int) {";
        //          0123456789...

        // Middle of an identifier
        assert_eq!(extract_token_at(line, 11), "foo_bar");
        // Start of the same identifier
        assert_eq!(extract_token_at(line, 11), "foo_bar");
        // On a keyword
        assert_eq!(extract_token_at(line, 4), "method");
        // On a delimiter character '('
        assert_eq!(extract_token_at(line, 18), "(");
        // At a space
        assert_eq!(extract_token_at(line, 10), " ");
        // Parameter type
        assert_eq!(extract_token_at(line, 19), "int");
        // Past end of line
        assert_eq!(extract_token_at(line, 999), "<eol>");
        // Exactly at line length
        assert_eq!(extract_token_at(line, line.len()), "<eol>");

        // Empty line
        assert_eq!(extract_token_at("", 0), "<eol>");

        // Identifier at very start of line
        assert_eq!(extract_token_at("hello world", 0), "hello");
    }

    #[test]
    fn test_can_create_init_action_context() {
        init_logging();
        let setup = setup_test_with_annotations(&["basic_lookup.dml"]);

        // Verify the context was created successfully and has device analysis
        let analysis_lock = setup.ctx.analysis.lock().unwrap();
        assert!(!analysis_lock.device_analysis.is_empty(),
                "Context should have device analysis");
    }

    #[test]
    fn test_basic_lookup_annotations() {
        init_logging();
        let setup = setup_test_with_annotations(&["basic_lookup.dml"]);
        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_multi_level_inheritance() {
        init_logging();
        let setup = setup_test_with_annotations(&["multi_level.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_cross_file_references() {
        init_logging();
        // Load both files - imported file first, then main file with annotations
        let setup = setup_test_with_annotations(&["cross_file_main.dml", "imported_file.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_simple_symbolic_annotations() {
        init_logging();
        let setup = setup_test_with_annotations(&["simple_symbolic.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_goto_implementation_comprehensive() {
        init_logging();
        // Test comprehensive goto-implementation scenarios:
        // - Methods: find all overriding declarations
        // - Templates: find all instantiation sites
        let setup = setup_test_with_annotations(&["goto_impl_test.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    /// Helper function to run annotation tests.
    /// Groups annotations by operation type in a single pass, then dispatches each group.
    /// All operation types are tested regardless of earlier failures — the full set of
    /// failures is collected and reported in a single panic at the end.
    #[track_caller]
    fn run_annotation_tests(
        ctx: &InitActionContext<MockOutput>,
        test_file: &Path,
        annotations: Vec<Annotation>,
    ) {
        let mut goto_def = Vec::new();
        let mut goto_decl = Vec::new();
        let mut goto_impl = Vec::new();
        let mut find_refs = Vec::new();

        for ann in &annotations {
            match ann.operation_type {
                OperationType::GotoDefinition => goto_def.push(ann),
                OperationType::GotoDeclaration => goto_decl.push(ann),
                OperationType::GotoImplementation => goto_impl.push(ann),
                OperationType::FindReferences => find_refs.push(ann),
            }
        }

        let mut all_sections = Vec::new();
        let mut total_failures = 0usize;

        #[allow(clippy::type_complexity)]
        let ops: Vec<(&[&Annotation], OperationType, &dyn Fn(&InitActionContext<MockOutput>, &ZeroFilePosition, &mut HashSet<DLSLimitation>) -> Result<Vec<ZeroSpan>, AnalysisLookupError>)> = vec![
            (&goto_def,  OperationType::GotoDefinition,     &definitions_at_fp),
            (&goto_decl, OperationType::GotoDeclaration,    &declarations_at_fp),
            (&goto_impl, OperationType::GotoImplementation, &implementations_at_fp),
            (&find_refs, OperationType::FindReferences,     &references_at_fp),
        ];

        for (anns, op_type, lookup_fn) in ops {
            if anns.is_empty() {
                continue;
            }
            let (fail_count, total, failures) =
                test_goto_operation(ctx, test_file, anns, op_type, lookup_fn);
            if !failures.is_empty() {
                total_failures += fail_count;
                all_sections.push(format!(
                    "{} out of {} {} annotation(s) failed:\n  {}",
                    fail_count, total, op_type,
                    failures.join("\n  ")
                ));
            }
        }

        assert!(all_sections.is_empty(),
            "{} annotation failure(s):\n\n{}",
            total_failures,
            all_sections.join("\n\n"));
    }

    /// Check whether a span matches a target's expected position and optional file
    fn target_matches_span(target: &ResolvedTarget, span: &ZeroSpan) -> bool {
        let position_matches = span.range.row_start.0 == target.line()
            && span.range.col_start.0 == target.col();
        if let Some(ref target_filename) = target.filename {
            let span_path = span.path();
            let span_filename = span_path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");
            position_matches && span_filename == target_filename
        } else {
            position_matches
        }
    }

    /// Format a human-readable display string for a missing target
    fn format_missing_target(target: &ResolvedTarget, main_filename: &str) -> String {
        // Extract the bare symbolic name (strip file prefix if present)
        let symbolic_name = target.resolved_name.split_once(':')
            .map_or(target.resolved_name.as_str(), |(_, name)| name);
        let location = if let Some(ref filename) = target.filename {
            format!("{}:{}:{}", filename, target.line() + 1, target.col() + 1)
        } else if let Some((file_part, _)) = target.resolved_name.split_once(':') {
            format!("{}:{}:{}", file_part, target.line() + 1, target.col() + 1)
        } else {
            format!("{}:{}:{}", main_filename, target.line() + 1, target.col() + 1)
        };
        format!("'{}' @ {}", symbolic_name, location)
    }

    /// Match targets against returned spans.
    ///
    /// For **positive** (non-negated) targets: checks they ARE present.
    /// For **negated** targets: checks they are NOT present.
    ///
    /// Returns `(found_positive, failures)` where `failures` are formatted error strings.
    fn match_targets_against_spans(
        targets: &[ResolvedTarget],
        spans: &[ZeroSpan],
        main_filename: &str,
    ) -> (usize, Vec<String>) {
        let mut found_positive = 0;
        let mut failures = Vec::new();
        for target in targets {
            let present = spans.iter().any(|span| target_matches_span(target, span));
            if target.negated {
                if present {
                    failures.push(format!("unexpectedly present: {}",
                                          format_missing_target(target, main_filename)));
                }
            } else if present {
                found_positive += 1;
            } else {
                failures.push(format!("missing: {}",
                                      format_missing_target(target, main_filename)));
            }
        }
        (found_positive, failures)
    }

    /// Deduplicate spans and optionally filter to the query file.
    /// Returns the relevant (deduplicated) spans.
    fn deduplicate_spans<'a>(
        spans: &'a [ZeroSpan],
        ann: &Annotation,
        test_file: &Path,
    ) -> Vec<&'a ZeroSpan> {
        let has_cross_file_targets = ann.targets.iter().any(|t| t.filename.is_some());
        let query_file_path = ann.location.file_path.as_deref().unwrap_or(test_file);
        let query_fname = query_file_path.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");

        let mut seen_positions = HashSet::new();
        let mut result = Vec::new();
        for span in spans {
            let span_path = span.path();
            let span_fname = span_path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");
            let key = (span.range.row_start.0, span.range.col_start.0, span_fname.to_string());
            if !seen_positions.insert(key) {
                continue;
            }
            if has_cross_file_targets || span_fname == query_fname {
                result.push(span);
            }
        }
        result
    }

    /// Find extra (unexpected) spans that don't match any positive (non-negated) target.
    /// Negated targets are excluded from this check — they are already validated in
    /// `match_targets_against_spans`.
    /// Returns formatted display strings for each extra span.
    fn find_extra_spans(ann: &Annotation, spans: &[&ZeroSpan]) -> Vec<String> {
        spans.iter()
            .filter(|span| !ann.targets.iter().any(|t| !t.negated && target_matches_span(t, span)))
            .map(|span| {
                let span_path = span.path();
                let fname = span_path.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("<unknown>");
                format!("{}:{}:{}", fname, span.range.row_start.0 + 1, span.range.col_start.0 + 1)
            })
            .collect()
    }

    /// Evaluate a single annotation against lookup results.
    /// Returns Ok(summary) on success, Err(detail) on failure.
    fn evaluate_annotation(
        ann: &Annotation,
        spans: &[ZeroSpan],
        test_file: &Path,
        location_str: &str,
    ) -> Result<String, String> {
        let main_filename = test_file.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("<unknown>");

        let relevant = deduplicate_spans(spans, ann, test_file);
        let relevant_spans: Vec<ZeroSpan> = relevant.into_iter().cloned().collect();
        let positive_count = ann.targets.iter().filter(|t| !t.negated).count();
        let negated_count = ann.targets.iter().filter(|t| t.negated).count();
        let (found_positive, target_failures) =
            match_targets_against_spans(&ann.targets, &relevant_spans, main_filename);
        let extra_targets = find_extra_spans(ann, &relevant_spans.iter().collect::<Vec<_>>());

        if target_failures.is_empty() && extra_targets.is_empty() {
            let mut summary = format!("✓ {} {} on '{}' found {}/{} expected",
                                      location_str, ann.operation_type, ann.token,
                                      found_positive, positive_count);
            if negated_count > 0 {
                summary.push_str(&format!(", {} correctly absent", negated_count));
            }
            return Ok(summary);
        }

        let mut detail = format!("✗ {} {} on '{}' found {}/{} expected",
                                 location_str, ann.operation_type, ann.token,
                                 found_positive, positive_count);
        if negated_count > 0 {
            detail.push_str(&format!(", {} negated", negated_count));
        }
        for f in &target_failures {
            detail.push_str(&format!("\n      {}", f));
        }
        for e in &extra_targets {
            detail.push_str(&format!("\n      extra:   {}", e));
        }
        Err(detail)
    }

    /// Generic function to test any goto operation.
    /// Accepts a pre-filtered slice of annotations that all share `ann_type`.
    /// Returns `(failure_count, total_count, failure_details)` instead of panicking,
    /// so the caller can aggregate failures across all operation types.
    fn test_goto_operation<F>(
        ctx: &InitActionContext<MockOutput>,
        test_file: &Path,
        test_annotations: &[&Annotation],
        _ann_type: OperationType,
        lookup_fn: F,
    ) -> (usize, usize, Vec<String>)
    where
        F: Fn(&InitActionContext<MockOutput>, &ZeroFilePosition, &mut HashSet<DLSLimitation>)
            -> Result<Vec<ZeroSpan>, AnalysisLookupError>,
    {
        let mut failures = Vec::new();

        for ann in test_annotations {
            let query_file = ann.location.file_path.as_deref().unwrap_or(test_file);
            let file_pos = make_file_position(query_file, ann.location.line(), ann.location.col());
            let mut limitations = HashSet::new();

            let query_filename = query_file.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("<unknown>");
            let location_str = format!("{}:{}:{}",
                                       query_filename,
                                       ann.location.line() + 1,
                                       ann.location.col() + 1);

            match lookup_fn(ctx, &file_pos, &mut limitations) {
                Ok(spans) => {
                    if let Err(detail) = evaluate_annotation(ann, &spans, test_file, &location_str) {
                        failures.push(detail);
                    }
                }
                Err(e) => {
                    failures.push(format!("✗ {} error: {:?}", location_str, e));
                }
            }
        }

        let fail_count = failures.len();
        (fail_count, test_annotations.len(), failures)
    }
}
