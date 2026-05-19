//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

use std::convert::TryFrom;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::SystemTime;
use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

use regex::Regex;

use crate::actions::analysis_storage::{AnalysisStorage, TimestampedStorage, AnalysisLookupError};
use crate::actions::{InitActionContext, DeviceAnalysisJobOptions};
use crate::concurrency::JobStatusKeeper;
use crate::analysis::{DeviceAnalysis, IsolatedAnalysis, ZeroFilePosition};
use crate::analysis::parsing::tree::{LeafToken, TreeElement, ZeroPosition, ZeroSpan};
use crate::analysis::structure::objects::Import;
use crate::vfs::{TextFile, Vfs};
use crate::span::{Position, ZeroIndexed};
use crate::file_management::{CanonPath, PathResolver};
use crate::server::io::Output;
use crate::actions::semantic_lookup::{definitions_at_fp, declarations_at_fp,
                                       implementations_at_fp, references_at_fp, DLSLimitation};

// Mock output for testing
#[derive(Clone, Debug)]
struct MockOutput;

impl Output for MockOutput {
    fn response(&self, _output: String) {
        // noop
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
    file_path: Option<PathBuf>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TargetKind {
    /// Must appear in the lookup results.
    Positive,
    /// Must NOT appear in the lookup results.
    Negated,
    /// Known to be incorrectly present today.
    Questionable,
}

/// Target specifier - a symbolic reference to where an annotation should resolve.
/// These are specified in test files as names like "my_func" or "file:my_func"
/// and get resolved to concrete positions via the name mapping.
#[derive(Debug, Clone)]
struct TargetSpec {
    name: String,
    filename: Option<String>,  // Relative path under `src/test/test_files/`, exactly as passed to `setup_test`
    kind: TargetKind,
}

impl TargetSpec {
    /// The lookup key for this target in the name map.
    fn key(&self) -> String {
        match self.filename {
            Some(ref filename) => format!("{}:{}", filename, self.name),
            None => self.name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
struct ResolvedTarget {
    spec: TargetSpec,
    /// Position of the target `@loc` declaration.
    position: Position<ZeroIndexed>,
}

impl Deref for ResolvedTarget {
    type Target = TargetSpec;
    fn deref(&self) -> &TargetSpec { &self.spec }
}

impl ResolvedTarget {
    /// Path of the file containing the target `@loc`, relative to
    /// `src/test/test_files/`. Always populated post-resolution.
    fn filename(&self) -> &str {
        self.spec.filename.as_deref()
            .expect("BUG: collect_file_annotations should have prefixed all bare targets with a filename")
    }

    fn line(&self) -> u32 {
        self.position.row.0
    }

    fn col(&self) -> u32 {
        self.position.col.0
    }
}

#[derive(Debug, Clone)]
struct Annotation<T> {
    operation_type: OperationType,
    location: SourceLocation,           // of the annotation
    targets: Vec<T>,                    // targeted names
    token: String,                      // token in source the annotation marked
}

type UnresolvedAnnotation = Annotation<TargetSpec>;
type ResolvedAnnotation = Annotation<ResolvedTarget>;

/// Look up the source token at a position by walking the parse tree.
fn token_at_position(
    analysis: &Arc<Mutex<AnalysisStorage>>,
    canon_path: &CanonPath,
    lines: &[&str],
    pos: Position<ZeroIndexed>,
) -> String {
    let analysis_lock = analysis.lock().unwrap();
    let isolated = match analysis_lock.get_isolated_analysis(canon_path) {
        Ok(iso) => iso,
        Err(_) => return "<no-analysis>".to_string(),
    };
    let leaf = match isolated.ast.get_leaf(pos) {
        Some(LeafToken::Actual(tok)) => tok,
        _ => return "<no-token>".to_string(),
    };
    let range = leaf.range;
    if range.row_start != range.row_end {
        return "<multi-line-token>".to_string();
    }
    let Some(line) = lines.get(range.row_start.0 as usize).copied() else {
        return "<bad-range>".to_string();
    };
    let start = range.col_start.0 as usize;
    let end = range.col_end.0 as usize;
    if end > line.len() {
        return "<bad-range>".to_string();
    }
    line[start..end].to_string()
}

/// Regex for `@loc[col]=name`
static RE_LOC: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@loc\[(\d+)\]=([A-Za-z_][A-Za-z0-9_]*)").unwrap()
});

/// Regex for operation annotations
static RE_OP: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"@(goto-def-decl|goto-def|goto-decl|goto-impl|goto-ref)\[(\d+)\]->([^@]*)").unwrap()
});

/// Regex for an operation annotation that is missing the required column bracket.
/// Used for detecting test-writer errors
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
/// - ..., !name, ... - Asserts that the location specified by name is NOT in the lookup results,
///   useful for verifying that incorrect but similar results are properly excluded.
///   Negated targets are still validated against @loc declarations.
///
/// Questionable targets:
/// - ..., ?name, ... - Asserts the target IS present, but marks it as a known
///   incorrect result. Matches like a positive target (suppresses "extra"
///   warnings), but tallied separately so the bug stays visible. Fails if the
///   target stops being returned, so we notice regressions or fixes.
///
/// Cross-file references:
/// - @goto-def[col]->file.dml:name - References a location in another file
#[track_caller]
fn parse_annotations(content: &str, file_path: Option<PathBuf>) -> (Vec<LocationDeclaration>, Vec<UnresolvedAnnotation>) {
    let mut locations = Vec::new();
    let mut operations = Vec::new();
    let lines: Vec<&str> = content.lines().collect();

    // Build a map to find the next non-annotated code line for any given line, used
    // to track rows of annotations
    let next_code_line = build_next_code_line_map(&lines);

    for (line_idx, line) in lines.iter().enumerate() {
        let line_num = u32::try_from(line_idx)
            .unwrap_or_else(|_| panic!("file too large: line index {} overflows u32", line_idx));

        let trimmed = line.trim();
        let is_comment_only = trimmed.starts_with("//");
        let effective_line = if is_comment_only {
            next_code_line[line_idx].unwrap_or(line_num)
        } else {
            line_num
        };

        if let Some(m) = RE_OP_NO_COL.find(line) {
            // Report missing column specifiers (test writer error)
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

        // Parse @loc annotations, converting their 1-indexing to the internal 0-indexing
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
            for op_type in op_types {
                operations.push(Annotation {
                    operation_type: op_type,
                    location: SourceLocation::new(effective_line, col - 1, file_path.clone()),
                    targets: targets.clone(),
                    token: String::new(),
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

/// Resolve symbolic names in targets to actual locations
#[track_caller]
fn resolve_annotations(
    unresolved: Vec<UnresolvedAnnotation>,
    name_map: &HashMap<String, Position<ZeroIndexed>>
) -> Vec<ResolvedAnnotation> {
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
    unresolved.into_iter().map(|ann| Annotation {
        operation_type: ann.operation_type,
        location: ann.location,
        token: ann.token,
        targets: ann.targets.into_iter().map(|spec| {
            let &position = name_map.get(&spec.key())
                .expect("BUG: validated in first pass");
            ResolvedTarget { spec, position }
        }).collect(),
    }).collect()
}

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

/// Regex for a single target entry
static RE_TARGET: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^([!?])?(?:([A-Za-z0-9_./-]+):)?([A-Za-z_][A-Za-z0-9_]*)$").unwrap()
});

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
            let kind = match cap.get(1).map(|m| m.as_str()) {
                Some("!") => TargetKind::Negated,
                Some("?") => TargetKind::Questionable,
                _ => TargetKind::Positive,
            };
            TargetSpec {
                name: cap[3].to_string(),
                filename: cap.get(2).map(|m| m.as_str().to_string()),
                kind,
            }
        })
        .collect()
}

/// Load a DML file, run isolated analysis, and store the result
#[track_caller]
fn load_isolated_analysis(
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
    ).unwrap_or_else(|e| panic!("isolated analysis failed for {}: {:?}", file_path.display(), &e));

    let timestamp = SystemTime::UNIX_EPOCH;
    let mut analysis = analysis_storage.lock().unwrap();
    analysis.isolated_analysis.insert(
        canon_path.clone(),
        TimestampedStorage { timestamp, stored: isolated },
    );

    canon_path
}

/// Test helper to create a ZeroFilePosition
fn make_file_position(path: &Path, line: u32, col: u32) -> ZeroFilePosition {
    ZeroFilePosition::new(ZeroPosition::from_u32(line, col), path.to_path_buf())
}

/// Create an InitActionContext for testing purposes with a given DML file
fn create_test_init_context<O: Output>(
    analysis: Arc<Mutex<AnalysisStorage>>,
    vfs: Arc<Vfs>,
) -> InitActionContext<O> {
    let ctx = InitActionContext::new_for_testing(Arc::clone(&analysis), vfs);
    // Populate device_active_contexts with all devices in the analysis storage
    {
        let analysis_lock = analysis.lock().unwrap();
        let mut active_contexts = ctx.device_active_contexts.lock().unwrap();
        for device_path in analysis_lock.device_analysis.keys() {
            active_contexts.insert(crate::actions::ContextDefinition::Device(device_path.clone()));
        }
    }

    ctx
}

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

struct TestSetup {
    ctx: InitActionContext<MockOutput>,
    analysis: Arc<Mutex<AnalysisStorage>>,
    main_file: PathBuf,
    main_canon_path: CanonPath,
    annotations: Vec<ResolvedAnnotation>,
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
    filename: String,
    locations: Vec<LocationDeclaration>,
    operations: Vec<UnresolvedAnnotation>,
    is_device: bool,
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
    analysis: &Arc<Mutex<AnalysisStorage>>,
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

    // Resolve display tokens against the AST now that the file has been
    // analyzed in isolation. This lets cursors land in the middle of an
    // identifier and still get the full identifier text in test output.
    let lines: Vec<&str> = content.lines().collect();
    for op in &mut operations {
        op.token = token_at_position(analysis, canon_path, &lines, op.location.position);
    }

    (locations, operations)
}

// Helper function to build a device analysis based on isolated analysis available within a storage
#[track_caller]
fn build_device_analysis_from_storage(
    analysis: &Arc<Mutex<AnalysisStorage>>,
    device_canon: &CanonPath,
) {
    let mut analysis_lock = analysis.lock().unwrap();

    let device_isolated = analysis_lock.get_isolated_analysis(device_canon)
        .expect("Failed to get device isolated analysis")
        .clone();

    let imp_map: HashMap<Import, CanonPath> = analysis_lock.import_map
        .get(device_canon)
        .and_then(|by_ctx| by_ctx.get(&Some(device_canon.clone())))
        .cloned()
        .unwrap_or_default();

    let dep_paths: HashSet<CanonPath> = analysis_lock
        .all_dependencies(device_canon, Some(device_canon));

    let timestamp = SystemTime::UNIX_EPOCH;
    let mut timed_bases = Vec::new();
    for dep in &dep_paths {
        if let Ok(isolated) = analysis_lock.get_isolated_analysis(dep) {
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
        DeviceAnalysisJobOptions { max_reference_cache_size: 0 },
        status,
    ).unwrap_or_else(|e| panic!("Failed to build device analysis with imports: {:?}", &e));

    analysis_lock.device_analysis.insert(
        device_canon.clone(),
        TimestampedStorage { timestamp, stored: device_analysis },
    );
}

#[track_caller]
fn setup_test_with_imports(
    filenames: &[&str],
    include_paths: &[(&str, &[&str])],
) -> TestSetup {
    assert!(!filenames.is_empty(), "At least one filename must be provided");

    let vfs = Arc::new(Vfs::new());
    let (sender, _receiver) = crossbeam::channel::unbounded::<crate::server::ServerToHandle>();
    let analysis = Arc::new(Mutex::new(AnalysisStorage::init(sender)));
    let base_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/test/test_files");

    let loaded_files: Vec<LoadedFile> = filenames.iter().map(|filename| {
        let file_path = base_dir.join(filename);
        let canon = load_isolated_analysis(&analysis, &vfs, &file_path);
        verify_no_parse_errors(&analysis, &canon, filename);
        let (locations, operations) =
            collect_file_annotations(&file_path, filename, &canon, &analysis);
        let is_device = analysis.lock().unwrap()
            .get_isolated_analysis(&canon)
            .expect("isolated analysis must exist for just-loaded file")
            .is_device_file();
        LoadedFile {
            canon_path: canon,
            filename: filename.to_string(),
            locations,
            operations,
            is_device,
        }
    }).collect();

    // Build a PathResolver with the base test_files directory as root and
    // any per-device include paths
    let mut resolver = PathResolver::from(Some(base_dir.clone()));
    let mut include_map: HashMap<CanonPath, Vec<PathBuf>> = HashMap::new();
    for (device_fname, paths) in include_paths {
        let device_loaded = loaded_files.iter()
            .find(|l| l.filename == *device_fname)
            .unwrap_or_else(|| panic!(
                "include_paths references unknown file '{}'", device_fname));
        let resolved_paths: Vec<PathBuf> = paths.iter()
            .map(|p| base_dir.join(p))
            .collect();
        include_map.insert(device_loaded.canon_path.clone(), resolved_paths);
    }
    resolver.set_include_paths(&include_map);

    analysis.lock().unwrap().update_all_context_dependencies(resolver);

    for loaded in &loaded_files {
        if loaded.is_device {
            build_device_analysis_from_storage(&analysis, &loaded.canon_path);
        }
    }

    let main_file = base_dir.join(filenames[0]);
    let main_canon = loaded_files[0].canon_path.clone();

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

#[track_caller]
fn setup_test(filenames: &[&str]) -> TestSetup {
    setup_test_with_imports(filenames, &[])
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Signature shared by all semantic-lookup entry points
    /// (`definitions_at_fp`, `declarations_at_fp`, ...).
    type LookupFn = fn(&InitActionContext<MockOutput>,
                      &ZeroFilePosition,
                      &mut HashSet<DLSLimitation>)
                      -> Result<Vec<ZeroSpan>, AnalysisLookupError>;

    fn init_logging() {
        let _ = env_logger::try_init();
    }

    #[test]
    fn test_can_load_and_parse_dml_file() {
        init_logging();
        let setup = setup_test(&["basic_lookup.dml"]);

        // Verify we can get the isolated analysis
        let analysis_lock = setup.analysis.lock().unwrap();
        let isolated = analysis_lock.get_isolated_analysis(&setup.main_canon_path);
        assert!(isolated.is_ok(), "Failed to get isolated analysis");
    }

    #[test]
    fn test_device_analysis_created() {
        init_logging();
        let setup = setup_test(&["basic_lookup.dml"]);

        // Check that device analysis was created
        let analysis_lock = setup.analysis.lock().unwrap();
        let device_count = analysis_lock.device_analysis.len();

        assert!(device_count > 0, "No device analysis was created");
    }

    #[test]
    fn test_toplevel_structure_parsed() {
        init_logging();
        let setup = setup_test(&["basic_lookup.dml"]);

        let analysis_lock = setup.analysis.lock().unwrap();
        let isolated = analysis_lock.get_isolated_analysis(&setup.main_canon_path)
            .expect("Failed to get isolated analysis");

        // Verify device was parsed
        assert!(isolated.toplevel.device.is_some(), "Device not found in toplevel");

        // Count templates
        let template_count = isolated.toplevel.spec.templates.len();
        assert!(template_count >= 2, "Expected at least 2 templates");
    }

    #[test]
    fn test_symbols_created_in_device_analysis() {
        init_logging();
        let setup = setup_test(&["basic_lookup.dml"]);

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

        assert!(locations.iter().any(|l| l.name == "t_def"), "missing t_def");
        assert!(locations.iter().any(|l| l.name == "p_def"), "missing p_def");

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
            "// @loc[34]=quirky_def\n",
            "template t is other { // @goto-def[10]->!t_def,other_def,?quirky_def\n",
            "}\n",
        );

        let (locations, operations) = parse_annotations(content, None);
        assert_eq!(locations.len(), 3);
        assert_eq!(operations.len(), 1);

        let op = &operations[0];
        assert_eq!(op.targets.len(), 3, "expected 3 targets");
        assert_eq!(op.targets[0].kind, TargetKind::Negated, "first target should be negated");
        assert_eq!(op.targets[0].name, "t_def");
        assert_eq!(op.targets[1].kind, TargetKind::Positive, "second target should be positive");
        assert_eq!(op.targets[1].name, "other_def");
        assert_eq!(op.targets[2].kind, TargetKind::Questionable, "third target should be questionable");
        assert_eq!(op.targets[2].name, "quirky_def");
    }

    #[test]
    fn test_token_at_position_recovers_full_identifier() {
        init_logging();
        // basic_lookup.dml line 10 (1-indexed) is:
        //     `template base_template { ...`
        // so `base_template` occupies 0-indexed columns 9..=21.
        let setup = setup_test(&["basic_lookup.dml"]);
        let content = std::fs::read_to_string(&setup.main_file)
            .expect("failed to read fixture");
        let lines: Vec<&str> = content.lines().collect();
        let row = 9; // 0-indexed line 10

        let at = |col: u32| token_at_position(
            &setup.analysis,
            &setup.main_canon_path,
            &lines,
            ZeroPosition::from_u32(row, col),
        );

        // Start, middle, and last character of the identifier all resolve to
        // the full token via the AST.
        assert_eq!(at(9),  "base_template", "start of identifier");
        assert_eq!(at(13), "base_template", "middle of identifier");
        assert_eq!(at(21), "base_template", "last character of identifier");
    }

    #[test]
    fn test_can_create_init_action_context() {
        init_logging();
        let setup = setup_test(&["basic_lookup.dml"]);

        // Verify the context was created successfully and has device analysis
        let analysis_lock = setup.ctx.analysis.lock().unwrap();
        assert!(!analysis_lock.device_analysis.is_empty(),
                "Context should have device analysis");
    }

    #[test]
    fn test_basic_lookup_annotations() {
        init_logging();
        let setup = setup_test(&["basic_lookup.dml"]);
        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_multi_level_inheritance() {
        init_logging();
        let setup = setup_test(&["multi_level.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_cross_file_references() {
        init_logging();
        // Load both files - imported file first, then main file with annotations
        let setup = setup_test(&["cross_file_main.dml", "imported_file.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_simple_symbolic_annotations() {
        init_logging();
        let setup = setup_test(&["simple_symbolic.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    #[test]
    fn test_goto_implementation_comprehensive() {
        init_logging();
        // Test comprehensive goto-implementation scenarios:
        // - Methods: find all overriding declarations
        // - Templates: find all instantiation sites
        let setup = setup_test(&["goto_impl_test.dml"]);

        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    /// Helper function to run annotation tests.
    #[track_caller]
    fn run_annotation_tests(
        ctx: &InitActionContext<MockOutput>,
        test_file: &Path,
        annotations: Vec<ResolvedAnnotation>,
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

        let mut run = |anns: &[&ResolvedAnnotation], op_type, lookup_fn: LookupFn| {
            if anns.is_empty() {
                return;
            }
            let (fail_count, total, failures) =
                test_goto_operation(ctx, test_file, anns, lookup_fn);
            if !failures.is_empty() {
                total_failures += fail_count;
                all_sections.push(format!(
                    "{} out of {} {} annotation(s) failed:\n  {}",
                    fail_count, total, op_type,
                    failures.join("\n  ")
                ));
            }
        };

        run(&goto_def,  OperationType::GotoDefinition,     definitions_at_fp);
        run(&goto_decl, OperationType::GotoDeclaration,    declarations_at_fp);
        run(&goto_impl, OperationType::GotoImplementation, implementations_at_fp);
        run(&find_refs, OperationType::FindReferences,     references_at_fp);

        assert!(all_sections.is_empty(),
            "{} annotation failure(s):\n\n{}",
            total_failures,
            all_sections.join("\n\n"));
    }

    /// Path of `p` relative to `src/test/test_files/`, or the full path
    /// string if it falls outside that directory.
    fn rel_to_test_files(p: &Path) -> String {
        let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src/test/test_files");
        let canon_base = base.canonicalize().unwrap_or(base);
        p.strip_prefix(&canon_base)
            .map(|r| r.to_string_lossy().into_owned())
            .unwrap_or_else(|_| p.to_string_lossy().into_owned())
    }

    /// Check whether a span matches a target's expected position and file.
    fn target_matches_span(target: &ResolvedTarget, span: &ZeroSpan) -> bool {
        let position_matches = span.range.row_start.0 == target.line()
            && span.range.col_start.0 == target.col();
        position_matches && rel_to_test_files(&span.path()) == target.filename()
    }

    /// Format a human-readable display string for a missing target.
    fn format_missing_target(target: &ResolvedTarget) -> String {
        format!("'{}' @ {}:{}:{}",
                target.name, target.filename(), target.line() + 1, target.col() + 1)
    }

    /// Match targets against returned spans.
    /// Returns `(found_positive, found_questionable, failures)` where
    /// `failures` are formatted error strings.
    fn match_targets_against_spans(
        targets: &[ResolvedTarget],
        spans: &[&ZeroSpan],
    ) -> (usize, usize, Vec<String>) {
        let mut found_positive = 0;
        let mut found_questionable = 0;
        let mut failures = Vec::new();
        for target in targets {
            let present = spans.iter().any(|span| target_matches_span(target, span));
            match target.kind {
                TargetKind::Positive => {
                    if present {
                        found_positive += 1;
                    } else {
                        failures.push(format!("missing: {}",
                                              format_missing_target(target)));
                    }
                }
                TargetKind::Negated => {
                    if present {
                        failures.push(format!("unexpectedly present: {}",
                                              format_missing_target(target)));
                    }
                }
                TargetKind::Questionable => {
                    if present {
                        found_questionable += 1;
                    } else {
                        failures.push(format!(
                            "questionable target absent (regression or fix?): {}",
                            format_missing_target(target)));
                    }
                }
            }
        }
        (found_positive, found_questionable, failures)
    }

    /// Deduplicate spans and optionally filter to the query file.
    /// Returns the relevant (deduplicated) spans.
    fn deduplicate_spans<'a>(
        spans: &'a [ZeroSpan],
        ann: &ResolvedAnnotation,
        test_file: &Path,
    ) -> Vec<&'a ZeroSpan> {
        let query_file_path = ann.location.file_path.as_deref().unwrap_or(test_file);
        let query_rel = rel_to_test_files(query_file_path);
        let has_cross_file_targets = ann.targets.iter().any(|t| t.filename() != query_rel);
        let mut seen_positions = HashSet::new();
        let mut result = Vec::new();
        for span in spans {
            let span_rel = rel_to_test_files(&span.path());
            let key = (span.range.row_start.0, span.range.col_start.0, span_rel.clone());
            if !seen_positions.insert(key) {
                continue;
            }
            if has_cross_file_targets || span_rel == query_rel {
                result.push(span);
            }
        }
        result
    }

    /// Find extra (unexpected) spans that don't match any positive or
    /// questionable target. Negated targets are excluded — they're already
    /// validated in `match_targets_against_spans`. Questionable targets are
    /// treated like positive ones here so we don't double-report a known bug.
    /// Returns formatted display strings for each extra span.
    fn find_extra_spans(ann: &ResolvedAnnotation, spans: &[&ZeroSpan]) -> Vec<String> {
        spans.iter()
            .filter(|span| !ann.targets.iter().any(|t|
                !matches!(t.kind, TargetKind::Negated)
                && target_matches_span(t, span)))
            .map(|span| {
                format!("{}:{}:{}", rel_to_test_files(&span.path()),
                        span.range.row_start.0 + 1, span.range.col_start.0 + 1)
            })
            .collect()
    }

    /// Evaluate a single annotation against lookup results.
    /// Returns Ok(summary) on success, Err(detail) on failure.
    fn evaluate_annotation(
        ann: &ResolvedAnnotation,
        spans: &[ZeroSpan],
        test_file: &Path,
        location_str: &str,
    ) -> Result<String, String> {
        let relevant = deduplicate_spans(spans, ann, test_file);
        let positive_count = ann.targets.iter()
            .filter(|t| matches!(t.kind, TargetKind::Positive)).count();
        let negated_count = ann.targets.iter()
            .filter(|t| matches!(t.kind, TargetKind::Negated)).count();
        let questionable_count = ann.targets.iter()
            .filter(|t| matches!(t.kind, TargetKind::Questionable)).count();
        let (found_positive, found_questionable, target_failures) =
            match_targets_against_spans(&ann.targets, &relevant);
        let extra_targets = find_extra_spans(ann, &relevant);

        let counts_suffix = |sep: &str| {
            let mut s = String::new();
            if negated_count > 0 {
                s.push_str(&format!("{sep}{} negated", negated_count));
            }
            if questionable_count > 0 {
                s.push_str(&format!("{sep}{}/{} questionable",
                                    found_questionable, questionable_count));
            }
            s
        };

        if target_failures.is_empty() && extra_targets.is_empty() {
            let mut summary = format!("✓ {} {} on '{}' found {}/{} expected",
                                      location_str, ann.operation_type, ann.token,
                                      found_positive, positive_count);
            if negated_count > 0 {
                summary.push_str(&format!(", {} correctly absent", negated_count));
            }
            if questionable_count > 0 {
                summary.push_str(&format!(", {} questionable (known-bug) still present",
                                          questionable_count));
            }
            return Ok(summary);
        }

        let mut detail = format!("✗ {} {} on '{}' found {}/{} expected",
                                 location_str, ann.operation_type, ann.token,
                                 found_positive, positive_count);
        detail.push_str(&counts_suffix(", "));
        for f in &target_failures {
            detail.push_str(&format!("\n      {}", f));
        }
        for e in &extra_targets {
            detail.push_str(&format!("\n      extra:   {}", e));
        }
        Err(detail)
    }

    /// Generic function to test any goto operation.
    /// Accepts a pre-filtered slice of annotations.
    /// Returns `(failure_count, total_count, failure_details)`
    fn test_goto_operation(
        ctx: &InitActionContext<MockOutput>,
        test_file: &Path,
        test_annotations: &[&ResolvedAnnotation],
        lookup_fn: LookupFn,
    ) -> (usize, usize, Vec<String>) {
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

    /// Goto-def on an `import "./..."` in a device file should resolve to
    /// the imported commoncode file (relative path resolution).
    #[test]
    fn test_goto_def_on_relative_import() {
        init_logging();
        let setup = setup_test(
            &["imports/test1_device.dml", "imports/test1_common.dml"],
        );
        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    /// Goto-def on an import in a commoncode file should also resolve, so
    /// chains of commoncode-only imports can be navigated.
    #[test]
    fn test_goto_def_on_commoncode_import() {
        init_logging();
        let setup = setup_test(
            &[
                "imports/test2_device.dml",
                "imports/test2_common_a.dml",
                "imports/test2_common_b.dml",
            ],
        );
        run_annotation_tests(&setup.ctx, &setup.main_file, setup.annotations);
    }

    /// When the same commoncode file is imported by two devices that
    /// resolve a given import string to different files (via different
    /// include paths), goto-def on that import should return BOTH
    /// resolutions.
    #[test]
    fn test_goto_def_on_import_resolves_per_device_context() {
        init_logging();
        let setup = setup_test_with_imports(
            &[
                "imports/test3_device_a.dml",
                "imports/test3_device_b.dml",
                "imports/test3_common.dml",
            ],
            &[
                ("imports/test3_device_a.dml", &["imports/test3_inc_a"]),
                ("imports/test3_device_b.dml", &["imports/test3_inc_b"]),
            ],
        );

        // Find the canonical paths of the two expected `shared.dml` targets.
        let base_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("src/test/test_files");
        let expected_a = CanonPath::from_path_buf(
            base_dir.join("imports/test3_inc_a/shared.dml")).unwrap();
        let expected_b = CanonPath::from_path_buf(
            base_dir.join("imports/test3_inc_b/shared.dml")).unwrap();

        // The import we're testing is `import "shared.dml";` on line 6
        // (0-indexed row 5) of test3_common.dml. Position the lookup inside
        // the quoted import string (col 11, 1-indexed => col 10, 0-indexed).
        let common_path = base_dir.join("imports/test3_common.dml");
        let file_pos = make_file_position(&common_path, 5, 10);

        let mut limitations = HashSet::new();
        let spans = definitions_at_fp(&setup.ctx, &file_pos, &mut limitations)
            .expect("definitions_at_fp failed");

        let result_paths: HashSet<PathBuf> = spans.iter()
            .map(|s| s.path())
            .collect();

        assert!(result_paths.contains(expected_a.as_path()),
                "expected to find variant-A shared.dml at {:?}, got {:?}",
                expected_a.as_path(), result_paths);
        assert!(result_paths.contains(expected_b.as_path()),
                "expected to find variant-B shared.dml at {:?}, got {:?}",
                expected_b.as_path(), result_paths);
        assert_eq!(spans.len(), 2,
                   "expected exactly 2 targets, got {}: {:?}",
                   spans.len(), result_paths);

        // Each result span should be at (0,0) per definitions_at_fp's
        // file-reference contract.
        for span in &spans {
            assert_eq!(span.range.row_start.0, 0);
            assert_eq!(span.range.col_start.0, 0);
        }
    }
}
