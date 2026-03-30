//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! SCIP (Source Code Intelligence Protocol) export support.
//!
//! This module walks the structural tree of each `IsolatedAnalysis`
//! to produce SCIP symbols based on code namespaces.
//!
//! For example, a method `foo` inside template `t` in file
//! `src/dev.dml` gets the symbol:
//!
//! ```text
//! dml simics . . src. `dev.dml`. t# foo().
//! ```
//!
//! The symbol path is: file path segments (as term descriptors) →
//! template/object nesting (as type/term descriptors) → leaf symbol.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use protobuf::Enum;
use protobuf::MessageField;

use scip::types::{
    Document, Index, Metadata, Occurrence, PositionEncoding, Relationship,
    SymbolInformation, SymbolRole, ToolInfo,
    symbol_information::Kind as ScipSymbolKind,
};

use crate::analysis::structure::objects::{
    CompObjectKind, DMLObjectCommon, MethodArgument, MethodModifier,
};
use crate::analysis::structure::toplevel::{ObjectDecl, StatementSpec};
use crate::analysis::symbols::{DMLSymbolKind, SymbolSource};
use crate::analysis::templating::objects::{
    DMLNamedMember, DMLObject,
};
use crate::analysis::DeviceAnalysis;
use crate::analysis::IsolatedAnalysis;
use crate::Span as ZeroSpan;
use crate::file_management::CanonPath;

use log::debug;

// ---------------------------------------------------------------------------
// Shared types and utilities
// ---------------------------------------------------------------------------

/// Per-file import resolution data for SCIP export.
///
/// Maps each source file (canonical path) to its list of
/// (import_statement_span, resolved_target_canonical_path) pairs.
pub type FileImportData = HashMap<CanonPath, Vec<(ZeroSpan, CanonPath)>>;

/// Map from declaration name spans to the SCIP symbol strings
/// emitted during the structural walk.  Built in the first pass
/// and consumed by the relationship-extraction pass to resolve
/// both source and target symbols from actual emitted data.
pub type SpanSymbolMap = HashMap<ZeroSpan, String>;

/// Extract relationship data from DeviceAnalysis semantic models.
///
/// Uses the `span_map` (built during the structural walk) to resolve
/// both source and target SCIP symbols from their declaration spans.
/// Returns a map from source SCIP symbol string → relationships,
/// which can be applied to SymbolInformation entries in the final index.
///
/// This covers:
/// - Template instantiation: composite objects → templates they `is`
/// - Method overrides: methods → the method they override via `default`
fn extract_relationships(
    devices: &[&DeviceAnalysis],
    span_map: &SpanSymbolMap,
) -> HashMap<String, Vec<Relationship>> {
    let mut result: HashMap<String, Vec<Relationship>> = HashMap::new();

    for device in devices {
        let container = &device.objects;

        // --- Composite object → template instantiation ---
        for (key, sym_ref) in &device.symbol_info.object_symbols {
            let sym = sym_ref.symbol.lock().unwrap();
            let source_span = &sym.loc;

            // Look up the SCIP symbol emitted for this object
            let Some(source_scip) = span_map.get(source_span) else {
                continue;
            };

            if let Some(comp_obj) = container.get(*key) {
                let mut rels: Vec<Relationship> = Vec::new();

                // Template instantiation relationships
                for (_templ_name, templ_arc) in &comp_obj.templates {
                    if let Some(loc) = &templ_arc.location {
                        // Look up the template's emitted SCIP symbol
                        if let Some(target_scip) = span_map.get(loc) {
                            let mut rel = Relationship::new();
                            rel.symbol = target_scip.clone();
                            rel.is_implementation = true;
                            rels.push(rel);
                        }
                    }
                }

                // Connect → Interface relationships
                if comp_obj.kind == CompObjectKind::Connect {
                    for child_obj in comp_obj.components.values() {
                        if let DMLObject::CompObject(impl_key) = child_obj {
                            if let Some(impl_obj) = container.get(*impl_key) {
                                if impl_obj.kind != CompObjectKind::Implement {
                                    continue;
                                }
                                for grandchild in impl_obj.components.values() {
                                    if let DMLObject::CompObject(iface_key) = grandchild {
                                        if let Some(iface_obj) = container.get(*iface_key) {
                                            if iface_obj.kind == CompObjectKind::Interface {
                                                let iface_span = &iface_obj.declloc;
                                                if let Some(target_scip) = span_map.get(iface_span) {
                                                    let mut rel = Relationship::new();
                                                    rel.symbol = target_scip.clone();
                                                    rel.is_implementation = true;
                                                    rels.push(rel);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if !rels.is_empty() {
                    rels.sort_by(|a, b| a.symbol.cmp(&b.symbol));
                    rels.dedup_by(|a, b| a.symbol == b.symbol);
                    result.entry(source_scip.clone())
                        .or_default()
                        .extend(rels);
                }
            }
        }

        // --- Method → override relationships ---
        for key_map in device.symbol_info.method_symbols.values() {
            for sym_ref in key_map.values() {
                let sym = sym_ref.symbol.lock().unwrap();
                if let SymbolSource::Method(_, methref) = &sym.source {
                    let source_span = methref.location();
                    let Some(source_scip) = span_map.get(source_span) else {
                        continue;
                    };

                    if let Some(default_call) = methref.get_default() {
                        let mut rels: Vec<Relationship> = Vec::new();
                        for overridden in default_call.flat_refs() {
                            let target_span = overridden.location();
                            if let Some(target_scip) = span_map.get(target_span) {
                                if target_scip != source_scip {
                                    let mut rel = Relationship::new();
                                    rel.symbol = target_scip.clone();
                                    rel.is_implementation = true;
                                    rels.push(rel);
                                }
                            }
                        }
                        if !rels.is_empty() {
                            rels.sort_by(|a, b| a.symbol.cmp(&b.symbol));
                            rels.dedup_by(|a, b| a.symbol == b.symbol);
                            result.entry(source_scip.clone())
                                .or_default()
                                .extend(rels);
                        }
                    }
                }
            }
        }
    }

    result
}

/// Convert a ZeroSpan range into the SCIP occurrence range format.
///
/// SCIP uses `[startLine, startChar, endLine, endChar]` (4 elements)
/// or `[startLine, startChar, endChar]` (3 elements, same-line).
/// All values are 0-based.
fn span_to_scip_range(span: &ZeroSpan) -> Vec<i32> {
    let r = &span.range;
    vec![
        r.row_start.0 as i32,
        r.col_start.0 as i32,
        r.row_end.0 as i32,
        r.col_end.0 as i32,
    ]
}

/// Map a DMLSymbolKind to a SCIP SymbolInformation Kind.
fn dml_kind_to_scip_kind(kind: &DMLSymbolKind) -> ScipSymbolKind {
    match kind {
        DMLSymbolKind::CompObject(comp_kind) => match comp_kind {
            CompObjectKind::Interface => ScipSymbolKind::Interface,
            CompObjectKind::Implement => ScipSymbolKind::Object,
            _ => ScipSymbolKind::Object,
        },
        DMLSymbolKind::Parameter => ScipSymbolKind::Constant,
        DMLSymbolKind::Constant => ScipSymbolKind::Constant,
        DMLSymbolKind::Extern => ScipSymbolKind::Variable,
        DMLSymbolKind::Hook => ScipSymbolKind::Event,
        DMLSymbolKind::Local => ScipSymbolKind::Variable,
        DMLSymbolKind::Loggroup => ScipSymbolKind::Constant,
        DMLSymbolKind::Method => ScipSymbolKind::Method,
        DMLSymbolKind::MethodArg => ScipSymbolKind::Parameter,
        DMLSymbolKind::Saved => ScipSymbolKind::Variable,
        DMLSymbolKind::Session => ScipSymbolKind::Variable,
        DMLSymbolKind::Template => ScipSymbolKind::Class,
        DMLSymbolKind::Typedef => ScipSymbolKind::TypeAlias,
    }
}

/// Check whether a character is a SCIP identifier character.
///
/// Per the SCIP symbol grammar:
///   `<identifier-character> ::= '_' | '+' | '-' | '$' | ASCII letter or digit`
fn is_scip_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '_' | '+' | '-' | '$')
}

/// Encode a name as a SCIP descriptor identifier.
///
/// Names consisting entirely of SCIP identifier characters are emitted
/// as-is (a "simple identifier").  Names that contain other characters
/// (e.g. dots, spaces) are backtick-escaped, with interior backticks
/// doubled.
fn sanitize_name(name: &str) -> String {
    if !name.is_empty() && name.chars().all(is_scip_identifier_char) {
        name.to_string()
    } else {
        let mut out = String::new();
        out.push('`');
        for c in name.chars() {
            if c == '`' {
                out.push_str("``");
            } else {
                out.push(c);
            }
        }
        out.push('`');
        out
    }
}

/// Build a SCIP symbol string representing a DML source file.
///
/// File symbols use the path relative to the project root (or the
/// full path for external files) as the descriptor.  Each path
/// component becomes a term descriptor with proper SCIP escaping.
fn make_file_symbol(path: &Path, project_root: &Path) -> String {
    let rel = path.strip_prefix(project_root).unwrap_or(path);
    let descriptors: String = rel.components()
        .filter_map(|c| {
            let s = c.as_os_str().to_str()?;
            Some(format!("{}.", sanitize_name(s)))
        })
        .collect();
    format!("dml simics . . {}", descriptors)
}

/// Extract import resolution data from an AnalysisStorage for a set
/// of device analyses.
///
/// For each file involved in any of the given devices, collects the
/// (import_span, resolved_path) pairs from the IsolatedAnalysis
/// import data and the import_map resolution data.
pub fn extract_import_data(
    isolated_analyses: &HashMap<CanonPath,
        crate::actions::analysis_storage::TimestampedStorage<IsolatedAnalysis>>,
    import_map: &HashMap<CanonPath,
        HashMap<Option<CanonPath>,
                HashMap<crate::analysis::structure::objects::Import, String>>>,
    devices: &[&DeviceAnalysis],
) -> FileImportData {
    let mut result = FileImportData::new();
    for device in devices {
        let device_context = Some(device.path.clone());
        for file_path in &device.dependant_files {
            if result.contains_key(file_path) {
                continue;
            }
            let mut imports = Vec::new();
            if let Some(analysis) = isolated_analyses.get(file_path) {
                let context_map = import_map.get(file_path);
                let resolved = context_map
                    .and_then(|cm| cm.get(&device_context))
                    .or_else(|| context_map.and_then(|cm| cm.get(&None)));
                for import_decl in analysis.stored.get_imports() {
                    let import = &import_decl.obj;
                    if let Some(resolved_map) = resolved {
                        if let Some(resolved_str) = resolved_map.get(import) {
                            if let Some(canon) =
                                CanonPath::from_path_buf(
                                    PathBuf::from(resolved_str)) {
                                imports.push((import.span, canon));
                            }
                        }
                    }
                }
            }
            result.insert(file_path.clone(), imports);
        }
    }
    result
}

/// Holds per-file occurrence and symbol information data
/// that will be assembled into SCIP Documents.
///
/// Uses HashMaps keyed by dedup keys so that duplicate entries
/// from multiple device analyses are naturally collapsed.
#[derive(Default)]
struct FileData {
    /// Occurrences keyed by (symbol, range, roles) to avoid duplicates.
    occurrences: HashMap<(String, Vec<i32>, i32), Occurrence>,
    /// SymbolInformation keyed by SCIP symbol string.
    symbols: HashMap<String, SymbolInformation>,
}

impl FileData {
    /// Insert an occurrence, deduplicating by (symbol, range, roles).
    fn add_occurrence(&mut self, occ: Occurrence) {
        let key = (
            occ.symbol.clone(),
            occ.range.clone(),
            occ.symbol_roles,
        );
        self.occurrences.entry(key).or_insert(occ);
    }

    /// Insert a SymbolInformation entry, deduplicating by symbol string.
    fn add_symbol_info(&mut self, sym_info: SymbolInformation) {
        self.symbols.entry(sym_info.symbol.clone()).or_insert(sym_info);
    }

    fn into_vecs(self) -> (Vec<Occurrence>, Vec<SymbolInformation>) {
        let mut occs: Vec<_> = self.occurrences.into_values().collect();
        occs.sort_by(|a, b| a.range.cmp(&b.range));
        let mut syms: Vec<_> = self.symbols.into_values().collect();
        syms.sort_by(|a, b| a.symbol.cmp(&b.symbol));
        (occs, syms)
    }
}

// ---------------------------------------------------------------------------
// Namespace path construction
// ---------------------------------------------------------------------------

/// Descriptor suffix for SCIP symbol construction.
#[derive(Debug, Clone, Copy)]
enum DescriptorSuffix {
    /// Term descriptor: `name.`
    Term,
    /// Type descriptor: `name#`
    Type,
    /// Method descriptor: `name().`
    Method,
}

impl DescriptorSuffix {
    fn as_str(self) -> &'static str {
        match self {
            DescriptorSuffix::Term => ".",
            DescriptorSuffix::Type => "#",
            DescriptorSuffix::Method => "().",
        }
    }
}

/// A single segment of the code-namespace path embedded in a SCIP
/// symbol string.
struct NamespaceSegment {
    name: String,
    suffix: DescriptorSuffix,
}

/// Build a global SCIP symbol string from file-relative path and
/// a chain of namespace segments.
///
/// Format:
///   `dml simics . . <file_path_descriptors> <namespace_descriptors>`
fn make_namespace_symbol(
    file_path: &Path,
    project_root: &Path,
    namespace: &[NamespaceSegment],
) -> String {
    let rel = file_path.strip_prefix(project_root).unwrap_or(file_path);
    let mut descriptors = String::new();

    // File path components as term descriptors
    for component in rel.components() {
        if let Some(s) = component.as_os_str().to_str() {
            descriptors.push_str(&sanitize_name(s));
            descriptors.push('.');
        }
    }

    // Code-level namespace descriptors
    for seg in namespace {
        descriptors.push_str(&sanitize_name(&seg.name));
        descriptors.push_str(seg.suffix.as_str());
    }

    format!("dml simics . . {descriptors}")
}

/// Build a document-local SCIP symbol for a method argument or local.
fn make_local_symbol(name: &str, id: u64) -> String {
    format!("local {}_{}", sanitize_name(name), id)
}

// ---------------------------------------------------------------------------
// Symbol emission helpers
// ---------------------------------------------------------------------------

/// Emit a definition occurrence + SymbolInformation for a named
/// declaration whose SCIP symbol uses a term descriptor.
fn emit_term_symbol(
    object: &DMLObjectCommon,
    scip_kind: ScipSymbolKind,
    doc_text: &str,
    file_path: &Path,
    project_root: &Path,
    namespace: &mut Vec<NamespaceSegment>,
    file_data: &mut FileData,
    span_map: &mut SpanSymbolMap,
) {
    let name = object.name.val.clone();
    let name_span = &object.name.span;
    let full_span = &object.span;

    namespace.push(NamespaceSegment {
        name: name.clone(),
        suffix: DescriptorSuffix::Term,
    });

    let sym = make_namespace_symbol(file_path, project_root, namespace);

    span_map.insert(*name_span, sym.clone());

    let mut occ = Occurrence::new();
    occ.range = span_to_scip_range(name_span);
    occ.symbol = sym.clone();
    occ.symbol_roles = SymbolRole::Definition.value();
    occ.enclosing_range = span_to_scip_range(full_span);
    file_data.add_occurrence(occ);

    let mut sym_info = SymbolInformation::new();
    sym_info.symbol = sym;
    sym_info.kind = scip_kind.into();
    sym_info.display_name = name;
    sym_info.documentation = vec![doc_text.to_string()];
    file_data.add_symbol_info(sym_info);

    namespace.pop();
}

// ---------------------------------------------------------------------------
// Recursive structural-tree walk
// ---------------------------------------------------------------------------

/// Recursively walk a `StatementSpec` and emit SCIP definition
/// occurrences and SymbolInformation entries for every declaration.
fn walk_spec(
    spec: &StatementSpec,
    file_path: &Path,
    project_root: &Path,
    namespace: &mut Vec<NamespaceSegment>,
    file_data: &mut FileData,
    local_counter: &mut u64,
    span_map: &mut SpanSymbolMap,
) {
    // --- Templates ---
    for template_decl in &spec.templates {
        emit_template(template_decl, file_path, project_root,
                      namespace, file_data, local_counter, span_map);
    }

    // --- Composite objects (bank, register, group, …) ---
    for obj_decl in &spec.objects {
        emit_composite_object(obj_decl, file_path, project_root,
                              namespace, file_data, local_counter, span_map);
    }

    // --- Methods ---
    for method_decl in &spec.methods {
        emit_method(method_decl, file_path, project_root,
                    namespace, file_data, local_counter, span_map);
    }

    // --- Parameters ---
    for param_decl in &spec.params {
        let doc = if param_decl.obj.is_default {
            "parameter default"
        } else {
            "parameter"
        };
        emit_term_symbol(
            &param_decl.obj.object,
            ScipSymbolKind::Constant,
            doc,
            file_path, project_root, namespace, file_data, span_map,
        );
    }

    // --- Session variables ---
    for sess_decl in &spec.sessions {
        for var_decl in &sess_decl.obj.vars {
            emit_term_symbol(
                &var_decl.object,
                ScipSymbolKind::Variable,
                "session",
                file_path, project_root, namespace, file_data, span_map,
            );
        }
    }

    // --- Saved variables ---
    for saved_decl in &spec.saveds {
        for var_decl in &saved_decl.obj.vars {
            emit_term_symbol(
                &var_decl.object,
                ScipSymbolKind::Variable,
                "saved",
                file_path, project_root, namespace, file_data, span_map,
            );
        }
    }

    // --- Hooks ---
    for hook_decl in &spec.hooks {
        let doc = if hook_decl.obj.shared { "shared hook" } else { "hook" };
        emit_term_symbol(
            &hook_decl.obj.object,
            ScipSymbolKind::Event,
            doc,
            file_path, project_root, namespace, file_data, span_map,
        );
    }

    // --- Constants ---
    for const_decl in &spec.constants {
        emit_term_symbol(
            &const_decl.obj.object,
            ScipSymbolKind::Constant,
            "constant",
            file_path, project_root, namespace, file_data, span_map,
        );
    }
}

// ---------------------------------------------------------------------------
// Per-declaration emitters
// ---------------------------------------------------------------------------

/// Emit SCIP data for a template declaration and recurse into its body.
fn emit_template(
    template_decl: &ObjectDecl<crate::analysis::structure::objects::Template>,
    file_path: &Path,
    project_root: &Path,
    namespace: &mut Vec<NamespaceSegment>,
    file_data: &mut FileData,
    local_counter: &mut u64,
    span_map: &mut SpanSymbolMap,
) {
    let tmpl = &template_decl.obj;
    let name = tmpl.object.name.val.clone();
    let name_span = &tmpl.object.name.span;
    let full_span = &tmpl.object.span;

    namespace.push(NamespaceSegment {
        name: name.clone(),
        suffix: DescriptorSuffix::Type,
    });

    let sym = make_namespace_symbol(file_path, project_root, namespace);

    span_map.insert(*name_span, sym.clone());

    // Definition occurrence
    let mut occ = Occurrence::new();
    occ.range = span_to_scip_range(name_span);
    occ.symbol = sym.clone();
    occ.symbol_roles = SymbolRole::Definition.value();
    occ.enclosing_range = span_to_scip_range(full_span);
    file_data.add_occurrence(occ);

    // SymbolInformation
    let mut sym_info = SymbolInformation::new();
    sym_info.symbol = sym;
    sym_info.kind = ScipSymbolKind::Class.into();
    sym_info.display_name = name;
    sym_info.documentation = vec!["template".to_string()];
    file_data.add_symbol_info(sym_info);

    // Recurse into the template's flattened spec
    walk_spec(
        &template_decl.spec,
        file_path,
        project_root,
        namespace,
        file_data,
        local_counter,
        span_map,
    );

    namespace.pop();
}

/// Emit SCIP data for a composite object declaration and recurse.
fn emit_composite_object(
    obj_decl: &ObjectDecl<crate::analysis::structure::objects::CompositeObject>,
    file_path: &Path,
    project_root: &Path,
    namespace: &mut Vec<NamespaceSegment>,
    file_data: &mut FileData,
    local_counter: &mut u64,
    span_map: &mut SpanSymbolMap,
) {
    let comp = &obj_decl.obj;
    let name = comp.object.name.val.clone();
    let name_span = &comp.object.name.span;
    let full_span = &comp.object.span;
    let scip_kind = dml_kind_to_scip_kind(
        &DMLSymbolKind::CompObject(comp.kind.kind));

    namespace.push(NamespaceSegment {
        name: name.clone(),
        suffix: DescriptorSuffix::Term,
    });

    let sym = make_namespace_symbol(file_path, project_root, namespace);

    span_map.insert(*name_span, sym.clone());

    let mut occ = Occurrence::new();
    occ.range = span_to_scip_range(name_span);
    occ.symbol = sym.clone();
    occ.symbol_roles = SymbolRole::Definition.value();
    occ.enclosing_range = span_to_scip_range(full_span);
    file_data.add_occurrence(occ);

    let mut sym_info = SymbolInformation::new();
    sym_info.symbol = sym;
    sym_info.kind = scip_kind.into();
    sym_info.display_name = name;
    sym_info.documentation = vec![comp.kind.kind.kind_name().to_string()];
    file_data.add_symbol_info(sym_info);

    // Recurse into nested declarations
    walk_spec(
        &obj_decl.spec,
        file_path,
        project_root,
        namespace,
        file_data,
        local_counter,
        span_map,
    );

    namespace.pop();
}

/// Emit SCIP data for a method declaration, including its arguments.
fn emit_method(
    method_decl: &ObjectDecl<crate::analysis::structure::objects::Method>,
    file_path: &Path,
    project_root: &Path,
    namespace: &mut Vec<NamespaceSegment>,
    file_data: &mut FileData,
    local_counter: &mut u64,
    span_map: &mut SpanSymbolMap,
) {
    let meth = &method_decl.obj;
    let name = meth.object.name.val.clone();
    let name_span = &meth.object.name.span;
    let full_span = &meth.object.span;

    namespace.push(NamespaceSegment {
        name: name.clone(),
        suffix: DescriptorSuffix::Method,
    });

    let sym = make_namespace_symbol(file_path, project_root, namespace);

    span_map.insert(*name_span, sym.clone());

    // Definition occurrence
    let mut occ = Occurrence::new();
    occ.range = span_to_scip_range(name_span);
    occ.symbol = sym.clone();
    occ.symbol_roles = SymbolRole::Definition.value();
    occ.enclosing_range = span_to_scip_range(full_span);
    file_data.add_occurrence(occ);

    // Documentation: modifier keywords
    let mut doc_parts: Vec<&str> = Vec::new();
    if meth.independent {
        doc_parts.push("independent");
    }
    match meth.modifier {
        MethodModifier::Shared => doc_parts.push("shared"),
        MethodModifier::Inline => doc_parts.push("inline"),
        MethodModifier::None => {}
    }
    doc_parts.push("method");
    if meth.default {
        doc_parts.push("default");
    }
    if meth.throws {
        doc_parts.push("throws");
    }

    let mut sym_info = SymbolInformation::new();
    sym_info.symbol = sym;
    sym_info.kind = ScipSymbolKind::Method.into();
    sym_info.display_name = name;
    sym_info.documentation = vec![doc_parts.join(" ")];
    file_data.add_symbol_info(sym_info);

    // Method arguments as document-local symbols
    for arg in &meth.arguments {
        let arg_name_str = match arg {
            MethodArgument::Typed(n, _) | MethodArgument::Inline(n) => n,
        };

        *local_counter += 1;
        let arg_sym = make_local_symbol(&arg_name_str.val, *local_counter);

        let mut arg_occ = Occurrence::new();
        arg_occ.range = span_to_scip_range(&arg_name_str.span);
        arg_occ.symbol = arg_sym.clone();
        arg_occ.symbol_roles = SymbolRole::Definition.value();
        arg_occ.enclosing_range = span_to_scip_range(full_span);
        file_data.add_occurrence(arg_occ);

        let mut arg_info = SymbolInformation::new();
        arg_info.symbol = arg_sym;
        arg_info.kind = ScipSymbolKind::Parameter.into();
        arg_info.display_name = arg_name_str.val.clone();
        file_data.add_symbol_info(arg_info);
    }

    namespace.pop();
}

// ---------------------------------------------------------------------------
// Per-file processing
// ---------------------------------------------------------------------------

/// Process a single `IsolatedAnalysis` into SCIP file data.
///
/// Returns a `FileData` containing all occurrences and symbol-info
/// entries for the file, plus the source path.
fn process_file(
    analysis: &IsolatedAnalysis,
    project_root: &Path,
    import_data: Option<&FileImportData>,
    span_map: &mut SpanSymbolMap,
) -> (PathBuf, FileData) {
    let file_path: PathBuf = analysis.path.clone().into();
    let mut file_data = FileData::default();
    let mut local_counter: u64 = 0;
    let mut namespace = Vec::new();

    // File-level symbol (definition at line 0)
    let file_sym = make_file_symbol(&file_path, project_root);
    {
        let mut def_occ = Occurrence::new();
        def_occ.range = vec![0, 0, 0]; // line 0, char 0, end char 0
        def_occ.symbol = file_sym.clone();
        def_occ.symbol_roles = SymbolRole::Definition.value();
        file_data.add_occurrence(def_occ);

        let mut sym_info = SymbolInformation::new();
        sym_info.symbol = file_sym.clone();
        sym_info.kind = ScipSymbolKind::File.into();
        sym_info.display_name = file_path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        // Import occurrences and relationships: for each import statement,
        // emit an Import-role occurrence referencing the imported file's
        // symbol, and also record an is_reference Relationship on this
        // file's SymbolInformation for explicit dependency tracking.
        let canon = &analysis.path;
        if let Some(imports) = import_data.and_then(|id| id.get(canon)) {
            for (import_span, resolved_path) in imports {
                let target_pathbuf: PathBuf = resolved_path.clone().into();
                let target_sym = make_file_symbol(&target_pathbuf, project_root);

                let mut imp_occ = Occurrence::new();
                imp_occ.range = span_to_scip_range(import_span);
                imp_occ.symbol = target_sym.clone();
                imp_occ.symbol_roles = SymbolRole::Import.value();
                file_data.add_occurrence(imp_occ);

                let mut rel = Relationship::new();
                rel.symbol = target_sym;
                rel.is_reference = true;
                sym_info.relationships.push(rel);
            }
        }

        file_data.add_symbol_info(sym_info);
    }

    let tl = &analysis.toplevel;

    // Top-level externs (not in StatementSpec)
    for ext_var in &tl.externs {
        for var_decl in &ext_var.vars {
            emit_term_symbol(
                &var_decl.object,
                ScipSymbolKind::Variable,
                "extern",
                &file_path,
                project_root,
                &mut namespace,
                &mut file_data,
                span_map,
            );
        }
    }

    // Top-level typedefs (not in StatementSpec)
    for typedef in &tl.typedefs {
        // Typedefs are type-like; use a type descriptor.
        namespace.push(NamespaceSegment {
            name: typedef.object.name.val.clone(),
            suffix: DescriptorSuffix::Type,
        });
        let sym = make_namespace_symbol(&file_path, project_root, &namespace);

        let mut occ = Occurrence::new();
        occ.range = span_to_scip_range(&typedef.object.name.span);
        occ.symbol = sym.clone();
        occ.symbol_roles = SymbolRole::Definition.value();
        occ.enclosing_range = span_to_scip_range(&typedef.object.span);
        file_data.add_occurrence(occ);

        let mut sym_info = SymbolInformation::new();
        sym_info.symbol = sym;
        sym_info.kind = ScipSymbolKind::TypeAlias.into();
        sym_info.display_name = typedef.object.name.val.clone();
        sym_info.documentation = vec![if typedef.is_extern {
            "extern typedef".to_string()
        } else {
            "typedef".to_string()
        }];
        file_data.add_symbol_info(sym_info);

        namespace.pop();
    }

    // Top-level loggroups (not in StatementSpec)
    for loggroup in &tl.loggroups {
        emit_term_symbol(
            &DMLObjectCommon {
                name: loggroup.name.clone(),
                span: loggroup.span,
            },
            ScipSymbolKind::Constant,
            "loggroup",
            &file_path,
            project_root,
            &mut namespace,
            &mut file_data,
            span_map,
        );
    }

    // Walk the main StatementSpec (templates, objects, methods, …)
    walk_spec(
        &tl.spec,
        &file_path,
        project_root,
        &mut namespace,
        &mut file_data,
        &mut local_counter,
        span_map,
    );

    (file_path, file_data)
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Build a complete SCIP Index from a set of isolated (per-file)
/// analyses, using code-namespace-based symbol names.
///
/// Each `IsolatedAnalysis` contributes one Document in the resulting
/// index. Symbols are named after their position in the structural
/// tree (file → template → object → method, etc.) rather than the
/// merged device hierarchy.
///
/// # Arguments
/// * `analyses` – map from canonical path to the per-file analysis
/// * `project_root` – workspace root, used to compute relative paths
/// * `import_data` – optional pre-resolved import data for emitting
///   import occurrences
pub fn build_scip_index(
    analyses: &HashMap<CanonPath, &IsolatedAnalysis>,
    project_root: &Path,
    import_data: Option<&FileImportData>,
    devices: &[&DeviceAnalysis],
) -> Index {
    debug!(
        "Building namespace-based SCIP index for {} file(s) rooted at {:?}",
        analyses.len(),
        project_root
    );

    let mut tool_info = ToolInfo::new();
    tool_info.name = "dls".to_string();
    tool_info.version = crate::version();

    let mut metadata = Metadata::new();
    metadata.tool_info = MessageField::some(tool_info);
    let root_str = project_root.to_string_lossy();
    metadata.project_root = if root_str.ends_with('/') {
        format!("file://{root_str}")
    } else {
        format!("file://{root_str}/")
    };
    metadata.text_document_encoding = scip::types::TextEncoding::UTF8.into();

    // --- Pass 1: walk all files, emit symbols, build span→symbol map ---
    let mut span_map = SpanSymbolMap::new();
    let mut file_results: Vec<(PathBuf, FileData)> = Vec::new();

    for (_, analysis) in analyses {
        let (path, data) = process_file(
            analysis, project_root, import_data, &mut span_map);
        file_results.push((path, data));
    }

    // --- Pass 2: extract relationships from DeviceAnalysis using span_map ---
    let sym_rels = extract_relationships(devices, &span_map);

    // --- Pass 3: assemble documents, injecting relationships ---
    let mut documents = Vec::new();
    let mut external_symbols = Vec::new();

    for (path, mut data) in file_results {
        // Apply relationships to SymbolInformation entries
        for sym_info in data.symbols.values_mut() {
            if let Some(rels) = sym_rels.get(&sym_info.symbol) {
                sym_info.relationships = rels.clone();
            }
        }

        let (occs, syms) = data.into_vecs();

        match path.strip_prefix(project_root) {
            Ok(rel) => {
                let mut doc = Document::new();
                doc.relative_path = rel.to_string_lossy().to_string();
                doc.language = "dml".to_string();
                doc.position_encoding =
                    PositionEncoding::UTF16CodeUnitOffsetFromLineStart.into();
                doc.occurrences = occs;
                doc.symbols = syms;
                documents.push(doc);
            }
            Err(_) => {
                external_symbols.extend(syms);
            }
        }
    }

    documents.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
    external_symbols.sort_by(|a, b| a.symbol.cmp(&b.symbol));

    let mut index = Index::new();
    index.metadata = MessageField::some(metadata);
    index.documents = documents;
    index.external_symbols = external_symbols;

    debug!(
        "Namespace-based SCIP index built with {} document(s)",
        index.documents.len()
    );
    index
}

/// Build just the span→SCIP-symbol map without producing the full index.
///
/// This is useful for other backends (e.g. object hierarchy) that need
/// to look up SCIP symbol paths for declarations.
pub fn build_span_symbol_map(
    analyses: &HashMap<CanonPath, &IsolatedAnalysis>,
    project_root: &Path,
) -> SpanSymbolMap {
    let mut span_map = SpanSymbolMap::new();
    for (_, analysis) in analyses {
        process_file(analysis, project_root, None, &mut span_map);
    }
    span_map
}

/// Write a SCIP index to a file.
pub fn write_scip_to_file(index: Index, output_path: &Path)
                          -> Result<(), String> {
    debug!("Writing SCIP index to {:?}", output_path);
    scip::write_message_to_file(output_path, index)
        .map_err(|e| format!("Failed to write SCIP index: {}", e))
}
