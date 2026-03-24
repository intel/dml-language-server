//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! SCIP (Source Code Intelligence Protocol) export support.
//!
//! This module converts DLS analysis data (DeviceAnalysis) into
//! the SCIP index format for use with code intelligence tools.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use protobuf::MessageField;
use protobuf::Enum;

use scip::types::{
    Document, Index, Metadata, Occurrence, PositionEncoding,
    Relationship, SymbolInformation, SymbolRole, ToolInfo,
    symbol_information::Kind as ScipSymbolKind,
};

use crate::analysis::symbols::{DMLSymbolKind, SymbolSource};
use crate::analysis::structure::objects::{CompObjectKind, MethodModifier};
use crate::analysis::templating::objects::{
    DMLHierarchyMember, DMLNamedMember, DMLObject, StructureContainer,
};
use crate::analysis::DeviceAnalysis;
use crate::analysis::IsolatedAnalysis;
use crate::Span as ZeroSpan;
use crate::file_management::CanonPath;

use log::debug;

/// Per-file import resolution data for SCIP export.
///
/// Maps each source file (canonical path) to its list of
/// (import_statement_span, resolved_target_canonical_path) pairs.
pub type FileImportData = HashMap<CanonPath, Vec<(ZeroSpan, CanonPath)>>;

/// Convert a ZeroSpan range into the SCIP occurrence range format.
///
/// SCIP uses `[startLine, startChar, endLine, endChar]` (4 elements)
/// or `[startLine, startChar, endChar]` (3 elements, same-line).
/// All values are 0-based.
fn span_to_scip_range(span: &ZeroSpan) -> Vec<i32> {
    let r = &span.range;
    let start_line = r.row_start.0 as i32;
    let start_char = r.col_start.0 as i32;
    let end_line = r.row_end.0 as i32;
    let end_char = r.col_end.0 as i32;

    if start_line == end_line {
        vec![start_line, start_char, end_char]
    } else {
        vec![start_line, start_char, end_line, end_char]
    }
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

/// Sanitize a name for use in SCIP symbol strings.
///
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
        // Escaped identifier: `<escaped-character>+`
        // Interior backticks are escaped by doubling them.
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

/// Build a `local` SCIP symbol string (document-scoped).
///
/// Used for method arguments, method locals, and other symbols that
/// are only visible within a single file scope.
fn make_local_symbol(name: &str, id: u64) -> String {
    format!("local {}_{}", sanitize_name(name), id)
}

/// Build a global SCIP symbol string from a qualified path.
///
/// Global symbols use the format:
///   `scheme ' ' manager ' ' package ' ' version ' ' descriptors...`
///
/// We use:
/// - scheme: `dml`
/// - manager: `simics`
/// - package: device name
/// - version: `.` (single dot = no version)
/// - descriptors: built from the qualified path segments
///
/// SCIP descriptor suffixes:
/// - `.` = namespace/term (banks, groups, etc.)
/// - `#` = type (templates, comp objects)
/// - `()` = method
fn make_global_symbol(device_name: &str, qualified_path: &str,
                      kind: &DMLSymbolKind) -> String {
    let segments: Vec<&str> = qualified_path.split('.').collect();
    let mut descriptors = String::new();
    for (i, seg) in segments.iter().enumerate() {
        let sanitized = sanitize_name(seg);
        if i == segments.len() - 1 {
            // Last segment gets suffix based on kind
            match kind {
                DMLSymbolKind::Method => {
                    descriptors.push_str(&sanitized);
                    descriptors.push_str("().");
                }
                DMLSymbolKind::Template => {
                    // Templates are the type-like concept in DML
                    descriptors.push_str(&sanitized);
                    descriptors.push('#');
                }
                _ => {
                    // Composite objects (device, bank, register, ...)
                    // are instances, not types — use term descriptor
                    descriptors.push_str(&sanitized);
                    descriptors.push('.');
                }
            }
        } else {
            // Intermediate segments are enclosing object instances
            // (device, bank, register, ...) — use term descriptor
            descriptors.push_str(&sanitized);
            descriptors.push('.');
        }
    }
    format!("dml simics {} . {}", sanitize_name(device_name), descriptors)
}

/// Build the SCIP symbol string for a given SymbolSource.
///
/// - DMLObject (comp or shallow): uses global symbol with qualified_name()
/// - Method: uses global symbol with parent's qualified_name + method name
/// - Template: uses global symbol at top level
/// - MethodArg / MethodLocal: uses local symbol
/// - Type: returns None (these are skipped)
fn scip_symbol_for_source(
    source: &SymbolSource,
    kind: &DMLSymbolKind,
    id: u64,
    device_name: &str,
    container: &StructureContainer,
) -> Option<(String, String)> {
    // Returns Some((scip_symbol, display_name))
    match source {
        SymbolSource::DMLObject(dml_obj) => {
            match dml_obj {
                DMLObject::CompObject(key) => {
                    if let Some(comp) = container.get(*key) {
                        let qname = comp.qualified_name(container);
                        let display = comp.identity().to_string();
                        let sym = make_global_symbol(device_name,
                                                     &qname, kind);
                        Some((sym, display))
                    } else {
                        None
                    }
                }
                DMLObject::ShallowObject(shallow) => {
                    let qname = shallow.qualified_name(container);
                    let display = shallow.identity().to_string();
                    let sym = make_global_symbol(device_name,
                                                 &qname, kind);
                    Some((sym, display))
                }
            }
        }
        SymbolSource::Method(parent_key, methref) => {
            let parent_qname = container.get(*parent_key)
                .map(|p| p.qualified_name(container))
                .unwrap_or_default();
            let method_name = methref.identity();
            let qname = if parent_qname.is_empty() {
                method_name.to_string()
            } else {
                format!("{}.{}", parent_qname, method_name)
            };
            let sym = make_global_symbol(
                device_name, &qname, &DMLSymbolKind::Method);
            Some((sym, method_name.to_string()))
        }
        SymbolSource::Template(templ) => {
            let sym = make_global_symbol(
                device_name, &templ.name, &DMLSymbolKind::Template);
            Some((sym, templ.name.clone()))
        }
        SymbolSource::MethodArg(_, name) => {
            let sym = make_local_symbol(&name.val, id);
            Some((sym, name.val.clone()))
        }
        SymbolSource::MethodLocal(_, name) => {
            let sym = make_local_symbol(&name.val, id);
            Some((sym, name.val.clone()))
        }
        SymbolSource::Type(_) => None,
    }
}

/// Build a short-form declaration signature for a DML symbol.
///
/// For composite objects this is just the object kind keyword
/// (e.g. `"register"`, `"bank"`).
/// For methods this is the modifier keywords from the declaration
/// (e.g. `"independent method default"`, `"shared method throws"`).
/// Other symbol kinds currently produce no documentation.
fn make_documentation(
    source: &SymbolSource,
    container: &StructureContainer,
) -> Vec<String> {
    match source {
        SymbolSource::DMLObject(DMLObject::CompObject(key)) => {
            if let Some(comp) = container.get(*key) {
                vec![comp.kind.kind_name().to_string()]
            } else {
                vec![]
            }
        }
        SymbolSource::Method(_, methref) => {
            let decl = methref.get_decl();
            let mut parts = Vec::new();
            if decl.independent {
                parts.push("independent");
            }
            match decl.modifier {
                MethodModifier::Shared => parts.push("shared"),
                MethodModifier::Inline => parts.push("inline"),
                MethodModifier::None => {}
            }
            parts.push("method");
            if decl.default {
                parts.push("default");
            }
            if decl.throws {
                parts.push("throws");
            }
            vec![parts.join(" ")]
        }
        _ => vec![],
    }
}

/// Build a map from definition/declaration name locations to their
/// enclosing AST spans, for use as SCIP `enclosing_range`.
///
/// For composite objects, each ObjectSpec has a `loc` (name span) and
/// a `span` (full `group foo is bar { ... }` range). For methods,
/// the MethodDecl has a name location and a full declaration span.
fn enclosing_ranges_for_source(
    source: &SymbolSource,
    container: &StructureContainer,
) -> HashMap<ZeroSpan, ZeroSpan> {
    let mut map = HashMap::new();
    match source {
        SymbolSource::DMLObject(DMLObject::CompObject(key)) => {
            if let Some(comp) = container.get(*key) {
                for spec in &comp.all_decls {
                    map.insert(spec.loc, spec.span);
                }
                // definitions may include specs not in all_decls
                for spec in &comp.definitions {
                    map.entry(spec.loc).or_insert(spec.span);
                }
            }
        }
        SymbolSource::Method(_, methref) => {
            let decl = methref.get_decl();
            map.insert(decl.name.span, decl.span);
        }
        SymbolSource::Template(templ) => {
            if let Some(loc) = templ.location {
                map.insert(loc, templ.spec.span);
            }
        }
        _ => {}
    }
    map
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

/// Convert a single DeviceAnalysis into SCIP Documents.
///
/// Returns a tuple of (documents, external_symbols). Files under the
/// project root become Documents with relative paths; files outside
/// (e.g. Simics builtins) contribute only their SymbolInformation to
/// `external_symbols` for hover/navigation support.
fn device_analysis_to_documents(
    device: &DeviceAnalysis,
    project_root: &Path,
    import_data: &FileImportData,
) -> (Vec<Document>, Vec<SymbolInformation>) {
    let mut file_data: HashMap<PathBuf, FileData> = HashMap::new();
    let container = &device.objects;
    let device_name = &device.name;

    // Iterate over all symbols in the device analysis
    for symbol_ref in device.symbol_info.all_symbols() {
        let sym = symbol_ref.symbol.lock().unwrap();

        // Build the SCIP symbol and display name from the source
        let (scip_symbol, display_name) = match scip_symbol_for_source(
            &sym.source, &sym.kind, sym.id, device_name, container,
        ) {
            Some(pair) => pair,
            None => continue, // Type symbols and unresolvable objects
        };

        debug!("SCIP symbol id={} kind={:?} scip={} defs={} decls={} refs={} impls={}",
               sym.id, sym.kind, &scip_symbol,
               sym.definitions.len(), sym.declarations.len(),
               sym.references.len(), sym.implementations.len());

        let kind = dml_kind_to_scip_kind(&sym.kind);
        let documentation = make_documentation(&sym.source, container);
        let enclosing = enclosing_ranges_for_source(&sym.source, container);

        // Record the primary location as a definition occurrence
        {
            let loc = &sym.loc;
            let file_path = loc.path();
            let data = file_data.entry(file_path).or_default();

            let mut occ = Occurrence::new();
            occ.range = span_to_scip_range(loc);
            occ.symbol = scip_symbol.clone();
            occ.symbol_roles = SymbolRole::Definition.value();
            if let Some(enc) = enclosing.get(loc) {
                occ.enclosing_range = span_to_scip_range(enc);
            }

            data.add_occurrence(occ);

            // Add SymbolInformation for this symbol (only once, at def site)
            let mut sym_info = SymbolInformation::new();
            sym_info.symbol = scip_symbol.clone();
            sym_info.kind = kind.into();
            sym_info.display_name = display_name;
            sym_info.documentation = documentation;

            // For comp objects, add Relationship entries for each
            // instantiated template (`is` declarations).
            if let SymbolSource::DMLObject(
                DMLObject::CompObject(key)) = &sym.source {
                if let Some(comp) = container.get(*key) {
                    for templ_name in comp.templates.keys() {
                        let templ_symbol = make_global_symbol(
                            device_name, templ_name,
                            &DMLSymbolKind::Template);
                        let mut rel = Relationship::new();
                        rel.symbol = templ_symbol;
                        rel.is_implementation = true;
                        sym_info.relationships.push(rel);
                    }

                    // For connects, emit is_implementation pointing
                    // to each interface object nested under its
                    // implement children.
                    if comp.kind == CompObjectKind::Connect {
                        for child_obj in comp.components.values() {
                            if let DMLObject::CompObject(impl_key) = child_obj {
                                if let Some(impl_obj) = container.get(*impl_key) {
                                    if impl_obj.kind != CompObjectKind::Implement {
                                        continue;
                                    }
                                    for grandchild in impl_obj.components.values() {
                                        if let DMLObject::CompObject(iface_key) = grandchild {
                                            if let Some(iface_obj) = container.get(*iface_key) {
                                                if iface_obj.kind == CompObjectKind::Interface {
                                                    let iface_qname = iface_obj.qualified_name(container);
                                                    let iface_sym = make_global_symbol(
                                                        device_name, &iface_qname,
                                                        &DMLSymbolKind::CompObject(CompObjectKind::Interface));
                                                    let mut rel = Relationship::new();
                                                    rel.symbol = iface_sym;
                                                    rel.is_implementation = true;
                                                    sym_info.relationships.push(rel);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            sym_info.relationships.sort_by(|a, b| a.symbol.cmp(&b.symbol));
            data.add_symbol_info(sym_info);
        }

        // Record additional definitions
        for def_span in &sym.definitions {
            // Skip if same as primary loc
            if *def_span == sym.loc {
                continue;
            }
            let file_path = def_span.path();
            let data = file_data.entry(file_path).or_default();

            let mut occ = Occurrence::new();
            occ.range = span_to_scip_range(def_span);
            occ.symbol = scip_symbol.clone();
            occ.symbol_roles = SymbolRole::Definition.value();
            if let Some(enc) = enclosing.get(def_span) {
                occ.enclosing_range = span_to_scip_range(enc);
            }
            data.add_occurrence(occ);
        }

        // Record declarations
        for decl_span in &sym.declarations {
            if *decl_span == sym.loc {
                continue;
            }
            let file_path = decl_span.path();
            let data = file_data.entry(file_path).or_default();

            let mut occ = Occurrence::new();
            occ.range = span_to_scip_range(decl_span);
            occ.symbol = scip_symbol.clone();
            // If this declaration site also appears in definitions,
            // it defines a value and gets the Definition role.
            // Otherwise it's an abstract/forward declaration.
            if sym.definitions.contains(decl_span) {
                occ.symbol_roles = SymbolRole::Definition.value();
            } else {
                occ.symbol_roles = SymbolRole::ForwardDefinition.value();
            }
            if let Some(enc) = enclosing.get(decl_span) {
                occ.enclosing_range = span_to_scip_range(enc);
            }
            data.add_occurrence(occ);
        }

        // Record references (read accesses)
        for ref_span in &sym.references {
            let file_path = ref_span.path();
            let data = file_data.entry(file_path).or_default();

            let mut occ = Occurrence::new();
            occ.range = span_to_scip_range(ref_span);
            occ.symbol = scip_symbol.clone();
            // Plain reference (no Definition/ReadAccess/WriteAccess role).
            // TODO: narrow down to ReadAccess/WriteAccess once the
            // analysis tracks access kinds.
            occ.symbol_roles = 0;
            data.add_occurrence(occ);
        }

        // Record implementation sites (`is template` occurrences)
        // These are references to the template, not definitions.
        // The actual implementation relationship is expressed via
        // Relationship entries on the comp object's SymbolInformation.
        for impl_span in &sym.implementations {
            let file_path = impl_span.path();
            let data = file_data.entry(file_path).or_default();

            let mut occ = Occurrence::new();
            occ.range = span_to_scip_range(impl_span);
            occ.symbol = scip_symbol.clone();
            // Plain reference — the implementation relationship is
            // expressed via Relationship entries, not occurrence roles.
            occ.symbol_roles = 0;
            data.add_occurrence(occ);
        }
    }

    // Emit file-level symbols and import occurrences.
    //
    // For each file in the device analysis, we create a file-level
    // symbol (with a Definition occurrence at line 0) and then emit
    // Import occurrences at each `import "..."` statement pointing
    // to the imported file's symbol.
    for dep_path in &device.dependant_files {
        let file_pathbuf: PathBuf = dep_path.clone().into();
        let file_sym = make_file_symbol(&file_pathbuf, project_root);

        // Definition occurrence at line 0 of the file
        let data = file_data.entry(file_pathbuf.clone()).or_default();
        let mut def_occ = Occurrence::new();
        def_occ.range = vec![0, 0, 0]; // line 0, char 0, end char 0
        def_occ.symbol = file_sym.clone();
        def_occ.symbol_roles = SymbolRole::Definition.value();
        data.add_occurrence(def_occ);

        // SymbolInformation for the file, with is_reference
        // relationships to each imported file's symbol.
        let mut sym_info = SymbolInformation::new();
        sym_info.symbol = file_sym.clone();
        sym_info.kind = ScipSymbolKind::File.into();
        sym_info.display_name = file_pathbuf.file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();

        // Import occurrences for each `import "..."` in this file,
        // plus is_reference relationships on the file symbol.
        if let Some(imports) = import_data.get(dep_path) {
            let mut seen_targets = HashSet::new();
            for (import_span, resolved_path) in imports {
                let target_pathbuf: PathBuf = resolved_path.clone().into();
                let target_sym = make_file_symbol(&target_pathbuf, project_root);

                let mut imp_occ = Occurrence::new();
                imp_occ.range = span_to_scip_range(import_span);
                imp_occ.symbol = target_sym.clone();
                imp_occ.symbol_roles = SymbolRole::Import.value();
                data.add_occurrence(imp_occ);

                if seen_targets.insert(target_sym.clone()) {
                    let mut rel = Relationship::new();
                    rel.symbol = target_sym;
                    rel.is_reference = true;
                    sym_info.relationships.push(rel);
                }
            }
        }
        sym_info.relationships.sort_by(|a, b| a.symbol.cmp(&b.symbol));
        data.add_symbol_info(sym_info);
    }

    // Assemble Documents, separating in-project from external files.
    let mut documents = Vec::new();
    let mut external_symbols = Vec::new();

    for (path, data) in file_data {
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
                // External file: keep symbol info for hover/navigation
                // but don't emit a document or occurrences
                external_symbols.extend(syms);
            }
        }
    }

    // Remove from external_symbols any symbol that already appears
    // in a document. This can happen when multiple internal Symbol
    // objects (e.g. from different templates) produce the same SCIP
    // symbol string but have their primary locations in different
    // files — one in-project and one external.
    let doc_symbol_strings: HashSet<&str> = documents.iter()
        .flat_map(|doc| doc.symbols.iter().map(|s| s.symbol.as_str()))
        .collect();
    external_symbols.retain(|s| !doc_symbol_strings.contains(s.symbol.as_str()));

    (documents, external_symbols)
}

/// Build a complete SCIP Index from one or more DeviceAnalyses.
///
/// # Arguments
/// * `devices` - The device analyses to export
/// * `project_root` - The workspace root path, used to compute relative paths
pub fn build_scip_index(
    devices: &[&DeviceAnalysis],
    project_root: &Path,
    import_data: &FileImportData,
) -> Index {
    debug!("Building SCIP index for {} device(s) rooted at {:?}",
           devices.len(), project_root);

    let mut tool_info = ToolInfo::new();
    tool_info.name = "dls".to_string();
    tool_info.version = crate::version();

    let mut metadata = Metadata::new();
    metadata.tool_info = MessageField::some(tool_info);
    let root_str = project_root.to_string_lossy();
    metadata.project_root = if root_str.ends_with('/') {
        format!("file://{}", root_str)
    } else {
        format!("file://{}/", root_str)
    };
    metadata.text_document_encoding = scip::types::TextEncoding::UTF8.into();

    // Collect documents from all devices, merging by relative_path.
    // We use FileData for deduplication across devices: the same symbol
    // or occurrence can appear in multiple DeviceAnalyses when they
    // share source files (e.g. common library code).
    let mut merged: HashMap<String, (Document, FileData)> = HashMap::new();
    let mut ext_dedup = FileData::default();

    for device in devices {
        let (docs, ext_syms) = device_analysis_to_documents(device, project_root, import_data);
        for doc in docs {
            let (_, dedup) = merged
                .entry(doc.relative_path.clone())
                .or_insert_with(|| {
                    let mut d = Document::new();
                    d.relative_path = doc.relative_path.clone();
                    d.language = doc.language.clone();
                    d.position_encoding = doc.position_encoding;
                    (d, FileData::default())
                });
            for occ in doc.occurrences {
                dedup.add_occurrence(occ);
            }
            for sym in doc.symbols {
                dedup.add_symbol_info(sym);
            }
        }
        for sym in ext_syms {
            ext_dedup.add_symbol_info(sym);
        }
    }

    // Move deduplicated data into the final documents, sorted for
    // deterministic output.
    let mut documents: Vec<Document> = merged.into_values().map(|(mut doc, dedup)| {
        let (occs, syms) = dedup.into_vecs();
        doc.occurrences = occs;
        doc.symbols = syms;
        doc
    }).collect();
    documents.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));

    let mut index = Index::new();
    index.metadata = MessageField::some(metadata);
    index.documents = documents;
    let (_, mut ext_syms) = ext_dedup.into_vecs();
    ext_syms.sort_by(|a, b| a.symbol.cmp(&b.symbol));
    index.external_symbols = ext_syms;

    debug!("SCIP index built with {} document(s)", index.documents.len());
    index
}

/// Write a SCIP index to a file.
pub fn write_scip_to_file(index: Index, output_path: &Path)
                          -> Result<(), String> {
    debug!("Writing SCIP index to {:?}", output_path);
    scip::write_message_to_file(output_path, index)
        .map_err(|e| format!("Failed to write SCIP index: {}", e))
}
