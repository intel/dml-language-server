//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Device-semantic object hierarchy export.
//!
//! Walks the `DeviceAnalysis` composite-object tree and produces
//! a JSON representation of the device object hierarchy including
//! objects, parameters, and methods.
//!
//! SCIP symbol paths are resolved via a pre-built `SpanSymbolMap`
//! (from the SCIP backend) that maps declaration name-spans to
//! their full SCIP symbol strings.
//!
//! Type and expression strings are extracted from the actual source
//! text using the spans stored in the analysis structures.

use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

use log::{debug, trace};
use serde::Serialize;
use serde_json;

use crate::analysis::structure::objects::{
    MethodModifier, ParamValue,
};
use crate::analysis::templating::methods::{DMLMethodArg, MethodDeclaration};
use crate::analysis::templating::objects::{
    DMLCompositeObject, DMLNamedMember, DMLObject,
    DMLShallowObject, DMLShallowObjectVariant, StructureContainer,
};
use crate::analysis::DeclarationSpan;
use crate::analysis::LocationSpan;
use crate::analysis::DeviceAnalysis;
use crate::backends::scip::SpanSymbolMap;
use crate::Span as ZeroSpan;

// ---------------------------------------------------------------------------
// Source text extraction
// ---------------------------------------------------------------------------

/// Cache of file contents for extracting source text from spans.
struct SourceTextCache {
    files: HashMap<PathBuf, Vec<String>>,
}

impl SourceTextCache {
    fn new() -> Self {
        SourceTextCache {
            files: HashMap::new(),
        }
    }

    /// Load a file's lines into the cache if not already present.
    fn ensure_loaded(&mut self, path: &Path) {
        if !self.files.contains_key(path) {
            match std::fs::read_to_string(path) {
                Ok(content) => {
                    let lines: Vec<String> =
                        content.lines().map(String::from).collect();
                    self.files.insert(path.to_path_buf(), lines);
                }
                Err(e) => {
                    trace!("Failed to read file {:?}: {}", path, e);
                    self.files.insert(path.to_path_buf(), vec![]);
                }
            }
        }
    }

    /// Extract the source text covered by a zero-indexed span.
    fn text_from_span(&mut self, span: &ZeroSpan) -> Option<String> {
        let path = span.path();
        self.ensure_loaded(&path);
        let lines = self.files.get(&path)?;

        let r = &span.range;
        let row_start = r.row_start.0 as usize;
        let row_end = r.row_end.0 as usize;
        let col_start = r.col_start.0 as usize;
        let col_end = r.col_end.0 as usize;

        if row_start >= lines.len() {
            return None;
        }

        if row_start == row_end {
            // Single-line span
            let line = &lines[row_start];
            let end = col_end.min(line.len());
            let start = col_start.min(end);
            Some(line[start..end].to_string())
        } else {
            // Multi-line span
            let mut result = String::new();
            for row in row_start..=row_end.min(lines.len() - 1) {
                let line = &lines[row];
                if row == row_start {
                    let start = col_start.min(line.len());
                    result.push_str(&line[start..]);
                } else if row == row_end {
                    let end = col_end.min(line.len());
                    result.push(' ');
                    result.push_str(&line[..end]);
                } else {
                    result.push(' ');
                    result.push_str(line);
                }
            }
            Some(result.trim().to_string())
        }
    }

    /// Extract text for a `DMLResolvedType` span, returning None for
    /// dummy types.
    fn type_text(
        &mut self,
        resolved: &crate::analysis::templating::types::DMLResolvedType,
    ) -> Option<String> {
        if resolved.is_dummy() {
            return None;
        }
        self.text_from_span(resolved.span())
    }
}

// ---------------------------------------------------------------------------
// JSON output types
// ---------------------------------------------------------------------------

/// A parameter in the hierarchy.
#[derive(Debug, Clone, Serialize)]
pub struct HierarchyParameter {
    pub scip_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value_expression: Option<String>,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub type_name: Option<String>,
}

/// A single argument in a method signature.
#[derive(Debug, Clone, Serialize)]
pub struct HierarchyMethodArg {
    pub name: String,
    #[serde(rename = "type")]
    pub type_name: String,
}

/// A method in the hierarchy.
#[derive(Debug, Clone, Serialize)]
pub struct HierarchyMethod {
    pub scip_name_of_method_used: String,
    pub arg_list: Vec<HierarchyMethodArg>,
    pub return_types: Vec<String>,
    pub modifiers: Vec<String>,
}

/// A composite object (device, bank, register, ...) in the hierarchy.
#[derive(Debug, Clone, Serialize)]
pub struct HierarchyObject {
    pub scip_name: String,
    pub kind: String,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub parameters: BTreeMap<String, HierarchyParameter>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub methods: BTreeMap<String, HierarchyMethod>,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub objects: BTreeMap<String, HierarchyObject>,
}

/// Top-level hierarchy: maps device short names to their hierarchy.
pub type DeviceHierarchy = BTreeMap<String, HierarchyObject>;

// ---------------------------------------------------------------------------
// Building the hierarchy
// ---------------------------------------------------------------------------

fn modifier_to_string(modifier: MethodModifier) -> &'static str {
    match modifier {
        MethodModifier::None => "none",
        MethodModifier::Shared => "shared",
        MethodModifier::Inline => "inline",
    }
}

fn build_method_entry(
    method: &crate::analysis::templating::methods::DMLMethodRef,
    span_map: &SpanSymbolMap,
    source: &mut SourceTextCache,
) -> HierarchyMethod {
    let decl = method.get_decl();

    let scip_name = span_map
        .get(method.location())
        .cloned()
        .unwrap_or_else(|| method.identity().to_string());

    let arg_list: Vec<HierarchyMethodArg> = method.args().iter().map(|arg| {
        match arg {
            DMLMethodArg::Typed(declaration) => {
                let type_text = source.type_text(&declaration.type_ref)
                    .unwrap_or_else(|| "<unresolved>".to_string());
                HierarchyMethodArg {
                    name: declaration.name.val.clone(),
                    type_name: type_text,
                }
            }
            DMLMethodArg::Inline(name) => HierarchyMethodArg {
                name: name.val.clone(),
                type_name: "inline".to_string(),
            },
        }
    }).collect();

    let return_types: Vec<String> = method.returns().iter().map(|rt| {
        source.type_text(rt)
            .unwrap_or_else(|| "<unresolved>".to_string())
    }).collect();

    let mut modifiers = Vec::new();
    let modifier_str = modifier_to_string(decl.modifier);
    if modifier_str != "none" {
        modifiers.push(modifier_str.to_string());
    }
    if decl.independent {
        modifiers.push("independent".to_string());
    }
    if decl.default {
        modifiers.push("default".to_string());
    }
    if decl.throws {
        modifiers.push("throws".to_string());
    }

    HierarchyMethod {
        scip_name_of_method_used: scip_name,
        arg_list,
        return_types,
        modifiers,
    }
}

fn build_param_entry(
    param: &crate::analysis::templating::objects::DMLParameter,
    parent_span: &ZeroSpan,
    span_map: &SpanSymbolMap,
    source: &mut SourceTextCache,
) -> HierarchyParameter {
    let def = param.get_likely_definition();

    // Look up the SCIP symbol for this parameter by scanning the
    // definition/declaration chain.  Auto-generated parameters borrow
    // the parent object's name span, so we skip any entry whose span
    // collides with the parent.  Walking used_definitions first, then
    // overridden definitions, then declarations means we pick the
    // nearest concrete declaration — and when built-in template
    // sources are available their declaration will be found here too.
    let scip_name = param.used_definitions.iter()
        .chain(param.definitions.iter())
        .chain(param.declarations.iter())
        .filter_map(|(_, p)| {
            let span = p.loc_span();
            if span != parent_span {
                span_map.get(span).cloned()
            } else {
                None
            }
        })
        .next()
        .unwrap_or_else(|| param.identity.clone());

    let value_expression = def.value.as_ref().and_then(|v| match v {
        ParamValue::Set(expr) => source.text_from_span(expr.span()),
        ParamValue::Auto(_) => Some("auto".to_string()),
    });

    let type_name = def.typed.as_ref().and_then(|t| {
        // DMLType is a ZeroSpan; extract the actual type text
        source.text_from_span(t)
    });

    HierarchyParameter {
        scip_name,
        value_expression,
        type_name,
    }
}

fn build_object_hierarchy(
    comp_obj: &DMLCompositeObject,
    container: &StructureContainer,
    span_map: &SpanSymbolMap,
    source: &mut SourceTextCache,
) -> HierarchyObject {
    let mut parameters = BTreeMap::new();
    let mut methods = BTreeMap::new();
    let mut objects = BTreeMap::new();

    let scip_name = span_map
        .get(comp_obj.location())
        .cloned()
        .unwrap_or_else(|| comp_obj.identity().to_string());

    for (name, dml_obj) in &comp_obj.components {
        match dml_obj {
            DMLObject::CompObject(key) => {
                if let Some(child) = container.get(*key) {
                    let short_name = child.identity().to_string();
                    objects.insert(
                        short_name,
                        build_object_hierarchy(
                            child, container, span_map, source),
                    );
                }
            }
            DMLObject::ShallowObject(DMLShallowObject {
                variant: DMLShallowObjectVariant::Parameter(param),
                ..
            }) => {
                parameters.insert(
                    name.clone(),
                    build_param_entry(
                        param, comp_obj.location(), span_map, source),
                );
            }
            DMLObject::ShallowObject(DMLShallowObject {
                variant: DMLShallowObjectVariant::Method(method_ref),
                ..
            }) => {
                methods.insert(
                    name.clone(),
                    build_method_entry(method_ref, span_map, source),
                );
            }
            // Sessions, saveds, constants, hooks are not included
            // in the hierarchy output per current specification.
            _ => {}
        }
    }

    HierarchyObject {
        scip_name,
        kind: comp_obj.kind.kind_name().to_string(),
        parameters,
        methods,
        objects,
    }
}

/// Build the device-semantic object hierarchy for the given device analyses.
///
/// `span_map` provides declaration-span → SCIP-symbol-path lookups,
/// typically built via `crate::backends::scip::build_span_symbol_map`.
///
/// Returns a map from device short name → hierarchy object.
pub fn build_hierarchy(
    devices: &[&DeviceAnalysis],
    span_map: &SpanSymbolMap,
) -> DeviceHierarchy {
    debug!("Building object hierarchy for {} device(s)", devices.len());

    let mut source = SourceTextCache::new();
    let mut hierarchy = DeviceHierarchy::new();

    for device in devices {
        let comp_obj = device.get_device_comp_obj();
        let entry = build_object_hierarchy(
            comp_obj, &device.objects, span_map, &mut source);
        hierarchy.insert(device.name.clone(), entry);
    }

    hierarchy
}

/// Serialize a `DeviceHierarchy` to pretty-printed JSON and write
/// it to the given file path.
pub fn write_hierarchy_to_file(
    hierarchy: &DeviceHierarchy,
    output_path: &Path,
) -> Result<(), String> {
    debug!("Writing object hierarchy to {:?}", output_path);
    let json = serde_json::to_string_pretty(hierarchy)
        .map_err(|e| format!("Failed to serialize hierarchy: {}", e))?;
    std::fs::write(output_path, json)
        .map_err(|e| format!("Failed to write hierarchy file: {}", e))
}
