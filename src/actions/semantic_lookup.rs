//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Stores currently completed analysis.

use log::{debug, error};

use std::collections::HashSet;
use std::{fmt, mem};
use std::sync::Arc;

use crate::actions::analysis_storage::{AnalysisLookupError, AnalysisStorage};
use crate::actions::{ContextDefinition, InitActionContext};
use crate::analysis::scope::{ContextedSymbol, ContextKey};
use crate::analysis::structure::objects::MaybeAbstract;
use crate::analysis::symbols::DMLSymbolKind;
use crate::analysis::{DeviceAnalysis, IsolatedAnalysis, LocationSpan, SymbolRef};

use crate::analysis::parsing::tree::{ZeroSpan, ZeroFilePosition};
use crate::analysis::reference::{Reference, ReferenceKind};
use crate::file_management::CanonPath;
use crate::server::Output;

// The issue number is used as the _unique identifier_ for this
// limitation, and should be the number of an open GITHUB
// issue.
// Example:
// DLSLimitation {
//         issue_num: 42,
//         description: "Example of a DLS limitation".to_string(),
//     };

#[derive(Clone, Debug, Eq, PartialEq, Hash,)]
pub struct DLSLimitation {
    pub issue_num: u64,
    pub description: String,
}

impl fmt::Display for DLSLimitation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (issue#{})", self.description, self.issue_num)
    }
}

pub fn type_semantic_limitation() -> DLSLimitation {
    DLSLimitation {
        issue_num: 65,
        description: "The DLS does not currently support semantic analysis of \
                      types, including reference finding".to_string(),
    }
}

pub fn isolated_template_limitation(template_name: &str) -> DLSLimitation {
    DLSLimitation {
        issue_num: 31,
        description:
        format!("References from, and definitions inside, a template \
                 cannot be evaluated \
                 without an instantiating object. Open a device file \
                 that uses the template '{}' to obtain such information.",
                 template_name),
    }
}

// Because symbols need to be tied to their source analysis that result is
// a [(DeviceAnalysis, [SymbolRef])] list
// The reference comes from an isolated analysis, and thus is disconnected from a device
// context
type DeviceSymbols<'t> = Vec<(&'t DeviceAnalysis, Vec<SymbolRef>)>;
enum SymbolsOrReference<'t> {
    Symbols(DeviceSymbols<'t>),
    Reference(Reference),
    Nothing,
}

struct AnalysisInfo <'a> {
    pub isolated_analysis: &'a IsolatedAnalysis,
    pub device_analysises: Vec<&'a DeviceAnalysis>,
}

#[allow(dead_code)]
struct SemanticLookup<'t> {
    pub stored_symbols: DeviceSymbols<'t>,
    pub found_ref: Option<Reference>,
    pub analysis_info: AnalysisInfo<'t>,
    pub recognized_limitations: HashSet<DLSLimitation>,
}

impl <'t> SemanticLookup<'t> {
    pub fn create_lookup(fp: &ZeroFilePosition,
                        analysis_lock: &'t AnalysisStorage,
                        ctx: &'t InitActionContext<impl Output>)
        -> Result<Self, AnalysisLookupError> {
        let active_filter = ctx.device_active_contexts.lock().unwrap().clone();
        let analysis_info = analysises_at_fp(analysis_lock, fp, Some(&active_filter))?;
        let mut recognized_limitations = HashSet::new();

        let lookup_result = get_refs_and_syms_at_fp(
            fp,
            &analysis_info,
            &mut recognized_limitations,
        )?;

        let mut stored_symbols = vec![];
        let mut found_ref = None;
        match lookup_result {
            SymbolsOrReference::Symbols(symbol_refs) => stored_symbols = symbol_refs,
            SymbolsOrReference::Reference(reference) => {
                found_ref = Some(reference.clone());
                stored_symbols = get_symbols_of_ref(
                    &reference,
                    &analysis_info,
                    &mut recognized_limitations)
            },
            SymbolsOrReference::Nothing => (),
        }

        Ok(SemanticLookup {
            stored_symbols,
            found_ref, 
            analysis_info,
            recognized_limitations,
        })
    }

    pub fn symbols(&'t self) -> Vec<SymbolRef> {
        self.stored_symbols.iter().flat_map(|(_, syms)|syms)
            .map(|s|Arc::clone(s)).collect::<Vec<_>>()
    }
}

fn analysises_at_fp<'t>(
    analysis_storage: &'t AnalysisStorage,
    fp: &ZeroFilePosition,
    active_filter: Option<&HashSet<ContextDefinition>>)
    -> Result<AnalysisInfo<'t>, AnalysisLookupError> {
    let canon_path = CanonPath::from_path_buf(fp.path())
        .ok_or(AnalysisLookupError::NoFile)?;
    let isolated_analysis = analysis_storage.get_isolated_analysis(&canon_path)?;
    let analysises = analysis_storage.filtered_device_analysises_containing_file(&canon_path, active_filter);
    if analysises.is_empty() {
        return Err(AnalysisLookupError::NoDeviceAnalysis);
    }
    Ok(AnalysisInfo {
        isolated_analysis,
        device_analysises: analysises,
    })
}

fn get_refs_and_syms_at_fp<'t>(
    fp: &ZeroFilePosition,
    analysis_info: &AnalysisInfo<'t>,
    relevant_limitations: &mut HashSet<DLSLimitation>)
    -> Result<SymbolsOrReference<'t>, AnalysisLookupError> {
    debug!("Looking up references and symbols at position {:?}", fp);
    let ref_at_pos = analysis_info.isolated_analysis.lookup_reference(fp);
    
    let context_sym_at_pos = context_symbol_at_pos(analysis_info.isolated_analysis, fp);
    let symbols_at_fp = context_sym_at_pos.map(|cs| {
        analysis_info.device_analysises.iter().map(|a|(*a, a.lookup_symbols(&cs, relevant_limitations))).collect::<Vec<_>>()
    });
    if let Some(syms) = symbols_at_fp {
        if ref_at_pos.is_some() {
            error!("Obtained both symbol and reference at {:?}\
                        (reference is {:?}), defaulted to symbol",
            &fp, ref_at_pos);
        }
        return Ok(SymbolsOrReference::Symbols(syms));
    }
    if let Some(refr) = ref_at_pos {
        if refr.reference_kind() == ReferenceKind::Type {
            relevant_limitations.insert(type_semantic_limitation());
        }
    }
    if let Some(refr) = ref_at_pos {
        Ok(SymbolsOrReference::Reference(refr.clone()))
    } else {
        debug!("No symbol or reference at point");
        Ok(SymbolsOrReference::Nothing)
    }
}

fn get_symbols_of_ref<'t>(reference: &Reference,
                          analysis_info: &AnalysisInfo<'t>,
                          relevant_limitations: &mut HashSet<DLSLimitation>)
    -> DeviceSymbols<'t> {
    debug!("Mapping {:?} to symbols", reference.loc_span());
    // Should be guaranteed by the context reference lookup above
    // (isolated analysis does exist)
    let mut symbols = vec![];
    let first_context = analysis_info.isolated_analysis
        .lookup_first_context(&reference.loc_span()
        .start_position());
    let mut any_template_used = false;
    for device in &analysis_info.device_analysises {
        // NOTE: This ends up being the correct place to warn users
        // about references inside uninstantiated templates,
        // but we have to perform some extra work to find out we are
        // in that case
        if let Some(ContextKey::Template(ref sym)) = first_context {
            if device.templates.templates.get(sym.name_ref())
            .and_then(|t|t.location.as_ref())
            .and_then(|loc|device.template_object_implementation_map.get(loc))
            .map_or(false, |impls|!impls.is_empty()) {
                any_template_used = true;
            }
        }
        symbols.append(&mut device.symbols_of_ref(*reference.loc_span()));
    }
    if let Some(ContextKey::Template(templ)) = first_context {
        if !any_template_used {
            relevant_limitations.insert(
                isolated_template_limitation(templ.name.as_str())
            );
        }
    }
        
    let mut symbols: DeviceSymbols<'t> = vec![];
    for device_analysis in &analysis_info.device_analysises {
        let syms = device_analysis.symbols_of_ref(*reference.loc_span());
        if !syms.is_empty() {
            debug!("Mapped {:?} to symbols {:?} in device analysis of {}", reference.loc_span(),
            syms.iter().map(|s|s.lock().unwrap().medium_debug_info()).collect::<Vec<_>>(),
            device_analysis.path.as_str());
            symbols.push((*device_analysis, syms));
        }
    }
    symbols
}

fn context_symbol_at_pos<'t>(isolated_analysis: &'t IsolatedAnalysis, pos: &ZeroFilePosition) ->
    Option<ContextedSymbol<'t>> {
    let mut context = isolated_analysis.lookup_context_symbol(pos);
    // Patch out leading 'device' context, unneeded
    if let Some(ref mut ic) = context {
        ic.remove_head_context();
    }
    context
}

fn symbol_implementations_of_symbol<'t>(symbol: &'t SymbolRef,
                                        analysis: &'t DeviceAnalysis)
    -> Vec<SymbolRef> {
    let symbol_lock = symbol.lock().unwrap();
    
    // Special case for methods, recursively follow the implementations to find
    // all methods
    if symbol_lock.kind == DMLSymbolKind::Method {
        // Re-dropping the lock here is sensible, as we want to iterate over non-locked symbols
        drop(symbol_lock);
        #[allow(clippy::mutable_key_type)]
        let mut syms: HashSet::<&'t SymbolRef> = HashSet::default();
        let mut next_iteration = vec![symbol];
        while let Some(next) = next_iteration.pop() {
            let next_lock = next.lock().unwrap();
            let (parent, _)= if let Some(meth_source) = next_lock.source.as_method() {
                meth_source
            } else {
                internal_error!("Expected method symbol source of symbol iterated over by\
                                     implementations_of_symbol, symbol is {:?}", next_lock);
                continue;
            };
            if !syms.contains(next) {
                syms.insert(next);
                for impl_ref in &next_lock.implementations {
                    // Note: We only need to find the implementations of the variant of this method
                    // under parent. Other variants are handled by finding multiple symbols at
                    // the ref location
                    if let Some(impl_sym) = analysis.symbol_info.method_symbols.get(impl_ref)
                        .and_then(|m|m.get(parent)) {
                        next_iteration.push(impl_sym);
                    } else {
                        internal_error!("Expected method implementation symbol to exist in \
                                             analysis, symbol is {:?}, implementation ref is {:?}",
                        next_lock, impl_ref);
                    }
                }
            }
        }
        syms.into_iter().map(Arc::clone).collect()
    } else {
        vec![]
    }
}

// NOTE: It'd be nice for some of these methods to return hashsets of symbolrefs.
// however, SymbolRefs can in fact be modified while these requests are running,
// due to symbol->reference caching, so it would break hashset invariance
// Instead, we will rely on actions.rs to make locations unique before passing to client
fn extra_implementations_of_method_symbols<'t>(semantic_lookup: &SemanticLookup<'t>)
    -> Vec<SymbolRef> {
    let mut full_method_symbol_set: Vec<SymbolRef> = vec![];
    for (_device_analysis, symbols) in &semantic_lookup.stored_symbols {
        for symbol in symbols {
            full_method_symbol_set.extend(
                symbol_implementations_of_symbol(symbol, _device_analysis).into_iter());
        }
    }
    full_method_symbol_set
}

pub fn implementations_at_fp(context: &InitActionContext<impl Output>,
                             fp: &ZeroFilePosition,
                             relevant_limitations: &mut HashSet<DLSLimitation>)
    -> Result<Vec<ZeroSpan>, AnalysisLookupError> {
    let analysis_lock = context.analysis.lock().unwrap();
    let mut semantic_lookup = SemanticLookup::create_lookup(
        fp,
        &analysis_lock,
        context)?;
    let method_symbols = extra_implementations_of_method_symbols(&semantic_lookup);
    mem::swap(relevant_limitations, &mut semantic_lookup.recognized_limitations);
    Ok(semantic_lookup.symbols()
       .into_iter()
       .filter(|s|s.lock().unwrap().kind != DMLSymbolKind::Method)
       .flat_map(|s|s.lock().unwrap().implementations.clone())
       .chain(method_symbols.into_iter()
              .map(|s|s.lock().unwrap().loc))
       .collect())
}

fn definitions_of_symbol<'t>(symbol: &'t SymbolRef, refr: Option<&Reference>)
    -> Vec<ZeroSpan> {
    let symbol_lock = symbol.lock().unwrap();
    if symbol_lock.kind == DMLSymbolKind::Method {
        if let Some((_, methsrc)) = symbol_lock.source.as_method() {
            let mut to_return = vec![];
            // If we goto-def directly on an abstract method,
            // give its first-level overrides
            if methsrc.is_abstract() && refr.is_none() {
                to_return = symbol_lock.implementations.iter()
                    .cloned().collect();
            }
            if to_return.is_empty() {
                to_return = symbol_lock.definitions.clone();
            }
            if to_return.is_empty() {
                to_return = symbol_lock.declarations.clone();
            }
            to_return
        } else {
             internal_error!("Expected method symbol source for method symbol {:?}", symbol_lock);
             vec![]
        }
    } else {
        symbol_lock.definitions.clone()
    }
}

pub fn definitions_at_fp(context: &InitActionContext<impl Output>,
                         fp: &ZeroFilePosition,
                         relevant_limitations: &mut HashSet<DLSLimitation>)
    -> Result<Vec<ZeroSpan>, AnalysisLookupError> {
    let analysis_lock = context.analysis.lock().unwrap();
    let mut semantic_lookup = SemanticLookup::create_lookup(
        fp,
        &analysis_lock,
        context)?;
    mem::swap(relevant_limitations, &mut semantic_lookup.recognized_limitations);
    Ok(semantic_lookup.symbols()
       .into_iter()
       .flat_map(|s|definitions_of_symbol(&s, semantic_lookup.found_ref.as_ref()))
       .collect())
}

pub fn declarations_at_fp(context: &InitActionContext<impl Output>,
                          fp: &ZeroFilePosition,
                          relevant_limitations: &mut HashSet<DLSLimitation>)
    -> Result<Vec<ZeroSpan>, AnalysisLookupError> {
    let analysis_lock = context.analysis.lock().unwrap();
    let mut semantic_lookup = SemanticLookup::create_lookup(
        fp,
        &analysis_lock,
        context)?;
    mem::swap(relevant_limitations, &mut semantic_lookup.recognized_limitations);
    Ok(semantic_lookup.symbols()
       .into_iter()
       .flat_map(|s|{
        let symlock = s.lock().unwrap();
        if matches!(symlock.kind, DMLSymbolKind::Method | DMLSymbolKind::Parameter){
            symlock.bases.clone()
        } else {
            symlock.declarations.clone()
        }})
       .collect())
}

pub fn references_at_fp(context: &InitActionContext<impl Output>,
                        fp: &ZeroFilePosition,
                        relevant_limitations: &mut HashSet<DLSLimitation>)
    -> Result<Vec<ZeroSpan>, AnalysisLookupError> {
    let analysis_lock = context.analysis.lock().unwrap();
    let mut semantic_lookup = SemanticLookup::create_lookup(
        fp,
        &analysis_lock,
        context)?;
    mem::swap(relevant_limitations, &mut semantic_lookup.recognized_limitations);
    Ok(semantic_lookup.symbols()
       .into_iter()
       .flat_map(|s|s.lock().unwrap().references.clone())
       .collect())
}