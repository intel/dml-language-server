//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! One-way notifications that the DLS receives from the client.

use crate::actions::{FileWatch, InitActionContext, VersionOrdering,
                     ContextDefinition, rpc_error_code};
use crate::file_management::CanonPath;
use crate::span::{Span};
use crate::vfs::{Change, VfsSpan};
use crate::lsp_data::*;

use jsonrpc::error::StandardError;
use log::{debug, error, warn};
use serde::{Serialize, Deserialize};

use lsp_types::notification::ShowMessage;
use lsp_types::request::RegisterCapability;

use std::collections::HashSet;
use std::sync::atomic::Ordering;
use std::thread;

pub use crate::lsp_data::notification::{
    Cancel, DidChangeConfiguration,
    DidChangeTextDocument, DidChangeWatchedFiles,
    DidChangeWorkspaceFolders,
    DidOpenTextDocument, DidCloseTextDocument, DidSaveTextDocument, Initialized,
};

use crate::server::{BlockingNotificationAction, Notification,
                    Output, ResponseError};

impl BlockingNotificationAction for Initialized {
    // Respond to the `initialized` notification.
    fn handle<O: Output>(
        _params: Self::Params,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        // These are the requirements for the pull-variant of
        // configuration update
        // - Dynamically registered DidChangeConfiguration
        // - ConfigurationRequest support
        if ctx.client_capabilities.did_change_configuration_support()
            == (true, true)
            && ctx.client_capabilities.configuration_support() {
                const CONFIG_ID: &str = "dls-config";
                let reg_params = RegistrationParams {
                    registrations: vec![Registration {
                            id: CONFIG_ID.to_owned(),
                        method: <DidChangeConfiguration as LSPNotification>
                            ::METHOD.to_owned(),
                        register_options: None,
                    }],
                };
                ctx.send_request::<RegisterCapability>(reg_params, &out);
            }

        // Register files we watch for changes based on config
        const WATCH_ID: &str = "dls-watch";
        let reg_params = RegistrationParams {
            registrations: vec![Registration {
                id: WATCH_ID.to_owned(),
                method: <DidChangeWatchedFiles as LSPNotification>
                    ::METHOD.to_owned(),
                register_options: FileWatch::new(ctx).map(
                    |fw|fw.watchers_config()),
            }],
        };
        ctx.send_request::<RegisterCapability>(reg_params, &out);
        Ok(())
    }
}

impl BlockingNotificationAction for DidOpenTextDocument {
    fn handle<O: Output>(
        params: Self::Params,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        debug!("on_open: {:?}", params.text_document.uri);
        let file_path = parse_file_path!(&params.text_document.uri, "on_open")?;
        ctx.reset_change_version(&file_path);
        ctx.vfs.set_file(&file_path, &params.text_document.text);
        ctx.add_direct_open(file_path.to_path_buf());
        if !ctx.config.lock().unwrap().analyse_on_save {
            ctx.isolated_analyze(&file_path, None, &out);
        }
        ctx.report_errors(&out);
        Ok(())
    }
}

impl BlockingNotificationAction for DidCloseTextDocument {
    fn handle<O: Output>(
        params: Self::Params,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        debug!("on_close: {:?}", params.text_document.uri);
        let file_path = parse_file_path!(&params.text_document.uri, "on_close")?;
        ctx.remove_direct_open(file_path.to_path_buf());
        ctx.report_errors(&out);
        Ok(())
    }
}

impl BlockingNotificationAction for DidChangeTextDocument {
    fn handle<O: Output>(
        params: Self::Params,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        debug!("on_change: {:?}, thread: {:?}", params, thread::current().id());
        if params.content_changes.is_empty() {
            return Ok(());
        }

        ctx.quiescent.store(false, Ordering::SeqCst);
        let file_path = parse_file_path!(
            &params.text_document.uri, "on_change")?;
        let canon_path = make_canon_path!(file_path.to_path_buf())?;
        let version_num = params.text_document.version;

        match ctx.check_change_version(&file_path, version_num) {
            VersionOrdering::Ok => {}
            VersionOrdering::Duplicate => return Ok(()),
            VersionOrdering::OutOfOrder => {
                out.notify(Notification::<ShowMessage>::new(ShowMessageParams {
                    typ: MessageType::WARNING,
                    message: format!("Out of order change in {:?}", file_path),
                }));
                return Ok(());
            }
        }

        let changes: Vec<Change> = params
            .content_changes
            .iter()
            .map(|i| {
                if let Some(range) = i.range {
                    let range = ls_util::range_to_dls(range);
                    Change::ReplaceText {
                        // LSP sends UTF-16 code units based offsets and length
                        span: VfsSpan::from_utf16(
                            Span::from_range(range, file_path.clone()),
                            i.range_length.map(u64::from),
                        ),
                        text: i.text.clone(),
                    }
                } else {
                    Change::AddFile { file: file_path.clone(), text: i.text.clone() }
                }
            })
            .collect();
        ctx.vfs.on_changes(&changes).expect("error committing to VFS");
        ctx.analysis.lock().unwrap().mark_file_dirty(&canon_path);

        if !ctx.config.lock().unwrap().analyse_on_save {
            ctx.isolated_analyze(&file_path, None, &out);
        }
        Ok(())
    }
}

impl BlockingNotificationAction for Cancel {
    fn handle<O: Output>(
        _params: CancelParams,
        _ctx: &mut InitActionContext<O>,
        _out: O,
    ) -> Result<(), ResponseError> {
        // Nothing to do.
        Ok(())
    }
}

impl BlockingNotificationAction for DidChangeConfiguration {
    fn handle<O: Output>(
        params: DidChangeConfigurationParams,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        debug!("config change: {:?}", params.settings);
        // New style config update, send re-config request
        if params.settings.is_null() || params.settings.as_object().
            map_or(false, |o|o.is_empty()) {
            let config_params = lsp_types::ConfigurationParams {
                items: vec![lsp_types::ConfigurationItem {
                    scope_uri: None,
                    section: Some("simics-modeling.dls".to_string()),
                }],
            };
            ctx.send_request::<lsp_types::request::WorkspaceConfiguration>(
                config_params, &out);
            return Ok(());
        }
        use std::collections::HashMap;
        let mut dups = HashMap::new();
        let mut unknowns = vec![];
        let mut deprecated = vec![];
        let settings = ChangeConfigSettings::try_deserialize(
            &params.settings,
            &mut dups,
            &mut unknowns,
            &mut deprecated,
        );
        crate::server::maybe_notify_unknown_configs(&out, &unknowns);
        crate::server::maybe_notify_deprecated_configs(&out, &deprecated);
        crate::server::maybe_notify_duplicated_configs(&out, &dups);

        let new_config = match settings {
            Ok(value) => value.dml,
            Err(err) => {
                warn!("Received unactionable config: {:?} (error: {:?})", params.settings, err);
                return Err(().into());
            }
        };
        let old = ctx.config.lock().unwrap().clone();
        ctx.config.lock().unwrap().update(new_config);
        ctx.maybe_changed_config(old, &out);

        Ok(())
    }
}

impl BlockingNotificationAction for DidSaveTextDocument {
    fn handle<O: Output>(
        params: DidSaveTextDocumentParams,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        let file_path = parse_file_path!(&params.text_document.uri, "on_save")?;

        ctx.vfs.file_saved(&file_path).unwrap();

        if ctx.config.lock().unwrap().analyse_on_save {
            ctx.isolated_analyze(&file_path, None, &out);
        }

        Ok(())
    }
}

impl BlockingNotificationAction for DidChangeWatchedFiles {
    fn handle<O: Output>(
        params: DidChangeWatchedFilesParams,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        if let Some(file_watch) = FileWatch::new(ctx) {
            if params.changes.iter().any(|c| file_watch.is_relevant(c)) {
                ctx.update_compilation_info(&out);
                ctx.update_linter_config(&out);
            }
        }
        Ok(())
    }
}

impl BlockingNotificationAction for DidChangeWorkspaceFolders {
    // Respond to the `initialized` notification.
    fn handle<O: Output>(
        params: DidChangeWorkspaceFoldersParams,
        ctx: &mut InitActionContext<O>,
        out: O,
    ) -> Result<(), ResponseError> {
        let added = params.event.added;
        let removed = params.event.removed;
        ctx.update_workspaces(added, removed, &out);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ChangeActiveContexts;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ContextDefinitionKindParam {
    Synthetic(lsp_types::Uri),
    Device(lsp_types::Uri),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ChangeActiveContextsParams {
    pub active_contexts: Vec<ContextDefinitionKindParam>,
    // None implies update ALL context paths
    pub uri: Option<lsp_types::Uri>,
}

impl ContextDefinitionKindParam {
    fn to_context_def(&self) -> Option<ContextDefinition> {
        Some(match self {
            ContextDefinitionKindParam::Synthetic(uri) =>
                ContextDefinition::Simulated(
                    parse_file_path!(&uri, "context path")
                        .ok()
                        .and_then(CanonPath::from_path_buf)?),
            ContextDefinitionKindParam::Device(uri) =>
                ContextDefinition::Device(
                    parse_file_path!(&uri, "context path")
                        .ok()
                        .and_then(CanonPath::from_path_buf)?),
        })
    }
}

impl LSPNotification for ChangeActiveContexts {
    const METHOD: &'static str = "$/changeActiveContexts";
    type Params = ChangeActiveContextsParams;
}

impl BlockingNotificationAction for ChangeActiveContexts {
    fn handle<O: Output>(
        params: ChangeActiveContextsParams,
        ctx: &mut InitActionContext<O>,
        out: O) -> Result<(), ResponseError> {
        debug!("ChangeActiveContexts: {:?}", params);
        let contexts: Vec<ContextDefinition> = params.active_contexts
            .iter()
            .filter_map(ContextDefinitionKindParam::to_context_def)
            .collect();
        if contexts.len() < params.active_contexts.len() {
            error!("Some context paths set by client were not valid \
                    canonizable paths, they have been discarded");
        }
        let (devices, others): (HashSet<_>, HashSet<_>) =
            contexts.into_iter()
            .partition(|spec|matches!(spec, ContextDefinition::Device(_)));
        if !others.is_empty() {
            error!("Tried to activate synthetic device contexts which are not \
                    yet supported, ignored these contexts.");
            error!("{:?}, {:?}", devices, others);
        }
        if let Some(uri) = &params.uri {
            if let Some(canon_path) = parse_file_path!(uri,
                                                       "ChangeActiveContexts")
                .ok().and_then(CanonPath::from_path_buf) {
                    ctx.update_contexts(&canon_path, devices);
                } else {
                    error!("Wanted to change activecontexts for {:?}, but \
                            failed to resolve it to an actual file", uri);
                }
        } else {
            *ctx.device_active_contexts.lock().unwrap()
                = devices;
        }
        // Re-report errors, since what we report for might have changed
        ctx.report_errors(&out);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;
    use lsp_types::Uri;
    use std::str::FromStr;

    #[test]
    fn context_definition_ser() {
        let context_dev = ContextDefinitionKindParam::Device(
            Uri::from_str("some_path/foo").unwrap());
        let context_syn = ContextDefinitionKindParam::Synthetic(
            Uri::from_str("some_path/foo").unwrap());
        assert_eq!(serde_json::from_str::<ContextDefinitionKindParam>(
            &serde_json::to_string(&context_dev).unwrap()).unwrap(),
                   context_dev);
        assert_eq!(serde_json::from_str::<ContextDefinitionKindParam>(
            &serde_json::to_string(&context_syn).unwrap()).unwrap(),
                   context_syn);
   }
}
