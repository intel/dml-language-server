//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! One-way notifications that the DLS receives from the client.

use crate::actions::{FileWatch, InitActionContext, VersionOrdering};
use crate::server::message::Request;
use crate::span::{Span};
use crate::vfs::{Change, VfsSpan};
use crate::lsp_data::*;

use log::{debug, warn};

use lsp_types::notification::ShowMessage;
use lsp_types::request::RegisterCapability;

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
        ctx: &mut InitActionContext,
        out: O,
    ) -> Result<(), ResponseError> {
        // TODO: register any dynamic capabilities

        // Register files we watch for changes based on config
        const WATCH_ID: &str = "dls-watch";
        let id = out.provide_id();
        let reg_params = RegistrationParams {
            registrations: vec![Registration {
                id: WATCH_ID.to_owned(),
                method: <DidChangeWatchedFiles as LSPNotification>
                    ::METHOD.to_owned(),
                register_options: FileWatch::new(ctx).map(
                    |fw|fw.watchers_config()),
            }],
        };

        let request = Request::<RegisterCapability>::new(id, reg_params);
        out.request(request);
        Ok(())
    }
}

impl BlockingNotificationAction for DidOpenTextDocument {
    fn handle<O: Output>(
        params: Self::Params,
        ctx: &mut InitActionContext,
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
        ctx: &mut InitActionContext,
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
        ctx: &mut InitActionContext,
        out: O,
    ) -> Result<(), ResponseError> {
        debug!("on_change: {:?}, thread: {:?}", params, thread::current().id());
        if params.content_changes.is_empty() {
            return Ok(());
        }

        ctx.quiescent.store(false, Ordering::SeqCst);
        let file_path = parse_file_path!(
            &params.text_document.uri, "on_change")?;
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
        ctx.analysis.lock().unwrap()
            .mark_file_dirty(&file_path.to_path_buf().into());

        if !ctx.config.lock().unwrap().analyse_on_save {
            ctx.isolated_analyze(&file_path, None, &out);
        }
        Ok(())
    }
}

impl BlockingNotificationAction for Cancel {
    fn handle<O: Output>(
        _params: CancelParams,
        _ctx: &mut InitActionContext,
        _out: O,
    ) -> Result<(), ResponseError> {
        // Nothing to do.
        Ok(())
    }
}

impl BlockingNotificationAction for DidChangeConfiguration {
    fn handle<O: Output>(
        params: DidChangeConfigurationParams,
        ctx: &mut InitActionContext,
        out: O,
    ) -> Result<(), ResponseError> {
        debug!("config change: {:?}", params.settings);
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

        ctx.config.lock().unwrap().update(new_config);
        ctx.maybe_changed_config(&out);

        Ok(())
    }
}

impl BlockingNotificationAction for DidSaveTextDocument {
    fn handle<O: Output>(
        params: DidSaveTextDocumentParams,
        ctx: &mut InitActionContext,
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
        ctx: &mut InitActionContext,
        out: O,
    ) -> Result<(), ResponseError> {
        if let Some(file_watch) = FileWatch::new(ctx) {
            if params.changes.iter().any(|c| file_watch.is_relevant(c)) {
                ctx.maybe_changed_config(&out);
            }
        }
        Ok(())
    }
}

impl BlockingNotificationAction for DidChangeWorkspaceFolders {
    // Respond to the `initialized` notification.
    fn handle<O: Output>(
        params: DidChangeWorkspaceFoldersParams,
        ctx: &mut InitActionContext,
        _out: O,
    ) -> Result<(), ResponseError> {
        let added = params.event.added;
        let removed = params.event.removed;
        ctx.update_workspaces(added, removed);
        Ok(())
    }
}
