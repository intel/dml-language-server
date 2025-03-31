//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! This module presents the DLS as a command line interface, it takes simple
//! versions of commands, turns them into messages the DLS will understand, runs
//! the DLS as usual and prints the JSON result back on the command line.

use crate::actions::requests;
use crate::actions::notifications;
use crate::config::{Config, DeviceContextMode};
use crate::lsp_data::parse_uri;
use crate::file_management::CanonPath;
use crate::server::{self, LsService, Notification, Request, RequestId};
use crate::vfs::Vfs;

use lsp_types::{
    ClientCapabilities, ClientInfo,
    DocumentSymbolParams,
    DidChangeConfigurationParams, DidChangeWorkspaceFoldersParams,
    DidOpenTextDocumentParams,
    GotoDefinitionParams,
    InitializeParams, Position, PartialResultParams,
    TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
    WindowClientCapabilities, WorkDoneProgressParams, WorkspaceFolder,
    WorkspaceFoldersChangeEvent, WorkspaceSymbolParams,
};
use lsp_types::notification::{DidChangeConfiguration,
                              DidChangeWorkspaceFolders,
                              DidOpenTextDocument};
use std::sync::atomic::{AtomicU64, Ordering};
use std::fmt;
use std::fs;
use std::io::{stdin, stdout, Write};
use std::marker::PhantomData;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use serde_json;
use lazy_static::lazy_static;

use log::debug;

/// Runs the DLS in command line mode.
pub fn run(compile_info_path: Option<PathBuf>,
           linting_enabled: Option<bool>,
           lint_cfg: Option<PathBuf>) {
    let sender = init(compile_info_path, linting_enabled, lint_cfg);

    loop {
        // Present a prompt and read from stdin.
        print!("> ");
        stdout().flush().unwrap();
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Could not read from stdin");

        // Split the input into an action command and args
        let mut bits = input.split_whitespace();
        let action = bits.next();
        let action = match action {
            Some(a) => a,
            None => continue,
        };

        // Switch on the action and build an appropriate message.
        let (msg, wait) = match action {
            "def" => {
                let file_name = bits.next().expect("Expected file name");
                let row = bits.next().expect("Expected line number");
                let col = bits.next().expect("Expected column number");
                (def(file_name, row, col).to_string(), 100)
            }
            "symbol" => {
                let query = bits.next().expect("Expected a query");
                (workspace_symbol(query).to_string(), 100)
            }
            "document" => {
                let query = bits.next().expect("Expected file name");
                (document_symbol(query).to_string(), 100)
            }
            "open" => {
                let file = bits.next().expect("Expected file name");
                // NOTE: Opening a DML file can cause the server to chain-open
                // several other files, there's no real good heuristic for how
                // long until it is done, so we wait for quite a bit here
                (open(file).to_string(), 10000)
            }
            "workspace" => {
                let dirs: Vec<String> = bits.map(str::to_string).collect();
                debug!("dirs are... {:?}", dirs);
                if dirs.is_empty() {
                    panic!("Expected directory name");
                }
                (add_workspaces(dirs).to_string(), 100)
            }
            "context-mode" => {
                let mode = bits.next().expect("expected context mode");
                (set_context_mode(mode.to_string()).to_string(), 100)
            }
            "contexts" => {
                let dirs: Vec<String> = bits.map(str::to_string).collect();
                (get_contexts(dirs).to_string(), 100)
            }
            "set-contexts" => {
                let dirs: Vec<String> = bits.map(str::to_string).collect();
                (set_contexts(dirs).to_string(), 100)
            }
            "h" | "help" => {
                help();
                continue;
            }
            "q" | "quit" => {
                sender.send(shutdown().to_string()).expect("Error sending on channel");
                sender.send(exit().to_string()).expect("Error sending on channel");
                // Sometimes we don't quite exit in time and we get an error on the channel. Hack it.
                thread::sleep(Duration::from_millis(100));
                return;
            }
            _ => {
                println!("Unknown action. Type 'help' to see available actions.");
                continue;
            }
        };

        // Send the message to the server.
        debug!("message: {:?}", msg);
        sender.send(msg).expect("Error sending on channel");
        // Give the result time to print before printing the prompt again.
        thread::sleep(Duration::from_millis(wait));
    }
}

fn def(file_name: &str, row: &str, col: &str)
       -> Request<requests::GotoDefinition> {
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier::new(parse_uri(file_name).unwrap()),
            position: Position::new(
                u32::from_str(row).expect("Bad line number"),
                u32::from_str(col).expect("Bad column number"),
            ),
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    };
    Request { id: next_id(), params, received: Instant::now(), _action: PhantomData }
}

fn workspace_symbol(query: &str) -> Request<requests::WorkspaceSymbolRequest> {
    let params = WorkspaceSymbolParams {
        query: query.to_owned(),
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
    };
    Request { id: next_id(), params, received: Instant::now(), _action: PhantomData }
}

fn document_symbol(file_name: &str)
                   -> Request<requests::DocumentSymbolRequest> {
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier::new(parse_uri(file_name).unwrap()),
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
    };
    Request { id: next_id(), params, received: Instant::now(), _action: PhantomData }
}

pub fn shutdown() -> Request<server::ShutdownRequest> {
    Request { id: next_id(), params: (), received: Instant::now(), _action: PhantomData }
}

pub fn exit() -> Notification<server::ExitNotification> {
    Notification { params: (), _action: PhantomData }
}

pub fn initialize(root_path: String) -> Request<server::InitializeRequest> {
    #[allow(deprecated)]
    let params = InitializeParams {
        process_id: None,
        client_info: Some(ClientInfo {
            name: "Command-Interface".to_string(),
            version: None,
        }),
        locale: None,
        trace: None,
        root_uri: Some(parse_uri(&root_path).unwrap()),
        root_path: None,
        initialization_options: None,
        capabilities: ClientCapabilities {
            general: None,
            notebook_document: None,
            workspace: None,
            window: Some(WindowClientCapabilities {
                work_done_progress: Some(true),
                show_message: None,
                show_document: None,
            }),
            text_document: None,
            experimental: None,
        },
        workspace_folders: None,
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
    };
    Request { id: next_id(), params, received: Instant::now(), _action: PhantomData }
}

fn open(file_name: &str)
        -> Notification<DidOpenTextDocument> {
    let path = PathBuf::from_str(file_name).unwrap();
    let canon_path: CanonPath = path.into();
    let text = fs::read_to_string(canon_path.as_path()).unwrap();
    let uri = parse_uri(canon_path.to_str().unwrap()).unwrap();
    #[allow(deprecated)]
    let params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem::new(
            uri,
            "dml".to_string(),
            0,
            text,
        ),
    };
    Notification { params, _action: PhantomData }
}

pub fn add_workspaces(workspaces: Vec<String>)
                      -> Notification<DidChangeWorkspaceFolders> {
    Notification { params: DidChangeWorkspaceFoldersParams {
        event: WorkspaceFoldersChangeEvent {
            added: workspaces.into_iter().map(
                |p|WorkspaceFolder {
                    uri: parse_uri(&p).unwrap(),
                    name: p,
                }).collect(),
            removed: vec![],
        }
    }, _action: PhantomData }
}

pub fn remove_workspaces(workspaces: Vec<String>)
                      -> Notification<DidChangeWorkspaceFolders> {
    Notification { params: DidChangeWorkspaceFoldersParams {
        event: WorkspaceFoldersChangeEvent {
            removed: workspaces.into_iter().map(
                |p|WorkspaceFolder {
                    uri: parse_uri(&p).unwrap(),
                    name: p,
                }).collect(),
            added: vec![],
        }
    }, _action: PhantomData }
}

fn set_context_mode(mode: String) -> Notification<DidChangeConfiguration> {
    let new_mode = match mode.to_lowercase().as_str() {
        "always" => DeviceContextMode::Always,
        "anynew" => DeviceContextMode::AnyNew,
        "first" => DeviceContextMode::First,
        "samemodule" => DeviceContextMode::SameModule,
        "never" => DeviceContextMode::Never,
        _ => panic!("Expected valid context mode."),
    };
    active_config.lock().unwrap().new_device_context_mode = new_mode;
    update_config(active_config.lock().unwrap().clone())
}

pub fn update_config(config: Config)
                     -> Notification<DidChangeConfiguration> {
    Notification {
        params: DidChangeConfigurationParams {
            settings: serde_json::json!({"dml": config}),
        },
        _action: PhantomData
    }
}

pub fn get_contexts(paths: Vec<String>) -> Request<requests::GetKnownContextsRequest> {
    Request {
        params: requests::GetKnownContextsParams {
            paths: Some(paths.into_iter()
                .map(|p|parse_uri(&p).unwrap())
                .collect()),
        },
        _action: PhantomData,
        id: next_id(),
        received: Instant::now(),
    }
}

pub fn set_contexts(paths: Vec<String>) -> Notification<notifications::ChangeActiveContexts> {
    Notification {
        params: notifications::ChangeActiveContextsParams {
            active_contexts: paths.into_iter()
                .map(|p|parse_uri(&p).unwrap())
                .map(|u|notifications::ContextDefinitionKindParam::Device(u))
                .collect(),
            uri: None,
        },
        _action: PhantomData,
    }
}

fn next_id() -> RequestId {
    static ID: AtomicU64 = AtomicU64::new(1);
    RequestId::Num(ID.fetch_add(1, Ordering::SeqCst))
}

// Custom reader and output for the DLS server.
#[derive(Clone)]
struct PrintlnOutput;

impl server::Output for PrintlnOutput {
    fn response(&self, output: String) {
        println!("{}", output);
    }

    fn provide_id(&self) -> RequestId {
        RequestId::Num(0)
    }

    fn success<D: ::serde::Serialize + fmt::Debug>(&self,
                                                   id: RequestId, data: &D) {
        println!("{}: {:#?}", id, data);
    }
}

struct ChannelMsgReader {
    channel: Mutex<Receiver<String>>,
}

impl ChannelMsgReader {
    fn new(rx: Receiver<String>) -> ChannelMsgReader {
        ChannelMsgReader { channel: Mutex::new(rx) }
    }
}

impl server::MessageReader for ChannelMsgReader {
    fn read_message(&self) -> Option<String> {
        let channel = self.channel.lock().unwrap();
        let msg = channel.recv().expect("Error reading from channel");
        Some(msg)
    }
}

lazy_static! {
static ref active_config: Mutex<Config> = Mutex::default();
}

// Initialize a server, returns the sender end of a channel for posting messages.
// The initialized server will live on its own thread and look after the receiver.
fn init(compile_info_path: Option<PathBuf>,
        linting_enabled: Option<bool>,
        lint_cfg_path: Option<PathBuf>) -> Sender<String> {
    let vfs = Arc::new(Vfs::new());
    let (sender, receiver) = channel();

    let config = Config {
        compile_info_path,
        linting_enabled: linting_enabled.unwrap_or(true),
        lint_cfg_path,
        .. Default::default()
    };
    *active_config.lock().unwrap() = config.clone();
    let service = LsService::new(
        vfs,
        Arc::new(Mutex::new(config)),
        Box::new(ChannelMsgReader::new(receiver)),
        PrintlnOutput,
    );
    thread::spawn(|| LsService::run(service));

    sender
        .send(
            initialize(::std::env::current_dir().unwrap().to_str().unwrap().to_owned()).to_string(),
        )
        .expect("Error sending init");
    println!("Initializing (look for `InitializeResult` message)...");

    sender
}

// Display help message.
fn help() {
    println!(
        "\
DLS command line interface.

Line and column numbers are zero indexed

Supported commands:
    help          display this message
    quit          exit

    open          file_name
                  opens a file to perform initial analysis
    workspace     [directory_name ...]
                  adds one or more directory names to the workspaces
                  of the server
    def           file_name line_number column_number
                  textDocument/definition
                  used for 'goto def'

    symbol        query
                  workspace/symbol

    document      file_name
                  textDocument/documentSymbol
    context-mode  mode
                  Set the device context mode
    contexts
                  Obtain active device contexts
                  for paths
    set-contexts  [paths ...]
                  Sets the active device contexts
                  to paths"
    );
}

// Reproducible on Windows where current dir can produce a url something like
// `file:////?\C:\Users\alex\project\dls` which will later cause issues
#[test]
fn url_workaround_unc_canonicals() {
    let current_dir = ::std::env::current_dir().unwrap();
    let url = parse_uri(current_dir.to_str().unwrap()).unwrap();

    let url_str = url.as_str();
    assert!(!url_str.starts_with(r"file:////?\"), "Unexpected UNC url {}",
            url.as_str());
}
