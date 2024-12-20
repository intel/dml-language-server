//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
use core::time::Duration;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::io::{BufReader, Write, Read};
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::thread::{self, JoinHandle};

use anyhow;
use jsonrpc::error::{standard_error, RpcError,
    StandardError::{self, ParseError, InvalidRequest}};
use thiserror::Error;
use crate::file_management::CanonPath;
use crossbeam::channel;
use lsp_types::{self, notification,
                DiagnosticSeverity,
                PublishDiagnosticsParams,
                ProgressParams, ProgressParamsValue, ProgressToken,
                WorkDoneProgress};
use subprocess::{ExitStatus, Popen, PopenConfig, Redirection};

use crate::config::Config;
use crate::server::{self, Notification};
use crate::cmd;
use crate::lsp_data::{parse_file_path, parse_uri};

use log::{debug, trace};

#[derive(Debug, PartialEq)]
pub struct Diagnostic {
    line: u32,
    desc: String,
}

impl From<lsp_types::Diagnostic> for Diagnostic {
    fn from(diag: lsp_types::Diagnostic) -> Diagnostic {
        Diagnostic {
            line: diag.range.start.line,
            desc: diag.message,
        }
    }
}

struct MessageReader {
    sender: channel::Sender<String>,
    reader: BufReader<fs::File>,
}

#[derive(Debug, PartialEq)]
enum ServerMessage {
    ProgressStart,
    ProgressEnd,
    Diagnostics(CanonPath, Vec<Diagnostic>),
    Response(serde_json::Value),
    Ack,
    Error(ExitStatus),
}

#[derive(Debug)]
struct Skipped(String);

impl Display for Skipped {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Skipped method '{}'", self.0)
    }
}

impl Error for Skipped {}

#[derive(Debug, Error)]
enum RpcErrorKind {
    #[error("RpcError '{0}'")]
    RpcError(String),
}

impl From<RpcError> for RpcErrorKind {
    fn from(err: RpcError) -> Self {
        RpcErrorKind::RpcError(serde_json::to_string(&err)
            .unwrap_or_default())
    }
}

impl From<String> for RpcErrorKind {
    fn from(err: String) -> Self {
        RpcErrorKind::RpcError(err)
    }
}

impl RpcErrorKind {
    fn new(code: StandardError) -> Self {
        RpcErrorKind::from(standard_error(code, None))
    }
}

pub struct ClientInterface {
    server: Popen,
    reader: channel::Receiver<String>,
    _reading_thread: JoinHandle<()>,
    diagnostics: HashMap<CanonPath, Vec<Diagnostic>>,
    waiting_for_received_diag: HashSet<CanonPath>,
    waiting_for_received_lint: HashSet<CanonPath>,
    linting_enabled: bool,
    has_received_ended_progress: bool,
    progress_token: Option<ProgressToken>,
}

impl ClientInterface {
    pub fn start(binary: &Path, wd: &Path, lint: bool) -> anyhow::Result<ClientInterface> {
        trace!("Clientinterface starting on {} with workspace {}",
               binary.to_str().unwrap(),
               wd.to_str().unwrap());

        let mut server = Popen::create(&[binary],
                                   PopenConfig {
                                       stdin: Redirection::Pipe,
                                       stdout: Redirection::Pipe,
                                       stderr: Redirection::None,
                                       ..Default::default()
                                   })?;
        let (sender, receiver) = channel::unbounded();
        let child_stdout = server.stdout.take().unwrap();
        let reading_thread = thread::spawn(move || {
            let reader = BufReader::new(child_stdout);
            let mut reader = MessageReader {
                sender,
                reader,
            };
            while {
                match server::io::read_message(&mut reader.reader) {
                    Ok(s) => {
                        reader.sender.send(s).is_ok()
                    },
                    Err(_) => false,
                }
            } {}
        });
        let mut s = ClientInterface {
            server,
            reader: receiver,
            _reading_thread: reading_thread,
            diagnostics: HashMap::default(),
            waiting_for_received_diag: HashSet::default(),
            waiting_for_received_lint: HashSet::default(),
            linting_enabled: lint,
            has_received_ended_progress: false,
            progress_token: None,
        };
        s.initialize(wd)?;
        Ok(s)
    }

    fn initialize(&mut self, wd: &Path) -> anyhow::Result<()> {
        self.send(
            cmd::initialize(
                wd.to_str().unwrap().to_owned()).to_string()
        )?;
        Ok(())
    }

    pub fn send(&mut self, mess: String) -> anyhow::Result<()> {
        let to_send = format!(
            "Content-Length: {}\r\n\r\n{}", mess.len(), mess);
        trace!("Sent {} to server", mess);
        write!(self.server.stdin.as_ref().unwrap(), "{}", to_send)?;
        Ok(())
    }

    pub fn open_file(&mut self, path: &Path) -> anyhow::Result<()> {
        let mut content = vec![];
        // TODO: send an error if this fails
        let canon_path: CanonPath = path.to_path_buf().into();
        fs::File::open(canon_path.as_path())?.read_to_end(&mut content)?;
        let params = lsp_types::DidOpenTextDocumentParams {
            text_document: lsp_types::TextDocumentItem::new(
                parse_uri(canon_path.to_str().unwrap()).unwrap(),
                "dml".to_string(),
                0,
                String::from_utf8_lossy(&content).to_string(),
            ),
        };
        self.send(Notification::<notification::DidOpenTextDocument> {
            params,
            _action: PhantomData,
        }.to_string())?;
        self.waiting_for_received_diag.insert(canon_path.clone());
        if self.linting_enabled {
            self.waiting_for_received_lint.insert(canon_path);
        }
        Ok(())
    }

    pub fn add_workspaces(&mut self, paths: Vec<PathBuf>)
                      -> anyhow::Result<()> {
        debug!("Adding workspaces {:?} to server", paths);
        self.send(cmd::add_workspaces(
            paths.into_iter()
                .map(|p|p.into_os_string().into_string().unwrap())
                .collect()).to_string())
    }

    pub fn set_config(&mut self, config: Config)
                      -> anyhow::Result<()> {
        debug!("Setting config to {:?}", config);
        self.send(cmd::update_config(config).to_string())
    }

    fn has_message(&self) -> bool {
        !self.reader.is_empty()
    }

    fn receive_diagnostic(&mut self, params: serde_json::Value)
                          -> anyhow::Result<ServerMessage> {
        let diagnostic_params: PublishDiagnosticsParams
            = serde_json::from_value(params)
            .map_err(|e|RpcErrorKind::from(e.to_string()))?;
        let file: CanonPath =
            parse_file_path(&diagnostic_params.uri)
            .map_err(|e|RpcErrorKind::from(e.to_string()))
            .map(|u|u.into())?;
        trace!("Received diagnotics from server: {:?}",
               diagnostic_params.diagnostics);
        // For-now good-enough heuristic for if we are receiving a lint or not
        if diagnostic_params.diagnostics.iter()
            .any(|d|d.severity.map(|s|s == DiagnosticSeverity::ERROR)
                 .unwrap_or(false)) {
                self.waiting_for_received_diag.remove(&file);
            } else {
                self.waiting_for_received_lint.remove(&file);
            }
        Ok(ServerMessage::Diagnostics(
            file, diagnostic_params.diagnostics
                .iter().cloned().map(Diagnostic::from)
                .collect()))
    }

    fn receive_progress(&mut self, params: serde_json::Value)
                        -> anyhow::Result<ServerMessage> {
        let progress_params: ProgressParams
            = serde_json::from_value(params)
            .map_err(|_|RpcErrorKind::new(InvalidRequest))?;
        let ProgressParamsValue::WorkDone(report)
            = &progress_params.value;
        match report {
            WorkDoneProgress::Begin(begin) => {
                if begin.title != "Analysing" {
                    Err(Skipped(
                        format!("progress {:?}",
                                report)).into())
                } else {
                    trace!("Server signalled analysis start");
                    self.progress_token = Some(
                        progress_params.token);
                    Ok(ServerMessage::ProgressStart)
                }
            },
            WorkDoneProgress::End(_) => {
                if Some(progress_params.token) ==
                    self.progress_token {
                        trace!("Server signalled analysis end");
                        Ok(ServerMessage::ProgressEnd)
                    }
                else {
                    Err(Skipped(
                        format!("progress {:?}",
                                report)).into())
                }
            },
            t => Err(Skipped(
                format!("progress {:?}", t)).into()),
        }
    }

    fn receive_maybe(&mut self) -> anyhow::Result<ServerMessage> {
        if let Some(errcode) = self.server.poll() {
            return Ok(ServerMessage::Error(errcode));
        }
        let msg = self.reader.recv()?;
        trace!("Received {} from server", msg);
        // Parse the message.
        let ls_command: serde_json::Value =
            serde_json::from_str(&msg).map_err(
                |_| RpcErrorKind::new(ParseError))?;

        let method = ls_command.get("method");
        let result = ls_command.get("result");

        match (method, result) {
            (Some(m), None) => {
                let method = m.as_str().ok_or(
                    RpcErrorKind::new(InvalidRequest))?.to_owned();
                let params = match ls_command.get("params")
                    .map(ToOwned::to_owned) {
                        Some(params @ serde_json::Value::Object(..)) |
                        Some(params @ serde_json::Value::Array(..)) => params,
                        _ => return Err(RpcErrorKind::new(
                            InvalidRequest).into()),
                    };
                match method.as_str() {
                    "textDocument/publishDiagnostics" => {
                        self.receive_diagnostic(params)
                    },
                    "$/progress" => {
                        self.receive_progress(params)
                    },
                    s => Err(Skipped(s.to_string()).into()),
                }
            },
            (None, Some(r)) => {
                let result = match r {
                    r @ serde_json::Value::Object(..) |
                    r @ serde_json::Value::Array(..) => r,
                    serde_json::Value::Null => return Ok(ServerMessage::Ack),
                    // Error response. Technically not an error to receive this
                    // but we want to ignore it
                    _ => return Err(RpcErrorKind::new(InvalidRequest).into())
                };
                Ok(ServerMessage::Response(result.clone()))
            },
            _ => Err(RpcErrorKind::new(InvalidRequest).into())
        }
    }

    fn receive(&mut self) -> ServerMessage {
        trace!("Waiting for a handleable message from server");
        match self.receive_maybe() {
            Ok(mess) => mess,
            Err(e) => {
                trace!("Failed or ignored: {:?}, retrying", e);
                self.receive()
            }
        }
    }

    pub fn wait_for_analysis(&mut self) -> Result<(), ExitStatus> {
        // Wait for progress start
        while {
            let received = self.receive();
            if let ServerMessage::Error(e) = received {
                trace!("server unexpectedly closed while waiting for analysis");
                return Err(e);
            }
            if received != ServerMessage::ProgressStart {
                trace!("skipped {:?} while waiting for analysis", received);
                true
            } else if received == ServerMessage::ProgressEnd {
                self.has_received_ended_progress = true;
                false
            } else {
                false
            }
        }{}
        // Gather the results
        while 'condition: {
            if self.has_received_ended_progress {
                break 'condition false;
            }
            match self.receive() {
                ServerMessage::Error(e) => {
                    trace!("server unexpectedly closed while waiting for analysis");
                    return Err(e);
                },
                ServerMessage::Diagnostics(path, diags) => {
                    self.diagnostics.insert(path, diags);
                    !self.has_received_ended_progress || self.has_message()
                },
                ServerMessage::ProgressEnd => {
                    // this can happen when an analysis finishes before the server
                    // message reading thread started the next one
                    self.has_received_ended_progress = true;
                    self.has_message()
                },
                _ => true,
            }
        } {}
        Ok(())
    }

    fn wait_for_ack(&mut self) {
        while {
            self.receive() != ServerMessage::Ack
        }{}
    }

    pub fn output_errors(&self) {
        for (path, diagnostics) in &self.diagnostics {
            for diag in diagnostics {
                println!("{} line {}: {}",
                         path.to_str().unwrap(),
                         diag.line,
                         diag.desc);
            }
        }
    }

    pub fn no_errors(&mut self) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn shutdown(&mut self) -> anyhow::Result<()> {
        self.send(cmd::shutdown().to_string())?;
        self.wait_for_ack();
        debug!("Servered ack-d shutdown");
        self.send(cmd::exit().to_string())?;
        // TODO: There appears to be, still, some sort of condition
        // that allows the server to ack shutdown while running
        // concurrent jobs. Meaning if this times out we might
        // panic in a server/subserver thread
        self.server.wait_timeout(Duration::from_millis(1000))?;
        Ok(())
    }
}
