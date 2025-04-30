//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Types, helpers, and conversions to and from LSP types.
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};
use std::str::FromStr;

pub use lsp_types::notification::Notification as LSPNotification;
pub use lsp_types::request::Request as LSPRequest;
pub use lsp_types::WorkspaceFolder as Workspace;
pub use lsp_types::*;

use serde::{Deserialize, Serialize};
use thiserror::Error;
use urlencoding;

use crate::config;
use crate::span;
use crate::Span as ZeroSpan;

/// An error that can occur when parsing a file URI.
#[derive(Debug)]
pub enum UriFileParseError {
    /// The URI scheme is not `file`.
    InvalidScheme,
    /// Invalid file path in the URI.
    InvalidFilePath,
}

impl Error for UriFileParseError {}

impl fmt::Display for UriFileParseError
where
    UriFileParseError: Error,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let description = match self {
            UriFileParseError::InvalidScheme => "URI scheme is not `file`",
            UriFileParseError::InvalidFilePath => "Invalid file path in URI",
        };
        write!(f, "{}", description)
    }
}

/// Parses the given URI into a `PathBuf`.
pub fn parse_file_path(uri: &Uri) -> Result<PathBuf, UriFileParseError> {
    // NOTE: We do not need to mirror the windows->unix style file separators
    // here, as windows also accepts backslashes
    if uri.scheme().map_or(false,|s|s.as_str() == "file") {
        let decoded_path = urlencoding::decode(uri.path().as_str())
            .map_err(|_|UriFileParseError::InvalidFilePath)?;
        Ok(Path::new(
            // Remove leading slash on windows, if any
            if decoded_path.starts_with('/') && cfg!(windows) {
                &decoded_path[1..]
            } else {
                &*decoded_path
            }).to_path_buf())
    } else {
        Err(UriFileParseError::InvalidScheme)
    }
}

#[derive(Debug, Clone)]
pub struct UriGenerationError(String);

impl Error for UriGenerationError {}

impl fmt::Display for UriGenerationError
where
    UriGenerationError: Error
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Parses a given str path into a URI
pub fn parse_uri(pathb: &str) -> Result<Uri, UriGenerationError> {
    let canonical = Path::new(pathb)
        .canonicalize()
        .expect("Could not canonicalize file name");
    let mut path = canonical.to_str().unwrap();
    // workaround for UNC path (see https://github.com/rust-lang/rust/issues/42869)
    // NOTE: The issue reference is dead but I trust the past
    if path.starts_with(r"\\?\") {
        path = &path[r"\\?\".len()..];
    }

    // Replace windows slashes with unix-style
    let fixed_path = path.replace('\\', "/");

    // Add an extra slash on windows, on unix it is implicit
    let extra_slash = if !fixed_path.starts_with('/') {
        "/"
    } else {
        ""
    };

    let to_parse = format!("file:{}{}", extra_slash, fixed_path);
    Uri::from_str(to_parse.as_str())
        .map_err(|e|UriGenerationError(
            format!("Invalid URI '{}'; {}", to_parse, e)))
}


/// Creates an edit for the given location and text.
pub fn make_workspace_edit(location: Location, new_text: String) -> WorkspaceEdit {
    // NOTE: I am unsure whether the Uri of a location is actually internally
    // mutable, but regardless we are unlikely to touch it while sending a
    // workspace edit
    #[allow(clippy::mutable_key_type)]
    let changes = vec![(location.uri,
                        vec![TextEdit { range: location.range, new_text }])]
        .into_iter()
        .collect();

    WorkspaceEdit { changes: Some(changes),
                    document_changes: None,
                    change_annotations: None }
}

/// Utilities for working with the language server protocol.
pub mod ls_util {
    use super::*;
    use crate::span::Span;

    /// Converts a language server protocol range into an DLS range.
    /// NOTE: this does not translate LSP UTF-16 code units offsets into Unicode
    /// Scalar Value offsets as expected by DLS.
    pub fn range_to_dls(r: Range) -> span::Range<span::ZeroIndexed> {
        span::Range::from_positions(position_to_dls(r.start), position_to_dls(r.end))
    }

    /// Converts a language server protocol position into an DLS position.
    pub fn position_to_dls(p: Position) -> span::Position<span::ZeroIndexed> {
        span::Position::new(
            span::Row::new_zero_indexed(p.line),
            span::Column::new_zero_indexed(p.character),
        )
    }

    /// Converts a language server protocol location into an DLS span.
    pub fn location_to_dls(
        l: &Location,
    ) -> Result<span::Span<span::ZeroIndexed>, UriFileParseError> {
        parse_file_path(&l.uri)
            .map(|path| Span::from_range(range_to_dls(l.range), path))
    }

    pub fn file_position_to_dls(
        fp: &TextDocumentPositionParams)
        -> Result<span::FilePosition<span::ZeroIndexed>, UriFileParseError> {
        parse_file_path(&fp.text_document.uri)
            .map(|path|span::FilePosition::<span::ZeroIndexed>::new(
                position_to_dls(fp.position),
                path))
    }

    /// Converts an DLS span into a language server protocol location.
    pub fn dls_to_location(span: &ZeroSpan) -> Location {
        // An DLS span has the same info as an LSP `Location`.
        Location { uri: parse_uri(span.path().to_str().unwrap()).unwrap(),
                   range: dls_to_range(span.range) }
    }

    /// Converts an DLS location into a language server protocol location.
    pub fn dls_location_to_location(l: &span::Location<span::ZeroIndexed>) -> Location {
        Location {
            uri: parse_uri(l.file.to_str().unwrap()).unwrap(),
            range: dls_to_range(span::Range::from_positions(l.position, l.position)),
        }
    }

    /// Converts an DLS range into a language server protocol range.
    pub fn dls_to_range(r: span::Range<span::ZeroIndexed>) -> Range {
        Range { start: dls_to_position(r.start()), end: dls_to_position(r.end()) }
    }

    /// Converts an DLS position into a language server protocol range.
    pub fn dls_to_position(p: span::Position<span::ZeroIndexed>) -> Position {
        Position { line: p.row.0, character: p.col.0 }
    }

    /// Creates a `Range` spanning the whole file as currently known by `Vfs`
    ///
    /// Panics if `Vfs` cannot load the file.
    pub fn range_from_file_string(content: impl AsRef<str>) -> Range {
        let content = content.as_ref();

        if content.is_empty() {
            Range { start: Position::new(0, 0), end: Position::new(0, 0) }
        } else {
            let mut line_count = content.lines().count() as u32 - 1;
            let col = if content.ends_with('\n') {
                line_count += 1;
                0
            } else {
                content
                    .lines()
                    .last()
                    .expect("String is not empty.")
                    .chars()
                    // LSP uses UTF-16 code units offset.
                    .map(|chr| chr.len_utf16() as u32)
                    .sum()
            };
            // Range is zero-based and end position is exclusive.
            Range { start: Position::new(0, 0),
                    end: Position::new(line_count, col) }
        }
    }
}

/* ------  Extension methods for JSON-RPC protocol types ------ */

/// Provides additional methods for the remote `Range` type.
pub trait RangeExt {
    /// `true` if both `Range`s overlap.
    fn overlaps(&self, other: &Self) -> bool;
}

impl RangeExt for Range {
    fn overlaps(&self, other: &Self) -> bool {
        self.start <= other.end && other.start <= self.end
    }
}

#[derive(Error, Debug)]
#[error("Serialize error")]
pub struct SerializeError;

impl From<()> for SerializeError {
    fn from(_: ()) -> Self {
        SerializeError {}
    }
}

/// `DidChangeConfigurationParams.settings` payload reading the `{ dml: {...} }` bit.
#[derive(Debug, Deserialize)]
pub struct ChangeConfigSettings {
    pub dml: config::Config,
}

impl ChangeConfigSettings {
    /// try to deserialize a ChangeConfigSettings from a json value, val is
    /// expected to be a Value::Object containing only one key "dml", all first
    /// level keys of dml's value are converted to snake_case, duplicated and
    /// unknown keys are reported
    pub fn try_deserialize(
        val: &serde_json::value::Value,
        dups: &mut std::collections::HashMap<String, Vec<String>>,
        unknowns: &mut Vec<String>,
        deprecated: &mut Vec<String>,
    ) -> Result<ChangeConfigSettings, SerializeError> {
        let mut ret = Err(().into());
        if let serde_json::Value::Object(map) = val {
            for (k, v) in map.iter() {
                if k != "dml" {
                    unknowns.push(k.to_string());
                    continue;
                }
                if let serde_json::Value::Object(_) = v {
                    if let Ok(dml) = config::Config::try_deserialize(v, dups, unknowns, deprecated)
                    {
                        ret = Ok(ChangeConfigSettings { dml });
                    }
                } else {
                    return Err(().into());
                }
            }
        }
        ret
    }
}

/* -----------------  JSON-RPC protocol types ----------------- */



/// Supported initialization options that can be passed in the `initialize`
/// request, under `initialization_options` key. These are specific to the DLS.
#[derive(Debug, Deserialize, Default)]
#[serde(default, rename_all = "camelCase")]
pub struct InitializationOptions {
    /// `true` if analysis should not be triggered immediately after receiving `initialize`.
    pub omit_init_analyse: bool,
    pub cmd_run: bool,
    /// `DidChangeConfigurationParams.settings` payload for upfront configuration.
    pub settings: Option<ChangeConfigSettings>,
}

impl InitializationOptions {
    /// try to deserialize a Initialization from a json value. If exists,
    /// val.settings is expected to be a Value::Object containing only one key,
    /// "dml", all first level keys of dml's value are converted to
    /// snake_case, duplicated and unknown keys are reported
    pub fn try_deserialize(
        mut val: serde_json::value::Value,
        dups: &mut std::collections::HashMap<String, Vec<String>>,
        unknowns: &mut Vec<String>,
        deprecated: &mut Vec<String>,
    ) -> Result<InitializationOptions, SerializeError> {
        let settings = val.get_mut("settings").map(|x| x.take()).and_then(|set| {
            ChangeConfigSettings::try_deserialize(&set, dups, unknowns, deprecated).ok()
        });

        Ok(InitializationOptions { settings, ..serde_json::from_value(val).map_err(|_| ())? })
    }
}

// Subset of flags from lsp_types::ClientCapabilities that affects this DLS.
// Passed in the `initialize` request under `capabilities`.
#[derive(Debug, PartialEq, Deserialize, Serialize, Clone, Default)]
#[serde(default)]
pub struct ClientCapabilities {
    pub root: Option<PathBuf>,
    pub capabilities: lsp_types::ClientCapabilities,
}

impl ClientCapabilities {
    pub fn new(params: &lsp_types::InitializeParams) -> ClientCapabilities {
        // TODO: Move client capabilities to use workspace folders
        #[allow(deprecated)]
        ClientCapabilities {
            root: params.root_uri.as_ref().and_then(
                |uri|parse_file_path!(uri,
                                      "decode root").ok()),
            capabilities: params.capabilities.clone(),
        }
    }
}

// Fast-access to fields we care about
impl ClientCapabilities {
    pub fn workspace_folder_support(&self) -> bool {
        self.capabilities.workspace.as_ref()
            .map_or(false, |w|w.workspace_folders.unwrap_or(false))
    }
    // (supported, dynamic registration supported)
    pub fn did_change_configuration_support(&self) -> (bool, bool) {
        if let Some(dcc) = self.capabilities.workspace.as_ref()
            .and_then(|w|w.did_change_configuration) {
                (true, dcc.dynamic_registration.unwrap_or(false))
            } else {
                (false, false)
            }
    }
    pub fn configuration_support(&self) -> bool {
        self.capabilities.workspace.as_ref()
            .map_or(false, |w|w.configuration.unwrap_or(false))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use lsp_types::Uri;

    fn make_platform_path(path: &'static str) -> PathBuf {
        if cfg!(windows) {
            PathBuf::from(format!("C:/{}", path))
        } else {
            PathBuf::from(format!("/{}", path))
        }
    }

    fn make_uri(path: PathBuf) -> Uri {
        let extra_slash = if cfg!(windows) {
            "/"
        } else {
            ""
        };
        Uri::from_str(&format!(r"file://{}{}",
                               extra_slash,
                               path.display())).unwrap()
    }

    // Checks that round-trip path->uri->path holds
    #[test]
    fn parse_uri_as_path() {
        let some_path = make_platform_path("path/a");
        let uri = make_uri(some_path.clone());
        let path_from_uri = parse_file_path(&uri).unwrap();
        assert_eq!(some_path, path_from_uri);
    }

    // Checks that round-truo uri->path->uri holds
    #[test]
    fn parse_path_as_uri() {
        let current_dir = ::std::env::current_dir().unwrap();
        let parsed_uri = parse_uri(current_dir.to_str().unwrap())
            .unwrap();
        let expected_str = if cfg!(windows) {
            &parsed_uri.path().as_str()[1..]
        } else {
            parsed_uri.path().as_str()
        };
        assert_eq!(parse_file_path(&parsed_uri).unwrap().to_str().unwrap(),
                   expected_str);
    }

    // Checks that an URI with unicode-escaped characters gets correctly decoded
    #[test]
    fn parse_unicode_path() {
        let (unicode_uri_path, expected_path) =
            if cfg!(windows) {
                (make_uri(PathBuf::from("c%3a/foo/".to_string())),
                 PathBuf::from("c:/foo/".to_string()))
            } else {
                (make_uri(PathBuf::from("/c%3a/foo/".to_string())),
                 PathBuf::from("/c:/foo/".to_string()))
            };
        assert_eq!(parse_file_path(&unicode_uri_path).unwrap(),
                   expected_path);
    }
}
