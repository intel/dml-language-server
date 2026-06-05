//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

//! Sub-process runner for the fuzz harness in `tests/fuzzing.rs`.
//!
//! Reads input bytes from stdin, runs the DML parser, and exits with one
//! of three signal/code disciplines that the parent process classifies:
//!
//!   * exit 0           — parsed cleanly.
//!   * exit 1           — Rust panic; message written to stderr as
//!                        `PANIC: <message>`.
//!   * killed by signal — uncaught crash (stack overflow → SIGSEGV,
//!                        abort → SIGABRT, etc.). The parent classifies as
//!                        `Panic` with a signal-number note.

use std::io::Read;
use std::str::FromStr;
use std::thread;

use logos::Logos;

/// Stack size for the parse worker thread. Production LSP workers use
/// Rust's default (~2 MiB on Linux), so we match that here
const PARSE_THREAD_STACK: usize = 2 * 1024 * 1024;

use dls::analysis::FileSpec;
use dls::analysis::parsing::lexer::TokenKind;
use dls::analysis::parsing::parser::{FileInfo, FileParser};
use dls::analysis::parsing::structure::{parse_toplevel, post_parse_toplevel};
use dls::analysis::parsing::tree::TreeElement;
use dls::vfs::TextFile;

fn parse_once(input: &str) {
    let text = TextFile::from_str(input).expect("TextFile::from_str is infallible");
    let path = std::path::PathBuf::from("fuzz.dml");
    let file_spec = FileSpec { path: &path, file: &text };

    let lexer = TokenKind::lexer(&text.text);
    let mut parser = FileParser::new(lexer);
    let mut file_info = FileInfo::default();
    let ast = parse_toplevel(&mut parser, &mut file_info, file_spec);

    let _ = parser.report_skips();
    let _ = ast.report_missing();
    let mut errors = Vec::new();
    post_parse_toplevel(&ast, &text, &mut errors);
}

fn panic_payload_to_string(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = payload.downcast_ref::<&str>() {
        (*s).to_owned()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "non-string panic payload".to_owned()
    }
}

fn main() {
    // Suppress the default panic printer; we emit our own stderr message
    // so the parent has a single, well-formed line to capture.
    std::panic::set_hook(Box::new(|_| {}));

    let mut input = String::new();
    if std::io::stdin().read_to_string(&mut input).is_err() {
        std::process::exit(2);
    }

    let result = thread::Builder::new()
        .name("dml-fuzz-parse".into())
        .stack_size(PARSE_THREAD_STACK)
        .spawn(move || std::panic::catch_unwind(|| parse_once(&input)))
        .expect("spawn parse worker")
        .join()
        .expect("parse worker thread join failed");
    match result {
        Ok(()) => std::process::exit(0),
        Err(payload) => {
            eprintln!("PANIC: {}", panic_payload_to_string(payload));
            std::process::exit(1);
        }
    }
}
