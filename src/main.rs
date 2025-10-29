//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! The DML Language Server.
//!
//! The DLS provides a server that runs in the background, providing IDEs,
//! editors, and other tools with information about DML devices.
use log::debug;
use std::path::PathBuf;
use std::sync::Arc;

use clap::{Parser, command, arg};

/// The main entry point to the DLS.
// Parses CLI arguments and then runs the server.
pub fn main() {
    dls::logging::init();
    let exit_code = main_inner();
    std::process::exit(exit_code);
}

#[derive(Parser, Debug)]
#[command(name = "dls")]
#[command(version)]
#[command(about = "The DML language server binary",
          long_about = "The DML language server binary, communicates over \
                        stdin/out using the language server protocol in \
                        order to provide syntactic and semantic analysis \
                        and feedback to DML files.")]
pub struct Args {
    #[arg(long = "cli")]
    /// Starts the DLS in command line mode
    cli: bool,
    /// Optional DML compile-info file (cli only)
    #[arg(long = "compile-info")]
    compile_info_path: Option<PathBuf>,
    /// Turn linting on or off (default on)
    #[arg(short = 'l', long = "linting")]
    linting_enabled: Option<bool>,
    /// Optional Lint CFG (cli only)
    #[arg(long = "lint-cfg")]
    lint_cfg_path: Option<PathBuf>,
}

fn main_inner() -> i32 {
    debug!("server main called");
    let Args {
        cli,
        compile_info_path,
        linting_enabled,
        lint_cfg_path,
    } = Args::parse();
    if cli {
        dls::cmd::run(compile_info_path, linting_enabled, lint_cfg_path);
        0
    } else {
        let vfs = Arc::new(dls::vfs::Vfs::new());
        dls::server::run_server(vfs)
    }
}
