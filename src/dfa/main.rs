//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! Utility to run DML Language Server directly on files
//!
//! Can be used for testing and obtaining understandable errors
//! rather than lsp messages

use dls::config::Config;

use std::path::PathBuf;
use std::convert::TryInto;
use std::io::Write;
use subprocess::ExitStatus;

use clap::{command, arg, Arg, ArgAction};

use dls::dfa::ClientInterface;

use log::debug;

pub fn main() {
    env_logger::init();
    let code = main_inner();
    std::process::exit(match code {
        Ok(()) => 0,
        Err(code) => code,
    });
}

#[derive(Debug)]
struct Args {
    binary: PathBuf,
    files: Vec<PathBuf>,
    workspaces: Vec<PathBuf>,
    compile_info: Option<PathBuf>,
    suppress_imports: Option<bool>,
    linting_enabled: Option<bool>,
    lint_cfg_path: Option<PathBuf>,
    test: bool,
    quiet: bool,
}

fn parse_args() -> Args {
    let args = command!()
        .about(
            "A non-interactive frontend for the DLS. \
             DML files will be handled by the server as if \
             they were opened by user. And info about diagnostics \
             will be printed. Multiple devices _can_ be analyzed at \
             the same time.")
        .arg_required_else_help(true)
        .arg(arg!(<DLS> "The DLS binary to use")
             .value_parser(clap::value_parser!(PathBuf)))
        .arg_required_else_help(true)
        .arg(Arg::new("workspace").short('w').long("workspace")
             .help("Emulate a specific path as a workspace root")
             .action(ArgAction::Append)
             .value_parser(clap::value_parser!(PathBuf))
             .required(false))
        .arg(Arg::new("test").short('t')
             .help("If any diagnostic errors are reported, \
                    exit with errorcode")
             .action(ArgAction::Set)
             .value_parser(clap::value_parser!(bool))
            .required(false))
        .arg(Arg::new("quiet").short('q')
             .help("Do not output information about which errors \
                    were reported")
             .required(false))
        .arg(Arg::new("compile-info").short('c').long("compile-info")
             .help("Use the specified file to determine compilation flags and \
                    include paths")
             .action(ArgAction::Set)
             .value_parser(clap::value_parser!(PathBuf))
             .required(false))
        .arg(Arg::new("suppress-imports").short('s').long("suppress-imports")
            .help("Analyses specified files only, without also analyzing files they import")
            .action(ArgAction::Set)
            .value_parser(clap::value_parser!(bool))
            .required(false))
        .arg(Arg::new("linting-enabled").short('l').long("linting-enabled")
             .help("Turns linting on/off (defaults to true)")
             .action(ArgAction::Set)
             .value_parser(clap::value_parser!(bool))
             .required(false))
        .arg(Arg::new("lint-cfg-path").short('p').long("lint-cfg-path")
             .help("Parse the specified file as a linting configuration file")
             .action(ArgAction::Set)
             .value_parser(clap::value_parser!(PathBuf))
             .required(false))
        .arg(arg!(<PATH> ... "DML files to analyze")
             .value_parser(clap::value_parser!(PathBuf)))
        .arg_required_else_help(false)
        .get_matches();
    Args {
        binary: args.get_one::<PathBuf>("DLS")
            .expect("'DLS' is required").clone(),
        files: args.get_many("PATH")
            .expect("internal error. 'PATH' was None")
            .cloned().collect(),
        workspaces: args.get_many::<PathBuf>("workspace")
            .map_or(vec![], |vr|vr.cloned().collect()),
        quiet: args.contains_id("quiet"),
        test: args.get_one::<bool>("test").cloned().unwrap_or(false),
        compile_info: args.get_one::<PathBuf>("compile-info")
            .cloned(),
        suppress_imports: args.get_one::<bool>("suppress-imports")
            .cloned(),
        linting_enabled: args.get_one::<bool>("linting-enabled")
            .cloned(),
        lint_cfg_path: args.get_one::<PathBuf>("lint-cfg-path")
            .cloned()
    }
}

fn main_inner() -> Result<(), i32> {
    let arg = parse_args();

    println!("DML direct file analysis ({})", env!("CARGO_PKG_VERSION"));
    debug!("DFA args are: {:?}", arg);

    let highest_folder = arg.files.iter().map(|p|p.as_path().parent().unwrap())
        .reduce(|a, n|if a.starts_with(n) {a} else {n}).unwrap();

    let mut workspace_rest = arg.workspaces.iter();
    let first_workspace = workspace_rest.next();

    let linting_enabled =  arg.linting_enabled.unwrap_or(true);

    let root = match first_workspace {
        Some(w) => w,
        None => highest_folder,
    };
    let mut dlsclient = ClientInterface::start(&arg.binary, root, linting_enabled)
        .map_err(|e|{
            std::io::stdout().write_all(
                format!("Failed to open client binary: {}\n",
                        e).as_bytes())
                .ok();
            1})?;
    dlsclient.add_workspaces(workspace_rest.cloned().collect()).or(Err(1))?;
    let config = Config {
        compile_info_path: arg.compile_info.clone(),
        suppress_imports: arg.suppress_imports.unwrap_or(false),
        linting_enabled,
        lint_cfg_path: arg.lint_cfg_path.clone(),
        .. Default::default()
    };
    dlsclient.set_config(config).ok();

    for file in &arg.files {
        dlsclient.open_file(file).or(Err(1))?;
    }
    let mut exit_code = Ok(());
    if !arg.files.is_empty() {

        if linting_enabled {
            println!("Linter is enabled");
        }

        dlsclient.wait_for_analysis().map_err(
            |e|match e {
                ExitStatus::Exited(u) => u.try_into().unwrap(),
                ExitStatus::Signaled(u) => u.into(),
                ExitStatus::Other(i) => i,
                ExitStatus::Undetermined => -1,
            })?;

        if !arg.quiet {
            dlsclient.output_errors();
        }
        if arg.test && !dlsclient.no_errors() {
            exit_code = Err(1);
        }
    }

    // Disregard this result, we dont _really_ care about shutting down
    // the server here
    dlsclient.shutdown().ok();

    exit_code
}
