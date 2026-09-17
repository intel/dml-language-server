//  © 2026 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

const CHILD_POLL_INTERVAL: Duration = Duration::from_millis(5);

fn runner_exe() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_dml-fuzz-runner"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Outcome {
    Ok,
    Panic,
    Timeout,
}

/// Spawn the parser runner as a child so hangs and process-level failures do
/// not take down the integration-test process.
pub(crate) fn run_with_timeout(input: &str, timeout: Duration)
                               -> (Outcome, Option<String>, Duration) {
    let started = Instant::now();

    let mut child = match Command::new(runner_exe())
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            return (Outcome::Panic,
                    Some(format!("failed to spawn parser runner: {e}")),
                    Duration::ZERO);
        }
    };

    let mut stdin = child.stdin.take().expect("piped");
    let input_bytes = input.as_bytes().to_vec();
    let writer = thread::spawn(move || {
        let _ = stdin.write_all(&input_bytes);
    });

    let deadline = started + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break Some(status),
            Ok(None) => {}
            Err(e) => {
                let _ = child.kill();
                let _ = child.wait();
                let _ = writer.join();
                return (Outcome::Panic,
                        Some(format!("wait on parser runner failed: {e}")),
                        started.elapsed());
            }
        }
        if Instant::now() >= deadline {
            break None;
        }
        thread::sleep(CHILD_POLL_INTERVAL);
    };

    let elapsed = started.elapsed();
    let status = match status {
        Some(status) => status,
        None => {
            let _ = child.kill();
            let _ = child.wait();
            let _ = writer.join();
            return (Outcome::Timeout, None, timeout);
        }
    };

    let mut stderr = String::new();
    if let Some(mut stream) = child.stderr.take() {
        let _ = stream.read_to_string(&mut stderr);
    }
    let _ = writer.join();

    if status.success() {
        return (Outcome::Ok, None, elapsed);
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signal) = status.signal() {
            let hint = match signal {
                11 => " (SIGSEGV; memory unsafety or guard-page hit)",
                6 => " (SIGABRT; Rust abort — stack overflow handler, assertion, or explicit abort)",
                _ => "",
            };
            return (Outcome::Panic,
                    Some(format!("killed by signal {signal}{hint}")),
                    elapsed);
        }
    }

    let message = stderr
        .lines()
        .find(|line| line.starts_with("PANIC: "))
        .map(|line| line["PANIC: ".len()..].to_owned())
        .unwrap_or_else(|| {
            if stderr.is_empty() {
                format!("exit code {}", status.code().unwrap_or(-1))
            } else {
                stderr.trim().to_owned()
            }
        });
    (Outcome::Panic, Some(message), elapsed)
}
