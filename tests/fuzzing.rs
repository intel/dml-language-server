//  © 2026 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

//! Fuzzing-style smoke tests for the DML parser.
//!
//! These tests do **not** validate parser output. They only assert that the
//! parser neither panics nor hangs on arbitrary inputs. When a bad input is
//! discovered, it is automatically reduced (delta-debugging style) to a small
//! reproducer before the test fails.
//!
//! Tuning via environment variables:
//!   * `DLS_FUZZ_ITERS` — number of generated inputs per test (default 32).
//!   * `DLS_FUZZ_SEED`  — PRNG seed (defaults to timestamp-based).

use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use rand::{RngExt, SeedableRng};
use rand::rngs::StdRng;
use rand::seq::IndexedRandom;

/// Per-input wall-clock budget.
const PARSE_TIMEOUT: Duration = Duration::from_secs(60);

/// Default number of times to fuzz per test
const DEFAULT_ITERS: usize = 32;

/// Floor on the per-probe budget while shrinking a `Panic` reproducer.
/// Without a floor, a sub-millisecond panic measurement would translate
/// into a probe budget so tight that scheduling jitter alone could push a
/// genuine panic past it and look like a hang.
const PANIC_PROBE_FLOOR: Duration = Duration::from_millis(100);

/// Upper bound on bytes per generated input
const MAX_INPUT_BYTES: usize = 64 * 1024;

/// How often to poll the child while waiting for it to finish. 5 ms is
/// negligible overhead next to the `PANIC_PROBE_FLOOR` (100 ms) and gives
/// us reasonably tight timeout enforcement.
const CHILD_POLL_INTERVAL: Duration = Duration::from_millis(5);

/// Path to the `dml-fuzz-runner` binary. Cargo sets `CARGO_BIN_EXE_<name>`
/// at compile time for integration tests and guarantees the bin is built
/// (with its `required-features`) before the test runs.
fn runner_exe() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_dml-fuzz-runner"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Outcome {
    Ok,
    Panic,
    Timeout,
}

/// Spawn the fuzz-runner subprocess on `input`, kill it after `timeout`,
/// and classify the exit status.
///
/// Running the parser in a child process (rather than a worker thread)
/// isolates uncatchable failures from the test process: a stack overflow
/// or any other signal-induced abort in the parser only kills the child
/// and is reported back as a `Panic` outcome with a signal note. The
/// caller's `cargo test` invocation keeps running.
fn run_with_timeout(input: &str, timeout: Duration)
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
                    Some(format!("failed to spawn fuzz runner: {e}")),
                    Duration::ZERO);
        }
    };

    // Write stdin from a helper thread so a large input can't deadlock
    // against the runner reading it.
    let mut stdin = child.stdin.take().expect("piped");
    let input_bytes = input.as_bytes().to_vec();
    let writer = thread::spawn(move || {
        let _ = stdin.write_all(&input_bytes);
        // Dropping stdin closes the pipe so the runner's read_to_string returns.
    });

    // Poll until the child exits or we hit the deadline.
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
                        Some(format!("wait on fuzz runner failed: {e}")),
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
        Some(s) => s,
        None => {
            let _ = child.kill();
            let _ = child.wait();
            let _ = writer.join();
            return (Outcome::Timeout, None, timeout);
        }
    };

    // Drain stderr (small: just our "PANIC: ..." line on panic, empty on success).
    let mut stderr = String::new();
    if let Some(mut s) = child.stderr.take() {
        let _ = s.read_to_string(&mut stderr);
    }
    let _ = writer.join();

    if status.success() {
        return (Outcome::Ok, None, elapsed);
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            let hint = match sig {
                11 => " (SIGSEGV; memory unsafety or guard-page hit)",
                6 => " (SIGABRT; Rust abort — stack overflow handler, assertion, or explicit abort)",
                _ => "",
            };
            return (Outcome::Panic,
                    Some(format!("killed by signal {sig}{hint}")),
                    elapsed);
        }
    }

    let msg = stderr
        .lines()
        .find(|l| l.starts_with("PANIC: "))
        .map(|l| l["PANIC: ".len()..].to_owned())
        .unwrap_or_else(|| {
            if stderr.is_empty() {
                format!("exit code {}", status.code().unwrap_or(-1))
            } else {
                stderr.trim().to_owned()
            }
        });
    (Outcome::Panic, Some(msg), elapsed)
}

/// The parser only accepts `&str`, so any bytes that aren't valid UTF-8 are
/// replaced with U+FFFD via lossy conversion
fn normalize(input: &[u8]) -> String {
    String::from_utf8_lossy(input).into_owned()
}


struct ShrinkResult {
    minimized: String,
    discovered_timeouts: Vec<String>,
}

/// Compute the per-probe budget while shrinking a `Panic`, based on the time to panic previously
fn panic_probe_budget(last_elapsed: Duration) -> Duration {
    let doubled = last_elapsed.saturating_mul(2);
    let with_floor = if doubled < PANIC_PROBE_FLOOR { PANIC_PROBE_FLOOR } else { doubled };
    if with_floor > PARSE_TIMEOUT { PARSE_TIMEOUT } else { with_floor }
}

/// Delta-debugging-style shrinker. Repeatedly tries to delete chunks of
/// decreasing size, keeping any candidate that still triggers `target`.
fn shrink(input: String, target: Outcome, mut last_elapsed: Duration) -> ShrinkResult {
    let mut current = input;
    let mut chunk = (current.len() / 2).max(1);
    let mut discovered_timeouts: Vec<String> = Vec::new();

    while chunk > 0 {
        loop {
            let mut i = 0;
            let mut progressed = false;
            while i + chunk <= current.len() {
                // Skip cuts that would split a UTF-8 scalar.
                if !current.is_char_boundary(i) || !current.is_char_boundary(i + chunk) {
                    i += 1;
                    continue;
                }

                let mut candidate = current.clone();
                candidate.drain(i..i + chunk);
                if candidate.is_empty() {
                    break;
                }

                let probe_budget = match target {
                    Outcome::Timeout => PARSE_TIMEOUT,
                    Outcome::Panic => panic_probe_budget(last_elapsed),
                    Outcome::Ok => unreachable!("shrink target must be a failure"),
                };
                let (outcome, _msg, elapsed) =
                    run_with_timeout(&candidate, probe_budget);

                if outcome == target {
                    current = candidate;
                    last_elapsed = elapsed;
                    progressed = true;
                } else if target == Outcome::Panic && outcome == Outcome::Timeout {
                    // A hang surfaced while we were chasing a panic. This
                    // is a new bug, not a smaller version of the current
                    // one; record it separately, then keep shrinking the
                    // panic from where we were.
                    let sub = shrink(candidate, Outcome::Timeout, Duration::ZERO);
                    discovered_timeouts.push(sub.minimized);
                    discovered_timeouts.extend(sub.discovered_timeouts);
                    i += chunk;
                } else {
                    i += chunk;
                }
            }
            if !progressed {
                break;
            }
        }
        chunk /= 2;
    }
    ShrinkResult { minimized: current, discovered_timeouts }
}

fn iter_count() -> usize {
    std::env::var("DLS_FUZZ_ITERS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_ITERS)
}

fn seed_from_env() -> u64 {
    std::env::var("DLS_FUZZ_SEED")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or_else(time_seed)
}

fn time_seed() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0x9E37_79B9_7F4A_7C15)
}

fn truncate_at_boundary(s: &mut String, max: usize) {
    if s.len() <= max {
        return;
    }
    let mut cut = max;
    while cut > 0 && !s.is_char_boundary(cut) {
        cut -= 1;
    }
    s.truncate(cut);
}

/// Short filename tag for a fuzzer label. Used in `fuzz-<tag>-<seed>.fail`.
fn file_tag(label: &str) -> &'static str {
    match label {
        "random-bytes" => "bytes",
        "random-ascii" => "ascii",
        "dml-grammar" => "grammar",
        // Fall back to the label itself if a new fuzzer is added without
        // updating this map; the file will still be unique per seed.
        _ => "other",
    }
}

/// Directory (relative to the package root, which is `cargo test`'s CWD
/// for integration tests) where tests dump generated artifacts. Lives
/// under `target/` so it's already gitignored and is wiped by
/// `cargo clean`.
const TEST_OUTPUT_DIR: &str = "target/test_output";

/// Write a reproducer to `target/test_output/fuzz-<tag>-<seed>[.timeout-<n>].fail`
/// and return its path. Write failures are surfaced as a panic so the
/// reproducer is never silently lost.
fn write_repro(label: &str, seed: u64, timeout_idx: Option<usize>, body: &str)
               -> PathBuf {
    let name = match timeout_idx {
        None => format!("fuzz-{}-{seed}.fail", file_tag(label)),
        Some(n) => format!("fuzz-{}-{seed}.timeout-{n}.fail", file_tag(label)),
    };
    let dir = PathBuf::from(TEST_OUTPUT_DIR);
    std::fs::create_dir_all(&dir)
        .unwrap_or_else(|e| panic!("failed to create {}: {}", dir.display(), e));
    let path = dir.join(&name);
    std::fs::write(&path, body)
        .unwrap_or_else(|e| panic!("failed to write reproducer to {}: {}",
                                   path.display(), e));
    path
}

/// Common fuzzing driver: generate, run, on failure shrink and report.
fn fuzz<F>(label: &str, base_seed: u64, salt: u64, iters: usize, mut gen: F)
where
    F: FnMut(&mut StdRng) -> Vec<u8>,
{
    if iters == 0 {
        return;
    }

    assert_eq!(run_with_timeout("", PARSE_TIMEOUT).0, Outcome::Ok,
               "fuzz harness failed on empty input");

    // Surface the base seed so a failing run (or a curious passing run
    // invoked with `--nocapture`) can be replayed via `DLS_FUZZ_SEED`.
    eprintln!("[{label}] seed={base_seed} iters={iters}");

    let rng_seed = base_seed ^ salt.wrapping_mul(0x9E37_79B9_7F4A_7C15);
    let mut rng = StdRng::seed_from_u64(rng_seed);
    for i in 0..iters {
        let raw = gen(&mut rng);
        // Normalize once; the shrinker then works on this exact string.
        // Cap the size so a pathological generator doesn't make shrinking
        // grindingly slow.
        let mut input = normalize(&raw);
        truncate_at_boundary(&mut input, MAX_INPUT_BYTES);
        let (outcome, panic_msg, elapsed) =
            run_with_timeout(&input, PARSE_TIMEOUT);
        if outcome != Outcome::Ok {
            eprintln!(
                "[{label}] iteration {i} produced {outcome:?} on input of {} bytes \
                 (parse took {:?}); attempting to minimize...",
                input.len(), elapsed,
            );
            let ShrinkResult { minimized, discovered_timeouts } =
                shrink(input, outcome, elapsed);
            let panic_note = panic_msg
                .as_deref()
                .map(|m| format!("; parser panic: {m}"))
                .unwrap_or_default();

            // Persist the minimized reproducer (and any timeout repros
            // discovered while shrinking) to files in CWD so they can be
            // re-fed to the runner without copy-pasting binary bytes out
            // of the panic message.
            let primary_path = write_repro(label, base_seed, None, &minimized);
            let mut extras_note = String::new();
            for (n, t) in discovered_timeouts.iter().enumerate() {
                let p = write_repro(label, base_seed, Some(n), t);
                extras_note.push_str(&format!("\n  timeout #{n}: {}", p.display()));
            }
            let extras = if extras_note.is_empty() {
                String::new()
            } else {
                format!(
                    "\n\nAlso discovered {} timeout reproducer(s) while shrinking:{}",
                    discovered_timeouts.len(), extras_note,
                )
            };
            panic!(
                "{label} fuzzer hit {outcome:?}{panic_note} (seed {base_seed}, iter {i}). \
                 Minimized to {} bytes; written to {}{extras}",
                minimized.len(), primary_path.display(),
            );
        }
    }
}


#[test]
fn fuzz_random_bytes() {
    let seed = seed_from_env();
    let iters = iter_count();
    // Truly-random bytes are mostly non-UTF8 and collapse to U+FFFD via
    // lossy conversion; they're noisy and slow to shrink. Keep this run
    // small — it exists mainly to catch lexer-level crashes on arbitrary
    // bytes, not to drive deep parser exploration.
    fuzz("random-bytes", seed, 0x0001, iters, |rng| {
        let len = rng.random_range(1..=512);
        (0..len).map(|_| rng.random::<u8>()).collect()
    });
}

#[test]
fn fuzz_random_ascii() {
    let seed = seed_from_env();
    let iters = iter_count();
    // ASCII-only printables (plus tab/newline) survive UTF-8 conversion
    // verbatim, so the shrinker can produce a small, human-readable
    // reproducer when the parser misbehaves.
    fuzz("random-ascii", seed, 0x0003, iters, |rng| {
        let len = rng.random_range(1..=4096);
        (0..len).map(|_| {
            // 0x20..=0x7e covers all printable ASCII; sprinkle in
            // whitespace so multi-line inputs are reachable.
            match rng.random_range(0u32..100) {
                0..=2 => b'\n',
                3..=4 => b'\t',
                _ => rng.random_range(0x20u8..=0x7e),
            }
        }).collect()
    });
}

// ---------------------------------------------------------------------------
// DML-grammar-approximation fuzzer
// ---------------------------------------------------------------------------
//
// This is a deliberately loose approximation of the DML 1.4 grammar
// (https://intel.github.io/simics/devices/dml-1.4/). It produces inputs that
// often *look* like DML — version header, optional device, then a mix of
// imports, parameters, methods, banks/registers/fields, templates, etc. —
// but it is intentionally allowed to wander outside the grammar so the
// parser's error-recovery paths get exercised too.


/// Usage:
/// ```
/// pick_random!(rng, [
///     format!("..."),
///     { let x = ident(rng); format!("is {x};") },
///     other_helper(rng),
/// ])
/// ```
macro_rules! pick_random {
    ($rng:expr, [ $($arm:expr),+ $(,)? ]) => {{
        const __N: usize = <[()]>::len(&[$( pick_random!(@unit $arm) ),+]);
        let __idx: usize = $rng.random_range(0..__N);
        pick_random!(@dispatch __idx, 0usize, { $($arm),+ })
    }};
    (@unit $e:expr) => { () };
    (@dispatch $idx:expr, $cur:expr, { $head:expr $(, $rest:expr)* }) => {
        if $idx == $cur {
            $head
        } else {
            pick_random!(@dispatch $idx, $cur + 1usize, { $($rest),* })
        }
    };
    (@dispatch $idx:expr, $cur:expr, { }) => {
        unreachable!("pick_random index out of range")
    };
}

const KEYWORDS: &[&str] = &[
    "device", "bank", "register", "field", "method", "template", "is",
    "parameter", "param", "import", "constant", "extern", "typedef",
    "struct", "layout", "bitfields", "loggroup", "in", "each", "group",
    "port", "subdevice", "interface", "implement", "attribute", "connect",
    "event", "session", "saved", "data", "header", "footer", "if", "else",
    "while", "for", "foreach", "switch", "case", "default", "return",
    "break", "continue", "throw", "try", "catch", "after", "log", "assert",
    "call", "inline", "local", "static", "const", "auto", "new", "delete",
    "sizeof", "typeof", "cast", "true", "false", "null", "undefined",
    "this", "shared", "hook",
];

const TYPES: &[&str] = &[
    "int", "uint", "uint8", "uint16", "uint32", "uint64",
    "int8", "int16", "int32", "int64", "bool", "double", "float",
    "char", "void",
];

const OPS: &[&str] = &[
    "+", "-", "*", "/", "%", "&", "|", "^", "~", "!", "&&", "||",
    "==", "!=", "<", ">", "<=", ">=", "<<", ">>", "=", "+=", "-=",
    "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "++", "--",
    "->", ".", "?", ":",
];

fn ident(rng: &mut StdRng) -> String {
    let len = rng.random_range(1..=8);
    let first = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
    let rest = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";
    let mut s = String::with_capacity(len + 1);
    s.push(*first.choose(rng).unwrap() as char);
    for _ in 0..len {
        s.push(*rest.choose(rng).unwrap() as char);
    }
    s
}

fn number(rng: &mut StdRng) -> String {
    pick_random!(rng, [
        format!("{}", rng.random_range(0u64..1024)),
        format!("0x{:x}", rng.random_range(0u64..0x1_0000)),
        format!("0b{:b}", rng.random_range(0u64..0x100)),
        format!("{}.{}", rng.random_range(0u64..100), rng.random_range(0u64..100)),
    ])
}

fn string_lit(rng: &mut StdRng) -> String {
    let len = rng.random_range(0..12);
    let chars = b"abcdefghijklmnopqrstuvwxyz 0123456789_-./";
    let mut s = String::from("\"");
    for _ in 0..len {
        s.push(*chars.choose(rng).unwrap() as char);
    }
    s.push('"');
    s
}

fn expr(rng: &mut StdRng, depth: u32) -> String {
    if depth == 0 || rng.random_ratio(1, 3) {
        return pick_random!(rng, [
            ident(rng),
            number(rng),
            string_lit(rng),
            (*["true", "false", "null", "this", "default"].choose(rng).unwrap()).into(),
        ]);
    }
    pick_random!(rng, [
        format!("({})", expr(rng, depth - 1)),
        {
            // Bind sub-expressions before picking the operator so evaluation
            // order is deterministic w.r.t. the RNG stream.
            let lhs = expr(rng, depth - 1);
            let op = *OPS.choose(rng).unwrap();
            let rhs = expr(rng, depth - 1);
            format!("{lhs} {op} {rhs}")
        },
        format!("{}({})", ident(rng), expr_list(rng, depth - 1)),
        format!("{}.{}", expr(rng, depth - 1), ident(rng)),
        format!("{}[{}]", expr(rng, depth - 1), expr(rng, depth - 1)),
        {
            let op = *["-", "!", "~", "++", "--", "&", "*"].choose(rng).unwrap();
            format!("{op}{}", expr(rng, depth - 1))
        },
    ])
}

fn expr_list(rng: &mut StdRng, depth: u32) -> String {
    let n = rng.random_range(0..4);
    (0..n).map(|_| expr(rng, depth)).collect::<Vec<_>>().join(", ")
}

fn type_ref(rng: &mut StdRng) -> String {
    let base: String = if rng.random_ratio(2, 3) {
        (*TYPES.choose(rng).unwrap()).into()
    } else {
        ident(rng)
    };
    let mut t = base;
    if rng.random_ratio(1, 4) { t.push('*'); }
    if rng.random_ratio(1, 5) { t = format!("const {t}"); }
    t
}

fn stmt(rng: &mut StdRng, depth: u32) -> String {
    if depth == 0 {
        return format!("{};", expr(rng, 2));
    }
    pick_random!(rng, [
        format!("{};", expr(rng, 3)),
        format!("local {} {} = {};", type_ref(rng), ident(rng), expr(rng, 3)),
        format!("return {};", expr(rng, 3)),
        format!("if ({}) {}", expr(rng, 3), block(rng, depth - 1)),
        format!(
            "if ({}) {} else {}",
            expr(rng, 3),
            block(rng, depth - 1),
            block(rng, depth - 1)
        ),
        format!("while ({}) {}", expr(rng, 3), block(rng, depth - 1)),
        // Deliberately emits a bare-identifier init (no `local int i = 0;`)
        // — this exercises the parser's error-recovery path for malformed
        // `for`-headers, not just well-formed loops.
        format!(
            "for ({} = 0; {} < {}; {}++) {}",
            ident(rng), ident(rng), expr(rng, 2), ident(rng), block(rng, depth - 1)
        ),
        format!("log info, {}: {};", number(rng), string_lit(rng)),
        format!("assert {};", expr(rng, 3)),
        format!(
            "#if ({}) {} #else {}",
            expr(rng, 3),
            block(rng, depth - 1),
            block(rng, depth - 1)
        ),
        format!("{} = {};", ident(rng), expr(rng, 3)),
    ])
}

fn block(rng: &mut StdRng, depth: u32) -> String {
    let n = rng.random_range(1..=4);
    let mut s = String::from("{\n");
    for _ in 0..n {
        s.push_str("    ");
        s.push_str(&stmt(rng, depth));
        s.push('\n');
    }
    s.push('}');
    s
}

fn method_decl(rng: &mut StdRng) -> String {
    let name = ident(rng);
    let n_args = rng.random_range(0..4);
    let args: Vec<String> = (0..n_args)
        .map(|_| format!("{} {}", type_ref(rng), ident(rng)))
        .collect();
    let returns = if rng.random_ratio(1, 2) {
        format!(" -> ({})", type_ref(rng))
    } else {
        String::new()
    };
    format!("method {name}({}){} {}\n", args.join(", "), returns, block(rng, 3))
}

fn template_decl(rng: &mut StdRng) -> String {
    let name = ident(rng);
    let n = rng.random_range(1..=3);
    let mut body = String::from("{\n");
    for _ in 0..n {
        body.push_str("    ");
        body.push_str(&top_member(rng));
        body.push('\n');
    }
    body.push('}');
    format!("template {name} {body}\n")
}

fn register_decl(rng: &mut StdRng) -> String {
    let name = ident(rng);
    let size = rng.random_range(1u64..=8).next_power_of_two();
    let offs = rng.random_range(0u64..0x1000);
    let mut s = format!("register {name} size {size} @ {offs:#x}");
    if rng.random_ratio(1, 2) {
        let n = rng.random_range(1..=2);
        s.push_str(" {\n");
        for _ in 0..n {
            s.push_str("        ");
            s.push_str(&format!("field {} @ [{}:{}];\n",
                                ident(rng),
                                rng.random_range(0..32),
                                rng.random_range(0..32)));
        }
        s.push_str("    }");
    } else {
        s.push(';');
    }
    s
}

fn top_member(rng: &mut StdRng) -> String {
    pick_random!(rng, [
        format!("param {} = {};", ident(rng), expr(rng, 3)),
        format!("param {} : {};", ident(rng), type_ref(rng)),
        method_decl(rng),
        register_decl(rng),
        format!("is {};", ident(rng)),
        format!("connect {} {{}}", ident(rng)),
        format!("attribute {} {{}}", ident(rng)),
        format!("session {} {} = {};", type_ref(rng), ident(rng), expr(rng, 2)),
    ])
}

fn toplevel_decl(rng: &mut StdRng) -> String {
    pick_random!(rng, [
        format!("import {};", string_lit(rng)),
        format!("typedef {} {};", type_ref(rng), ident(rng)),
        format!("constant {} = {};", ident(rng), expr(rng, 3)),
        format!("extern {} {};", type_ref(rng), ident(rng)),
        template_decl(rng),
        method_decl(rng),
        format!(
            "bank {} {{\n    {}\n}}\n",
            ident(rng),
            (0..rng.random_range(1..=3))
                .map(|_| register_decl(rng))
                .collect::<Vec<_>>()
                .join("\n    ")
        ),
        format!("loggroup {};", ident(rng)),
        // Sometimes emit something deliberately weird: a keyword stuck in
        // an unexpected spot. Exercises error recovery.
        format!("{} {} ;", KEYWORDS.choose(rng).unwrap(), ident(rng)),
        format!("param {} = {};", ident(rng), expr(rng, 3)),
    ])
}

fn dml_grammar_gen(rng: &mut StdRng) -> Vec<u8> {
    let mut s = String::new();
    if rng.random_ratio(7, 8) {
        let ver = if rng.random_ratio(1, 2) { "1.4" } else { "1.2" };
        s.push_str(&format!("dml {ver};\n"));
    }
    if rng.random_ratio(1, 2) {
        s.push_str(&format!("device {};\n", ident(rng)));
    }
    let n = rng.random_range(1..=24);
    for _ in 0..n {
        s.push_str(&toplevel_decl(rng));
        s.push('\n');
    }
    s.into_bytes()
}

#[test]
fn fuzz_dml_grammar() {
    let seed = seed_from_env();
    let iters = iter_count();
    fuzz("dml-grammar", seed, 0x0002, iters, dml_grammar_gen);
}
