//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

use std::io::Write;
use std::fmt;
use std::sync::LazyLock;

use chrono::Local;

macro_rules! info {
    ($($arg:tt)*) => {
        crate::log_with_fn!(log::info, log::Level::Info, $($arg)*)
    };
}

macro_rules! trace {
    ($($arg:tt)*) => {
        crate::log_with_fn!(log::trace, log::Level::Trace, $($arg)*)
    };
}

macro_rules! debug {
    ($($arg:tt)*) => {
        crate::log_with_fn!(log::debug, log::Level::Debug, $($arg)*)
    };
}

macro_rules! error {
    ($($arg:tt)*) => {
        crate::log_with_fn!(log::error, log::Level::Error, $($arg)*)
    };
}

#[allow(clippy::crate_in_macro_def)]
#[macro_export]
macro_rules! log_with_fn {
    ($log:path, $level:expr, $($arg:tt)*) => {
        if (log::log_enabled!($level)) {
            $log!("{}", $crate::format_truncated!(*crate::logging::MAX_LOG_MESSAGE_LENGTH, $($arg)*));
        }
    }
}

pub (crate) use {debug, error, info, trace};

pub static MAX_LOG_MESSAGE_LENGTH: LazyLock<Option<usize>> =
    LazyLock::new(|| {
        std::env::var("MAX_LOG_MESSAGE_LENGTH").ok()
            .or_else(||Some("1000".to_string()))
            .and_then(|s| s.parse().ok())
    });

#[macro_export]
macro_rules! format_truncated {
    ($trunc_len:expr, $($arg:tt)*) => {{
        $crate::logging::format_truncated($trunc_len, format_args!($($arg)*))
    }};
}

struct TruncatedWriter {
    output: String,
    max_len: usize,
    truncated: bool,
}

impl fmt::Write for TruncatedWriter {
    fn write_str(&mut self, value: &str) -> fmt::Result {
        if self.truncated {
            return Err(fmt::Error);
        }
        let remaining = self.max_len - self.output.len();
        if value.len() <= remaining {
            self.output.push_str(value);
            return Ok(());
        }
        let mut end = remaining;
        while !value.is_char_boundary(end) {
            end -= 1;
        }
        self.output.push_str(&value[..end]);
        self.truncated = true;
        // Stop recursive Debug/Display formatting as soon as the limit is exceeded.
        Err(fmt::Error)
    }
}

pub fn format_truncated(max_len: Option<usize>, args: fmt::Arguments<'_>) -> String {
    let Some(max_len) = max_len.filter(|length|*length > 0) else {
        return fmt::format(args);
    };
    let mut writer = TruncatedWriter {
        output: String::new(),
        max_len,
        truncated: false,
    };
    if fmt::write(&mut writer, args).is_err() && !writer.truncated {
        panic!("a formatting trait implementation returned an error while formatting a log message");
    }
    if writer.truncated {
        writer.output.push_str(" ...");
    }
    writer.output
}

pub fn init() {
    // Slightly dumb way to do this, build the logger once with defaults
    // to get the max level for all modules from env, then build again
    // to remove this restriction and place it on log:: instead
    let dummy_logger = env_logger::Builder::from_default_env().build();
    let max_level = dummy_logger.filter();
    env_logger::Builder::from_default_env()
        .filter_level(log::LevelFilter::Trace)
        .format(local_time_format)
        .init();
    set_global_log_level(max_level);
}

/// Replicates `env_logger`'s default record format, except the timestamp is
/// rendered in the system's local time zone
fn local_time_format(
    buf: &mut env_logger::fmt::Formatter,
    record: &log::Record<'_>,
) -> std::io::Result<()> {
    // ISO 8601 with seconds precision (env_logger's default), but local time
    // with the local UTC offset instead of the "Z" suffix.
    let ts = Local::now().format("%Y-%m-%dT%H:%M:%S%:z");
    let level = record.level();
    let style = buf.default_level_style(level);
    // Formatting 'style' normally enables it, doing it with :# disables it
    write!(buf, "[{ts} {style}{level:<5}{style:#}")?;
    let target = record.target();
    if !target.is_empty() {
        write!(buf, " {target}")?;
    }
    let msg = record.args().to_string();
    if msg.contains('\n') {
        writeln!(buf, "] {}", msg.replace('\n', "\n    "))
    } else {
        writeln!(buf, "] {msg}")
    }
}

pub fn set_global_log_level(level: log::LevelFilter) {
    log::set_max_level(level);
}

#[cfg(test)]
mod tests {
    use std::fmt;

    #[test]
    fn test_format_truncated() {
        let s = format_truncated!(Some(5), "Hello, world!");
        assert_eq!(s, "Hello ...");
        let s = format_truncated!(Some(20), "Hello, world!");
        assert_eq!(s, "Hello, world!");
        let s = format_truncated!(None, "Hello, world!");
        assert_eq!(s, "Hello, world!");
        let s = format_truncated!(Some(0), "Hello, world!");
        assert_eq!(s, "Hello, world!");
    }

    #[test]
    fn test_format_unicode() {
        // These particular emojis are 4 bytes each
        let s = format_truncated!(Some(8), "😀😀😀😀😀");
        assert_eq!(s, "😀😀 ...");
        let s = format_truncated!(Some(7), "😀😀😀😀😀");
        assert_eq!(s, "😀 ...");
        let s = format_truncated!(Some(11), "😀😀😀😀😀");
        assert_eq!(s, "😀😀 ...");
        let s = format_truncated!(Some(30), "😀😀😀😀😀");
        assert_eq!(s, "😀😀😀😀😀");
    }

    #[test]
    fn formatting_options_are_preserved() {
        let value = 42;
        let expected = format!("{value:08x} {number:>6.2} {:#?}", [1, 2], number = 1.25);
        assert_eq!(format_truncated!(Some(expected.len()),
            "{value:08x} {number:>6.2} {:#?}", [1, 2], number = 1.25), expected);
        assert_eq!(format_truncated!(Some(5), "{value:08x}"), "00000 ...");
    }

    #[test]
    fn truncation_stops_formatting() {
        struct TooLong;
        impl fmt::Display for TooLong {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("12345")?;
                formatter.write_str("6")?;
                panic!("formatting should have stopped");
            }
        }
        assert_eq!(format_truncated!(Some(5), "{TooLong}"), "12345 ...");
    }

    #[test]
    fn debug_collections_stop_visiting_elements_at_the_limit() {
        use std::cell::Cell;

        struct Counted<'a>(&'a Cell<usize>);
        impl fmt::Debug for Counted<'_> {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.set(self.0.get() + 1);
                formatter.write_str("entry")
            }
        }
        let visits = Cell::new(0);
        let entries: Vec<_> = (0..10_000).map(|_|Counted(&visits)).collect();
        let result = format_truncated!(Some(10), "{entries:?}");
        assert_eq!(result, "[entry, en ...");
        assert_eq!(visits.get(), 2);
    }

    #[test]
    fn unicode_truncation_across_writes() {
        assert_eq!(format_truncated!(Some(5), "{}{}", "abc", "😀def"), "abc ...");
        assert_eq!(format_truncated!(Some(4), "{}{}", "abcd", ""), "abcd");
        assert_eq!(format_truncated!(Some(1), "😀"), " ...");
    }

    #[test]
    #[should_panic(expected = "formatting trait implementation returned an error")]
    fn formatter_errors_are_not_silenced() {
        struct Broken;
        impl fmt::Display for Broken {
            fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
                Err(fmt::Error)
            }
        }
        let _ = format_truncated!(Some(5), "{Broken}");
    }
}
