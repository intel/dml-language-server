//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

use std::sync::LazyLock;

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
        let s = format!($($arg)*);
        'check_block: {
            if let Some(max_len) = $trunc_len {
                if s.len() > max_len {
                    let mut slice_index = max_len;
                    while !s.is_char_boundary(slice_index) {
                        slice_index -= 1;
                    }
                    break 'check_block [&s[..slice_index], " ..."].concat();
                }
            }
            s
        }
    }};
}

pub fn init() {
    // Slightly dumb way to do this, build the logger once with defaults
    // to get the max level for all modules from env, then build again
    // to remove this restriction and place it on log:: instead
    let dummy_logger = env_logger::Builder::from_default_env().build();
    let max_level = dummy_logger.filter();
    env_logger::Builder::from_default_env()
        .filter_level(log::LevelFilter::Trace)
        .init();
    set_global_log_level(max_level);
}

pub fn set_global_log_level(level: log::LevelFilter) {
    log::set_max_level(level);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_format_truncated() {
        let s = format_truncated!(Some(5), "Hello, world!");
        assert_eq!(s, "Hello ...");
        let s = format_truncated!(Some(20), "Hello, world!");
        assert_eq!(s, "Hello, world!");
        let s = format_truncated!(None, "Hello, world!");
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
}
