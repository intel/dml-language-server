//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

// static LOG_RELOADER: LazyLock<ReloadHandle<LevelFilter<Logger>>>
//     = LazyLock::new(||{
//         let env_log = env_logger::Builder::from_default_env().build();
//         let max_level = env_log.filter();
//         // If the environment did not specify logging, let everything through
//         // on a filter level,
//         let filter_log = LevelFilter::new(
//             max_level.to_level().unwrap_or(Level::Trace),
//             env_log);
//         let reload_log = ReloadLog::new(filter_log);
//         let logger = Box::new(reload_log);
//         let handle = logger.handle();
//         log::set_boxed_logger(logger).expect("Failed to set logger");
//         log::set_max_level(max_level);
//         handle
//     });

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
