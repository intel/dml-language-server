//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

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
