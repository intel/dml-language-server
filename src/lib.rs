//  © 2024 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT
//! The DML Language Server.
//!
//! The DLS provides a server that runs in the background, providing IDEs,
//! editors, and other tools with information about DML code.
//!
//! Currently, it supports syntax parsing of DML 1.4 files, as well
//! as basic functionality such as 'goto definition', 'goto reference',
//! 'goto implementation', 'goto base'.

#![warn(rust_2018_idioms)]
#![warn(clippy::clone_on_ref_ptr)]
#![allow(clippy::cognitive_complexity,
         clippy::too_many_arguments,
         clippy::redundant_closure,
         clippy::needless_lifetimes,
         clippy::only_used_in_recursion)]

// TODO: replace internal error panics with internal log errors where
// possible
macro_rules! internal_error {
    ( $mess:expr, $( $arg:expr ),* ) => {
        error!("Internal Error: {}", format!($mess, $( $arg ),*));
    };
    ( $mess:expr ) => {
        error!("Internal Error: {}", $mess);
    }
}

#[macro_use]
pub mod actions;
pub mod analysis;
pub mod cmd;
pub mod concurrency;
pub mod config;
pub mod dfa;
pub mod file_management;
pub mod lint;
pub mod lsp_data;
pub mod server;
pub mod span;
pub mod utility;
pub mod vfs;
#[cfg(test)]
pub mod test;

type Span = span::Span<span::ZeroIndexed>;

pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}
