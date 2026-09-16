//  © 2026 Intel Corporation
//  SPDX-License-Identifier: Apache-2.0 and MIT

//! Regression tests for parser termination during malformed-delimiter recovery.

#[path = "common/parser_runner.rs"]
mod parser_runner;

use std::time::Duration;

use parser_runner::{Outcome, run_with_timeout};

const DELIMITER_REGRESSION_TIMEOUT: Duration = Duration::from_secs(5);

fn assert_delimiter_recovery_terminates(declaration: &str) {
    let input = format!(
        "dml 1.4;\ndevice delimiter_recovery_test;\n{declaration}\n\
         session int valid_after_recovery;\n");
    let (outcome, message, elapsed) =
        run_with_timeout(&input, DELIMITER_REGRESSION_TIMEOUT);
    assert_eq!(outcome, Outcome::Ok,
               "parser did not terminate normally after {elapsed:?}: {message:?}");
}

#[test]
fn delimiter_recovery_struct_member_semicolon() {
    assert_delimiter_recovery_terminates(
        "typedef struct { int value } struct_missing_member_semicolon_t;");
}

#[test]
fn delimiter_recovery_layout_member_semicolon() {
    assert_delimiter_recovery_terminates(
        "typedef layout \"big-endian\" { int value } \
         layout_missing_member_semicolon_t;");
}

#[test]
fn delimiter_recovery_bitfield_at() {
    assert_delimiter_recovery_terminates(
        "typedef bitfields 8 { uint1 bit [0]; } bitfield_missing_at_t;");
}

#[test]
fn delimiter_recovery_bitfield_brackets() {
    assert_delimiter_recovery_terminates(
        "typedef bitfields 8 { uint1 bit @ 0; } bitfield_missing_bracket_t;");
}

#[test]
fn delimiter_recovery_bitfield_range_operand() {
    assert_delimiter_recovery_terminates(
        "typedef bitfields 8 { uint1 bit @ [7:]; } \
         bitfield_missing_operand_t;");
}

#[test]
fn delimiter_recovery_bitfield_semicolon() {
    assert_delimiter_recovery_terminates(
        "typedef bitfields 8 { uint1 bit @ [0] } \
         bitfield_missing_semicolon_t;");
}

#[test]
fn delimiter_recovery_typeof_parenthesis() {
    assert_delimiter_recovery_terminates(
        "session typeof(value missing_typeof_rparen;");
}

#[test]
fn delimiter_recovery_array_declarator() {
    assert_delimiter_recovery_terminates("session int malformed_array[;");
}
