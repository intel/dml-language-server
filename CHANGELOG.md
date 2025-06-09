<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# Change Log

## 0.9.11

## 0.9.10
- Added indentation-style rules to the lint module with configurable indent size as specified in [example_lint_cfg.json](./example_files/example_lint_cfg.README)
- Added "explicit\_param\_decl" as a know provisional (NOTE: the provisionals
  actual functionality is not implemented)
- Configuration changes are now correctly tracked with the pull-model, and the
  server should correctly update most settings without a restart on config
  change.

## 0.9.9
- Fixed an issue that could cause analysis thread crash when an object was declared both
  as an array and not one
- Added LSP protocol extension notification "changeActiveContexts" and request
  "getKnownContexts" which together can be used to determine and control the active
  device contexts for the server.
- Moved warning feedback when syntactic/semantic analysis is not available to
  server output from server message
- Goto-definition/declaration/implementation/references will now try to wait
  for ongoing device analysis on the file before responding.
- The DLS warning message setting 'once' will now warn once per file-warning
  combination, rather than once per warning.
- The DLS will no longer multi-report 'WorkDoneProgressEnd'.
- Added a configuration option, "lint\_direct\_only", that disables lint reports
  for files not directly opened in the editor. Defaults to true.

## 0.9.8
- Added "warning" as a valid log statement type.
- Added warning feedback through LSP messages when analysis might be unavailable
  or only partially complete
-- When isolated/device (also known as syntactic/semantic) analysis is not available
-- When requesting symbol information about types
-- When requesting symbol or reference information about symbols or references from
   inside an uninstantiated template
- Fine-grained the 'showWarnings' setting, can now be set to 'once', 'always',
  or 'never'
- Fixed error where server did not correctly handle unicode-encoded paths received from client
- Server will no longer message on start.
- Correct behavior of DFA "-t" flag

## 0.9.7
- Fixed DLS crash that occured when an object was declarated both with array dimensions and without

## 0.9.6
- Added DFA command-line-option "--suppress-imports" which makes the server not recurse analysis into imported files

## 0.9.5
- Fixed parse error when encountering "default" method calls while parsing switch statements
- Fixed range comparison operation that would occasionally break sorting invariants
