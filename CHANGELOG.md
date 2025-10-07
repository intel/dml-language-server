<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# Change Log

## 0.9.14
- Slight optimization to the memory usage of device-level analysis which
  should improve runtimes and reduce the chance of the DLS running out
  of memory on larger devices.
- Optimizations to reference matching, which should greatly speed up semantic
  analysis speed of larger devices.
- Corrected range of error reporting of unknown templates
- Fixed instances of the language server crashing when tracked files were
  removed while it was running.
- The DLS will now interrupt ongoing work when it would be redundant
  (in some cases), resulting in performance improvements.

## 0.9.13
- Corrected the name of "explicit\_param\_decls" provisional. Note that it still
  has no semantic effect.
- The parameter declaration patterns ":=" and ": \<type\> = \<val\>" are now
  correctly parsed.
- You can now annotate dml source to disable reporting of specific lints for specific files or lines,
  see [USAGE.md](USAGE.md) for instructions on how to use it
- Disabled the invariant check for the parameter 'size' to be set on register
  objects. Will be re-enabled when constant-folding is added to the DLS.
- Fixed issue where statements under top-level in-eachs were not correctly tracked.
- Moved storage of reference->symbol mapping to on-demand timing, should significantly speed
  up device analysises
- Unknown fields in the lint configuration file are now detected and reported as errors, helping users identify and correct typos or unsupported configuration options.
- CLI tool DFA now uses default one-indexed line count for reporting warnings on analyzed files.
  `--zero-indexed` flag can be set to `true` when executing DFA for using zero-indexed counting if required.
- Added support for spacing linting rules SpReserved, SpBinop, SpTernary, SpPtrDecl and NspPtrDecl.
- Fixed inability to goto definition/reference on template references in 'in each' and 'for each' constructs
- Fixed incorrect parsing of comments at the last line of a file

## 0.9.12
- Added 'simics\_util\_vect' as a known provisional (with no DLS semantics)
- Diagnostics sent from the server will now indicate their source as 'dml' or 'dml-lint'
- Linting warnings can now annotate their messages with which linting rule enforces it. You can turn this off by setting 'annotate_lints' to 'false' in your lint config file
- Minor fixes to the description of linting messages
- Fixed an issue where diagnostics would sometime be reported within the wrong file
- Corrected incorrect parsing around tuple-declared 'local' variables in for-loops
- Added the ability to declare 'saved' or 'session' variables in for-loops
- Added functionality for goto-def/decl/ref to or from variables declared
  within a function scope.

## 0.9.11
- Fixed deadlock when a configuration update happens

## 0.9.10
- Added indentation-style rules to the lint module with configurable indent size as specified in [example_lint_cfg.json](./example_files/example_lint_cfg.README)
- Added "explicit\_param\_decl" as a know provisional (NOTE: the provisionals
  actual functionality is not implemented)
- Configuration changes are now correctly tracked with the pull-model, and the
  server should correctly update most settings without a restart on config
  change.
- Fixed issue where server could crash in rare cases when opening a file

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
