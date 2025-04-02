<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# Change Log

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
