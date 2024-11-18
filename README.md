<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# DML Language Server (DLS)

The DLS provides a server that runs in the background, providing IDEs,
editors, and other tools with information about DML device and common code.
It currently supports basic syntax error reporting, symbol search,
'goto-definition', 'goto-implementation', 'goto-reference', and 'goto-base'.
It also has some basic configurable linting support, in the form of warning
messages.

Future planned features are extended semantic and type analysis, basic
refactoring patterns, improved language construct templates, renaming
support, and more.

Do note that the DLS only supports DML 1.4 code, and there are no plans to
extend this functionality to support DML 1.2 code. It can only perform
analysis on files declared as using DML 1.4 version.

## Building

Simply run "cargo build --release" in the checkout directory.

## Running

The DLS is built to work with the Language Server Protocol, and as such it in
theory supports many IDEs and editors. However currently the only implemented
language client is the Simics Modeling Extension for Visual Studio Code, which
is not yet publicly available.

See [clients.md](clients.md) for information about how to implement
your own language client compatible with the DML language server.
