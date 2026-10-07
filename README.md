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
messages. For user-targeted instructions, see [USAGE.md](USAGE.md).

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

## <a id="dml-compile-commands"></a> DML Compile Commands & CMake Compile Commands
The language server leverages configuration info from a workspace/project in
order to obtain per-module information used to resolve imports and get relevant
command-line DMLC flags for specific devices.

You can provide this information by either providing CMake or DML compile commands to
the server. If both are configured, the server will prefer the DML compile commands.

### CMake Compile Commands
By setting the "CMAKE_EXPORT_COMPILE_COMMANDS" flag to `1` when you configure
your cmake build tree, it will generate a `compile_commands.json` file.

You can configure the DLS to use this file for locating import-paths by setting the
`cmakeCompileInfoPath` setting on the server configuration to the path of the
compile commands file. A drawback of using CMake compile commands
(as compared to DML compile commands below) is that it currently cannot find
DMLC compile-time flags and forward them to the server.

### DML Compile Commands
This is a slightly more powerful variant of the above, as it also tracks
compile-time flags to DMLC.

It is a json file with the following format:
```
{
  <full path to device file>: {
    "includes": [<include folders as full paths>],
    "dmlc_flags": [<flags passed to dmlc invocation>]
  },
  ... <more device paths>
}
```

This will add the include folders specified when analysing files included,
directly or indirectly, from the specified device file. In the future the DLS
will be able to respect certain flags provided under the `dmlc_flags` fields as
well.

You can configure the DLS to use this file for locating import-paths by setting the
`cmakeCompileInfoPath` setting on the server configuration to the path of the
compile commands file.
