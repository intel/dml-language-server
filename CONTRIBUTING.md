# Contributing

### License

DML Language Server is dual-licensed under the terms in [APACHE 2.0](./LICENSE-APACHE) and [MIT](./LICENSE-MIT). By contributing to the project, you agree to the license and copyright terms therein and release your contribution under these terms.

### Sign your work

Please use the sign-off line at the end of the patch. Your signature certifies that you wrote the patch or otherwise have the right to pass it on as an open-source patch. The rules are pretty simple: if you can certify
the below (from [developercertificate.org](http://developercertificate.org/)):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
660 York Street, Suite 102,
San Francisco, CA 94110 USA

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.

Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

Then you just add a line to every git commit message:

    Signed-off-by: Joe Smith <joe.smith@email.com>

Use your real name (sorry, no pseudonyms or anonymous contributions.)

If you set your `user.name` and `user.email` git configs, you can sign your
commit automatically with `git commit -s`.

## How-To

This section provides information for developers who want to contribute to the
DLS or run it in a heavily customised configuration.

Testing, reporting issues, writing documentation, writing tests,
writing code, and implementing clients are all extremely valuable.

To get help with this, either leave an issue on the repo or contact the primary
developer directly.

If you want to implement DLS support in an editor, see [clients.md](clients.md).

## Building

```
git clone https://github.com/intel/dml-language-server.git
cd dml-language-server
cargo build --release
```

## Running and testing

There are three main ways to run the DLS, you can run it directly with:

```
cargo run --bin dls
```
Which starts up the DLS in a server-mode, so its of narrow applicability unless
you are planning on directly sending LSP communication into it. By passing the
'--cli' option you start up the server in command-line mode, see [CLI](#cli).

You can run the Direct-File-Analysis (DFA) binary with:
```
cargo run --bin dfa [options ...] <dls-binary> [files ...]
```
This will analyze the specified 'files' using the 'dls-binary' server binary
as-if they had been opened in a language client, this is usefull to quickly
test and debug initial file analysis and error reporting without advanced
client-interaction.

Most commonly, you'll use an IDE plugin to invoke the DLS binary for you
(see [README.md](README.md) for details).

Test the crate using `cargo test`.

Testing is unfortunately minimal. There is support for some regression tests,
but not many actual tests exists yet. There is significant work to do
before we have a comprehensive testing story.

### <a id="cli"></a>CLI

You can run DLS in the command line mode which is useful for debugging and
testing, especially to narrow down a bug to either the DLS or a client.

You need to run it with the
`--cli` flag, e.g., `cargo run --bin dls -- --cli`. This should initialize the
DLS and then give you a `>` prompt.
Look for the final message that will signal the end of the
initialization phase which will look something like:
```
{"jsonrpc":"2.0","method":"window/showMessage","params":{"message": "DML Language Server Started", "type": 3}}
```

Type `help` (or just `h`) to see the [commands available][CLI_COMMANDS]. Note
that the [positions][LSP_POSITION] in the requests and the responses are
_zero-based_ (contrary to what you'll normally see in the IDE line numbers).

[LSP_POSITION]: https://github.com/Microsoft/language-server-protocol/blob/gh-pages/specification.md#position

[CLI_COMMANDS]: dls/src/cmd.rs#L508-L542

## Implementation overview

The idea behind the DLS is to grant fast and mostly-accurate feedback for DML
development, of either specific devices or DML common-code. Due to complexities
in the DML language, and the high level of customization available when running
DMLC, this information will not be perfectly accurate or holistic at all times.
However the goal is to give a best-effort analysis that minimizes noisy
incorrect feedback and maximizes the amount of valuable info a developer can
extract, as well as providing basic standard-issue IDE functionality.

### Analysis

The DLS tracks changes to files, and keeps the changed file in memory (i.e., the
DLS does not need the IDE to save a file before providing data). These changed
files are tracked by the 'Virtual File System'.

Analysis is divided into two phases.
- The 'isolated' analysis analyses each
DML file individually, and does parsing and some post-parse processing. This
analysis perform syntactic analysis and is enough to report basic syntax errors,
- The 'device' analysis analyses from the perspective of a particular DML file with
a device declaration, pulling in information from the various isolated
analysises. This analysis performs semantic analysis, and is what
powers reference-matching, object resolution, etc.

Isolated analysis is started as soon as the user opens or changes a file
(although this can be configured in the settings to be only on save).
In addition the DLS will use information about import paths and workspaces to
try to resolve file imports, and recursively analyse any file imported from
an opened one. See the section on [include paths](#include-paths) for more
information on them. Device analysis is started as soon as it is determined that
it could be, or needs to be re-done. Commonly this is when an isolated analysis
involved in the import tree of a device-file is updated, if all files that the
device depends on have had an isolated analysis done. There are corner-case
exceptions to this (allowing for a partially correct device analysis when an
imported file is missing) but for simplicity that is not described here.

### Communicating with IDEs

The DLS communicates with IDEs via the [Language Server protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md).

The LS protocol uses JSON sent over stdin/stdout. The JSON is rather dynamic -
we can't make structs to easily map to many of the protocol objects. The client
sends commands and notifications to the DLS. Commands must get a reply,
notifications do not. Usually the structure of the reply is dictated by the
protocol spec. The DLS can also send notifications to the client. So for a long
running task (such as a build), the DLS will reply quickly to acknowledge the
request, then send a message later with the result of the task.

Associating requests with replies is done using an id which must be handled by
the DLS.

### <a id="include-paths"></a>Include Paths
In order to support fairly generic and complicating import configurations, the
DLS uses a context-aware methodology in order to resolve import paths.

Sidenote: be aware that due to past DMLC 1.2/1.4 interoperability functionality,
the DLS will correctly search the "./" and "./1.4/" of every path it searches.

When importing a file "A.dml" from some file "B", the DLS will search, in no
particular order:
- The folder of B
- The root folder of the workspace
- All other workspace roots
- Every include path specified by the [DML Compile commands](README.md#dml-compile-commands) under the related module

### <a id="device contexts"></a>Device Contexts
Semantic analysis in the DLS is based around 'device contexts', essentially
each device context contributes an analysis of files through the lens of a
particular device file. Thus, a 'device context' is actually just the path
to a particular device file. A device context is considered 'active' if
the results of it are being used when querying the DLS for diagnostics or
other information(note that the analysis is performed regardless).

Whether a new device context is activated when a new file containing a
device declaration is opened is decided by the 'device context mode'
specified in the server configuration. The available modes are:
- *Always*: New device contexts are always activated.
- *Never*: New device contexts are never activated (by the DLS, they can stil be
  activated by the client, see below).
- *AnyNew*: New device contexts that import, directly or indirectly, a file which
  is not imported, directly or indirectly, by an active device context are
  activated.
- *SameModule*: New device contexts that are in a subfolder of the folder of an
  already active device context, or where an active device context is in a
  subfolder of the new device context, are activated.
- *First*: New device contexts where no directly or indirectly imported file
  is imported, directly or indirectly, by an active device context are
  activated.

In addition, the client can control the active device context by sending a
notification to change it (see [extensions to the language server protocol](clients.md#extensions-to-the-language-server-protocol)).
If clients allow users to set the device context mode to anything except 'always',
it is strongly recommended to also implement the direct control of device contexts.
