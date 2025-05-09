<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# Implementing clients

A short guide to implementing DLS support in your favourite editor.

Typically you will need to implement an extension or plugin in whatever format
and language your editor requires.

If your editor has a LSP-supporting library, then this will be much more straightforward. Implementing a client without the support of a library is outside the scope of this document.

## Preliminaries

Check with the maintainers of this repo to see if support already exists or is in development. If you start developing support for some editor, it is good to inform us so that we can track this.

If you encounter bugs or unexpected behaviour while implementing a client, please file an issue or create a PR.

## Where there is existing LSP support

If your editor has LSP support, then getting up and running is relatively easy. You
need a way to run the DLS and point the editor's LSP client at it. Hopefully
that is only a few lines of code. The next step is to ensure that the DLS gets
re-started after a crash - the LSP client may or may not do this automatically
(VSCode will do this five times before stopping).

Once you have this basic support in place, the hard work begins:

* Implement [extensions to the protocol](clients.md#extensions-to-the-language-server-protocol)
* Client-side configuration.
  - You'll need to send the `workspace/didChangeConfiguration` notification when
    configuration changes.
  - For the config options, see [config.rs](./src/config.rs#L99-L111)
* Check for and install the DLS
  - Download the latest [binary](https://github.com/intel/dml-language-server/actions/workflows/rust.yml).
    Currently, official releases are not being made from a public-facing repository. So you should on occassion
    update to the latest binary from a green nightly build.
* Client-side features
  - e.g., code snippets, build tasks, syntax highlighting
* Testing
* Ensure alignment with existing DML semantics
  - e.g., syntax highlighting
* 'Marketing'
  - because we want people to actually use the extension
  - documentation - users need to know how to install and use the extension
  - keep us informed about status so we can advertise it appropriately
  - keep the DLS website updated
  - submit the extension to the editor package manager or marketplace

## Where there is no LSP support

If your editor has no existing LSP support, you'll need to do all the above plus
implement (parts of) the LSP. This is a fair amount of work, but probably not as
bad as it sounds. The LSP is a fairly simple JSON over stdio protocol. The
interesting bit is tying the client end of the protocol to functionality in your
editor.

### Required message support

The DLS currently requires support for the following messages. Note that we
often don't use anywhere near all the options, so even with this subset, you
don't need to implement everything.

Notifications:

* `exit`
* `initialized`
* `textDocument/didOpen`
* `textDocument/didChange`
* `textDocument/didSave`
* `workspace/didChangeConfiguration`
* `workspace/didChangeWatchedFiles`
* `cancel`

Requests:

* `shutdown`
* `initialize`
* `textDocument/definition`
* `textDocument/declaration`
* `textDocument/implementation`
* `textDocument/references`
* `textDocument/documentSymbol`
* `textDocument/workspaceSymbol`
* `workspace/symbol`

From Server to client:

* `client/registerCapability`
* `client/unregisterCapability`
* `textDocument/publishDiagnostics`

### Extensions to the Language Server Protocol

The DLS also uses some custom messages to provide additional info to clients or
to allow them to exert special control over the language server.

Notifications:
* '$/changeActiveContexts'
  The client sends this notification to the server to control for which paths
  semantic errors and information is used. Usually you would precede this with
  a '\$getKnownContexts' request to find which paths are available, and for which
  contexts are already active. Details on semantic analysis is provided
  [here](CONTRIBUTING.md#analysis). Parameters are:
  ```typescript
  interface ContextDefinitionKindParam {
      device: DocumentUri;
  }

  interface ChangeActiveContextsParams {
      activeContexts: ContextDefinitionKindParam[];
      // Specifies tge document for which 'activeContexts' should be the
      // only active contexts. If not set, 'activeContexts' should be the
      // only active contexts for all files
      uri?: DocumentUri;
  }
  ```

Requests:
* '$/getKnownContexts'
  The client sends this request to the server together with a list of
  paths, to obtain the paths with device declarations that import any file on
  the original list, directly or indirectly, together with information
  about which of those paths are used for semantic errors or information.
  Parameters are:
  ```typescript
  interface GetKnownContextsParams {
      paths?: DocumentUri[];
  }
  ```
  'paths' specifies the paths for which the known contexts should be reported,
  will report all contexts if paths is empty or not set.
  The response is
  ```typescript
  interface ContextDefinitionKindParam {
      // The URI of the known context
      device: DocumentUri;
  }
  interface ContextDefinitionParam {
      kind: ContextDefinitionKindParam;
      // 'true' if the context is active on the server
      active: boolean;
      // 'true' if the server has an available device analysis
      // for this context
      ready: boolean;
  }
  export type GetKnownContextsResponse = ContextDefinitionParam[];
  ```

Note that since older version of the language server did not know about these
notifications/requests, you can check if the server you are working towards
supports these by checking if the 'experimental' field of the server
capabilities contains at least this field:
```
interface ExperimentalServerCapabilities {
    contextControl = true;
}
```

From Server to Client, these notifcations have a specified data value:
* `window/progress`
  Uses the 'WorkDoneProgressBegin', 'WorkDoneProgressReport', and
  'WorkDoneProgressEnd' schema. Notably it will signal a 'WorkDoneProgressBegin'
  with the title "Analysing" when it starts on any long-term work, and a
  'WorkDoneProgressEnd' with the corresponding id when it finishes all such
  work.

## Resources

* [LSP spec](https://microsoft.github.io/language-server-protocol/specification)
* [CONTRIBUTING.md](CONTRIBUTING.md) - overview of the DLS and how to build, test, etc.

## Getting help

We're happy to help however we can. The best way to get help is either to
leave a comment on an issue in this repo, or to send me (jonatan.waern@intel.com) an email.
