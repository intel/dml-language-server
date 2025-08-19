<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# Usage Instructions
This document describes general instructions and advice directed at
end-users of the DML language server. For advice or details on
client implementation see [clients.md](clients.md). This document
_only_ pertains to details that are client-agnostic. For client-specific
details consult the documentation of the client.

As this file is currently a work in progress, relevant details may be
missing or incomplete.

## In-Line Linting Configuration
It may be desireable to control linting on a per-file basis, rather than
relying on the linting configuration. This can be done with in-line
linting configuration inside comments.

The general syntax is:
`// dls-lint: <command>=<target>`
Note that only one-line comments are allowed, and only if no text is between
the comment start and 'dls-lint'.

Currently supported commands are:
* 'allow-file' Will not report the lint rule specified by \<target> for the
   entire file, regardless of where 'allow-file' is declared
* 'allow' Will not report the lint rule specified by \<target> for the next
   line without a leading comment, or for the current line if declared
   outside a leading comment.

Lint warnings will report which rule caused them in their message, which is the
same identifier used for \<target>.

For example
```
// dls-lint: allow-file=long_lines
method now_we_can_declare_this_method_with_a_really_really_really_really_long name() {

// dls-lint: allow=nsp_unary
// dls-lint: allow=indent_no_tabs
	param p = (1 ++ *
            4); // dls-lint: allow=indent_paren_expr
}
```
Will allow 'long_lines' globally, 'nsp_unary' and 'indent_no_tabs' on the
`param p = (1 ++ *` line, and 'indent_paren_expr' on the `'4);` line.
