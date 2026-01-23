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

## Clarification of semantics of Symbol Lookups
DML is at its core a _declarative_ language centered around a Device,
and declarations are merged together according to the hierarchical
structure. Thus, the meaning of `goto-definiton`, `goto-declaration`,
`goto-implementations`, and `goto-references` may not be similar to how
they behave in other languages.
Here is a clarification of what each operation means for each object
kind in DML. It is worth noting that unless otherwise specified, a goto-
operation on a reference is equivalent to the same operation on each symbol
that it could refer to.

*NOTE:* Due to limitations in the DLS, a matching from references to
declarations is not always possible. `goto-references` may provide incomplete
info and `goto-definition` and similar on a reference may fail in some cases.
See issues [#13](https://github.com/intel/dml-language-server/issues/13),
[#23](https://github.com/intel/dml-language-server/issues/23),
[#26](https://github.com/intel/dml-language-server/issues/26),
[#30](https://github.com/intel/dml-language-server/issues/30), and
[#65](https://github.com/intel/dml-language-server/issues/65).

### On Composite Objects (banks, registers, implements, etc.)
`goto-declaration` and `goto-definition` on an composite object are equivalent.
They will pointreturn the locations of all object declarations that may be
merged with the one at the name.

`goto-implementations` on objects is currently unused.

`goto-references` will go to any location where the object is referred to
directly.

### On Methods
`goto-declaration` on a method will find the most-overridden declaration
for this method, this will usually be a 'default' or abstract declaration.

`goto-definition` on a method reference will find all defintions of that method
that could be called from that reference. Note that this will NOT point to
method declarations that are entirely overridden. `goto-definition` on a method
name is a no-op and will just point back to that same method.

`goto-implementations` on a method will find all method declarations that
could override it.

`goto-references` will go to any location where the method is referred to
directly (including call sites).

### On Parameters
`goto-declaration` on a parameter will find the most-overridden declaration of
this parameter, usually a 'default' or abstract declaration.

`goto-definition` on a parameter will find all definitions that are used within
a method.

`goto-implementations` on a parameter is currently unused.

`goto-references` will go to any location where the parameter is referred to
directly.

### On Templates
`goto-declaration` and `goto-definition` are equivalent, and will both give the
template declaration site.

`goto-implementations` will give each location where the template is directly
instantiated.

`goto-references` will go to any location where the template is referred to,
including direct instantiation sites (so this is a super-set of
goto-implementations`)

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
