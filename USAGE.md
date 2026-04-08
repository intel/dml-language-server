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
structure. Thus, the meaning of `goto-definition`, `goto-declaration`,
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
`goto-declaration` and `goto-definition` on a composite object are equivalent.
They will find the locations of all object declarations that may be
merged with the one at the name.

`goto-implementations` on objects will find all `in each` declaration block that
applies to that object.

`goto-references` will go to any location where the symbol is referred to
directly.

### On Methods
`goto-declaration` on a method will find the most-overridden declaration
or definition of the method, this will usually be a 'default' or
abstract declaration.

`goto-definition` on a method reference will find all definitions of that method
that could be called from that reference, or the declaration of the method if it
has no non-abstract definitions. Note that this will NOT point to
method declarations that are entirely overridden. `goto-definition` on a method
name will go to the nearest definition of the method. For non-abstract methods
this is a no-op.

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
`goto-implementations`)

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

## SCIP Export

The DLS can export a [SCIP index](https://sourcegraph.com/docs/code-search/code-navigation/scip)
of analyzed DML devices. SCIP (Source Code Intelligence Protocol) is a
language-agnostic format for code intelligence data, used by tools such as
Sourcegraph for cross-repository navigation and code search.

### Invocation

SCIP export is available through the DFA (DML File Analyzer) binary via the
`--scip-output <path>` flag:
```
dfa --compile-info <compile_commands.json> --workspace <project root> --scip-output <scip_file_name> [list of devices to analyze, ]
```

It is worth noting that SCIP format specifies that symbols from documents that are not under the project root (which we define as the workspace) get slotted under external symbols with no occurances tracked.

### SCIP schema details
Here we list how we have mapped DML specifically to the SCIP format.

#### SCIP symbol kind mappings

DML symbol kinds are mapped to SCIP `SymbolInformation.Kind` as follows:

- `Constant` — Parameter, Constant, Loggroup
- `Variable` — Extern, Saved, Session, Local
- `Parameter` — MethodArg
- `Event` — Hook
- `Method` — Method
- `Class` — Template
- `TypeAlias` — Typedef
- `Object` — All composite objects (Device, Bank, Register, Field, Group, Port, Connect, Attribute, Event, Subdevice, Implement)
- `Interface` — Interface

Note: SCIP's `Object` kind is used for DML composite objects because they are
instantiated structural components in the device hierarchy, not types or
namespaces. `Event` is used for DML hooks because they represent named event
points that can be sent or listened to.

Since SCIP's `Kind` enum is too coarse to distinguish between the various DML
composite object kinds (e.g. `register` vs `bank` vs `attribute`), the
`SymbolInformation.documentation` field carries a short-form declaration
signature that disambiguates:

- **Composite objects:** the DML keyword for the object kind, e.g. `register`,
  `bank`, `attribute`, `group`, `field`, `device`, etc.
- **Methods:** the DML declaration modifiers, e.g. `method`,
  `independent method default`, `shared method throws`.
- **Other symbol kinds:** no documentation is emitted.

#### Symbol Naming Scheme

SCIP symbols follow the format:
`<scheme> ' ' <manager> ' ' <package> ' ' <version> ' ' <descriptors>`

For DML, the scheme is `dml`, the manager is `simics`, version is `.` (currently we cannot extract simics version here), and the
package is the device name. Descriptors are built from the fully qualified path
through the device hierarchy:

```
dml simics sample_device . sample_device.regs.r1.offset.
                                                       ^ term (parameter)
dml simics sample_device . sample_device.regs.r1.read().
                                                      ^ method
dml simics sample_device . bank#
                              ^ 'type' (template)
```

Descriptor suffixes follow the SCIP standard:
- `.` (term) — used for composite objects, parameters, and other named values
- `#` (type) — used only for templates
- `().` (method) — used for methods

#### Local Symbols

Method arguments and method-local variables use SCIP local symbols of the form
`local <name>_<id>`, where `<id>` is the internal symbol identifier. Local
symbols are scoped to a single document and are not navigable across files.

#### Occurrence Roles

DML definitions (including the primary symbol location) are emitted with the
SCIP `Definition` role. Declarations that also appear as definitions share
this role. Declarations that do _not_ define a value (e.g. abstract method
declarations, or `default` parameter declarations that are overridden) are
emitted with the `ForwardDefinition` role.

References (including template instantiation sites from `is` statements) are
emitted as plain references with no additional role flags. Access-kind
refinement (`ReadAccess` / `WriteAccess`) is not yet tracked.

#### Enclosing Ranges

For composite object definitions and method declarations, each `Definition`
or `ForwardDefinition` occurrence includes an `enclosing_range` that spans
the full AST node (e.g. the complete `register r1 is ... { ... }` block or
the full method body). This allows consumers to associate the definition site
with the extent of the construct it names.

#### Deduplication and Determinism

When multiple device analyses share source files (e.g. common library code),
the SCIP export deduplicates occurrences and symbol information so that each
(symbol, range, role) triple and each symbol entry appears at most once.
All output is sorted deterministically: documents by relative path,
occurrences by range, symbols by symbol string, and relationships by symbol.

#### Relationships

Composite objects that instantiate templates (via `is some_template`) emit
SCIP `Relationship` entries with `is_implementation = true` pointing to the
template symbol.

Methods that override a default or abstract method from a template also emit
`is_implementation` relationships pointing to the overridden method's symbol.
For example, if template `foo` defines `method a` as `default` and object `b`
implements `foo` with its own definition of `method a`, then `b.a` will carry
an `is_implementation` relationship to `foo.a`. This lets consumers identify
which method version is the active override and navigate the override chain.

#### File Symbols and Imports

Each source file involved in the analysis gets a dedicated SCIP symbol of kind
`File`. A `Definition` occurrence is emitted at line 0 of each file so that
navigation to the file symbol opens the file itself.

For each `import "..."` statement, an `Import` occurrence is emitted at the
import statement's span, referencing the imported file's symbol. Additionally,
the importing file's `SymbolInformation` carries an `is_reference = true`
`Relationship` entry for each imported file, enabling explicit file-level
dependency tracking without needing to scan occurrences.

File symbols use the format:
```
dml simics . . path/to/file_dml.
```
where path segments are separated by term descriptors (`.`).

## Device Object Hierarchy Export

The DLS can export a JSON representation of the device-semantic object
hierarchy for analyzed DML devices. This gives a structured view of the
instantiated device tree — the composite objects, parameters, and methods
that make up the device after all templates have been merged — rather than
the raw syntactic AST.

### Invocation

Object hierarchy export is available through the DFA binary via the
`--object-hierarchy <path>` flag:

```
dfa <dls_binary> --workspace <project_root> --object-hierarchy output.json <device files ...>
```

This can be combined with other flags such as `--compile-info`,
`--scip-output`, etc.

### Output Format

The output is a JSON file whose top-level keys are device names (one per
analysed device). Each device maps to a recursive object hierarchy:

```json
{
  "sample_device": {
    "scip_name": "dml simics . . src. sample.dml. sample_device.",
    "kind": "device",
    "parameters": {
      "register_size": {
        "scip_name": "dml simics . . src. sample.dml. sample_device. register_size.",
        "value_expression": "4",
        "type": "int"
      }
    },
    "methods": {
      "init": {
        "scip_name_of_method_used": "dml simics . . src. sample.dml. sample_device. init().",
        "arg_list": [],
        "return_types": [],
        "modifiers": ["default"]
      }
    },
    "objects": {
      "regs": {
        "scip_name": "dml simics . . src. sample.dml. regs.",
        "kind": "bank",
        "parameters": { ... },
        "methods": { ... },
        "objects": {
          "r0": {
            "scip_name": "...",
            "kind": "register",
            ...
          }
        }
      }
    }
  }
}
```

### Schema Reference

#### Top Level

A map from device short name (string) to a **HierarchyObject**.

#### HierarchyObject

| Field        | Type                                  | Description |
|--------------|---------------------------------------|-------------|
| `scip_name`  | string                                | Full SCIP symbol path for this object. |
| `kind`       | string                                | DML composite object kind (`device`, `bank`, `register`, `field`, `group`, `port`, `connect`, `attribute`, `event`, `subdevice`, `implement`, `interface`). |
| `parameters` | map\<string, HierarchyParameter\>     | Parameters declared on this object, keyed by short name. Omitted when empty. |
| `methods`    | map\<string, HierarchyMethod\>        | Methods declared on this object, keyed by short name. Omitted when empty. |
| `objects`    | map\<string, HierarchyObject\>        | Child composite objects, keyed by short name. Omitted when empty. |

#### HierarchyParameter

| Field              | Type            | Description |
|--------------------|-----------------|-------------|
| `scip_name`        | string          | Full SCIP symbol path for this parameter. |
| `value_expression` | string or null  | Source text of the parameter value expression (e.g. `0x100`, `"hello"`), or `"auto"` for auto-parameters. Null if abstract. |
| `type`             | string or null  | Source text of the declared type annotation (e.g. `uint64`). Null if untyped. |

#### HierarchyMethod

| Field                      | Type                       | Description |
|----------------------------|----------------------------|-------------|
| `scip_name_of_method_used` | string                     | Full SCIP symbol path of the concrete method definition used. |
| `arg_list`                 | array of HierarchyMethodArg | Method arguments in declaration order. |
| `return_types`             | array of string            | Source text of each return type. |
| `modifiers`                | array of string            | Applicable modifiers: `shared`, `inline`, `independent`, `default`, `throws`. |

#### HierarchyMethodArg

| Field  | Type   | Description |
|--------|--------|-------------|
| `name` | string | Argument name. |
| `type` | string | Source text of the argument type, or `"inline"` for untyped inline arguments. |

### Relationship to SCIP

The `scip_name` fields in the hierarchy correspond directly to the SCIP symbol
strings produced by the SCIP export (see previous section). This allows
consumers to cross-reference the hierarchy with the SCIP index — for example,
looking up the precise source location of a parameter via its SCIP symbol, or
navigating override chains for methods.

Auto-generated parameters that are synthesised during device analysis (such as
`_ident`, `indices`, `qname`, `parent`, `obj`) do not have a source-level
declaration of their own. Their `scip_name` is resolved by walking the
parameter's definition/declaration chain (used definitions → overridden
definitions → declarations) looking for the nearest declaration that has a SCIP
symbol — for example, the declaration in a built-in template. If no declaration
in the chain has a SCIP symbol, the short identity name is used (e.g.
`_ident`).
