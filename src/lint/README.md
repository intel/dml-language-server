<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: Apache-2.0 and MIT
-->
# Lint module architecture

The linting jobs for a file are triggered after the abstract syntax tree (AST) representation of a source file has been created by the IsolatedAnalysisJob run for that file.

Once the AST exists, the linter job will receive the [IsolatedAnalysis](../analysis/symbols.rs) output and use the ast representation to perform checks on each of the tree's nodes.

For more details regarding this, follow logic for `lint_analyze()` to see how the lint process is spawned, and how it will finally trigger our main entry point at `LinterAnalysis::new()`.

## Main logic

The function `begin_style_check` is the main entry point of the linter job, called when `LinterAnalysis::new()` is invoked from a spawned linter job.

`begin_style_check()` will run a loop on all top level AstObjects in the tree. In turn, each AstObject will implement its own `style_check()` function and recursively call its implementation on each of the tree node's children. This way, the whole tree is covered and every location in the sources is reached.

A secondary loop is executed for checks that are decoupled from the regular DML syntax structure. These are more text based checks such as finding trailing whitespace and defining maximum line length. Such examples are run on a line per line basis, so the text itself is analyzed instead of the abstract representation.

After both of these loops, the function will return a vector of identified violations to the enabled rules:
 - `begin_style_check() -> Vec<LocalDMLError>`

These returned local errors will then be processed from `LocalDMLError` vector into a `DMLError` vector and passed to the language server to be notified as warnings to the client editor.

## Rules submodule

The rules submodule defines the style checks to be applied to source code, as detailed in the [DML Style guide](https://simics-download.pdx.intel.com/simics-6/docs/html/simics-dml-style/index.html). 
All spacing rules are defined in [spacing.rs](./rules/spacing.rs). We will take this file as an example.

Each rule is implemented as a struct, and a method `check()` is defined so each rule has an algorithm to evaluate the AST node from which it is triggered. Lets take `SpBraces` as example.
Also, for each rule an associated type (`SpBracesArgs`) details the required arguments the rule's `check()` implementation will require for evaluating the rule logic.
Associated functions are defined also for these Args types to extract the required information from each type of node. To expand on the example, `SpBracesArgs` implements functions to extract the information required for the `SpBraces` rule from either the `CompoundContent` tree node object, or from the `ObjectStatementsContent` tree node object type.

#### Indentation enforcement

The indentation rules (found in [indentation.rs](./rules/indentation.rs)) are implemented by tracking "indentation levels" for each code block and its corresponding expected depth is calculated depending on how deep each statement is nested.
The indentation size is fixed, based on the `indentation_spaces` parameter (which can be configured in rule IN1 using the lint config.json file). This parameter is used to multiply the indentation level by the amount of configured spaces per indent.

The indentation level is tracked by the parameter `depth`, which is passed down in the `style_check()` logic to each node in the tree in the `AuxParams` struct.
This depth increases depending on each node type. A function `should_increment_depth() -> bool` is part of the `TreeElement` trait which defaults to false. Each TreeElement implementation for a particular node content type can define its own procedure to increment depth if required.

### Configurable rules

Each rule should be configurable to be either enabled/disabled, or have any extra parameters (like line length threshold for example) as required.

The client extension should specify a declarative file with this configuration. This file is used to define `LintCfg` struct which is passed down to each linter job. The linter job can then setup the rule settings accordingly.
See [example_lint_cfg.json](../../example_files/example_lint_cfg.json) and its corresponding README for details on how to use this file.
Users can decide to define the config file in order to turn off any rules or set parameters avaiable for customizing their behavior.
Currently, the lint module will be enabled by default on the DLS. The `simics-modeling.dls.lintingEnabled` setting in the workspace configuration can be set to disable the style checks.

An instance of each defined rule struct type is initialized, and their configurations are set as fields within each instance. This is done depending on the passed configuration from the server at the moment the linting job is spawned.

### How to implement more rules

Adding more rules to the module should be done by following the same procedure done for existing rules.

The rules submodule has categories for different type of style rules on separate files (spacing, indentation, etc). First of all, start by creating a test for a new rule:

1. Define a code snippet that displays violations of the rule that needs to be implemented
2. Assert the snippet in the test module for the corresponding rule category.
3. Execute `cargo test lint` to make sure the created test runs and fails.

Now that a test for this rule is defined, proceed to create the logic required to make this test succeed:

1. Define the rule struct
2. Create the rule's Args struct
3. Define `check()` logic for each rule, based on common Args struct
4. Define functions to extract the Args on each corresponding type of tree node
5. Make the call to the rule instance's `check()` function on all corresponding TreeElement variants.
