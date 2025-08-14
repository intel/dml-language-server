# Supported Rules

Below are listed the currently supported rules for linting:

## Spacing
- **SpReserved**, `sp_reserved`: spaces around reserved words, such as `if`, `else`, `default`, `size`, `const` and `in`, except when a reserved word is used as an identifier (e.g., `local uint8 *data;`). Currently supported reserved words: `if`, `for` and `while`.
- **SpBinop**, `sp_binop`: spaces around binary operators except for derefencing operators (dot `a.b` and arrow `a->b` )
- **SpTernary**, `sp_ternary`: spaces around `?` and `:` in ternary conditional expressions
- **SpBraces**, `sp_brace`: spaces around braces (`{` and `}`)
- **SpPunct**, `sp_punct`: spaces after but not before colon, semicolon and comma
- **SpPtrDecl**, `sp_ptrdecl`: spaces between a type and the `*` marking a pointer
- **NspFunpar**, `nsp_funpar`: no spaces between a function/method name and its opening parenthesis
- **NspInparen**, `nsp_inparen`: no spaces immediately inside parentheses or brackets
- **NspUnary**, `nsp_unary`: no spaces between a unary operator and its operand
- **NspPtrDecl**, `nsp_ptrdecl`: no spaces after the `*` marking a pointer in a declaration
- **NspTrailing**, `nsp_trailing`: no spaces between the last token in a line and the corresponding newline `\n`

## Indentation
- **IN1**, `indent_size`: Lines are indented a fixed amount of spaces for each indentation level. Defaults to 4, can be set to a custom value by defining field "indentation_spaces" lint configuration [json file](../../example_files/example_lint_cfg.README)
- **IN2**, `indent_no_tabs`: Tab characters (ASCII 9) should never be used to indent lines
- **IN3**, `indent_code_block`: If the previous line contains an opening brace without corresponding closing brace, the current line should be indented one level more than the previous line
- **IN4**, `indent_closing_brace`: Closing braces at the beginning of a line should be aligned to the corresponding indentation level of the statement that started the code block. A closing brace should only ever appear on the same line as the opening brace, or first on a line
- **IN5**, `indent_paren_expr`: A continuation line that is broken inside a parenthesized expression is indented to line up inside the corresponding parenthesis on the previous line
```
    x = interrupt_disabled || is_intr(interrupt_device,
                                      irq_level);
```
- **IN7**: Any line not indented according to the other indentation rules is indented to same level as the preceding non-empty line (this rule is implicit in implementation of `indent_code_block`)
- **IN9**, `indent_switch_case`: Case labels should be indented at the same level as the switch keyword, statements should be indented one level deeper and not in the same line as the case label
```
    switch (p->what) {
    case Winning:
        b = 1000;
    case Losing:
        b = -1000;
        break;
    default:
        callback();
    }
```
- **IN10**, `indent_empty_loop`: When the body of a while or for loop is left empty, indent the semicolon to the appropriate statement level

```
    for (s = 0; (1 << s) < x; s++)
        ;
```

## Line Length
- **LL1**, `long_lines`: Lines should be kept shorter than 80 characters. This limit can be set to a custom value
- **LL5**, `method_output_break`: Break long method declarations with output parameters before the arrow.
```
    method inquiry_status(uint64 physical_address)
        -> (uint16 status) {
        ...
    }
```

##### Check [Issue #76 For remaining and planned checks](https://github.com/intel/dml-language-server/issues/76)
