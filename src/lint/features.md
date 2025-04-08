# Supported Rules

Below are listed the currently supported rules for linting:

## Spacing
- **SP.braces**: spaces around braces (`{` and `}`)
- **SP.punct**: spaces after but not before colon, semicolon and comma
- **NSP.funpar**: no spaces between a function/method name and its opening parenthesis
- **NSP.inparen**: no spaces immediately inside parentheses or brackets
- **NSP.unary**: no spaces between a unary operator and its operand
- **NSP.TrailingWhitespace**: no spaces between the last token in a line and the corresponding newline `\n`

## Indentation
- **IN1**: Lines are indented a fixed amount of spaces for each indentation level. Defaults to 4, can be set to a custom value
- **IN2**: Tab characters (ASCII 9) should never be used to indent lines
- **IN3**: If the previous line contains an opening brace without corresponding closing brace, the current line should be indented one level more than the previous line
- **IN4**: Closing braces at the beginning of a line should be aligned to the corresponding indentation level of the statement that started the code block. A closing brace should only ever appear on the same line as the opening brace, or first on a line
- **IN5**: A continuation line that is broken inside a parenthesized expression is indented to line up inside the corresponding parenthesis on the previous line
```
    x = interrupt_disabled || is_intr(interrupt_device,
                                      irq_level);
```
- **IN7**: Any line not indented according to the other indentation rules is indented to same level as the preceding non-empty line (this rule is implicit in enforcement of **IN3**)
- **IN9**: Case labels should be indented at the same level as the switch keyword, statements should be indented one level deeper and not in the same line as the case label
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
- **IN10**: When the body of a while or for loop is left empty, indent the semicolon to the appropriate statement level

```
    for (s = 0; (1 << s) < x; s++)
        ;
```

## Line Length
- **LL1**: Lines should be kept shorter than 80 characters. This limit can be set to a custom value

##### Check [Issue #76 For remaining and planned checks](https://github.com/intel/dml-language-server/issues/76)
