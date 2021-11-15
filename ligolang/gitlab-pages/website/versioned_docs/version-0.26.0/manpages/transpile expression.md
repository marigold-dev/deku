
### SYNOPSIS

**ligo transpile expression** *SYNTAX* *_EXPRESSION* *SYNTAX* \[*OPTION*\]\...

### DESCRIPTION

This sub-command transpiles a LIGO expression to another syntax. Comments are currently not transpiled. Please use at your own risk.

### ARGUMENTS

**SYNTAX**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "reasonligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively).

**_EXPRESSION**

The expression that will be compiled.

**SYNTAX**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "reasonligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively).

### OPTIONS

**-d --dialect &lt;PASCALIGO_DIALECT&gt;**

The pascaligo dialect that will be used. Currently supported dialects are "terse" and "verbose". By default the dialect is "terse".

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

