
### SYNOPSIS

**ligo transpile contract** *SOURCE_FILE* *SYNTAX* \[*OPTION*\]\...

### DESCRIPTION

This sub-command transpiles a source file to another syntax. It does not use the build system, but the source file is preprocessed. Comments are currently not transpiled. Please use at your own risk.

### ARGUMENTS

**SOURCE_FILE**

SOURCE_FILE is the path to the smart contract file.

**SYNTAX**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "reasonligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively).

### OPTIONS

**-s --syntax &lt;SYNTAX&gt;**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).

**-d --dialect &lt;PASCALIGO_DIALECT&gt;**

The pascaligo dialect that will be used. Currently supported dialects are "terse" and "verbose". By default the dialect is "terse".

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

**-o --output-file &lt;OUTPUT_FILE&gt;**

If used, prints the output into the specified file instead of stdout

