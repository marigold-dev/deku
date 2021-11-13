
### SYNOPSIS

**ligo print pretty-print** *SOURCE_FILE* \[*OPTION*\]\...

### DESCRIPTION

This sub-command pretty-prints a source file in LIGO. The width of the pretty-printed text is adjusted to the number of columns in the terminal (or 60 if it cannot be determined).

### ARGUMENTS

**SOURCE_FILE**

SOURCE_FILE is the path to the smart contract file.

### OPTIONS

**-s --syntax &lt;SYNTAX&gt;**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

