
### SYNOPSIS

**ligo print preprocessed** *SOURCE_FILE* \[*OPTION*\]\...

### DESCRIPTION

This sub-command runs the pre-processor on a LIGO source file and outputs the result. The directive `#include` directly inlines the included file and therefore its content appears in the output. In contrast, the directive `#import` includes the file as a module and therefore the content of the imported file is not printed by this sub-command.

### ARGUMENTS

**SOURCE_FILE**

SOURCE_FILE is the path to the smart contract file.

### OPTIONS

**-s --syntax &lt;SYNTAX&gt;**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

