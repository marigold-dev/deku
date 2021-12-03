
### SYNOPSIS

**ligo print mini-c** *SOURCE_FILE* \[*OPTION*\]\...

### DESCRIPTION

This sub-command prints the source file in the Mini-C stage. Internally, it uses the build system to type and compile the contract. Compilation is applied after combination in the AST typed stage.

### ARGUMENTS

**SOURCE_FILE**

SOURCE_FILE is the path to the smart contract file.

### OPTIONS

**-s --syntax &lt;SYNTAX&gt;**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).

**--infer**

Enable type inference

**-p --protocol &lt;PROTOCOL_VERSION&gt;**

Choose protocol's types/values pre-loaded into the LIGO environment  (edo). By default, the current protocol (edo) will be used

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

**--optimize &lt;ENTRY_POINT&gt;**

Apply Mini-C optimizations as if compiling for this entry_point

