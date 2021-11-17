
### SYNOPSIS

**ligo compile expression** *SYNTAX* *_EXPRESSION* \[*OPTION*\]\...

### DESCRIPTION

This sub-command compiles a LIGO expression to a Michelson value. It works by compiling the LIGO expression to a Michelson expression and then interpreting it using Michelson's interpreter.

### ARGUMENTS

**SYNTAX**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "reasonligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively).

**_EXPRESSION**

The expression that will be compiled.

### OPTIONS

**--infer**

Enable type inference

**-p --protocol &lt;PROTOCOL_VERSION&gt;**

Choose protocol's types/values pre-loaded into the LIGO environment  (edo). By default, the current protocol (edo) will be used

**--init-file &lt;INIT_FILE&gt;**

The path to the smart contract file to be used for context initialization.

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

**--without-run**

Disable running of compiled expression.

**--michelson-format &lt;MICHELSON_FORMAT&gt;**

Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.

**--warn &lt;BOOL&gt;**

Indicates whether warning messages should be printed in stderr or not

**--werror &lt;BOOL&gt;**

Indicates whether warning messages should be treated as errors or not

