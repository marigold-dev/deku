
### SYNOPSIS

**ligo compile storage** *SOURCE_FILE* *STORAGE_EXPRESSION* \[*OPTION*\]\...

### DESCRIPTION

This sub-command compiles an initial storage for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which originates a contract.

### ARGUMENTS

**SOURCE_FILE**

SOURCE_FILE is the path to the smart contract file.

**STORAGE_EXPRESSION**

The expression that will be compiled.

### OPTIONS

**-e --entry-point &lt;ENTRY_POINT&gt;**

The entry-point that will be compiled.

**-s --syntax &lt;SYNTAX&gt;**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).

**--infer**

Enable type inference

**-p --protocol &lt;PROTOCOL_VERSION&gt;**

Choose protocol's types/values pre-loaded into the LIGO environment  (edo). By default, the current protocol (edo) will be used

**--amount &lt;AMOUNT&gt;**

The tezos amount the Michelson interpreter will use for the transaction.

**--balance &lt;BALANCE&gt;**

The balance the Michelson interpreter will use for the contract balance.

**--sender &lt;SENDER&gt;**

The sender the Michelson interpreter transaction will use.

**--source &lt;SOURCE&gt;**

The source the Michelson interpreter transaction will use.

**--now &lt;NOW&gt;**

The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10

**--format &lt;DISPLAY-FORMAT&gt;**

The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.

**--michelson-format &lt;MICHELSON_FORMAT&gt;**

Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.

**-o --output-file &lt;OUTPUT_FILE&gt;**

If used, prints the output into the specified file instead of stdout

**--warn &lt;BOOL&gt;**

Indicates whether warning messages should be printed in stderr or not

**--werror &lt;BOOL&gt;**

Indicates whether warning messages should be treated as errors or not

