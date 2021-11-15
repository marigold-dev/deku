
### SYNOPSIS

**ligo repl** *SYNTAX* \[*OPTION*\]\...

### ARGUMENTS

**SYNTAX**

The syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "reasonligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively).

### OPTIONS

**-p --protocol &lt;PROTOCOL_VERSION&gt;**

Choose protocol's types/values pre-loaded into the LIGO environment  (edo). By default, the current protocol (edo) will be used

**--infer**

Enable type inference

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

**--init-file &lt;INIT_FILE&gt;**

The path to the smart contract file to be used for context initialization.

