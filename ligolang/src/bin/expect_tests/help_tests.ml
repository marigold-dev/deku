open Cli_expect

let%expect_test _ =
  (* TODO good? *)
  run_ligo_good [] ;
  [%expect {|
    Usageligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)
    Global options (must come before the command) |} ] ;

  run_ligo_good [ "--help" ] ;
  [%expect {|
    Usageligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)
    Global options (must come before the command) |} ] ;
 
  run_ligo_good [ "compile" ; "contract" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        compile contract SOURCE_FILE [-e --entry-point <ENTRY_POINT>]
        [-v --views <ON_CHAIN_VIEWS>]
        [-s --syntax <SYNTAX>]
        [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--format <DISPLAY-FORMAT>]
        [--disable-michelson-typechecking]
        [--michelson-format <MICHELSON_FORMAT>]
        [-o --output-file <OUTPUT_FILE>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)


    Commands for compiling from Ligo to Michelson
    compile contract SOURCE_FILE [-e --entry-point <ENTRY_POINT>]
    [-v --views <ON_CHAIN_VIEWS>]
    [-s --syntax <SYNTAX>]
    [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--format <DISPLAY-FORMAT>]
    [--disable-michelson-typechecking]
    [--michelson-format <MICHELSON_FORMAT>]
    [-o --output-file <OUTPUT_FILE>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command compiles a contract to Michelson code. It expects a source file and an entrypoint function that has the type of a contract: "parameter * storage -> operations list * storage".
    SOURCE_FILE: SOURCE_FILE is the path to the smart contract file.
    -e --entry-point <ENTRY_POINT>: the entry-point that will be compiled.
      Defaults to `main`.
    -v --views <ON_CHAIN_VIEWS>: A list of declaration name that will be compiled as on-chain views, separated by ','
      Defaults to ``.
    -s --syntax <SYNTAX>: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).
      Defaults to `auto`.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --disable-michelson-typechecking: Disable Michelson typecking, this might produce ill-typed Michelson code.
    --michelson-format <MICHELSON_FORMAT>: Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.
      Defaults to `text`.
    -o --output-file <OUTPUT_FILE>: If used, prints the output into the specified file instead of stdout
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;

  run_ligo_good [ "compile" ; "parameter" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        compile parameter SOURCE_FILE PARAMETER_EXPRESSION [-e --entry-point <ENTRY_POINT>]
        [-s --syntax <SYNTAX>]
        [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--amount <AMOUNT>]
        [--balance <BALANCE>]
        [--sender <SENDER>]
        [--source <SOURCE>]
        [--now <NOW>]
        [--format <DISPLAY-FORMAT>]
        [--michelson-format <MICHELSON_FORMAT>]
        [-o --output-file <OUTPUT_FILE>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)


    Commands for compiling from Ligo to Michelson
    compile parameter SOURCE_FILE PARAMETER_EXPRESSION [-e --entry-point <ENTRY_POINT>]
    [-s --syntax <SYNTAX>]
    [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--amount <AMOUNT>]
    [--balance <BALANCE>]
    [--sender <SENDER>]
    [--source <SOURCE>]
    [--now <NOW>]
    [--format <DISPLAY-FORMAT>]
    [--michelson-format <MICHELSON_FORMAT>]
    [-o --output-file <OUTPUT_FILE>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command compiles a parameter for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which calls a contract.
    SOURCE_FILE: SOURCE_FILE is the path to the smart contract file.
    PARAMETER_EXPRESSION: the expression that will be compiled.
    -e --entry-point <ENTRY_POINT>: the entry-point that will be compiled.
      Defaults to `main`.
    -s --syntax <SYNTAX>: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).
      Defaults to `auto`.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --amount <AMOUNT>: The tezos amount the Michelson interpreter will use for the transaction.
      Defaults to `0`.
    --balance <BALANCE>: The balance the Michelson interpreter will use for the contract balance.
      Defaults to `0`.
    --sender <SENDER>: The sender the Michelson interpreter transaction will use.
    --source <SOURCE>: The source the Michelson interpreter transaction will use.
    --now <NOW>: The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --michelson-format <MICHELSON_FORMAT>: Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.
      Defaults to `text`.
    -o --output-file <OUTPUT_FILE>: If used, prints the output into the specified file instead of stdout
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;

  run_ligo_good [ "compile"; "storage" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        compile storage SOURCE_FILE STORAGE_EXPRESSION [-e --entry-point <ENTRY_POINT>]
        [-s --syntax <SYNTAX>]
        [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--amount <AMOUNT>]
        [--balance <BALANCE>]
        [--sender <SENDER>]
        [--source <SOURCE>]
        [--now <NOW>]
        [--format <DISPLAY-FORMAT>]
        [--michelson-format <MICHELSON_FORMAT>]
        [-o --output-file <OUTPUT_FILE>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)


    Commands for compiling from Ligo to Michelson
    compile storage SOURCE_FILE STORAGE_EXPRESSION [-e --entry-point <ENTRY_POINT>]
    [-s --syntax <SYNTAX>]
    [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--amount <AMOUNT>]
    [--balance <BALANCE>]
    [--sender <SENDER>]
    [--source <SOURCE>]
    [--now <NOW>]
    [--format <DISPLAY-FORMAT>]
    [--michelson-format <MICHELSON_FORMAT>]
    [-o --output-file <OUTPUT_FILE>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command compiles an initial storage for a given contract to a Michelson expression. The resulting Michelson expression can be passed as an argument in a transaction which originates a contract.
    SOURCE_FILE: SOURCE_FILE is the path to the smart contract file.
    STORAGE_EXPRESSION: the expression that will be compiled.
    -e --entry-point <ENTRY_POINT>: the entry-point that will be compiled.
      Defaults to `main`.
    -s --syntax <SYNTAX>: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).
      Defaults to `auto`.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --amount <AMOUNT>: The tezos amount the Michelson interpreter will use for the transaction.
      Defaults to `0`.
    --balance <BALANCE>: The balance the Michelson interpreter will use for the contract balance.
      Defaults to `0`.
    --sender <SENDER>: The sender the Michelson interpreter transaction will use.
    --source <SOURCE>: The source the Michelson interpreter transaction will use.
    --now <NOW>: The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --michelson-format <MICHELSON_FORMAT>: Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.
      Defaults to `text`.
    -o --output-file <OUTPUT_FILE>: If used, prints the output into the specified file instead of stdout
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;

  run_ligo_good [ "run" ; "dry-run" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        run dry-run SOURCE_FILE PARAMETER_EXPRESSION STORAGE_EXPRESSION [-e --entry-point <ENTRY_POINT>]
        [--amount <AMOUNT>]
        [--balance <BALANCE>]
        [--sender <SENDER>]
        [--source <SOURCE>]
        [--now <NOW>]
        [-s --syntax <SYNTAX>]
        [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--format <DISPLAY-FORMAT>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)


    Commands for executing Ligo code
    run dry-run SOURCE_FILE PARAMETER_EXPRESSION STORAGE_EXPRESSION [-e --entry-point <ENTRY_POINT>]
    [--amount <AMOUNT>]
    [--balance <BALANCE>]
    [--sender <SENDER>]
    [--source <SOURCE>]
    [--now <NOW>]
    [-s --syntax <SYNTAX>]
    [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--format <DISPLAY-FORMAT>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command runs a LIGO contract on a given storage and parameter. The context is initialized from a source file where the contract is implemented. The interpretation is done using Michelson's interpreter.
    SOURCE_FILE: SOURCE_FILE is the path to the smart contract file.
    PARAMETER_EXPRESSION: the expression that will be compiled.
    STORAGE_EXPRESSION: the expression that will be compiled.
    -e --entry-point <ENTRY_POINT>: the entry-point that will be compiled.
      Defaults to `main`.
    --amount <AMOUNT>: The tezos amount the Michelson interpreter will use for the transaction.
      Defaults to `0`.
    --balance <BALANCE>: The balance the Michelson interpreter will use for the contract balance.
      Defaults to `0`.
    --sender <SENDER>: The sender the Michelson interpreter transaction will use.
    --source <SOURCE>: The source the Michelson interpreter transaction will use.
    --now <NOW>: The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')
    -s --syntax <SYNTAX>: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).
      Defaults to `auto`.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;

  run_ligo_good [ "run" ; "evaluate-call" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        run evaluate-call SOURCE_FILE PARAMETER_EXPRESSION [-e --entry-point <ENTRY_POINT>]
        [--amount <AMOUNT>]
        [--balance <BALANCE>]
        [--sender <SENDER>]
        [--source <SOURCE>]
        [--now <NOW>]
        [-s --syntax <SYNTAX>]
        [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--format <DISPLAY-FORMAT>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)
    

    Commands for executing Ligo code
    run evaluate-call SOURCE_FILE PARAMETER_EXPRESSION [-e --entry-point <ENTRY_POINT>]
    [--amount <AMOUNT>]
    [--balance <BALANCE>]
    [--sender <SENDER>]
    [--source <SOURCE>]
    [--now <NOW>]
    [-s --syntax <SYNTAX>]
    [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--format <DISPLAY-FORMAT>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command runs a LIGO function on a given argument. The context is initialized from a source file where the function is implemented. The interpretation is done using Michelson's interpreter.
    SOURCE_FILE: SOURCE_FILE is the path to the smart contract file.
    PARAMETER_EXPRESSION: the expression that will be compiled.
    -e --entry-point <ENTRY_POINT>: the entry-point that will be compiled.
      Defaults to `main`.
    --amount <AMOUNT>: The tezos amount the Michelson interpreter will use for the transaction.
      Defaults to `0`.
    --balance <BALANCE>: The balance the Michelson interpreter will use for the contract balance.
      Defaults to `0`.
    --sender <SENDER>: The sender the Michelson interpreter transaction will use.
    --source <SOURCE>: The source the Michelson interpreter transaction will use.
    --now <NOW>: The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')
    -s --syntax <SYNTAX>: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).
      Defaults to `auto`.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;

  run_ligo_good [ "run" ; "evaluate-expr" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        run evaluate-expr SOURCE_FILE [-e --entry-point <ENTRY_POINT>]
        [--amount <AMOUNT>]
        [--balance <BALANCE>]
        [--sender <SENDER>]
        [--source <SOURCE>]
        [--now <NOW>]
        [-s --syntax <SYNTAX>]
        [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--format <DISPLAY-FORMAT>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)


    Commands for executing Ligo code
    run evaluate-expr SOURCE_FILE [-e --entry-point <ENTRY_POINT>]
    [--amount <AMOUNT>]
    [--balance <BALANCE>]
    [--sender <SENDER>]
    [--source <SOURCE>]
    [--now <NOW>]
    [-s --syntax <SYNTAX>]
    [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--format <DISPLAY-FORMAT>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command evaluates a LIGO definition. The context is initialized from a source file where the definition is written. The interpretation is done using a Michelson interpreter.
    SOURCE_FILE: SOURCE_FILE is the path to the smart contract file.
    -e --entry-point <ENTRY_POINT>: the entry-point that will be compiled.
      Defaults to `main`.
    --amount <AMOUNT>: The tezos amount the Michelson interpreter will use for the transaction.
      Defaults to `0`.
    --balance <BALANCE>: The balance the Michelson interpreter will use for the contract balance.
      Defaults to `0`.
    --sender <SENDER>: The sender the Michelson interpreter transaction will use.
    --source <SOURCE>: The source the Michelson interpreter transaction will use.
    --now <NOW>: The NOW value the Michelson interpreter will use (e.g. '2000-01-01T10:10:10Z')
    -s --syntax <SYNTAX>: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo", "reasonligo" and "jsligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, and .jsligo respectively).
      Defaults to `auto`.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;

  run_ligo_good [ "compile" ; "expression" ; "--help" ] ;
  [%expect {|
    Error
      Unterminated command, here are possible completions.
        compile expression SYNTAX _EXPRESSION [--infer]
        [-p --protocol <PROTOCOL_VERSION>]
        [--init-file <INIT_FILE>]
        [--format <DISPLAY-FORMAT>]
        [--without-run]
        [--michelson-format <MICHELSON_FORMAT>]
        [--warn <BOOL>]
        [--werror <BOOL>]

    Usage
    ligo [global options] command [command options]
    ligo --help (for global options)
    ligo [global options] command --help (for command options)
    ligo --version (for version information)

    To browse the documentation
    ligo [global options] man (for a list of commands)
    ligo [global options] man -v 3 (for the full manual)

    Global options (must come before the command)


    Commands for compiling from Ligo to Michelson
    compile expression SYNTAX _EXPRESSION [--infer]
    [-p --protocol <PROTOCOL_VERSION>]
    [--init-file <INIT_FILE>]
    [--format <DISPLAY-FORMAT>]
    [--without-run]
    [--michelson-format <MICHELSON_FORMAT>]
    [--warn <BOOL>]
    [--werror <BOOL>]
    This sub-command compiles a LIGO expression to a Michelson value. It works by compiling the LIGO expression to a Michelson expression and then interpreting it using Michelson's interpreter.
    SYNTAX: the syntax that will be used. Currently supported syntaxes are "pascaligo", "cameligo" and "reasonligo". By default, the syntax is guessed from the extension (.ligo, .mligo, .religo, .jsligo respectively).
    _EXPRESSION: the expression that will be compiled.
    --infer: enable type inference
    -p --protocol <PROTOCOL_VERSION>: Choose protocol's types/values pre-loaded into the LIGO environment  (edo ,
      hangzhou). By default, the current protocol (edo) will be used
      Defaults to `current`.
    --init-file <INIT_FILE>: The path to the smart contract file to be used for context initialization.
    --format <DISPLAY-FORMAT>: The format that will be used by the CLI. Available formats are 'dev', 'json', and 'human-readable' (default). When human-readable lacks details (we are still tweaking it), please contact us and use another format in the meanwhile.
      Defaults to `human-readable`.
    --without-run: disable running of compiled expression.
    --michelson-format <MICHELSON_FORMAT>: Is the format that will be used by compile contract for the resulting Michelson. Available formats are 'text' (default), 'json' and 'hex'.
      Defaults to `text`.
    --warn <BOOL>: Indicates whether warning messages should be printed in stderr or not
      Defaults to `true`.
    --werror <BOOL>: Indicates whether warning messages should be treated as errors or not
      Defaults to `false`. |} ] ;
